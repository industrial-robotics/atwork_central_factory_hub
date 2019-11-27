
/***************************************************************************
 *  manager.cpp - Fawkes plugin manager
 *
 *  Created: Wed Nov 15 23:31:55 2006 (on train to Cologne)
 *  Copyright  2006-2009  Tim Niemueller [www.niemueller.de]
 *
 ****************************************************************************/

/*  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version. A runtime exception applies to
 *  this software (see LICENSE.GPL_WRE file mentioned below for details).
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  Read the full text in the LICENSE.GPL_WRE file in the doc directory.
 */

#include <plugin/manager.h>
#include <plugin/listener.h>
#include <plugin/loader.h>

#include <core/plugin.h>
#include <core/threading/thread_collector.h>
#include <core/threading/thread_initializer.h>
#include <core/threading/mutex_locker.h>
#include <core/exception.h>
#include <config/config.h>
#include <utils/system/dynamic_module/module_manager.h>

#include <algorithm>
#include <cstring>
#include <cstdlib>
#include <cerrno>

#include <sys/types.h>
#include <dirent.h>

using llsfrb::ConfigEntryNotFoundException;

namespace fawkes {
#if 0 /* just to make Emacs auto-indent happy */
}
#endif

/// @cond INTERNALS
class plname_eq
{
public:
  plname_eq(std::string name) {
    __name = name;
  }
  bool operator()(Plugin *plugin)
  {
    return (__name == plugin->name());
  }
private:
  std::string __name;
};
/// @endcond INTERNALS

/** @class PluginManager <plugin/manager.h>
 * Fawkes Plugin Manager.
 * This class provides a manager for the plugins used in fawkes. It can
 * load and unload modules.
 *
 * @author Tim Niemueller
 */

/** Constructor.
 * @param thread_collector thread manager plugin threads will be added to
 * and removed from appropriately.
 * @param config Fawkes configuration
 * @param meta_plugin_prefix Path prefix for meta plugins
 * @param module_flags flags to use to open plugin modules
 * @param init_cache true to initialize the plugin cache, false to skip this
 * step. Note that some functions like transmitting a list of available plugins
 * is unavailable until the cache has been initialized. You can defer
 * initialization of the cache if required.
 */
PluginManager::PluginManager(ThreadCollector *thread_collector,
			     Configuration *config,
			     const char *meta_plugin_prefix,
			     Module::ModuleFlags module_flags,
			     bool init_cache)
{
  __mutex = new Mutex();
  this->thread_collector = thread_collector;
  plugin_loader = new PluginLoader(PLUGINDIR, config);
  plugin_loader->get_module_manager()->set_open_flags(module_flags);
  next_plugin_id = 1;
  __config = config;
  __meta_plugin_prefix = meta_plugin_prefix;

  if (init_cache) {
    init_pinfo_cache();
  }
}


/** Destructor. */
PluginManager::~PluginManager()
{
  __pinfo_cache.lock();
  __pinfo_cache.clear();
  __pinfo_cache.unlock();
  // Unload all plugins
  for (rpit = plugins.rbegin(); rpit != plugins.rend(); ++rpit) {
    try {
      thread_collector->force_remove((*rpit)->threads());
    } catch (Exception &e) {
      // We want it to be quiet on destruction, i.e. Fawkes quitting
      //LibLogger::log_warn("PluginManager", "Forced unloading of %s caused exception",
      //		  (*rpit)->name());
      //LibLogger::log_warn("PluginManager", e);
    }
    plugin_loader->unload(*rpit);
  }
  plugins.clear();
  plugin_ids.clear();
  delete plugin_loader;
  delete __mutex;
}


/** Set flags to open modules with.
 * @param flags flags to pass to modules when opening them
 */
void
PluginManager::set_module_flags(Module::ModuleFlags flags)
{
  plugin_loader->get_module_manager()->set_open_flags(flags);
}


/** Initialize plugin info cache. */
void
PluginManager::init_pinfo_cache()
{
  __pinfo_cache.lock();

  DIR *plugin_dir;
  struct dirent* dirp;
  const char *file_ext = "." SOEXT;

  if ( NULL == (plugin_dir = opendir(PLUGINDIR)) ) {
    throw Exception( "Plugin directory %s could not be opened", PLUGINDIR);
  }

  for (unsigned int i = 0; NULL != (dirp = readdir(plugin_dir)); ++i) {
    char *file_name   = dirp->d_name;
    char *pos         = strstr(file_name, file_ext);
    std::string plugin_name = std::string(file_name).substr(0, strlen(file_name) - strlen(file_ext));
    if (NULL != pos) {
      try {
	__pinfo_cache.push_back(make_pair(plugin_name,
					  plugin_loader->get_description(plugin_name.c_str())));
      } catch (Exception &e) {
//	LibLogger::log_warn("PluginManager", "Could not get description of plugin %s, "
//			    "exception follows", plugin_name.c_str());
//	LibLogger::log_warn("PluginManager", e);
      }
    }
  }

  closedir(plugin_dir);

  try {
    Configuration::ValueIterator *i = __config->search(__meta_plugin_prefix.c_str());
    while (i->next()) {
      if (i->is_string()) {
	std::string p = std::string(i->path()).substr(__meta_plugin_prefix.length());
	std::string s = std::string("Meta: ") + i->get_string();

	__pinfo_cache.push_back(make_pair(p, s));
      }
    }
    delete i;
  } catch (Exception &e) {
  }

  __pinfo_cache.sort();
  __pinfo_cache.unlock();
}

/** Generate list of all available plugins.
 * @return list of plugins that are available, each plugin is represented by
 * a pair of strings. The first string is the plugin name, the second is its
 * description.
 */
std::list<std::pair<std::string, std::string> >
PluginManager::get_available_plugins()
{
  std::list<std::pair<std::string, std::string> > rv;

  std::list<std::pair<std::string, std::string> >::iterator i;
  for (i = __pinfo_cache.begin(); i != __pinfo_cache.end(); ++i) {
    rv.push_back(*i);
  }

  return rv;
}

/** Get list of loaded plugins.
 * @return list of names of real and meta plugins currently loaded
 */
std::list<std::string>
PluginManager::get_loaded_plugins()
{
  std::list<std::string> rv;

  plugins.lock();
  for (pit = plugins.begin(); pit != plugins.end(); ++pit) {
    rv.push_back((*pit)->name());
  }
  plugins.unlock();
  __meta_plugins.lock();
  for (__mpit = __meta_plugins.begin(); __mpit != __meta_plugins.end(); ++__mpit) {
    rv.push_back(__mpit->first);
  }
  __meta_plugins.unlock();

  return rv;
}


/** Check if plugin is loaded.
 * @param plugin_name plugin to check if it is loaded
 * @return true if the plugin is currently loaded, false otherwise
 */
bool
PluginManager::is_loaded(const char *plugin_name)
{
  if (plugin_loader->is_loaded(plugin_name)) {
    return true;
  } else {
    // Could still be a meta plugin
    return (__meta_plugins.find(plugin_name) != __meta_plugins.end());
  }
}


/** Parse a list of plugin types.
 * Takes a comma-separated list of plugins and parses them into the individual
 * plugin names.
 * @param plugin_type_list string containing a comma-separated list of plugin types
 * @return parsed list of plugin types
 */
std::list<std::string>
PluginManager::parse_plugin_list(const char *plugin_list)
{
  std::list<std::string> rv;

  char *plugins = strdup(plugin_list);
  char *saveptr;
  char *plugin;

  plugin = strtok_r(plugins, ",", &saveptr);
  while ( plugin ) {
    rv.push_back(plugin);
    plugin = strtok_r(NULL, ",", &saveptr);
  }
  free(plugins);

  return rv;
}


/** Load plugin.
 * The loading is interrupted if any of the plugins does not load properly.
 * The already loaded plugins are *not* unloaded, but kept.
 * @param plugin_list string containing a comma-separated list of plugins
 * to load. The plugin list can contain meta plugins.
 */
void
PluginManager::load(const char *plugin_list)
{
  std::list<std::string> pp = parse_plugin_list(plugin_list);

  for (std::list<std::string>::iterator i = pp.begin(); i != pp.end(); ++i) {
    if ( i->length() == 0 ) continue;

    bool try_real_plugin = true;
    if ( __meta_plugins.find(*i) == __meta_plugins.end() ) {
      std::string meta_plugin = __meta_plugin_prefix + *i;
      try {
	std::string pset = __config->get_string(meta_plugin.c_str());
	if (pset.length() == 0) {
	  throw Exception("Refusing to load an empty meta plugin");
	}
	//printf("Going to load meta plugin %s (%s)\n", i->c_str(), pset.c_str());
	__meta_plugins.lock();
	// Setting has to happen here, so that a meta plugin will not cause an
	// endless loop if it references itself!
	__meta_plugins[*i] = pset;
	__meta_plugins.unlock();
	try {
//	  LibLogger::log_info("PluginManager", "Loading plugins %s for meta plugin %s",
//	                      pset.c_str(), i->c_str());
	  load(pset.c_str());
	  notify_loaded(i->c_str());
	} catch (Exception &e) {
	  e.append("Could not initialize meta plugin %s, aborting loading.", i->c_str());
	  __meta_plugins.erase_locked(*i);
	  throw;
	}

	try_real_plugin = false;
      } catch (ConfigEntryNotFoundException &e) {
	// no meta plugin defined by that name
	//printf("No meta plugin defined with the name %s\n", i->c_str());
	try_real_plugin = true;
      }
    }

    if (try_real_plugin &&
	(find_if(plugins.begin(), plugins.end(), plname_eq(*i)) == plugins.end()))
    {
      try {
	//printf("Going to load real plugin %s\n", i->c_str());
	Plugin *plugin = plugin_loader->load(i->c_str());
	plugins.lock();
	try {
	  thread_collector->add(plugin->threads());
	  plugins.push_back(plugin);
	  plugin_ids[*i] = next_plugin_id++;
	  notify_loaded(i->c_str());
	} catch (CannotInitializeThreadException &e) {
	  e.prepend("Plugin >>> %s <<< could not be initialized, unloading", i->c_str());
	  plugins.unlock();
	  plugin_loader->unload(plugin);
	  throw;
	}
	plugins.unlock();
      } catch (Exception &e) {
	MutexLocker lock(__meta_plugins.mutex());
	if ( __meta_plugins.find(*i) == __meta_plugins.end() ) {
	  // only throw exception if no meta plugin with that name has
	  // already been loaded
	  throw;
	}
      }
    }
  }
}


/** Unload plugin.
 * Note that this method does not allow to pass a list of plugins, but it will
 * only accept a single plugin at a time.
 * @param plugin_name plugin to unload, can be a meta plugin.
 */
void
PluginManager::unload(const char *plugin_name)
{
  MutexLocker lock(plugins.mutex());
  if ( (pit = find_if(plugins.begin(), plugins.end(), plname_eq(plugin_name)))
       != plugins.end()) {
    try {
      thread_collector->remove((*pit)->threads());
      plugin_loader->unload(*pit);
      plugins.erase(pit);
      plugin_ids.erase(plugin_name);
      notify_unloaded(plugin_name);
      // find all meta plugins that required this module, this can no longer
      // be considered loaded
      __meta_plugins.lock();
      __mpit = __meta_plugins.begin();
      while (__mpit != __meta_plugins.end()) {
	std::list<std::string> pp = parse_plugin_list(__mpit->second.c_str());

	bool erase = false;
	for (std::list<std::string>::iterator i = pp.begin(); i != pp.end(); ++i) {
	  if ( *i == plugin_name ) {
	    erase = true;
	    break;
	  }
	}
	if ( erase ) {
	  LockMap< std::string, std::string >::iterator tmp = __mpit;
	  ++__mpit;
	  notify_unloaded(tmp->first.c_str());
	  __meta_plugins.erase(tmp);
	} else {
	  ++__mpit;
	}
      }
      __meta_plugins.unlock();

    } catch (Exception &e) {
//      LibLogger::log_error("PluginManager", "Could not finalize one or more threads of plugin %s, NOT unloading plugin", plugin_name);
      throw;
    }
  } else if (__meta_plugins.find(plugin_name) != __meta_plugins.end()) {
    std::list<std::string> pp = parse_plugin_list(__meta_plugins[plugin_name].c_str());

    for (std::list<std::string>::reverse_iterator i = pp.rbegin(); i != pp.rend(); ++i) {
      if ( i->length() == 0 ) continue;
      if ((find_if(plugins.begin(), plugins.end(), plname_eq(*i)) == plugins.end())
	   && (__meta_plugins.find(*i) != __meta_plugins.end()) ) {
	continue;
      }

      __meta_plugins.erase_locked(*i);
//      LibLogger::log_info("PluginManager", "UNloading plugin %s for meta plugin %s",
//			  i->c_str(), plugin_name);
      unload(i->c_str());
    }
  }
}

/** Add listener.
 * Listeners are notified of plugin load and unloda events.
 * @param listener listener to add
 */
void
PluginManager::add_listener(PluginManagerListener *listener)
{
  __listeners.lock();
  __listeners.push_back(listener);
  __listeners.sort();
  __listeners.unique();
  __listeners.unlock();
}

/** Remove listener.
 * @param listener listener to remove
 */
void
PluginManager::remove_listener(PluginManagerListener *listener)
{
  __listeners.remove_locked(listener);
}

void
PluginManager::notify_loaded(const char *plugin_name)
{
  __listeners.lock();
  for (__lit = __listeners.begin(); __lit != __listeners.end(); ++__lit) {
    try {
      (*__lit)->plugin_loaded(plugin_name);
    } catch (Exception &e) {
//      LibLogger::log_warn("PluginManager", "PluginManagerListener threw exception "
//			  "during notification of plugin loaded, exception follows.");
//      LibLogger::log_warn("PluginManager", e);
    }
  }
  __listeners.unlock();
}

void
PluginManager::notify_unloaded(const char *plugin_name)
{
  __listeners.lock();
  for (__lit = __listeners.begin(); __lit != __listeners.end(); ++__lit) {
    try {
      (*__lit)->plugin_unloaded(plugin_name);
    } catch (Exception &e) {
//      LibLogger::log_warn("PluginManager", "PluginManagerListener threw exception "
//			  "during notification of plugin unloaded, exception follows.");
//      LibLogger::log_warn("PluginManager", e);
    }
  }
  __listeners.unlock();
}


/** Lock plugin manager.
 * This is an utility method that you can use for mutual access to the plugin
 * manager. The mutex is not used internally, but meant to be used from
 * callers.
 */
void
PluginManager::lock()
{
  __mutex->lock();
}


/** Try to lock plugin manager.
 * This is an utility method that you can use for mutual access to the plugin
 * manager. The mutex is not used internally, but meant to be used from
 * callers.
 * @return true if the lock was acquired, false otherwise
 */
bool
PluginManager::try_lock()
{
  return __mutex->try_lock();
}

/** Unlock plugin manager. */
void
PluginManager::unlock()
{
  __mutex->unlock();
}

} // end namespace fawkes

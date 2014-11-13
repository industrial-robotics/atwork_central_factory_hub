
/***************************************************************************
 *  manager.cpp - Fawkes Aspect Manager
 *
 *  Created: Thu Nov 25 00:34:06 2010 (based on inifin.h)
 *  Copyright  2006-2010  Tim Niemueller [www.niemueller.de]
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

#include <aspect/manager.h>
#include <aspect/inifins/clips.h>
#include <aspect/inifins/configurable.h>
#include <aspect/inifins/logging.h>
#include <aspect/inifins/protobuf_comm.h>
#include <aspect/aspect.h>

namespace fawkes {
#if 0 /* just to make Emacs auto-indent happy */
}
#endif

/** @class AspectManager <aspect/manager.h>
 * Aspect and aspect initializer/finalizer manager.
 * This class is the central gatekeeper to aspects for the main application.
 * It manages the initializers/finalizers and thus the aspects which are
 * currently available in the system. It assures that these are not removed
 * before the last thread with an aspect is gone.
 * @author Tim Niemueller
 */

/** Constructor. */
AspectManager::~AspectManager()
{
  std::map<std::string, AspectIniFin *>::iterator i;
  for (i = __default_inifins.begin(); i != __default_inifins.end(); ++i) {
    delete i->second;
  }
  __default_inifins.clear();
}

/** Register initializer/finalizer.
 * @param inifin aspect initializer/finalizer to register
 */
void
AspectManager::register_inifin(AspectIniFin *inifin)
{
  if (__inifins.find(inifin->get_aspect_name()) != __inifins.end()) {
    throw Exception("An initializer for %s has already been registered",
		    inifin->get_aspect_name());
  }
  __inifins[inifin->get_aspect_name()] = inifin;
}

/** Unregister initializer/finalizer.
 * @param inifin aspect initializer/finalizer to unregister
 */
void
AspectManager::unregister_inifin(AspectIniFin *inifin)
{
  if (__inifins.find(inifin->get_aspect_name()) == __inifins.end()) {
    throw Exception("An initializer for %s has not been registered",
		    inifin->get_aspect_name());
  }
  if (! __threads[inifin->get_aspect_name()].empty()) {
    throw Exception("Threads with the %s aspect are still alive, cannot "
		    "unregister the aspect", inifin->get_aspect_name());
  }
  __inifins.erase(inifin->get_aspect_name());
  __threads.erase(inifin->get_aspect_name());
}

/** Check if threads for a particular aspect still exist.
 * @param aspect_name name of the aspect to check for
 * @return true if thread for the given aspect have been registered,
 * false otherwise
 */
bool
AspectManager::has_threads_for_aspect(const char *aspect_name)
{
  return (__threads.find(aspect_name) != __threads.end()) &&
         (! __threads[aspect_name].empty());
}

void
AspectManager::init(Thread *thread)
{
  Aspect *aspected_thread = dynamic_cast<Aspect *>(thread);
  if (aspected_thread != NULL) { // thread has aspects to initialize
    const std::list<const char *> &aspects = aspected_thread->get_aspects();

    std::list<const char *> initialized;

    try {
      std::list<const char *>::const_iterator i;
      for (i = aspects.begin(); i != aspects.end(); ++i) {
	if (__inifins.find(*i) == __inifins.end()) {
	  throw CannotInitializeThreadException("Thread '%s' has the %s, "
						"but no initiliazer is known.",
						thread->name(), *i);
	}
	__inifins[*i]->init(thread);
	initialized.push_back(*i);
      }

      for (i = aspects.begin(); i != aspects.end(); ++i) {
	__threads[*i].push_back(thread);
      }
    } catch (CannotInitializeThreadException &e) {
      std::list<const char *>::const_reverse_iterator i;
      for (i = initialized.rbegin(); i != initialized.rend(); ++i) {
	__inifins[*i]->finalize(thread);
      }
      throw;
    } catch (Exception &e) {
      std::list<const char *>::const_reverse_iterator i;
      for (i = initialized.rbegin(); i != initialized.rend(); ++i) {
	__inifins[*i]->finalize(thread);
      }
      CannotInitializeThreadException ce;
      ce.append(e);
      throw ce;
    }
  }
}

void AspectManager::finalize(Thread *thread)
{
  Aspect *aspected_thread = dynamic_cast<Aspect *>(thread);
  if (aspected_thread != NULL) { // thread has aspects to finalize
    const std::list<const char *> &aspects = aspected_thread->get_aspects();

    std::list<const char *>::const_iterator i;
    for (i = aspects.begin(); i != aspects.end(); ++i) {
      if (__inifins.find(*i) == __inifins.end()) {
	throw CannotFinalizeThreadException("Thread '%s' has the %s, "
					    "but no finalizer is known.",
					    thread->name(), *i);
      }
      __inifins[*i]->finalize(thread);
    }

    // We remove the threads afterwards, because we assume that the plugin
    // will not be unloaded, if the finalization throws an exception.
    for (i = aspects.begin(); i != aspects.end(); ++i) {
      __threads[*i].remove(thread);
    }
  }
}


bool
AspectManager::prepare_finalize(Thread *thread)
{
  Aspect *aspected_thread = dynamic_cast<Aspect *>(thread);
  if (aspected_thread != NULL) { // thread has aspects to finalize
    const std::list<const char *> &aspects = aspected_thread->get_aspects();

    std::list<const char *>::const_iterator i;
    for (i = aspects.begin(); i != aspects.end(); ++i) {
      if (__inifins.find(*i) == __inifins.end()) {
	throw CannotFinalizeThreadException("Thread '%s' has the %s, "
					    "but no finalizer is known.",
					    thread->name(), *i);
      }
      if (!__inifins[*i]->prepare_finalize(thread)) {
	return false;
      }
    }
  }

  return true;
}

/** Register default aspect initializer/finalizer.
 * This loads initializer/finalizer of all aspects which are in the
 * Fawkes aspect library.
 * @param config configuration for ConfigurableAspect
 * @param logger logger for LoggingAspect
 */
void
AspectManager::register_default_inifins(Configuration *config,
					Logger *logger,
					CLIPS::Environment *clips,
					Mutex *clips_mutex,
					protobuf_clips::ClipsProtobufCommunicator *protobuf_comm)
{
  if (! __default_inifins.empty())  return;

  ConfigurableAspectIniFin *conf_aif = new ConfigurableAspectIniFin(config);
  LoggingAspectIniFin *log_aif = new LoggingAspectIniFin(logger);
  CLIPSAspectIniFin *clips_aif = new CLIPSAspectIniFin(clips, clips_mutex);
  ProtobufCommAspectIniFin *protobuf_aif = new ProtobufCommAspectIniFin(protobuf_comm);

  __default_inifins[conf_aif->get_aspect_name()] = conf_aif;
  __default_inifins[log_aif->get_aspect_name()] = log_aif;
  __default_inifins[clips_aif->get_aspect_name()] = clips_aif;
  __default_inifins[protobuf_aif->get_aspect_name()] = protobuf_aif;

  std::map<std::string, AspectIniFin *>::iterator i;
  for (i = __default_inifins.begin(); i != __default_inifins.end(); ++i) {
    __inifins[i->first] = i->second;
  }
}

} // end namespace fawkes

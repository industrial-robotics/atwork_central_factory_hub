
/***************************************************************************
 *  manager.h - Fawkes Aspect Manager
 *
 *  Created: Thu Nov 25 00:27:42 2010 (based on inifin.h)
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

#ifndef __ASPECT_MANAGER_H_
#define __ASPECT_MANAGER_H_

#include <core/threading/thread_initializer.h>
#include <core/threading/thread_finalizer.h>

#include <map>
#include <list>
#include <string>

namespace CLIPS {
  class Environment;
}

namespace llsfrb {
  class Configuration;
  class Logger;
}

namespace protobuf_clips {
  class ClipsProtobufCommunicator;
}

using protobuf_clips::ClipsProtobufCommunicator;
using llsfrb::Configuration;
using llsfrb::Logger;

namespace fawkes {
#if 0 /* just to make Emacs auto-indent happy */
}
#endif

class Thread;
class Mutex;
class AspectIniFin;

namespace tf {
  class Transformer;
}

class AspectManager : public ThreadInitializer, public ThreadFinalizer
{
 public:
  virtual ~AspectManager();

  virtual void init(Thread *thread);
  virtual void finalize(Thread *thread);
  virtual bool prepare_finalize(Thread *thread);

  void register_inifin(AspectIniFin *inifin);
  void unregister_inifin(AspectIniFin *inifin);

  bool has_threads_for_aspect(const char *aspect_name);

  void register_default_inifins(Configuration *config,
				Logger *logger,
				CLIPS::Environment *clips,
				Mutex *clips_mutex,
				protobuf_clips::ClipsProtobufCommunicator *protobuf_comm);

 private:
  std::map<std::string, AspectIniFin *> __inifins;
  std::map<std::string, AspectIniFin *> __default_inifins;
  std::map<std::string, std::list<Thread *> > __threads;
};


} // end namespace fawkes

#endif

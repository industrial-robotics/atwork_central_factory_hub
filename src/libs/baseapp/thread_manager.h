
/***************************************************************************
 *  thread_manager.h - Fawkes thread manager
 *
 *  Created: Thu Nov  3 19:08:23 2006 (on train to Cologne)
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

#ifndef __LIBS_BASEAPP_THREAD_MANAGER_H_
#define __LIBS_BASEAPP_THREAD_MANAGER_H_

#include <core/threading/thread_list.h>
#include <core/threading/thread_collector.h>
#include <core/exception.h>

#include <core/utils/lock_map.h>
#include <list>

namespace fawkes {
#if 0 /* just to make Emacs auto-indent happy */
}
#endif
class Mutex;
class WaitCondition;
class ThreadInitializer;
class ThreadFinalizer;

class ThreadManager
: public ThreadCollector
{
 public:
  ThreadManager();
  ThreadManager(ThreadInitializer *initializer, ThreadFinalizer *finalizer);
  virtual ~ThreadManager();

  void set_inifin(ThreadInitializer *initializer,
		  ThreadFinalizer *finalizer);

  virtual void add(ThreadList &tl)
  {
    add_maybelocked(tl, /* lock */ true);
  }

  virtual void add(Thread *t)
  {
    add_maybelocked(t, /* lock */ true);
  }

  virtual void remove(ThreadList &tl)
  {
    remove_maybelocked(tl, /* lock */ true);
  }

  virtual void remove(Thread *t)
  {
    remove_maybelocked(t, /* lock */ true);
  }

  virtual void force_remove(ThreadList &tl);
  virtual void force_remove(Thread *t);

 private:
  void internal_add_thread(Thread *t);
  void internal_remove_thread(Thread *t);
  void add_maybelocked(ThreadList &tl, bool lock);
  void add_maybelocked(Thread *t, bool lock);
  void remove_maybelocked(ThreadList &tl, bool lock);
  void remove_maybelocked(Thread *t, bool lock);

 private:
  ThreadInitializer *__initializer;
  ThreadFinalizer   *__finalizer;

  ThreadList     __untimed_threads;
  WaitCondition *__waitcond_timedthreads;

  bool __interrupt_timed_thread_wait;

};

} // end namespace fawkes

#endif

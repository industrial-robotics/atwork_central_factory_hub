
/***************************************************************************
 *  thread_manager.cpp - Thread manager
 *
 *  Created: Thu Nov  3 19:11:31 2006 (on train to Cologne)
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

#include <baseapp/thread_manager.h>
#include <core/threading/thread.h>
#include <core/threading/mutex_locker.h>
#include <core/threading/wait_condition.h>
#include <core/threading/thread_initializer.h>
#include <core/threading/thread_finalizer.h>
#include <core/exceptions/software.h>
#include <core/exceptions/system.h>

namespace fawkes {
#if 0 /* just to make Emacs auto-indent happy */
}
#endif

/** @class ThreadManager <baseapp/thread_manager.h>
 * Base application thread manager.
 * This class provides a manager for the threads. Threads are memorized by
 * their wakeup hook. When the thread manager is deleted, all threads are
 * appropriately cancelled, joined and deleted. Thus the thread manager
 * can be used for "garbage collection" of threads.
 *
 * The thread manager allows easy wakeup of threads of a given wakeup hook.
 *
 * The thread manager needs a thread initializer. Each thread that is added
 * to the thread manager is initialized with this. The runtime type information
 * (RTTI) supplied by C++ can be used to initialize threads if appropriate
 * (if the thread has certain aspects that need special treatment).
 *
 * @author Tim Niemueller
 */

/** Constructor.
 * When using this constructor you need to make sure to call set_inifin()
 * before any thread is added.
 */
ThreadManager::ThreadManager()
{
  __initializer = NULL;
  __finalizer   = NULL;
  __waitcond_timedthreads = new WaitCondition();
  __interrupt_timed_thread_wait = false;
}

/** Constructor.
 * This contsructor is equivalent to the one without parameters followed
 * by a call to set_inifins().
 * @param initializer thread initializer
 * @param finalizer thread finalizer
 */
ThreadManager::ThreadManager(ThreadInitializer *initializer,
			     ThreadFinalizer *finalizer)
{
  __initializer = NULL;
  __finalizer   = NULL;
  __waitcond_timedthreads = new WaitCondition();
  __interrupt_timed_thread_wait = false;
  set_inifin(initializer, finalizer);
}


/** Destructor. */
ThreadManager::~ThreadManager()
{
  try {
    __untimed_threads.force_stop(__finalizer);
  } catch (Exception &e) {} // ignore

  delete __waitcond_timedthreads;
}


/** Set initializer/finalizer.
 * This method has to be called before any thread is added/removed.
 * @param initializer thread initializer
 * @param finalizer thread finalizer
 */
void
ThreadManager::set_inifin(ThreadInitializer *initializer, ThreadFinalizer *finalizer)
{
  __initializer = initializer;
  __finalizer   = finalizer;
}


/** Remove the given thread from internal structures.
 * Thread is removed from the internal structures. If the thread has the
 * BlockedTimingAspect then the hook is added to the changed list.
 *
 * @param t thread to remove
 * @param changed list of changed hooks, appropriate hook is added if necessary
 */
void
ThreadManager::internal_remove_thread(Thread *t)
{
  __untimed_threads.remove_locked(t);
}


/** Add the given thread to internal structures.
 * Thread is added to the internal structures. If the thread has the
 * BlockedTimingAspect then the hook is added to the changed list.
 *
 * @param t thread to add
 * @param changed list of changed hooks, appropriate hook is added if necessary
 */
void
ThreadManager::internal_add_thread(Thread *t)
{
  __untimed_threads.push_back_locked(t);
}


/** Add threads.
 * Add the given threads to the thread manager. The threads are initialised
 * as appropriate and started. See the class documentation for supported
 * specialisations of threads and the performed initialisation steps.
 * If the thread initializer cannot initalize one or more threads no thread
 * is added. In this regard the operation is atomic, either all threads are
 * added or none.
 * @param tl thread list with threads to add
 * @exception CannotInitializeThreadException thrown if at least one of the
 * threads could not be initialised
 */
void
ThreadManager::add_maybelocked(ThreadList &tl, bool lock)
{
  if ( ! (__initializer && __finalizer) ) {
    throw NullPointerException("ThreadManager: initializer/finalizer not set");
  }

  if ( tl.sealed() ) {
    throw Exception("Not accepting new threads from list that is not fresh, "
		    "list '%s' already sealed", tl.name());
  }

  tl.lock();

  // Try to initialise all threads
  try {
    tl.init(__initializer, __finalizer);
  } catch (Exception &e) {
    tl.unlock();
    throw;
  }

  tl.seal();
  tl.start();

  // All thread initialized, now add threads to internal structure
  for (ThreadList::iterator i = tl.begin(); i != tl.end(); ++i) {
    internal_add_thread(*i);
  }

  tl.unlock();
}


/** Add one thread.
 * Add the given thread to the thread manager. The thread is initialized
 * as appropriate and started. See the class documentation for supported
 * specialisations of threads and the performed initialisation steps.
 * If the thread initializer cannot initalize the thread it is not added.
 * @param thread thread to add
 * @param lock if true the environment is locked before adding the thread
 * @exception CannotInitializeThreadException thrown if at least the
 * thread could not be initialised
 */
void
ThreadManager::add_maybelocked(Thread *thread, bool lock)
{
  if ( thread == NULL ) {
    throw NullPointerException("FawkesThreadMananger: cannot add NULL as thread");
  }

  if ( ! (__initializer && __finalizer) ) {
    throw NullPointerException("ThreadManager: initializer/finalizer not set");
  }

  try {
    __initializer->init(thread);
  } catch (CannotInitializeThreadException &e) {
    e.append("Adding thread in ThreadManager failed");
    throw;
  }

  // if the thread's init() method fails, we need to finalize that very
  // thread only with the finalizer, already initialized threads muts be
  // fully finalized
  try {
    thread->init();
  } catch (CannotInitializeThreadException &e) {
    __finalizer->finalize(thread);
    throw;
  } catch (Exception &e) {
    CannotInitializeThreadException
      cite("Could not initialize thread '%s'", thread->name());
    cite.append(e);
    __finalizer->finalize(thread);
    throw cite;
  } catch (std::exception &e) {
    CannotInitializeThreadException
      cite("Could not initialize thread '%s'", thread->name());
    cite.append("Caught std::exception or derivative: %s", e.what());
    __finalizer->finalize(thread);
    throw cite;
  } catch (...) {
    CannotInitializeThreadException
      cite("Could not initialize thread '%s'", thread->name());
    cite.append("Unknown exception caught");
    __finalizer->finalize(thread);
    throw cite;
  }

  thread->start();
  internal_add_thread(thread);
}


/** Remove the given threads.
 * The thread manager tries to finalize and stop the threads and then removes the
 * threads from the internal structures.
 *
 * This may fail if at least one thread of the given list cannot be finalized, for
 * example if prepare_finalize() returns false or if the thread finalizer cannot
 * finalize the thread. In this case a CannotFinalizeThreadException is thrown.
 *
 * @param tl threads to remove.
 * @exception CannotFinalizeThreadException At least one thread cannot be safely
 * finalized
 * @exception ThreadListNotSealedException if the given thread lits tl is not
 * sealed the thread manager will refuse to remove it
 */
void
ThreadManager::remove_maybelocked(ThreadList &tl, bool lock)
{
  if ( ! (__initializer && __finalizer) ) {
    throw NullPointerException("ThreadManager: initializer/finalizer not set");
  }


  if ( ! tl.sealed() ) {
    throw ThreadListNotSealedException("(ThreadManager) Cannot remove unsealed thread "
				       "list. Not accepting unsealed list '%s' for removal",
				       tl.name());
  }

  tl.lock();

  try {
    if ( ! tl.prepare_finalize(__finalizer) ) {
      tl.cancel_finalize();
      tl.unlock();
      throw CannotFinalizeThreadException("One or more threads in list '%s' cannot be "
					  "finalized", tl.name());
    }
  } catch (CannotFinalizeThreadException &e) {
    tl.unlock();
    throw;
  } catch (Exception &e) {
    tl.unlock();
    e.append("One or more threads in list '%s' cannot be finalized", tl.name());
    throw CannotFinalizeThreadException(e);
  }

  tl.stop();
  try {
    tl.finalize(__finalizer);
  } catch (Exception &e) {
    tl.unlock();
    throw;
  }

  for (ThreadList::iterator i = tl.begin(); i != tl.end(); ++i) {
    internal_remove_thread(*i);
  }

  tl.unlock();
}


/** Remove the given thread.
 * The thread manager tries to finalize and stop the thread and then removes the
 * thread from the internal structures.
 *
 * This may fail if the thread cannot be finalized, for
 * example if prepare_finalize() returns false or if the thread finalizer cannot
 * finalize the thread. In this case a CannotFinalizeThreadException is thrown.
 *
 * @param thread thread to remove.
 * @exception CannotFinalizeThreadException At least one thread cannot be safely
 * finalized
 */
void
ThreadManager::remove_maybelocked(Thread *thread, bool lock)
{
  if ( thread == NULL ) return;

  if ( ! (__initializer && __finalizer) ) {
    throw NullPointerException("ThreadManager: initializer/finalizer not set");
  }

  try {
    if ( ! thread->prepare_finalize() ) {
      thread->cancel_finalize();
      throw CannotFinalizeThreadException("Thread '%s'cannot be finalized", thread->name());
    }
  } catch (CannotFinalizeThreadException &e) {
    e.append("ThreadManager cannot stop thread '%s'", thread->name());
    thread->cancel_finalize();
    throw;
  }

  thread->cancel();
  thread->join();
  __finalizer->finalize(thread);
  thread->finalize();

  internal_remove_thread(thread);
}




/** Force removal of the given threads.
 * The thread manager tries to finalize and stop the threads and then removes the
 * threads from the internal structures.
 *
 * This will succeed even if a thread of the given list cannot be finalized, for
 * example if prepare_finalize() returns false or if the thread finalizer cannot
 * finalize the thread.
 *
 * <b>Caution, using this function may damage your robot.</b>
 *
 * @param tl threads to remove.
 * @exception ThreadListNotSealedException if the given thread lits tl is not
 * sealed the thread manager will refuse to remove it
 * The threads are removed from thread manager control. The threads will be stopped
 * before they are removed (may cause unpredictable results otherwise).
 */
void
ThreadManager::force_remove(ThreadList &tl)
{
  if ( ! tl.sealed() ) {
    throw ThreadListNotSealedException("Not accepting unsealed list '%s' for removal",
				       tl.name());
  }

  tl.lock();
  bool caught_exception = false;
  Exception exc("Forced removal of thread list %s failed", tl.name());
  try {
    tl.force_stop(__finalizer);
  } catch (Exception &e) {
    caught_exception = true;
    exc = e;
  }

  for (ThreadList::iterator i = tl.begin(); i != tl.end(); ++i) {
    internal_remove_thread(*i);
  }

  tl.unlock();

  if (caught_exception) {
    throw exc;
  }
}


/** Force removal of the given thread.
 * The thread manager tries to finalize and stop the thread and then removes the
 * thread from the internal structures.
 *
 * This will succeed even if the thread cannot be finalized, for
 * example if prepare_finalize() returns false or if the thread finalizer cannot
 * finalize the thread.
 *
 * <b>Caution, using this function may damage your robot.</b>
 *
 * @param thread thread to remove.
 * @exception ThreadListNotSealedException if the given thread lits tl is not
 * sealed the thread manager will refuse to remove it
 * The threads are removed from thread manager control. The threads will be stopped
 * before they are removed (may cause unpredictable results otherwise).
 */
void
ThreadManager::force_remove(fawkes::Thread *thread)
{
  try {
    thread->prepare_finalize();
  } catch (Exception &e) {
    // ignore
  }

  thread->cancel();
  thread->join();
  if (__finalizer) __finalizer->finalize(thread);
  thread->finalize();

  internal_remove_thread(thread);
}

} // end namespace fawkes

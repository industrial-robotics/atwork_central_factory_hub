
/***************************************************************************
 *  clips.cpp - Fawkes CLIPS Aspect initializer/finalizer
 *
 *  Created: Tue Aug 26 Wed 16:15:00 2014
 *  Copyright  2014 Sven Schneider
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

#include <aspect/inifins/clips.h>
#include <aspect/clips.h>

namespace fawkes {
#if 0 /* just to make Emacs auto-indent happy */
}
#endif

/** @class CLIPSAspectIniFin <aspect/inifins/clips.h>
 * Initializer/finalizer for the CLIPSAspect.
 * @author Sven Schneider
 */

/** Constructor.
 * @param clips CLIPS environment instance to pass to threads
 * @param clips_mutex Mutex instance to pass to threads
 */
CLIPSAspectIniFin::CLIPSAspectIniFin(CLIPS::Environment *clips,
    fawkes::Mutex *clips_mutex)
  : AspectIniFin("CLIPSAspect")
{
  clips_ = clips;
  clips_mutex_ = clips_mutex;
}


void
CLIPSAspectIniFin::init(Thread *thread)
{
  CLIPSAspect *clips_thread;
  clips_thread = dynamic_cast<CLIPSAspect *>(thread);
  if (clips_thread == 0) {
    throw CannotInitializeThreadException("Thread '%s' claims to have the "
        "CLIPSAspect, but RTTI says it has not. ", thread->name());
  }

  clips_thread->init_CLIPSAspect(clips_, clips_mutex_);
}


void
CLIPSAspectIniFin::finalize(Thread *thread)
{
}


} // end namespace fawkes

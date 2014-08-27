
/***************************************************************************
 *  clips.cpp - CLIPS aspect for Fawkes
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

#include <aspect/clips.h>

namespace fawkes {
#if 0 /* just to make Emacs auto-indent happy */
}
#endif

/** @class CLIPSAspect <aspect/clips.h>
 * Thread aspect to access a CLIPS environment.
 * Give this aspect to your thread to gain access to the central CLIPS
 * environment.  However, the CLIPS environment must be locked before it can be
 * accessed safely, using the provided mutex.
 *
 * It is guaranteed that if used properly from within plugins that
 * init_CLIPSAspect() is called before the thread is started and that
 * you can access the CLIPS environment via the clips member and the CLIPS
 * environment mutex via the clips_mutex member.
 *
 * @ingroup Aspects
 * @author Sven Schneider
 */


/** @var CLIPS::Environment CLIPSAspect::clips
 * This is the CLIPS::Environment member used to access the CLIPS environment.
 * The environment will remain valid for the whole lifetime of the thread.
 */

/** @var Mutex CLIPSAspect::clips_mutex
 * This is the Mutex member used to access the CLIPS environment mutex.
 * The mutex will remain valid for the whole lifetime of the thread.
 */

/** Constructor. */
CLIPSAspect::CLIPSAspect() : clips(0), clips_mutex(0)
{
  add_aspect("CLIPSAspect");
}


/** Virtual empty Destructor. */
CLIPSAspect::~CLIPSAspect()
{
}


/** Set the CLIPS environment and the environment mutex.
 * It is guaranteed that this is called for a CLIPS thread before
 * Thread::start() is called (when running regularly inside Fawkes).
 * @param clips CLIPS environment instance to use.
 * @param clips_mutex Mutex instance to use.
 */
void
CLIPSAspect::init_CLIPSAspect(CLIPS::Environment *clips, fawkes::Mutex *clips_mutex)
{
  this->clips = clips;
  this->clips_mutex = clips_mutex;
}

} // end namespace fawkes

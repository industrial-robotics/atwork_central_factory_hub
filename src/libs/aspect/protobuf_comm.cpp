/***************************************************************************
 *  protobuf_comm.cpp - Protobuf communication aspect for Fawkes
 *
 *  Created: Nov 12 13:25:12 2014
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

#include <aspect/protobuf_comm.h>

namespace fawkes
{
#if 0 /* just to make Emacs auto-indent happy */
}
#endif

/** @class ProtobufCommAspect <aspect/protobuf_comm.h>
 * Thread aspect to access the protobuf communication.
 * Give this aspect to your thread to gain access to the protobuf communication.
 */

/** Constructor. */
ProtobufCommAspect::ProtobufCommAspect()
{
    add_aspect("ProtobufCommAspect");
}

/** Virtual empty Destructor. */
ProtobufCommAspect::~ProtobufCommAspect()
{
}

/** Set the configuration
 * It is guaranteed that this is called for a thread before
 * Thread::start() is called (when running regularly inside Fawkes).
 * @param protobuf_comm Protobuf communication instance to use.
 */
void ProtobufCommAspect::init_ProtobufCommAspect(ClipsProtobufCommunicator *protobuf_comm)
{
    this->protobuf_comm = protobuf_comm;
}

} // end namespace fawkes

/***************************************************************************
 *  configurable.cpp - Fawkes Protobuf communication Aspect initializer/finalizer
 *
 *  Created: Nov 12 14:48:13 2014
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

#include <aspect/inifins/protobuf_comm.h>
#include <aspect/protobuf_comm.h>
#include <protobuf_clips/communicator.h>

namespace fawkes
{
#if 0 /* just to make Emacs auto-indent happy */
}
#endif

/** @class ProtobufCommAspectIniFin <aspect/inifins/protobuf_comm.h>
 * Initializer/finalizer for the ProtobufCommAspect.
 * @author Frederik Hegger
 */

/** Constructor.
 * @param protobuf_comm protobuf communication instance to pass to threads
 */
ProtobufCommAspectIniFin::ProtobufCommAspectIniFin(ClipsProtobufCommunicator *protobuf_comm) :
        AspectIniFin("ProtobufCommAspect")
{
    __protobuf_comm = protobuf_comm;
}

void ProtobufCommAspectIniFin::init(Thread *thread)
{
    ProtobufCommAspect *protobuf_comm_thread;
    protobuf_comm_thread = dynamic_cast<ProtobufCommAspect *>(thread);
    if (protobuf_comm_thread == NULL)
    {
        throw CannotInitializeThreadException("Thread '%s' claims to have the "
                "ProtobufCommAspect, but RTTI says it "
                "has not. ", thread->name());
    }

    protobuf_comm_thread->init_ProtobufCommAspect(__protobuf_comm);
}

void ProtobufCommAspectIniFin::finalize(Thread *thread)
{
}

} // end namespace fawkes

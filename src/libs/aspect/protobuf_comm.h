/***************************************************************************
 *  protobuf_comm.h - Protobuf communication aspect for Fawkes
 *
 *  Created: Nov 12 13:33:10 2014
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

#ifndef __ASPECT_PROTOBUF_COMM_H_
#define __ASPECT_PROTOBUF_COMM_H_

#include <aspect/aspect.h>
#include <protobuf_clips/communicator.h>

using protobuf_clips::ClipsProtobufCommunicator;

namespace fawkes
{
#if 0 /* just to make Emacs auto-indent happy */
}
#endif

class ProtobufCommAspect : public virtual Aspect
{
    public:
        ProtobufCommAspect();
        virtual ~ProtobufCommAspect();

        void init_ProtobufCommAspect(ClipsProtobufCommunicator *protobuf_comm);

    protected:
        ClipsProtobufCommunicator *protobuf_comm;
};

} // end namespace fawkes

#endif

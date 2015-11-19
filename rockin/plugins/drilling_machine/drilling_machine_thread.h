/***************************************************************************
 *  drilling_machine_thread.h - Thread to communicate with the drilling machine
 *
 *  Created: Mon Nov 11 09:16:11 2014
 *  Copyright  2014 Frederik Hegger
 ****************************************************************************/

/*  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  Read the full text in the LICENSE.GPL file in the doc directory.
 */

#ifndef __PLUGINS_DRILLING_MACHINE_THREAD_H_
#define __PLUGINS_DRILLING_MACHINE_THREAD_H_

#include <core/threading/thread.h>
#include <aspect/logging.h>
#include <aspect/clips.h>
#include <aspect/configurable.h>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <plugins/msgs/DeviceDrillingMachine.pb.h>
#include <zmq.hpp>

class DrillingMachineThread: public fawkes::Thread, public fawkes::LoggingAspect, public fawkes::ConfigurableAspect, public fawkes::CLIPSAspect
{
    public:
        DrillingMachineThread();

        virtual void init();
        virtual void loop();
        virtual void finalize();

    private:
        void clips_move_drill_up();
        void clips_move_drill_down();
        DrillingMachineStatus::State clips_get_device_state();
        int clips_is_device_connected();

        void moveDrill(DrillingMachineCommand::Command drill_command);
        void receiveAndBufferStatusMsg();

    private:
        zmq::context_t *zmq_context_;
        zmq::socket_t *zmq_publisher_;
        zmq::socket_t *zmq_subscriber_;

        unsigned int cfg_timer_interval_;

        DrillingMachineStatus last_status_msg_;
        zmq::message_t zmq_message_;


        boost::posix_time::ptime prev_device_update_timestamp_;
        boost::posix_time::ptime last_sent_command_timestamp_;
};

#endif

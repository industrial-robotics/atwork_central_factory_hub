/***************************************************************************
 *  force_fitting_machine_thread.h - Thread to communicate with the force fitting machine
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

#ifndef __PLUGINS_FORCE_FITTING_MACHINE_THREAD_H_
#define __PLUGINS_FORCE_FITTING_MACHINE_THREAD_H_

#include <core/threading/thread.h>
#include <aspect/logging.h>
#include <aspect/clips.h>
#include <aspect/configurable.h>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <plugins/msgs/DeviceForceFittingMachine.pb.h>
#include <zmq.hpp>

class ForceFittingMachineThread: public fawkes::Thread, public fawkes::LoggingAspect, public fawkes::ConfigurableAspect, public fawkes::CLIPSAspect
{
    public:
        ForceFittingMachineThread();

        virtual void init();
        virtual void loop();
        virtual void finalize();

    private:
        void clips_move_drill_up();
        void clips_move_drill_down();
        ForceFittingMachineStatus::State clips_get_device_state();
        int clips_is_device_connected();

        void moveDrill(ForceFittingMachineCommand::Command drill_command);
        void receiveAndBufferStatusMsg();

    private:
        zmq::context_t *zmq_context_;
        zmq::socket_t *zmq_publisher_;
        zmq::socket_t *zmq_subscriber_;

        unsigned int cfg_timer_interval_;

        ForceFittingMachineStatus last_status_msg_;
        zmq::message_t zmq_message_;

        std::string default_network_interface_;

        boost::posix_time::ptime prev_device_update_timestamp_;
        boost::posix_time::ptime last_sent_command_timestamp_;
};

#endif

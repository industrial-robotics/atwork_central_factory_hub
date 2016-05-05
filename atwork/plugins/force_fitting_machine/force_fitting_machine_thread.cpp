/***************************************************************************
 *  force_fitting_machine_thread.cpp - Thread to communicate with the force fitting machine
 *
 *  Created: Mon Nov 12 09:16:11 2014
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

#include "force_fitting_machine_thread.h"

#include <core/threading/mutex_locker.h>
#include <boost/lexical_cast.hpp>
#include <boost/thread/thread.hpp>

/** @class ForceFittingMachineThread
 * Thread to communicate with @Work force fitting machine.
 * @author Frederik Hegger
 */

/** Constructor. */
ForceFittingMachineThread::ForceFittingMachineThread() :
        Thread("ForceFittingMachineThread", Thread::OPMODE_CONTINUOUS), zmq_context_(NULL), zmq_publisher_(NULL), zmq_subscriber_(NULL), cfg_timer_interval_(40), default_network_interface_("eth0")
{
}

void ForceFittingMachineThread::init()
{
    std::string host_ip_address = "";
    std::string host_command_port = "";
    std::string host_status_port = "";

    last_sent_command_timestamp_ = boost::posix_time::microsec_clock::local_time();

    try
    {
        cfg_timer_interval_ = config->get_uint("/llsfrb/clips/timer-interval");
    } catch (fawkes::Exception &e)
    {
        // use default value
    }

    try
    {
        if (config->get_bool("/llsfrb/force-fitting-machine/enable"))
        {
            if (config->exists("/llsfrb/force-fitting-machine/host") && config->exists("/llsfrb/force-fitting-machine/command_port") && config->exists("/llsfrb/force-fitting-machine/status_port"))
            {
                host_ip_address = config->get_string("/llsfrb/force-fitting-machine/host");
                host_command_port = "epgm://" + default_network_interface_ + ":" + boost::lexical_cast<std::string>(config->get_uint("/llsfrb/force-fitting-machine/command_port"));
                host_status_port = "epgm://" + host_ip_address + ":" + boost::lexical_cast<std::string>(config->get_uint("/llsfrb/force-fitting-machine/status_port"));

                zmq_context_ = new zmq::context_t(1);

                int msg_limit = 1;

                // add publisher to send status messages
                logger->log_info("ForceFittingMachine", "Connecting to the command port: %s", host_command_port.c_str());
                zmq_publisher_ = new zmq::socket_t(*zmq_context_, ZMQ_PUB);
                zmq_publisher_->setsockopt(ZMQ_CONFLATE, &msg_limit, sizeof(msg_limit));
                zmq_publisher_->bind(host_command_port.c_str());

                // add subscriber to receive command messages from a client
                logger->log_info("ForceFittingMachine", "Connecting to the status port: %s", host_status_port.c_str());
                zmq_subscriber_ = new zmq::socket_t(*zmq_context_, ZMQ_SUB);
                zmq_subscriber_->setsockopt(ZMQ_SUBSCRIBE, "", 0);
                zmq_subscriber_->setsockopt(ZMQ_CONFLATE, &msg_limit, sizeof(msg_limit));
                zmq_subscriber_->connect(host_status_port.c_str());
            }
        }
    } catch (fawkes::Exception &e)
    {
        logger->log_warn("ForceFittingMachine", "Cannot create communication for the force fitting machine: %s", e.what());

        delete zmq_context_;
        delete zmq_publisher_;
        delete zmq_subscriber_;

        zmq_context_ = NULL;
        zmq_publisher_ = NULL;
        zmq_subscriber_ = NULL;
    }

    fawkes::MutexLocker lock(clips_mutex);

    clips->add_function("force-fitting-machine-move-drill-up", sigc::slot<void>(sigc::mem_fun(*this, &ForceFittingMachineThread::clips_move_drill_up)));
    clips->add_function("force-fitting-machine-move-drill-down", sigc::slot<void>(sigc::mem_fun(*this, &ForceFittingMachineThread::clips_move_drill_down)));
    clips->add_function("force-fitting-machine-get-state", sigc::slot<int>(sigc::mem_fun(*this, &ForceFittingMachineThread::clips_get_device_state)));
    clips->add_function("force-fitting-machine-is-device-connected", sigc::slot<int>(sigc::mem_fun(*this, &ForceFittingMachineThread::clips_is_device_connected)));

    if (!clips->build("(deffacts have-feature-force-fitting-machine (have-feature ForceFittingMachine))"))
        logger->log_warn("ForceFittingMachine", "Failed to build deffacts have-feature-force-fitting-machine");
}

void ForceFittingMachineThread::finalize()
{
    delete zmq_context_;
    delete zmq_publisher_;
    delete zmq_subscriber_;

    zmq_context_ = NULL;
    zmq_publisher_ = NULL;
    zmq_subscriber_ = NULL;
}

void ForceFittingMachineThread::loop()
{
    receiveAndBufferStatusMsg();

    boost::this_thread::sleep(boost::posix_time::milliseconds(cfg_timer_interval_));
}

ForceFittingMachineStatus::State ForceFittingMachineThread::clips_get_device_state()
{
    if (last_status_msg_.has_state())
    {
        return last_status_msg_.state();
    }

    return ForceFittingMachineStatus::UNKNOWN;
}

int ForceFittingMachineThread::clips_is_device_connected()
{
    return (last_status_msg_.has_is_device_connected() && last_status_msg_.is_device_connected());
}

void ForceFittingMachineThread::clips_move_drill_up()
{
    moveDrill(ForceFittingMachineCommand::MOVE_UP);
}

void ForceFittingMachineThread::clips_move_drill_down()
{
    moveDrill(ForceFittingMachineCommand::MOVE_DOWN);
}

void ForceFittingMachineThread::moveDrill(ForceFittingMachineCommand::Command drill_command)
{
    boost::posix_time::time_duration time_diff;
    ForceFittingMachineCommand command_msg;
    std::string serialized_string;

    if (!zmq_publisher_)
        return;

    // prevent sending to many messages to the device
    time_diff = boost::posix_time::microsec_clock::local_time() - last_sent_command_timestamp_;
    if (time_diff.total_milliseconds() < 200)
        return;


    command_msg.set_command(drill_command);

    zmq::message_t *query = NULL;
    try
    {
        command_msg.SerializeToString(&serialized_string);
        query = new zmq::message_t(serialized_string.length());
        memcpy(query->data(), serialized_string.c_str(), serialized_string.length());
        zmq_publisher_->send(*query);

        last_sent_command_timestamp_ = boost::posix_time::microsec_clock::local_time();

        logger->log_info("ForceFittingMachine", "Send drill command: %d", command_msg.command());

    } catch (fawkes::Exception &e)
    {
        logger->log_warn("ForceFittingMachine", "Failed to send force fitting command: %s", e.what());

        if (query != NULL)
            delete query;
    }
}

void ForceFittingMachineThread::receiveAndBufferStatusMsg()
{
    if (!zmq_subscriber_)
        return;

    fawkes::MutexLocker lock(clips_mutex);

    if (zmq_subscriber_->recv(&zmq_message_, ZMQ_NOBLOCK))
    {
        last_status_msg_.ParseFromArray(zmq_message_.data(), zmq_message_.size());

        // remember time of last received msg
        prev_device_update_timestamp_ = boost::posix_time::microsec_clock::local_time();
    } else
    {
        boost::posix_time::time_duration time_diff = boost::posix_time::microsec_clock::local_time() - prev_device_update_timestamp_;

        if (time_diff.total_seconds() >= 3)
        {
            last_status_msg_.set_state(ForceFittingMachineStatus::UNKNOWN);
            last_status_msg_.set_is_device_connected(false);
        }
    }
}

/***************************************************************************
 *  drilling_machine_thread.cpp - Thread to communicate with the drilling machine
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

#include "drilling_machine_thread.h"

#include <core/threading/mutex_locker.h>
#include <boost/lexical_cast.hpp>
#include <boost/thread/thread.hpp>

/** @class DrillingMachineThread
 * Thread to communicate with @Work drilling machine.
 * @author Frederik Hegger
 */

/** Constructor. */
DrillingMachineThread::DrillingMachineThread() :
        Thread("DrillingMachineThread", Thread::OPMODE_CONTINUOUS), zmq_context_(NULL), zmq_publisher_(NULL), zmq_subscriber_(NULL), cfg_timer_interval_(40)
{
}

void DrillingMachineThread::init()
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
        if (config->exists("/llsfrb/drilling-machine/host") && config->exists("/llsfrb/drilling-machine/command_port") && config->exists("/llsfrb/drilling-machine/status_port"))
        {
            host_ip_address = config->get_string("/llsfrb/drilling-machine/host");
            host_command_port = "epgm://" + host_ip_address + ":" + boost::lexical_cast<std::string>(config->get_uint("/llsfrb/drilling-machine/command_port"));
            host_status_port = "epgm://" + host_ip_address + ":" + boost::lexical_cast<std::string>(config->get_uint("/llsfrb/drilling-machine/status_port"));

            zmq_context_ = new zmq::context_t(1);

            int msg_limit = 1;

            // add publisher to send status messages
            logger->log_info("DrillingMachine", "Connecting to the command port: %s", host_command_port.c_str());
            zmq_publisher_ = new zmq::socket_t(*zmq_context_, ZMQ_PUB);
            zmq_publisher_->setsockopt(ZMQ_CONFLATE, &msg_limit, sizeof(msg_limit));
            zmq_publisher_->bind(host_command_port.c_str());

            // add subscriber to receive command messages from a client
            logger->log_info("DrillingMachine", "Connecting to the status port: %s", host_status_port.c_str());
            zmq_subscriber_ = new zmq::socket_t(*zmq_context_, ZMQ_SUB);
            zmq_subscriber_->setsockopt(ZMQ_SUBSCRIBE, "", 0);
            zmq_subscriber_->setsockopt(ZMQ_CONFLATE, &msg_limit, sizeof(msg_limit));
            zmq_subscriber_->connect(host_status_port.c_str());
        }
    } catch (fawkes::Exception &e)
    {
        logger->log_warn("DrillingMachine", "Cannot create communication for the drilling machine: %s", e.what());

        delete zmq_context_;
        delete zmq_publisher_;
        delete zmq_subscriber_;

        zmq_context_ = NULL;
        zmq_publisher_ = NULL;
        zmq_subscriber_ = NULL;
    }

    fawkes::MutexLocker lock(clips_mutex);

    clips->add_function("drilling-machine-move-drill-up", sigc::slot<void>(sigc::mem_fun(*this, &DrillingMachineThread::clips_move_drill_up)));
    clips->add_function("drilling-machine-move-drill-down", sigc::slot<void>(sigc::mem_fun(*this, &DrillingMachineThread::clips_move_drill_down)));
    clips->add_function("drilling-machine-get-state", sigc::slot<int>(sigc::mem_fun(*this, &DrillingMachineThread::clips_get_device_state)));
    clips->add_function("drilling-machine-is-device-connected", sigc::slot<int>(sigc::mem_fun(*this, &DrillingMachineThread::clips_is_device_connected)));

    if (!clips->build("(deffacts have-feature-drilling-machine (have-feature DrillingMachine))"))
        logger->log_warn("DrillingMachine", "Failed to build deffacts have-feature-drilling-machine");
}

void DrillingMachineThread::finalize()
{
    delete zmq_context_;
    delete zmq_publisher_;
    delete zmq_subscriber_;

    zmq_context_ = NULL;
    zmq_publisher_ = NULL;
    zmq_subscriber_ = NULL;
}

void DrillingMachineThread::loop()
{
    receiveAndBufferStatusMsg();

    boost::this_thread::sleep(boost::posix_time::milliseconds(cfg_timer_interval_));
}

DrillingMachineStatus::State DrillingMachineThread::clips_get_device_state()
{
    fawkes::MutexLocker lock(clips_mutex);

    if (last_status_msg_.has_state())
    {
        return last_status_msg_.state();
    }

    return DrillingMachineStatus::UNKNOWN;
}

int DrillingMachineThread::clips_is_device_connected()
{
    fawkes::MutexLocker lock(clips_mutex);
    return (last_status_msg_.has_is_device_connected() && last_status_msg_.is_device_connected());
}

void DrillingMachineThread::clips_move_drill_up()
{
    moveDrill(DrillingMachineCommand::MOVE_UP);
}

void DrillingMachineThread::clips_move_drill_down()
{
    moveDrill(DrillingMachineCommand::MOVE_DOWN);
}

void DrillingMachineThread::moveDrill(DrillingMachineCommand::Command drill_command)
{
    boost::posix_time::time_duration time_diff;
    DrillingMachineCommand command_msg;
    std::string serialized_string;

    if (!zmq_publisher_)
        return;

    fawkes::MutexLocker lock(clips_mutex);
    // prevent sending to many messages to the device
    time_diff = boost::posix_time::microsec_clock::local_time() - last_sent_command_timestamp_;
    if (time_diff.total_milliseconds() < 1000)
        return;
        
    if(last_status_msg_.state() == DrillingMachineStatus::MOVING_UP || last_status_msg_.state() == DrillingMachineStatus::MOVING_DOWN)
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

    } catch (fawkes::Exception &e)
    {
        logger->log_debug("DrillingMachine", "Failed to send drilling command: %s", e.what());

        if (query != NULL)
            delete query;
    }
}

void DrillingMachineThread::receiveAndBufferStatusMsg()
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
            last_status_msg_.set_state(DrillingMachineStatus::UNKNOWN);
            last_status_msg_.set_is_device_connected(false);
        }
    }
}

/***************************************************************************
 *  conveyor_belt_thread.cpp - Thread to communicate with the conveyor belt
 *
 *  Created: Mon Oct 06 16:39:11 2014
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

#include "conveyor_belt_thread.h"

#include <core/threading/mutex_locker.h>
#include <boost/lexical_cast.hpp>
#include <boost/thread/thread.hpp>

/** @class ConveyorBeltThread
 * Thread to communicate with @Work conveyor belt.
 * @author Frederik Hegger
 */

/** Constructor. */
ConveyorBeltThread::ConveyorBeltThread() :
        Thread("ConveyorBeltThread", Thread::OPMODE_CONTINUOUS), zmq_context_(NULL), zmq_publisher_(NULL), zmq_subscriber_(NULL), cfg_timer_interval_(40), default_network_interface_("eth0")
{
}

void ConveyorBeltThread::init()
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
        if (config->get_bool("/llsfrb/conveyor-belt/enable"))
        {
            if (config->exists("/llsfrb/conveyor-belt/host") && config->exists("/llsfrb/conveyor-belt/command_port") && config->exists("/llsfrb/conveyor-belt/status_port"))
            {
                host_ip_address = config->get_string("/llsfrb/conveyor-belt/host");
                host_command_port = "epgm://" + default_network_interface_ + ":" + boost::lexical_cast<std::string>(config->get_uint("/llsfrb/conveyor-belt/command_port"));
                host_status_port = "epgm://" + host_ip_address + ":" + boost::lexical_cast<std::string>(config->get_uint("/llsfrb/conveyor-belt/status_port"));

                zmq_context_ = new zmq::context_t(1);
                int msg_limit = 1;

                // add publisher to send status messages
                logger->log_info("ConveyorBelt", "Connecting to the command port: %s", host_command_port.c_str());
                zmq_publisher_ = new zmq::socket_t(*zmq_context_, ZMQ_PUB);
                zmq_publisher_->setsockopt(ZMQ_CONFLATE, &msg_limit, sizeof(msg_limit));
                zmq_publisher_->bind(host_command_port.c_str());

                // add subscriber to receive command messages from a client
                logger->log_info("ConveyorBelt", "Connecting to the status port: %s", host_status_port.c_str());
                zmq_subscriber_ = new zmq::socket_t(*zmq_context_, ZMQ_SUB);
                zmq_subscriber_->setsockopt(ZMQ_SUBSCRIBE, "", 0);
                zmq_subscriber_->setsockopt(ZMQ_CONFLATE, &msg_limit, sizeof(msg_limit));
                zmq_subscriber_->connect(host_status_port.c_str());
            }
        }
    } catch (std::exception &e)
    {
        logger->log_warn("ConveyorBelt", "Cannot create communication for the conveyor belt: %s", e.what());

        delete zmq_context_;
        delete zmq_publisher_;
        delete zmq_subscriber_;

        zmq_context_ = NULL;
        zmq_publisher_ = NULL;
        zmq_subscriber_ = NULL;
    }

    fawkes::MutexLocker lock(clips_mutex);

    clips->add_function("conveyor-belt-start-belt", sigc::slot<void>(sigc::mem_fun(*this, &ConveyorBeltThread::clips_start_belt)));
    clips->add_function("conveyor-belt-stop-belt", sigc::slot<void>(sigc::mem_fun(*this, &ConveyorBeltThread::clips_stop_belt)));
    clips->add_function("conveyor-belt-is-running", sigc::slot<bool>(sigc::mem_fun(*this, &ConveyorBeltThread::clips_is_belt_running)));
    clips->add_function("conveyor-belt-is-device-connected", sigc::slot<bool>(sigc::mem_fun(*this, &ConveyorBeltThread::clips_is_device_connected)));

    if (!clips->build("(deffacts have-feature-conveyor-belt (have-feature ConveyorBelt))"))
        logger->log_warn("ConveyorBelt", "Failed to build deffacts have-feature-conveyor-belt");
}

void ConveyorBeltThread::finalize()
{
    delete zmq_context_;
    delete zmq_publisher_;
    delete zmq_subscriber_;

    zmq_context_ = NULL;
    zmq_publisher_ = NULL;
    zmq_subscriber_ = NULL;
}

void ConveyorBeltThread::loop()
{
    receiveAndBufferStatusMsg();

    boost::this_thread::sleep(boost::posix_time::milliseconds(cfg_timer_interval_));
}

bool ConveyorBeltThread::clips_is_belt_running()
{
    if (last_status_msg_.has_mode() && (last_status_msg_.mode() == START))
        return true;
    else
        return false;
}

bool ConveyorBeltThread::clips_is_device_connected()
{
    if (last_status_msg_.has_is_device_connected() && last_status_msg_.is_device_connected())
        return true;
    else
        return false;
}

void ConveyorBeltThread::clips_start_belt()
{
    setConveyorBeltRunMode(START);
}

void ConveyorBeltThread::clips_stop_belt()
{
    setConveyorBeltRunMode(STOP);
}

void ConveyorBeltThread::setConveyorBeltRunMode(RunMode mode)
{
    boost::posix_time::time_duration time_diff;
    ConveyorBeltCommand command_msg;
    std::string serialized_string;

    if (!zmq_publisher_)
        return;

    // prevent sending to many messages to the device
    time_diff = boost::posix_time::microsec_clock::local_time() - last_sent_command_timestamp_;
    if (time_diff.total_milliseconds() < 1000)
        return;

    command_msg.set_mode(mode);

    zmq::message_t *query = NULL;
    try
    {
        command_msg.SerializeToString(&serialized_string);
        query = new zmq::message_t(serialized_string.length());
        memcpy(query->data(), serialized_string.c_str(), serialized_string.length());
        zmq_publisher_->send(*query);

        last_sent_command_timestamp_ = boost::posix_time::microsec_clock::local_time();

        logger->log_info("ConveyorBelt", "Set run state: %d", command_msg.mode());

        delete query;

    } catch (fawkes::Exception &e)
    {
        logger->log_warn("ConveyorBelt", "Failed to set run state: %s", e.what());

        if (query != NULL)
            delete query;
    }
}

void ConveyorBeltThread::receiveAndBufferStatusMsg()
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
            last_status_msg_.set_mode(STOP);
            last_status_msg_.set_is_device_connected(false);
        }
    }
}

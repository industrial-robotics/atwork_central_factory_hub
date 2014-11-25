/***************************************************************************
 *  quality_control_camera_thread.cpp - Thread to communicate with the quality
 *                                      control camera
 *
 *  Created: Mon Nov 12 10:26:11 2014
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

#include "quality_control_camera_thread.h"

#include <core/threading/mutex_locker.h>
#include <boost/lexical_cast.hpp>
#include <boost/thread/thread.hpp>
#include <protobuf_comm/peer.h>

/** @class QualityControlCameraThread
 * Thread to communicate with @Work quality control camera.
 * @author Frederik Hegger
 */

/** Constructor. */
QualityControlCameraThread::QualityControlCameraThread() :
        Thread("QualityControlCameraThread", Thread::OPMODE_CONTINUOUS), zmq_context_(NULL), zmq_service_(NULL), zmq_subscriber_(NULL), cfg_timer_interval_(40), default_network_interface_("eth0")
{
}

void QualityControlCameraThread::init()
{
    std::string host_ip_address = "";
    std::string host_service_port = "";
    std::string host_status_port = "";

    try
    {
        cfg_timer_interval_ = config->get_uint("/llsfrb/clips/timer-interval");
    } catch (fawkes::Exception &e)
    {
        // use default value
    }

    try
    {
        if (config->get_bool("/llsfrb/quality-control-camera/enable"))
        {
            if (config->exists("/llsfrb/quality-control-camera/host") && config->exists("/llsfrb/quality-control-camera/service_port") && config->exists("/llsfrb/quality-control-camera/status_port"))
            {
                host_ip_address = config->get_string("/llsfrb/quality-control-camera/host");
                host_service_port = "tcp://" + host_ip_address + ":" + boost::lexical_cast<std::string>(config->get_uint("/llsfrb/quality-control-camera/service_port"));
                host_status_port = "tcp://" + host_ip_address + ":" + boost::lexical_cast<std::string>(config->get_uint("/llsfrb/quality-control-camera/status_port"));

                zmq_context_ = new zmq::context_t(1);

                // add publisher to send status messages
                logger->log_info("QualityControlCamera", "Connecting to the service port: %s", host_service_port.c_str());
                zmq_service_ = new zmq::socket_t(*zmq_context_, ZMQ_REQ);
                zmq_service_->connect(host_service_port.c_str());

                // add subscriber to receive command messages from a client
                logger->log_info("QualityControlCamera", "Connecting to the status port: %s", host_status_port.c_str());
                zmq_subscriber_ = new zmq::socket_t(*zmq_context_, ZMQ_SUB);
                zmq_subscriber_->setsockopt(ZMQ_SUBSCRIBE, "", 0);
                zmq_subscriber_->connect(host_status_port.c_str());
            }
        }
    } catch (fawkes::Exception &e)
    {
        logger->log_warn("QualityControlCamera", "Cannot create communication for the quality control camera: %s", e.what());

        delete zmq_context_;
        delete zmq_service_;
        delete zmq_subscriber_;

        zmq_context_ = NULL;
        zmq_service_ = NULL;
        zmq_subscriber_ = NULL;
    }

    fawkes::MutexLocker lock(clips_mutex);

    clips->add_function("quality-control-camera-send-image-to-peer", sigc::slot<void, long int>(sigc::mem_fun(*this, &QualityControlCameraThread::clips_send_image_to_peer)));
    clips->add_function("quality-control-camera-is-device-connected", sigc::slot<int>(sigc::mem_fun(*this, &QualityControlCameraThread::clips_is_device_connected)));

    if (!clips->build("(deffacts have-feature-quality-control-camera (have-feature QualityControlCamera))"))
        logger->log_warn("QualityControlCamera", "Failed to build deffacts have-feature-quality-control-camera");
}

void QualityControlCameraThread::finalize()
{
    delete zmq_context_;
    delete zmq_service_;
    delete zmq_subscriber_;

    zmq_context_ = NULL;
    zmq_service_ = NULL;
    zmq_subscriber_ = NULL;
}

void QualityControlCameraThread::loop()
{
    receiveAndBufferStatusMsg();

    boost::this_thread::sleep(boost::posix_time::milliseconds(cfg_timer_interval_));
}

int QualityControlCameraThread::clips_is_device_connected()
{
    if (last_status_msg_.has_is_device_connected() && last_status_msg_.is_device_connected())
        return true;
    else
        return false;
}

void QualityControlCameraThread::clips_send_image_to_peer(long int peer_id)
{
    if (!zmq_service_)
        return;

    ImageRequest image_request;
    Image image_msg;
    std::string serialized_string;

    zmq::message_t *query = NULL;
    zmq::message_t request;
    try
    {
        image_request.SerializeToString(&serialized_string);
        query = new zmq::message_t(serialized_string.length());
        memcpy(query->data(), serialized_string.c_str(), serialized_string.length());

        zmq_service_->send(*query);

        logger->log_info("QualityControlCamera", "Send image request");

        if (zmq_service_->recv(&request) && image_msg.ParseFromArray(request.data(), request.size()))
        {
            logger->log_info("QualityControlCamera", "Image received -> width: %d, height: %d", image_msg.width(), image_msg.height());

            std::map<long int, protobuf_comm::ProtobufBroadcastPeer *> peers = protobuf_comm->peers();
            if (peers.find(peer_id) != peers.end())
                peers[peer_id]->send(device_image_to_image(image_msg));
            else
                logger->log_error("QualityControlCamera", "Could not sent image to peer. Peer %d does not exist.", peer_id);
        }

        delete query;

    } catch (std::exception &e)
    {
        logger->log_error("QualityControlCamera", "Could not send image to peer %d: %s", peer_id, e.what());

        if (query != NULL)
            delete query;
    }
}

void QualityControlCameraThread::receiveAndBufferStatusMsg()
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
            last_status_msg_.set_is_device_connected(false);
    }
}

std::shared_ptr<google::protobuf::Message> QualityControlCameraThread::device_image_to_image(const Image &img) const
{
  // We cannot link to the rockin_msgs directly, due to problems with the
  // static initialization of messages. Therefore, we create the message using
  // dynamic compilation and reflection to set up the message's fields.
  std::string msg_name = "rockin_msgs.Image";
  std::shared_ptr<google::protobuf::Message> msg = protobuf_comm->message_register().new_message_for(msg_name);

  const google::protobuf::Descriptor *desc = msg->GetDescriptor();
  const google::protobuf::Reflection *refl = msg->GetReflection();
  const google::protobuf::FieldDescriptor* fd_height = desc->FindFieldByName("height");
  const google::protobuf::FieldDescriptor* fd_width = desc->FindFieldByName("width");
  const google::protobuf::FieldDescriptor* fd_encoding = desc->FindFieldByName("encoding");
  const google::protobuf::FieldDescriptor* fd_step = desc->FindFieldByName("step");
  const google::protobuf::FieldDescriptor* fd_data = desc->FindFieldByName("data");
  refl->SetUInt32(msg.get(), fd_height, img.height());
  refl->SetUInt32(msg.get(), fd_width, img.width());
  refl->SetUInt32(msg.get(), fd_encoding, img.encoding());
  refl->SetUInt32(msg.get(), fd_step, img.step());
  refl->SetString(msg.get(), fd_data, img.data());

  return msg;
}

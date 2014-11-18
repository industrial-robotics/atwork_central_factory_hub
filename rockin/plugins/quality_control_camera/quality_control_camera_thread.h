/***************************************************************************
 *  quality_control_camera_thread.h - Thread to communicate with the quality
 *                                    control camera
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

#ifndef __PLUGINS_QUALITY_CONTROL_CAMERA_THREAD_H_
#define __PLUGINS_QUALITY_CONTROL_CAMERA_THREAD_H_

#include <core/threading/thread.h>
#include <aspect/logging.h>
#include <aspect/clips.h>
#include <aspect/configurable.h>
#include <aspect/protobuf_comm.h>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <plugins/msgs/DeviceCamera.pb.h>
#include <plugins/msgs/DeviceImage.pb.h>
#include <msgs/Image.pb.h>
#include <zmq.hpp>

class QualityControlCameraThread: public fawkes::Thread, public fawkes::LoggingAspect, public fawkes::ConfigurableAspect, public fawkes::CLIPSAspect, public fawkes::ProtobufCommAspect
{
    public:
        QualityControlCameraThread();

        virtual void init();
        virtual void loop();
        virtual void finalize();

    private:
        void clips_send_image_to_peer(long int peer_id);
        int clips_is_device_connected();
        std::shared_ptr<google::protobuf::Message> device_image_to_image(const Image &img) const;

        void receiveAndBufferStatusMsg();

        zmq::context_t *zmq_context_;
        zmq::socket_t *zmq_service_;
        zmq::socket_t *zmq_subscriber_;

        unsigned int cfg_timer_interval_;

        CameraStatus last_status_msg_;
        zmq::message_t zmq_message_;

        std::string default_network_interface_;

        boost::posix_time::ptime prev_device_update_timestamp_;
};

#endif

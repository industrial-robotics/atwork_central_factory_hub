/***************************************************************************
 *  triggered_conveyor_belt_simulation_thread.h - Thread to communicate with the
 *                                                conveyor belt and quality
 *                                                control camera.
 *
 *  Created: Mon Oct 12 15:39:11 2015
 *  Copyright  2015 Sven Schneider
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

#ifndef __PLUGINS_TRIGGERED_CONVEYOR_BELT_SIMULATION_THREAD_H_
#define __PLUGINS_TRIGGERED_CONVEYOR_BELT_SIMULATION_THREAD_H_

#include <core/threading/thread.h>
#include <aspect/logging.h>
#include <aspect/clips.h>
#include <aspect/configurable.h>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <random>
#include <zmq.hpp>

class TriggeredConveyorBeltSimulationThread : public fawkes::Thread, public fawkes::LoggingAspect, public fawkes::ConfigurableAspect, public fawkes::CLIPSAspect
{
    public:
        TriggeredConveyorBeltSimulationThread();

        virtual void init();
        virtual void loop();
        virtual void finalize();

    private:
        void clips_start_belt();
        void clips_stop_belt();
        bool clips_is_belt_running();
        bool clips_is_belt_connected();
        bool clips_is_camera_connected();

    private:
        unsigned int cfg_timer_interval_;
        bool is_conveyor_belt_running_;

        std::default_random_engine random_generator_;

        boost::posix_time::ptime time_to_stop_;
};

#endif

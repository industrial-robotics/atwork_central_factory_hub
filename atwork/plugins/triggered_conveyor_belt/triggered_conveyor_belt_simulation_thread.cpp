/***************************************************************************
 *  triggered_conveyor_belt_simulation_thread.cpp - Thread to communicate with
 *                                                  the conveyor belt and
 *                                                  quality control camera.
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

#include "triggered_conveyor_belt_simulation_thread.h"

#include <core/threading/mutex_locker.h>
#include <boost/lexical_cast.hpp>
#include <boost/thread/thread.hpp>

/** @class TriggeredConveyorBeltSimulationThread
 * Thread to communicate with the @Work conveyor belt and quality control
 * camera.
 *
 * @author Sven Schneider
 */

TriggeredConveyorBeltSimulationThread::TriggeredConveyorBeltSimulationThread() :
        Thread("TriggeredConveyorBeltSimulationThread", Thread::OPMODE_CONTINUOUS),
        cfg_timer_interval_(40), is_conveyor_belt_running_(false)
{
}

void TriggeredConveyorBeltSimulationThread::init()
{
    try
    {
        cfg_timer_interval_ = config->get_uint("/llsfrb/clips/timer-interval");
    } catch (fawkes::Exception &e)
    {
        // use default value
    }

    fawkes::MutexLocker lock(clips_mutex);

    clips->add_function("conveyor-belt-start-belt", sigc::slot<void>(sigc::mem_fun(*this, &TriggeredConveyorBeltSimulationThread::clips_start_belt)));
    clips->add_function("conveyor-belt-stop-belt", sigc::slot<void>(sigc::mem_fun(*this, &TriggeredConveyorBeltSimulationThread::clips_stop_belt)));
    clips->add_function("conveyor-belt-is-running", sigc::slot<bool>(sigc::mem_fun(*this, &TriggeredConveyorBeltSimulationThread::clips_is_belt_running)));
    clips->add_function("conveyor-belt-is-device-connected", sigc::slot<bool>(sigc::mem_fun(*this, &TriggeredConveyorBeltSimulationThread::clips_is_belt_connected)));

    if (!clips->build("(deffacts have-feature-conveyor-belt (have-feature ConveyorBelt))"))
        logger->log_warn("TriggeredConveyorBelt", "Failed to build deffacts have-feature-conveyor-belt");


    clips->add_function("quality-control-camera-is-device-connected", sigc::slot<bool>(sigc::mem_fun(*this, &TriggeredConveyorBeltSimulationThread::clips_is_camera_connected)));

    if (!clips->build("(deffacts have-feature-quality-control-camera (have-feature QualityControlCamera))"))
        logger->log_warn("TriggeredConveyorBelt", "Failed to build deffacts have-feature-quality-control-camera");
}

void TriggeredConveyorBeltSimulationThread::finalize()
{
}

void TriggeredConveyorBeltSimulationThread::loop()
{
    // unconditional sleep
    boost::this_thread::sleep(boost::posix_time::milliseconds(cfg_timer_interval_));

    if (!is_conveyor_belt_running_) return;

    // check if the simulated conveyor belt should keep on running
    boost::posix_time::ptime now = boost::posix_time::microsec_clock::local_time();
    if (now < time_to_stop_) return;


    // stop the conveyor belt
    is_conveyor_belt_running_ = false;

    // randomly select a plate
    std::uniform_int_distribution<int> plate_selection_distribution(2, 3);
    std::string detected_plate;
    switch (plate_selection_distribution(random_generator_)) {
        case 0: detected_plate = "NO_PLATE"; break;
        case 1: detected_plate = "UNKNOWN_PLATE"; break;
        case 2: detected_plate = "FAULTY_PLATE"; break;
        case 3: detected_plate = "UNUSABLE_PLATE"; break;
        default: detected_plate = "UNKNOWN_PLATE"; break;
    }

    // let CLIPS know about the plate by asserting it as a fact
    std::stringstream sstr;
    sstr << "(quality-control-camera-object " << detected_plate << ")";

    fawkes::MutexLocker lock(clips_mutex);
    clips->assert_fact(sstr.str());

}

bool TriggeredConveyorBeltSimulationThread::clips_is_belt_running()
{
    return is_conveyor_belt_running_;
}

bool TriggeredConveyorBeltSimulationThread::clips_is_belt_connected()
{
    return true;
}

bool TriggeredConveyorBeltSimulationThread::clips_is_camera_connected()
{
    return true;
}

void TriggeredConveyorBeltSimulationThread::clips_start_belt()
{
    is_conveyor_belt_running_ = true;

    // generate a random time between 1 and 10 seconds
    std::uniform_real_distribution<double> stop_time_distribution(1.0, 10.0);
    double random_seconds = stop_time_distribution(random_generator_);

    time_to_stop_ = boost::posix_time::microsec_clock::local_time()
                    + boost::posix_time::seconds(random_seconds);
}

void TriggeredConveyorBeltSimulationThread::clips_stop_belt()
{
    is_conveyor_belt_running_ = false;
}

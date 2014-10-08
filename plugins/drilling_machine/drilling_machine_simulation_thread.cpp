/***************************************************************************
 *  drilling_machine_simulation_thread.cpp - Thread to simulate the drilling machine
 *
 *  Created: Mon Oct 5 12:25:11 2015
 *  Copyright  2015 Alexander Moriarty
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

#include "drilling_machine_simulation_thread.h"

#include <core/threading/mutex_locker.h>
#include <boost/lexical_cast.hpp>
#include <boost/thread/thread.hpp>

/** @class DrillingMachineSimulationThread
 * Thread to simulate RoCKIn@Work drilling machine.
 * @author Alexander Moriarty
 */

/** Constructor. */
DrillingMachineSimulationThread::DrillingMachineSimulationThread() :
    Thread("DrillingMachineSimulationThread", Thread::OPMODE_CONTINUOUS),
    cfg_timer_interval_(40)
{
}

void DrillingMachineSimulationThread::init()
{
    try
    {
        cfg_timer_interval_ = config->get_uint("/llsfrb/clips/timer-interval");
    } catch (fawkes::Exception &e)
    {
        // use default value
    }

    last_sent_command_timestamp_ = boost::posix_time::microsec_clock::local_time();

    logger->log_info("DrillingMachineSim", "Plugin is in simulation mode.");


    last_status_msg_.set_state(DrillingMachineStatus::AT_TOP);
    last_status_msg_.set_is_device_connected(true);
    tick_ = ENCODER_MAX_;
    last_command_msg_.set_command(DrillingMachineCommand::MOVE_UP);
    logger->log_info("DrillingMachineSim", "simulation initialized to AT_TOP, tick at %d", tick_);

    fawkes::MutexLocker lock(clips_mutex);

    clips->add_function("drilling-machine-move-drill-up", sigc::slot<void>(sigc::mem_fun(*this, &DrillingMachineSimulationThread::clips_move_drill_up)));
    clips->add_function("drilling-machine-move-drill-down", sigc::slot<void>(sigc::mem_fun(*this, &DrillingMachineSimulationThread::clips_move_drill_down)));
    clips->add_function("drilling-machine-get-state", sigc::slot<int>(sigc::mem_fun(*this, &DrillingMachineSimulationThread::clips_get_device_state)));
    clips->add_function("drilling-machine-is-device-connected", sigc::slot<int>(sigc::mem_fun(*this, &DrillingMachineSimulationThread::clips_is_device_connected)));

    if (!clips->build("(deffacts have-feature-drilling-machine (have-feature DrillingMachine))"))
        logger->log_warn("DrillingMachineSim", "Failed to build deffacts have-feature-drilling-machine");
}

void DrillingMachineSimulationThread::finalize()
{
}

void DrillingMachineSimulationThread::loop()
{
    boost::this_thread::sleep(boost::posix_time::milliseconds(cfg_timer_interval_));
    fawkes::MutexLocker lock(clips_mutex);
    if (last_command_msg_.command() == DrillingMachineCommand::MOVE_DOWN)
    {
        if (tick_ > 1)
        {
            last_status_msg_.set_state(DrillingMachineStatus::MOVING_DOWN);
            tick_ -= 1;
        } else
        {
            last_status_msg_.set_state(DrillingMachineStatus::AT_BOTTOM);
        }
    } else if (last_command_msg_.command() == DrillingMachineCommand::MOVE_UP)
    {
        if (tick_ < ENCODER_MAX_)
        {
            last_status_msg_.set_state(DrillingMachineStatus::MOVING_UP);
            tick_ += 1;
        } else
        {
            last_status_msg_.set_state(DrillingMachineStatus::AT_TOP);
        }
    }
}

DrillingMachineStatus::State DrillingMachineSimulationThread::clips_get_device_state()
{
    fawkes::MutexLocker lock(clips_mutex);

    if (last_status_msg_.has_state())
    {
        return last_status_msg_.state();
    }
    return DrillingMachineStatus::UNKNOWN;
}

int DrillingMachineSimulationThread::clips_is_device_connected()
{
    fawkes::MutexLocker lock(clips_mutex);
    return (last_status_msg_.has_is_device_connected() && last_status_msg_.is_device_connected());
}

void DrillingMachineSimulationThread::clips_move_drill_up()
{
    moveDrill(DrillingMachineCommand::MOVE_UP);
}

void DrillingMachineSimulationThread::clips_move_drill_down()
{
    moveDrill(DrillingMachineCommand::MOVE_DOWN);
}

void DrillingMachineSimulationThread::moveDrill(DrillingMachineCommand::Command drill_command)
{
    // prevent sending to many messages to the device
    boost::posix_time::time_duration time_diff;
    time_diff = boost::posix_time::microsec_clock::local_time() - last_sent_command_timestamp_;
    if (time_diff.total_milliseconds() < 200)
        return;

    fawkes::MutexLocker lock(clips_mutex);
    last_command_msg_.set_command(drill_command);
    last_sent_command_timestamp_ = boost::posix_time::microsec_clock::local_time();

    logger->log_debug("DrillingMachineSim", "Send drill command: %d", drill_command);
}

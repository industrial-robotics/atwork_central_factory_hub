/***************************************************************************
 *  drilling_machine_mockup_thread.cpp - Thread to mockup the drilling machine
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

#include "drilling_machine_mockup_thread.h"

#include <core/threading/mutex_locker.h>
#include <boost/lexical_cast.hpp>
#include <boost/thread/thread.hpp>

/** @class DrillingMachineMockupThread
 * Thread to mockup RoCKIn@Work drilling machine.
 * @author Alexander Moriarty
 */

/** Constructor. */
DrillingMachineMockupThread::DrillingMachineMockupThread() :
    Thread("DrillingMachineMockupThread", Thread::OPMODE_CONTINUOUS),
    cfg_timer_interval_(40)
{
}

void DrillingMachineMockupThread::init()
{
    try
    {
        cfg_timer_interval_ = config->get_uint("/llsfrb/clips/timer-interval");
    } catch (fawkes::Exception &e)
    {
        // use default value
    }

    logger->log_info("DrillingMachine", "Plugin is in mockup mode.");

    fawkes::MutexLocker lock(clips_mutex);

    clips->add_function("drilling-machine-move-drill-up", sigc::slot<void>(sigc::mem_fun(*this, &DrillingMachineMockupThread::clips_move_drill_up)));
    clips->add_function("drilling-machine-move-drill-down", sigc::slot<void>(sigc::mem_fun(*this, &DrillingMachineMockupThread::clips_move_drill_down)));
    clips->add_function("drilling-machine-get-state", sigc::slot<int>(sigc::mem_fun(*this, &DrillingMachineMockupThread::clips_get_device_state)));
    clips->add_function("drilling-machine-is-device-connected", sigc::slot<int>(sigc::mem_fun(*this, &DrillingMachineMockupThread::clips_is_device_connected)));

    if (!clips->build("(deffacts have-feature-drilling-machine (have-feature DrillingMachine))"))
        logger->log_warn("DrillingMachine", "Failed to build deffacts have-feature-drilling-machine");
}

void DrillingMachineMockupThread::finalize()
{
}

void DrillingMachineMockupThread::loop()
{
    boost::this_thread::sleep(boost::posix_time::milliseconds(cfg_timer_interval_));
}

DrillingMachineStatus::State DrillingMachineMockupThread::clips_get_device_state()
{
    return DrillingMachineStatus::UNKNOWN;
}

int DrillingMachineMockupThread::clips_is_device_connected()
{
    return false;
}

void DrillingMachineMockupThread::clips_move_drill_up()
{
}

void DrillingMachineMockupThread::clips_move_drill_down()
{
}

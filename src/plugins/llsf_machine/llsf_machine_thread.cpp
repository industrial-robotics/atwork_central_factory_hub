/***************************************************************************
 *  llsf_machine_thread.cpp - Thread to communicate with LLSF machines
 *
 *  Created: Wed Aug 20 17:00:00 2014
 *  Copyright  2012  Tim Niemueller [www.niemueller.de]
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

#include "llsf_machine_thread.h"

#include <llsf_sps/sps_comm.h>
#include <core/threading/mutex_locker.h>
#include <boost/thread/thread.hpp>
#include <boost/chrono.hpp>

using namespace llsf_utils;
using namespace llsf_sps;

/** @class LLSFMachineThread "llsf_machine_thread.h"
 * Thread to communicate with LLSF machines.
 * @author Tim Niemueller
 */

/** Constructor. */
LLSFMachineThread::LLSFMachineThread()
  : Thread("LLSFMachineThread", Thread::OPMODE_CONTINUOUS),
    sps_(0), cfg_timer_interval_(40), cfg_machine_assignment_(ASSIGNMENT_2014)
{
}

void
LLSFMachineThread::init()
{
  try {
    cfg_timer_interval_ = config->get_uint("/llsfrb/clips/timer-interval");
  } catch (fawkes::Exception &e) {
    // use default value
  }

  cfg_machine_assignment_ = ASSIGNMENT_2014;
  try {
    std::string m_ass_str = config->get_string("/llsfrb/game/machine-assignment");
    if (m_ass_str == "2013") {
      cfg_machine_assignment_ = ASSIGNMENT_2013;
    } else if (m_ass_str == "2014") {
      cfg_machine_assignment_ = ASSIGNMENT_2014;
    } else {
      logger->log_warn("LLSFMachine", "Invalid machine assignment '%s', using 2014",
          m_ass_str.c_str());
      cfg_machine_assignment_ = ASSIGNMENT_2014;
    }
  } catch (fawkes::Exception &e) {} // ignored, use default
  logger->log_info("LLSFMachine", "Using %s machine assignment",
        (cfg_machine_assignment_ == ASSIGNMENT_2013) ? "2013" : "2014");

  try {
    sps_ = NULL;
    if (config->get_bool("/llsfrb/sps/enable")) {
      logger->log_info("LLSFMachine", "Connecting to SPS");
      bool test_lights = true;

      try {
        test_lights = config->get_bool("/llsfrb/sps/test-lights");
      } catch (fawkes::Exception &e) {} // ignore, use default

      if (config->exists("/llsfrb/sps/hosts") && cfg_machine_assignment_ == ASSIGNMENT_2014) {
        sps_ = new SPSComm(config->get_strings("/llsfrb/sps/hosts"),
            config->get_uint("/llsfrb/sps/port"));
      } else {
        sps_ = new SPSComm(config->get_string("/llsfrb/sps/host").c_str(),
            config->get_uint("/llsfrb/sps/port"));
      }

      sps_->reset_lights();
      sps_->reset_rfids();

      if (test_lights) {
        sps_->test_lights();
      }
    }
  } catch (fawkes::Exception &e) {
    logger->log_warn("LLSFMachine", "Cannot connect to SPS, running without");
    delete sps_;
    sps_ = NULL;
  }


  fawkes::MutexLocker lock(clips_mutex);

  clips->add_function("sps-set-signal", sigc::slot<void, std::string, std::string, std::string>(sigc::mem_fun(*this, &LLSFMachineThread::clips_sps_set_signal)));

  if (!clips->build("(deffacts have-feature-llsf-machine (have-feature LLSFMachine))")) {
    logger->log_warn("LLSFMachine", "Failed to build deffacts have-feature-llsf-machine");
  }
}

void
LLSFMachineThread::finalize()
{
}

void
LLSFMachineThread::loop()
{
  sps_read_rfids();

  boost::chrono::milliseconds duration(cfg_timer_interval_);
  boost::this_thread::sleep_for(duration);
}



void
LLSFMachineThread::clips_sps_set_signal(std::string machine, std::string light, std::string state)
{
  if (!sps_) return;

  try {
    unsigned int m = to_machine(machine, cfg_machine_assignment_);
    sps_->set_light(m, light, state);
  } catch (fawkes::Exception &e) {
    logger->log_warn("LLSFMachine", "Failed to set signal: %s", e.what());
  }
}


void
LLSFMachineThread::sps_read_rfids()
{
  if (!sps_)  return;

  fawkes::MutexLocker lock(clips_mutex);

  try {
    std::vector<uint32_t> puck_ids = sps_->read_rfids();
    for (unsigned int i = 0; i < puck_ids.size(); ++i) {
      const char *machine_name = to_string(i, cfg_machine_assignment_);
      if (puck_ids[i] == SPSComm::NO_PUCK) {
        clips->assert_fact_f("(rfid-input (machine %s) (has-puck FALSE))",
            machine_name);
      } else {
        clips->assert_fact_f("(rfid-input (machine %s) (has-puck TRUE) (id %u))",
            machine_name, puck_ids[i]);
      }
    }
  } catch (fawkes::Exception &e) {
    logger->log_warn("LLSFMachine", "Failed to read RFIDs");
    logger->log_warn("LLSFMachine", e);
    try {
      sps_->try_reconnect();
      logger->log_info("LLSFMachine", "Successfully reconnected");
    } catch (fawkes::Exception &e) {
      logger->log_error("LLSFMachine", "Failed to reconnect");
      logger->log_error("LLSFMachine", e);
    }
  }
}

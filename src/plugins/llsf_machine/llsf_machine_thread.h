/***************************************************************************
 *  llsf_machine_thread.h - Thread to communicate with the LLSF machines
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

#ifndef __PLUGINS_LLSF_MACHINE_THREAD_H_
#define __PLUGINS_LLSF_MACHINE_THREAD_H_

#include <core/threading/thread.h>
#include <aspect/logging.h>
#include <aspect/clips.h>
#include <aspect/configurable.h>
#include <utils/llsf/machines.h>


namespace llsf_sps {
  class SPSComm;
}

class LLSFMachineThread
: public fawkes::Thread,
  public fawkes::LoggingAspect,
  public fawkes::ConfigurableAspect,
  public fawkes::CLIPSAspect
{
 public:
  LLSFMachineThread();

  virtual void init();
  virtual void loop();
  virtual void finalize();

 private:
  void clips_sps_set_signal(std::string machine, std::string light, std::string state);
  void sps_read_rfids();

 private:
  llsf_sps::SPSComm *sps_;
  unsigned int cfg_timer_interval_;
  llsf_utils::MachineAssignment cfg_machine_assignment_;
};

#endif

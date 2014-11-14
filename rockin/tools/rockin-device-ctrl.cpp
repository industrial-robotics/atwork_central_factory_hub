/***************************************************************************
 *  rockin-device-ctrl.cpp - control networked devices
 *
 *  Copyright  2014  Sven Schneider
 ****************************************************************************/

/*  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in
 *   the documentation and/or other materials provided with the
 *   distribution.
 * - Neither the name of the authors nor the names of its contributors
 *   may be used to endorse or promote products derived from this
 *   software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#define BOOST_DATE_TIME_POSIX_TIME_STD_CONFIG

#include <config/yaml.h>

#include <protobuf_comm/client.h>
#include <utils/system/argparser.h>

#include <msgs/DrillingMachine.pb.h>
#include <msgs/ConveyorBelt.pb.h>

using namespace protobuf_comm;
using namespace rockin_msgs;
using namespace fawkes;

ProtobufStreamClient *client_ = 0;
DrillingMachineStatus drilling_machine_status;
ConveyorBeltStatus conveyor_belt_status;



void handle_message(uint16_t comp_id, uint16_t msg_type,
      std::shared_ptr<google::protobuf::Message> msg)
{
  std::shared_ptr<DrillingMachineStatus> ds;
  if ((ds = std::dynamic_pointer_cast<DrillingMachineStatus>(msg))) {
    drilling_machine_status = *ds;
  }

  std::shared_ptr<ConveyorBeltStatus> cs;
  if ((cs = std::dynamic_pointer_cast<ConveyorBeltStatus>(msg))) {
    conveyor_belt_status = *cs;
  }
}


bool control_drilling_machine(const std::string &command)
{
  if (!drilling_machine_status.has_state()) return false;

  DrillingMachineStatus::State state = drilling_machine_status.state();
  DrillingMachineCommand msg;

  if (command == "down") {
    if (state == DrillingMachineStatus::AT_BOTTOM) return true;
    if (state == DrillingMachineStatus::MOVING_DOWN) return false;
    if (state == DrillingMachineStatus::UNKNOWN) return false;

    msg.set_command(DrillingMachineCommand::MOVE_DOWN);
  } else if (command == "up") {
    if (state == DrillingMachineStatus::AT_TOP) return true;
    if (state == DrillingMachineStatus::MOVING_UP) return false;
    if (state == DrillingMachineStatus::UNKNOWN) return false;

    msg.set_command(DrillingMachineCommand::MOVE_UP);
  } else {
    std::cerr << "Command '" << command << "' is invalid for the drilling machine." << std::endl;
    return true;
  }

  client_->send(msg);

  return false;
}


bool control_conveyor_belt(const std::string &command)
{
  if (!conveyor_belt_status.has_state()) return false;

  ConveyorBeltRunMode state = conveyor_belt_status.state();
  ConveyorBeltCommand msg;

  if (command == "start") {
    if (state == START) return true;

    msg.set_command(START);
  } else if (command == "stop") {
    if (state == STOP) return true;

    msg.set_command(STOP);
  } else {
    std::cerr << "Command '" << command << "' is invalid for the conveyor belt." << std::endl;
    return true;
  }

  client_->send(msg);

  return false;
}


int main(int argc, char **argv)
{
  ArgumentParser argp(argc, argv, "");

  if (argp.num_items() != 2) {
    std::cout << "Usage: " << argv[0] << " <device> <command>" << std::endl;
    exit(1);
  }

  std::string device = argp.items()[0];
  std::string command = argp.items()[1];

  llsfrb::YamlConfiguration config(CONFDIR);
  config.load("config.yaml");

  client_ = new ProtobufStreamClient();

  MessageRegister & message_register = client_->message_register();
  message_register.add_message_type<DrillingMachineStatus>();
  message_register.add_message_type<DrillingMachineCommand>();
  message_register.add_message_type<ConveyorBeltStatus>();
  message_register.add_message_type<ConveyorBeltCommand>();

  client_->signal_received().connect(handle_message);
  client_->async_connect(
      config.get_string("/llsfrb/shell/refbox-host").c_str(),
      config.get_uint("/llsfrb/shell/refbox-port"));

  bool quit = false;
  while (!quit) {
    if (device == "drilling_machine") {
      quit = control_drilling_machine(command);
    } else if (device == "conveyor_belt") {
      quit = control_conveyor_belt(command);
    } else {
      std::cerr << "Device '" << device << "' is invalid." << std::endl;
      quit = true;
    }

    usleep(100000);
  }

  delete client_;

  // Delete all global objects allocated by libprotobuf
  google::protobuf::ShutdownProtobufLibrary();
}

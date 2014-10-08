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

#include <algorithm>

#include <config/yaml.h>
#include <utils/system/argparser.h>

#include <protobuf_comm/client.h>
#include <msgs/BenchmarkState.pb.h>

using namespace protobuf_comm;
using namespace fawkes;

bool quit_ = false;
bool is_connected_ = false;

void handle_message(uint16_t comp_id, uint16_t msg_type,
      std::shared_ptr<google::protobuf::Message> msg)
{
  std::shared_ptr<rockin_msgs::BenchmarkState> s;
  if ((s = std::dynamic_pointer_cast<rockin_msgs::BenchmarkState>(msg))) {
    std::cout << "State: ";
    switch (s->state()) {
      case rockin_msgs::BenchmarkState::RUNNING: std::cout << "RUNNING"; break;
      case rockin_msgs::BenchmarkState::PAUSED: std::cout << "PAUSED"; break;
      case rockin_msgs::BenchmarkState::FINISHED: std::cout << "FINISHED"; break;
      case rockin_msgs::BenchmarkState::STOPPED: std::cout << "STOPPED"; break;
    }
    std::cout << std::endl;

    std::cout << "Benchmark Scenario: ";
    switch (s->scenario().type()) {
      case rockin_msgs::BenchmarkScenario::NONE: std::cout << "NONE"; break;
      case rockin_msgs::BenchmarkScenario::FBM: std::cout << "FBM"; break;
      case rockin_msgs::BenchmarkScenario::TBM: std::cout << "TBM"; break;
    }
    std::cout << s->scenario().type_id();
    if (s->scenario().has_description()) std::cout << " (" << s->scenario().description() << ")";
    std::cout << std::endl;

    quit_ = true;
  }
}

void handle_connected()
{
  is_connected_ = true;
}




int main(int argc, char **argv)
{
  ArgumentParser argp(argc, argv, "s:b:e:");

  if (argp.num_items() > 4) {
    std::cout << "Usage: " << argv[0] << " [-s <state>]  [-b <benchmark>]  [-e <event>]" << std::endl;
    exit(1);
  }

  bool has_state = false;
  bool has_scenario = false;
  bool has_event = false;
  std::string state = "";
  std::string scenario = "";
  std::string event = "";

  if (argp.has_arg("s")) {
    state = argp.arg("s");
    has_state = true;
    std::transform(state.begin(), state.end(), state.begin(), ::tolower);
  }

  if (argp.has_arg("b")) {
    scenario = argp.arg("b");
    has_scenario = true;
    std::transform(scenario.begin(), scenario.end(), scenario.begin(), ::tolower);
  }

  if (argp.has_arg("e")) {
    event = argp.arg("e");
    has_event = true;
    std::transform(event.begin(), event.end(), event.begin(), ::tolower);
  }

  llsfrb::YamlConfiguration config(CONFDIR);
  config.load("config.yaml");

  ProtobufStreamClient client;

  MessageRegister &message_register = client.message_register();
  message_register.add_message_type<rockin_msgs::BenchmarkState>();

  client.signal_connected().connect(handle_connected);
  if ((!has_state) && (!has_scenario)) client.signal_received().connect(handle_message);
  client.async_connect(
      config.get_string("/llsfrb/shell/refbox-host").c_str(),
      config.get_uint("/llsfrb/shell/refbox-port"));

  while (!is_connected_);

  while (!quit_) {
    if (has_state) {
      rockin_msgs::SetBenchmarkState cmd;

      if (state == "running") cmd.set_state(rockin_msgs::BenchmarkState::RUNNING);
      else if (state == "paused") cmd.set_state(rockin_msgs::BenchmarkState::PAUSED);
      else if (state == "finished") cmd.set_state(rockin_msgs::BenchmarkState::FINISHED);
      else break;

      client.send(cmd);
      quit_ = true;
    }

    if (has_event) {
      rockin_msgs::SetBenchmarkTransitionEvent cmd;

      if (event == "reset") cmd.set_event(rockin_msgs::SetBenchmarkTransitionEvent::RESET);
      if (event == "start") cmd.set_event(rockin_msgs::SetBenchmarkTransitionEvent::START);
      if (event == "stop") cmd.set_event(rockin_msgs::SetBenchmarkTransitionEvent::STOP);
      if (event == "pause") cmd.set_event(rockin_msgs::SetBenchmarkTransitionEvent::PAUSE);

      client.send(cmd);
      quit_ = true;
    }

    if (has_scenario) {
      rockin_msgs::SetBenchmarkScenario cmd;

      if (scenario == "fbm1") {
        cmd.mutable_scenario()->set_type(rockin_msgs::BenchmarkScenario::FBM);
        cmd.mutable_scenario()->set_type_id(1);
      } else if (scenario == "fbm2") {
        cmd.mutable_scenario()->set_type(rockin_msgs::BenchmarkScenario::FBM);
        cmd.mutable_scenario()->set_type_id(2);
      } else if (scenario == "fbm3") {
        cmd.mutable_scenario()->set_type(rockin_msgs::BenchmarkScenario::FBM);
        cmd.mutable_scenario()->set_type_id(3);
      } else if (scenario == "tbm1") {
        cmd.mutable_scenario()->set_type(rockin_msgs::BenchmarkScenario::TBM);
        cmd.mutable_scenario()->set_type_id(1);
      } else if (scenario == "tbm2") {
        cmd.mutable_scenario()->set_type(rockin_msgs::BenchmarkScenario::TBM);
        cmd.mutable_scenario()->set_type_id(2);
      } else if (scenario == "tbm3") {
        cmd.mutable_scenario()->set_type(rockin_msgs::BenchmarkScenario::TBM);
        cmd.mutable_scenario()->set_type_id(3);
      } else if (scenario == "none") {
        cmd.mutable_scenario()->set_type(rockin_msgs::BenchmarkScenario::NONE);
        cmd.mutable_scenario()->set_type_id(0);
      } else {
        std::cerr << "The provided benchmark scenario '" << scenario << "' is invalid" << std::endl;
        break;
      }

      client.send(cmd);
      quit_ = true;
    }

    usleep(100000);
  }

  // Delete all global objects allocated by libprotobuf
  google::protobuf::ShutdownProtobufLibrary();
}

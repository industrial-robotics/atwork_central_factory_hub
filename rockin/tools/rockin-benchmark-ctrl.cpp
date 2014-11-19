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
      case rockin_msgs::BenchmarkState::INIT: std::cout << "INIT"; break;
      case rockin_msgs::BenchmarkState::RUNNING: std::cout << "RUNNING"; break;
      case rockin_msgs::BenchmarkState::PAUSED: std::cout << "PAUSED"; break;
      case rockin_msgs::BenchmarkState::FINISHED: std::cout << "FINISHED"; break;
    }
    std::cout << std::endl;

    std::cout << "Phase: ";
    switch (s->phase().type()) {
      case rockin_msgs::BenchmarkPhase::NONE: std::cout << "NONE"; break;
      case rockin_msgs::BenchmarkPhase::FBM: std::cout << "FBM"; break;
      case rockin_msgs::BenchmarkPhase::TBM: std::cout << "TBM"; break;
    }
    std::cout << s->phase().type_id();
    if (s->phase().has_description()) std::cout << " (" << s->phase().description() << ")";
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
  ArgumentParser argp(argc, argv, "s:p:");

  if (argp.num_items() > 3) {
    std::cout << "Usage: " << argv[0] << " [-s <state>]  [-p <phase>]" << std::endl;
    exit(1);
  }

  bool has_state = false;
  bool has_phase = false;
  std::string state = "";
  std::string phase = "";

  if (argp.has_arg("s")) {
    state = argp.arg("s");
    has_state = true;
    std::transform(state.begin(), state.end(), state.begin(), ::tolower);
  }

  if (argp.has_arg("p")) {
    phase = argp.arg("p");
    has_phase = true;
    std::transform(phase.begin(), phase.end(), phase.begin(), ::tolower);
  }

  llsfrb::YamlConfiguration config(CONFDIR);
  config.load("config.yaml");

  ProtobufStreamClient client;

  MessageRegister &message_register = client.message_register();
  message_register.add_message_type<rockin_msgs::BenchmarkState>();

  client.signal_connected().connect(handle_connected);
  if ((!has_state) && (!has_phase)) client.signal_received().connect(handle_message);
  client.async_connect(
      config.get_string("/llsfrb/shell/refbox-host").c_str(),
      config.get_uint("/llsfrb/shell/refbox-port"));

  while (!is_connected_);

  while (!quit_) {
    if (has_state) {
      rockin_msgs::SetBenchmarkState cmd;

      if (state == "init") cmd.set_state(rockin_msgs::BenchmarkState::INIT);
      else if (state == "running") cmd.set_state(rockin_msgs::BenchmarkState::RUNNING);
      else if (state == "paused") cmd.set_state(rockin_msgs::BenchmarkState::PAUSED);
      else if (state == "finished") cmd.set_state(rockin_msgs::BenchmarkState::FINISHED);
      else break;

      client.send(cmd);
      quit_ = true;
    }

    if (has_phase) {
      rockin_msgs::SetBenchmarkPhase cmd;

      if (phase == "fbm1") {
        cmd.mutable_phase()->set_type(rockin_msgs::BenchmarkPhase::FBM);
        cmd.mutable_phase()->set_type_id(1);
      } else if (phase == "fbm2") {
        cmd.mutable_phase()->set_type(rockin_msgs::BenchmarkPhase::FBM);
        cmd.mutable_phase()->set_type_id(2);
      } else if (phase == "tbm1") {
        cmd.mutable_phase()->set_type(rockin_msgs::BenchmarkPhase::TBM);
        cmd.mutable_phase()->set_type_id(1);
      } else if (phase == "tbm2") {
        cmd.mutable_phase()->set_type(rockin_msgs::BenchmarkPhase::TBM);
        cmd.mutable_phase()->set_type_id(2);
      } else if (phase == "tbm3") {
        cmd.mutable_phase()->set_type(rockin_msgs::BenchmarkPhase::TBM);
        cmd.mutable_phase()->set_type_id(3);
      } else if (phase == "none") {
        cmd.mutable_phase()->set_type(rockin_msgs::BenchmarkPhase::NONE);
        cmd.mutable_phase()->set_type_id(0);
      } else {
        std::cerr << "The provided phase '" << phase << "' is invalid" << std::endl;
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

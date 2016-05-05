/***************************************************************************
 *  atwork-device-ctrl.cpp - control networked devices
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

#include <msgs/AttentionMessage.pb.h>

using namespace protobuf_comm;
using namespace atwork_pb_msgs;
using namespace fawkes;


protobuf_comm::ProtobufStreamClient client;
std::string host;
int port;


void handle_message(uint16_t comp_id, uint16_t msg_type,
      std::shared_ptr<google::protobuf::Message> msg)
{
  std::shared_ptr<AttentionMessage> am;
  if ((am = std::dynamic_pointer_cast<AttentionMessage>(msg))) {
    std::cout << am->message() << std::endl;
  }
}


void handle_disconnect(const boost::system::error_code &error)
{
  usleep(100000);
  client.async_connect(host.c_str(), port);
}


int main(int argc, char **argv)
{
  llsfrb::YamlConfiguration config(CONFDIR);
  config.load("config.yaml");

  MessageRegister &message_register = client.message_register();
  message_register.add_message_type<AttentionMessage>();

  client.signal_received().connect(handle_message);
  client.signal_disconnected().connect(handle_disconnect);
  host = config.get_string("/llsfrb/shell/refbox-host");
  port = config.get_uint("/llsfrb/shell/refbox-port");
  client.async_connect(host.c_str(), port);

  while (true) {
    usleep(100000);
  }

  // Delete all global objects allocated by libprotobuf
  google::protobuf::ShutdownProtobufLibrary();
}

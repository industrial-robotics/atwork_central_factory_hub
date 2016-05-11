/***************************************************************************
 *  atwork-fake-robot.cpp - fake a robot
 *
 *  Copyright  2013  Tim Niemueller [www.niemueller.de]
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

#include <protobuf_comm/peer.h>
#include <utils/system/argparser.h>

#include <msgs/BeaconSignal.pb.h>
#include <msgs/VersionInfo.pb.h>
#include <msgs/BenchmarkState.pb.h>
#include <msgs/Inventory.pb.h>
#include <msgs/TaskInfo.pb.h>
//#include <msgs/DrillingMachine.pb.h>
#include <msgs/ConveyorBelt.pb.h>
//#include <msgs/Camera.pb.h>
//#include <msgs/BenchmarkFeedback.pb.h>
//#include <msgs/RobotStatusReport.pb.h>
#include <msgs/LoggingStatus.pb.h>

#include <boost/asio.hpp>
#include <boost/date_time.hpp>

using namespace protobuf_comm;
using namespace atwork_pb_msgs;
using namespace fawkes;

static bool quit = false;
static boost::asio::deadline_timer *timer_ = NULL;
std::string name_;
std::string team_name_;
unsigned long seq_ = 0;
int conveyor_belt_cycle_ = 0;
BenchmarkState::Phase current_benchmark_phase_ = BenchmarkState::EXECUTION;
ProtobufBroadcastPeer *peer_public_ = NULL;
ProtobufBroadcastPeer *peer_team_ = NULL;
bool crypto_setup_ = false;

llsfrb::Configuration *config_;

void
signal_handler(const boost::system::error_code& error, int signum)
{
  if (!error) {
    quit = true;

    if (timer_) {
      timer_->cancel();
    }
  }
}

void
handle_recv_error(boost::asio::ip::udp::endpoint &endpoint, std::string msg)
{
  printf("Receive error from %s:%u: %s\n",
   endpoint.address().to_string().c_str(), endpoint.port(), msg.c_str());
}

void
handle_send_error(std::string msg)
{
  printf("Send error: %s\n", msg.c_str());
}

void
handle_message(boost::asio::ip::udp::endpoint &sender,
         uint16_t component_id, uint16_t msg_type,
         std::shared_ptr<google::protobuf::Message> msg)
{
  std::shared_ptr<BeaconSignal> b;
  if ((b = std::dynamic_pointer_cast<BeaconSignal>(msg))) {
    std::cout << "Detected robot: " << b->team_name() << " " << b->peer_name() << " (seq " << b->seq() << ")" << std::endl;
  }

  std::shared_ptr<VersionInfo> vi;
  if ((vi = std::dynamic_pointer_cast<VersionInfo>(msg))) {
    std::cout << "VersionInfo received: " << vi->version_string() << std::endl;
  }

  std::shared_ptr<BenchmarkState> bs;
  if ((bs = std::dynamic_pointer_cast<BenchmarkState>(msg))) {
    std::cout << "BenchmarkState received:" << std::endl;

    std::cout << "  Time: " << bs->benchmark_time().sec() << "s" << std::endl;

    std::cout << "  Benchmark scenario: ";
    switch (bs->scenario().type()) {
      case BenchmarkScenario::NONE: std::cout << "NONE"; break;
      case BenchmarkScenario::BNT:  std::cout << "BNT"; break;
      case BenchmarkScenario::BMT:  std::cout << "BMT"; break;
      case BenchmarkScenario::BTT:  std::cout << "BTT"; break;
      case BenchmarkScenario::PPT:  std::cout << "PPT"; break;
      case BenchmarkScenario::CBT:  std::cout << "CBT"; break;
      case BenchmarkScenario::AWF:  std::cout << "AWF"; break;
      case BenchmarkScenario::IRL:  std::cout << "IRL"; break;
    }
    std::cout << " " << bs->scenario().type_id();
    if (bs->scenario().has_description()) std::cout << " (" << bs->scenario().description() << ")";
    std::cout << std::endl;

    std::cout << "  Phase: ";
    switch (bs->phase()) {
        case BenchmarkState::CALIBRATION: std::cout << "CALIBRATION" << std::endl; break;
        case BenchmarkState::PREPARATION: std::cout << "PREPARATION" << std::endl; break;
        case BenchmarkState::EXECUTION: std::cout << "EXECUTION" << std::endl; break;
    }
    current_benchmark_phase_ = bs->phase();

    std::cout << "  State: ";
    switch (bs->state()) {
      case BenchmarkState::RUNNING: std::cout << "RUNNING" << std::endl; break;
      case BenchmarkState::PAUSED: std::cout << "PAUSED" << std::endl; break;
      case BenchmarkState::FINISHED: std::cout << "FINISHED" << std::endl; break;
      case BenchmarkState::STOPPED: std::cout << "STOPPED" << std::endl; break;
    }

    std::cout << "  Known teams: ";
    for (int i = 0; i < bs->known_teams_size(); i++) std::cout << bs->known_teams(i) << ", ";
    std::cout << std::endl;

    std::cout << "  Connected teams: ";
    for (int i = 0; i < bs->connected_teams_size(); i++) std::cout << bs->connected_teams(i) << ", ";
    std::cout << std::endl;
  }

  std::shared_ptr<Inventory> in;
  if ((in = std::dynamic_pointer_cast<Inventory>(msg))) {
    std::cout << "Inventory received:" << std::endl;

    for (int i = 0; i < in->items_size(); i++) {
      const Inventory_Item &item = in->items(i);
      std::cout << "  Object " << i << ": " << item.object().description() << std::endl;
      if (item.has_location()) std::cout << "    In location: " << item.location().description() << std::endl;
      if (item.has_container()) std::cout << "    In container: " << item.container().description() << std::endl;
      if (item.has_quantity()) std::cout << "    Quantity: " << item.quantity() << std::endl;
    }
  }

  /**
  std::shared_ptr<OrderInfo> o;
  if ((o = std::dynamic_pointer_cast<OrderInfo>(msg))) {
    std::cout << "OrderInfo received" << std::endl;

    for (int i = 0; i < o->orders_size(); i++) {
      const Order &order = o->orders(i);
      std::cout << "  Order " << i << ":" << std::endl;
      std::cout << "    Identifier: " << order.id() << std::endl;
      std::cout << "    Status: ";
      switch (order.status()) {
        case Order::OFFERED: std::cout << "OFFERED"; break;
        case Order::TIMEOUT: std::cout << "TIMEOUT"; break;
        case Order::IN_PROGRESS: std::cout << "IN_PROGRESS"; break;
        case Order::PAUSED: std::cout << "PAUSED"; break;
        case Order::ABORTED: std::cout << "ABORTED"; break;
        case Order::FINISHED: std::cout << "FINISHED"; break;
      }
      std::cout << std::endl;
      std::cout << "    Object: " << order.object().description() << std::endl;
      if (order.has_container()) std::cout << "    Container: " << order.container().description() << std::endl;
      std::cout << "    Quantity delivered: " << order.quantity_delivered() << std::endl;
      if (order.has_quantity_requested()) std::cout << "    Quantity requested: " << order.quantity_requested() << std::endl;
      if (order.has_destination()) std::cout << "    Destination: " << order.destination().description() << std::endl;
      if (order.has_source()) std::cout << "    Source: " << order.source().description() << std::endl;
      if (order.has_processing_team()) std::cout << "    Processing team: " << order.processing_team() << std::endl;
    }
  }
  **/

  std::shared_ptr<TriggeredConveyorBeltStatus> cb;
  if ((cb = std::dynamic_pointer_cast<TriggeredConveyorBeltStatus>(msg))) {
    std::cout << "Conveyor belt status received: ";
    switch (cb->state()) {
      case ConveyorBeltRunMode::START: std::cout << "RUNNING"; break;
      case ConveyorBeltRunMode::STOP: std::cout << "STOPPED"; break;
    }
    std::cout << " [Cycle: " << cb->cycle() << "]" << std::endl;

    conveyor_belt_cycle_ = cb->cycle();
  }
}


void
handle_timer(const boost::system::error_code& error)
{
  if (! error) {
    boost::posix_time::ptime now(boost::posix_time::microsec_clock::universal_time());
    std::shared_ptr<BeaconSignal> signal(new BeaconSignal());
    Time *time = signal->mutable_time();
    boost::posix_time::time_duration const since_epoch =
      now - boost::posix_time::from_time_t(0);

    time->set_sec(static_cast<google::protobuf::int64>(since_epoch.total_seconds()));
    time->set_nsec(
      static_cast<google::protobuf::int64>(since_epoch.fractional_seconds() * 
             (1000000000/since_epoch.ticks_per_second())));

    signal->set_peer_name(name_);
    signal->set_team_name(team_name_);
    signal->set_seq(++seq_);
    peer_team_->send(signal);


    // Command the conveyor belt
    /*
    TriggeredConveyorBeltCommand cb_cmd;
    cb_cmd.set_command(ConveyorBeltRunMode::START);
    cb_cmd.set_next_cycle(conveyor_belt_cycle_ + 1);
    peer_public_->send(cb_cmd);
    */

    // Send if the robot is logging offline benchmarking data
    LoggingStatus logging;
    logging.set_is_logging(true);
    peer_team_->send(logging);


    timer_->expires_at(timer_->expires_at()
          + boost::posix_time::milliseconds(2000));
    timer_->async_wait(handle_timer);
  }

}


int
main(int argc, char **argv)
{
  ArgumentParser argp(argc, argv, "");

  if (argp.num_items() != 2) {
    std::cout << "Usage: " << argv[0] << " <name> <team>" << std::endl;
    exit(1);
  }

  name_ = argp.items()[0];
  team_name_ = argp.items()[1];

  config_ = new llsfrb::YamlConfiguration(CONFDIR);
  config_->load("config.yaml");

  if (config_->exists("/llsfrb/comm/public-peer/send-port") &&
      config_->exists("/llsfrb/comm/public-peer/recv-port") )
  {
    peer_public_ = new ProtobufBroadcastPeer(config_->get_string("/llsfrb/comm/public-peer/host"),
               config_->get_uint("/llsfrb/comm/public-peer/recv-port"),
               config_->get_uint("/llsfrb/comm/public-peer/send-port"));
  } else {
    peer_public_ = new ProtobufBroadcastPeer(config_->get_string("/llsfrb/comm/public-peer/host"),
               config_->get_uint("/llsfrb/comm/public-peer/port"));
  }

  MessageRegister & message_register = peer_public_->message_register();
  message_register.add_message_type<BeaconSignal>();
  message_register.add_message_type<VersionInfo>();
  message_register.add_message_type<BenchmarkState>();
  message_register.add_message_type<Inventory>();
  //message_register.add_message_type<OrderInfo>();
  message_register.add_message_type<TriggeredConveyorBeltStatus>();

  std::string cfg_prefix =
    std::string("/llsfrb/comm/") + team_name_ + "-peer/";

  // better to this dynamically be reacting to the public GameState
  // this way you can also play unencrypted training games
  std::string crypto_key = "", cipher = "aes-128-cbc";
  try {
    crypto_key = config_->get_string(("/llsfrb/game/crypto-keys/" + team_name_).c_str());
  } catch (Exception &e) {
    printf("No encryption key configured for team, not enabling crypto");
  }

  if (config_->exists((cfg_prefix + "send-port").c_str()) &&
      config_->exists((cfg_prefix + "recv-port").c_str()) )
  {
    peer_team_ = new ProtobufBroadcastPeer(config_->get_string((cfg_prefix + "host").c_str()),
             config_->get_uint((cfg_prefix + "recv-port").c_str()),
             config_->get_uint((cfg_prefix + "send-port").c_str()),
             &message_register /*, crypto_key, cipher*/);
  } else {
    peer_team_ = new ProtobufBroadcastPeer(config_->get_string((cfg_prefix + "host").c_str()),
             config_->get_uint((cfg_prefix + "port").c_str()),
             &message_register/*, crypto_key, cipher*/);
  }

  boost::asio::io_service io_service;

  peer_public_->signal_received().connect(handle_message);
  peer_public_->signal_recv_error().connect(handle_recv_error);
  peer_public_->signal_send_error().connect(handle_send_error);

  peer_team_->signal_received().connect(handle_message);
  peer_team_->signal_recv_error().connect(handle_recv_error);
  peer_team_->signal_send_error().connect(handle_send_error);

#if BOOST_ASIO_VERSION >= 100601
  // Construct a signal set registered for process termination.
  boost::asio::signal_set signals(io_service, SIGINT, SIGTERM);

  // Start an asynchronous wait for one of the signals to occur.
  signals.async_wait(signal_handler);
#endif

  timer_ = new boost::asio::deadline_timer(io_service);
  timer_->expires_from_now(boost::posix_time::milliseconds(2000));
  timer_->async_wait(handle_timer);

  do {
    io_service.run();
    io_service.reset();
  } while (! quit);

  delete timer_;
  delete peer_team_;
  delete peer_public_;
  delete config_;

  // Delete all global objects allocated by libprotobuf
  google::protobuf::ShutdownProtobufLibrary();
}

#include <chrono>
#include <iomanip>

#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>

#include <config/yaml.h>
#include <utils/system/argparser.h>

#include <protobuf_comm/client.h>
#include <msgs/BenchmarkControl.pb.h>
#include <msgs/BenchmarkState.pb.h>
#include <msgs/ConveyorBelt.pb.h>

#include <gtkmm.h>
#include <pangomm.h>
#include <glibmm.h>



protobuf_comm::ProtobufStreamClient client;
std::string host;
int port;
Glib::RefPtr<Gtk::Builder> builder;
boost::mutex mutex;
std::shared_ptr<atwork_pb_msgs::BenchmarkState> benchmark_state;



void handle_message(uint16_t comp_id, uint16_t msg_type,
      std::shared_ptr<google::protobuf::Message> msg)
{
  // Prevent simultaneous access to the refbox state from gui and network
  boost::mutex::scoped_lock lock(mutex);

  if (std::dynamic_pointer_cast<atwork_pb_msgs::BenchmarkState>(msg)) {
    benchmark_state = std::dynamic_pointer_cast<atwork_pb_msgs::BenchmarkState>(msg);
  }
}



bool timeout_handler() {
  // Prevent simultaneous access to the refbox state from gui and network
  boost::mutex::scoped_lock lock(mutex);

  if (benchmark_state) {
    Gtk::Button *button_start = 0;
    Gtk::Button *button_pause = 0;
    Gtk::Button *button_stop = 0;
    Gtk::Button *button_shuffle= 0;
    Gtk::Button *button_next = 0;
    builder->get_widget("button_start", button_start);
    builder->get_widget("button_pause", button_pause);
    builder->get_widget("button_stop", button_stop);
    builder->get_widget("button_shuffle", button_shuffle);
    builder->get_widget("button_next", button_next);

    switch (benchmark_state->state()) {
      case atwork_pb_msgs::BenchmarkState::STOPPED:
        button_start->set_sensitive(true);
        button_pause->set_sensitive(false);
        button_stop->set_sensitive(false);
        button_shuffle->set_sensitive(true);
        button_next->set_sensitive(false);
      break;

      case atwork_pb_msgs::BenchmarkState::RUNNING:
        button_start->set_sensitive(false);
        button_pause->set_sensitive(true);
        button_stop->set_sensitive(true);
        button_shuffle->set_sensitive(false);
        button_next->set_sensitive(true);
      break;

      case atwork_pb_msgs::BenchmarkState::PAUSED:
        button_start->set_sensitive(true);
        button_pause->set_sensitive(false);
        button_stop->set_sensitive(false);
        button_shuffle->set_sensitive(false);
        button_next->set_sensitive(false);
      break;

      case atwork_pb_msgs::BenchmarkState::FINISHED:
        if (benchmark_state->phase() == atwork_pb_msgs::BenchmarkState::EXECUTION) {
            button_start->set_sensitive(false);
            button_pause->set_sensitive(false);
            button_stop->set_sensitive(false);
            button_shuffle->set_sensitive(false);
            button_next->set_sensitive(false);
        } else {
            button_start->set_sensitive(true);
            button_pause->set_sensitive(false);
            button_stop->set_sensitive(false);
            button_shuffle->set_sensitive(false);
            button_next->set_sensitive(false);
        }
      break;
    }
  }
  return true;
}


void handle_disconnect(const boost::system::error_code &error)
{
  usleep(100000);
  client.async_connect(host.c_str(), port);
}


void on_start_click()
{
  if (!client.connected()) return;

  atwork_pb_msgs::SetBenchmarkTransitionEvent cmd_event;
  cmd_event.set_event(atwork_pb_msgs::SetBenchmarkTransitionEvent::START);
  client.send(cmd_event);
}


void on_pause_click()
{
  if (!client.connected()) return;

  atwork_pb_msgs::SetBenchmarkTransitionEvent cmd_event;
  cmd_event.set_event(atwork_pb_msgs::SetBenchmarkTransitionEvent::PAUSE);
  client.send(cmd_event);
}


void on_stop_click()
{
  if (!client.connected()) return;

  atwork_pb_msgs::SetBenchmarkTransitionEvent cmd_event;
  cmd_event.set_event(atwork_pb_msgs::SetBenchmarkTransitionEvent::STOP);
  client.send(cmd_event);
}


void on_shuffle_click()
{
  if (!client.connected()) return;

  atwork_pb_msgs::SetBenchmarkTransitionEvent cmd_event;
  cmd_event.set_event(atwork_pb_msgs::SetBenchmarkTransitionEvent::SHUFFLE);
  client.send(cmd_event);
}


void on_next_click()
{
  if (!client.connected()) return;

  atwork_pb_msgs::SetBenchmarkTransitionEvent cmd_event;
  cmd_event.set_event(atwork_pb_msgs::SetBenchmarkTransitionEvent::SKIP);
  client.send(cmd_event);
}


void on_reset_click()
{
  if (!client.connected()) return;

  Gtk::ComboBoxText *combobox_benchmark = 0;
  builder->get_widget("combobox_benchmark", combobox_benchmark);
  std::string benchmark = combobox_benchmark->get_active_text();

  atwork_pb_msgs::SetBenchmarkScenario cmd_scenario;
  if (benchmark == "BNT1") {
    cmd_scenario.mutable_scenario()->set_type(atwork_pb_msgs::BenchmarkScenario::BNT);
    cmd_scenario.mutable_scenario()->set_type_id(1);
  } else if (benchmark == "BMT1") {
    cmd_scenario.mutable_scenario()->set_type(atwork_pb_msgs::BenchmarkScenario::BMT);
    cmd_scenario.mutable_scenario()->set_type_id(1);
  } else if (benchmark == "BTT1") {
    cmd_scenario.mutable_scenario()->set_type(atwork_pb_msgs::BenchmarkScenario::BTT);
    cmd_scenario.mutable_scenario()->set_type_id(1);
  } else if (benchmark == "BTT2") {
    cmd_scenario.mutable_scenario()->set_type(atwork_pb_msgs::BenchmarkScenario::BTT);
    cmd_scenario.mutable_scenario()->set_type_id(2);
  } else if (benchmark == "BTT3") {
    cmd_scenario.mutable_scenario()->set_type(atwork_pb_msgs::BenchmarkScenario::BTT);
    cmd_scenario.mutable_scenario()->set_type_id(3);
  } else if (benchmark == "PPT1") {
    cmd_scenario.mutable_scenario()->set_type(atwork_pb_msgs::BenchmarkScenario::PPT);
    cmd_scenario.mutable_scenario()->set_type_id(1);
  } else if (benchmark == "CBT1") {
    cmd_scenario.mutable_scenario()->set_type(atwork_pb_msgs::BenchmarkScenario::CBT);
    cmd_scenario.mutable_scenario()->set_type_id(1);
  } else if (benchmark == "CBT2") {
    cmd_scenario.mutable_scenario()->set_type(atwork_pb_msgs::BenchmarkScenario::CBT);
    cmd_scenario.mutable_scenario()->set_type_id(2);
  } else if (benchmark == "AWF1") {
    cmd_scenario.mutable_scenario()->set_type(atwork_pb_msgs::BenchmarkScenario::AWF);
    cmd_scenario.mutable_scenario()->set_type_id(1);
  } else if (benchmark == "AST") {
    cmd_scenario.mutable_scenario()->set_type(atwork_pb_msgs::BenchmarkScenario::BTT);
    cmd_scenario.mutable_scenario()->set_type_id(4);
  } else if (benchmark == "None") {
    cmd_scenario.mutable_scenario()->set_type(atwork_pb_msgs::BenchmarkScenario::NONE);
    cmd_scenario.mutable_scenario()->set_type_id(0);
  }
  client.send(cmd_scenario);

  atwork_pb_msgs::SetBenchmarkTransitionEvent cmd_event;
  cmd_event.set_event(atwork_pb_msgs::SetBenchmarkTransitionEvent::RESET);
  client.send(cmd_event);
}


void on_cb_start_click()
{
  if (!client.connected()) return;

  atwork_pb_msgs::ConveyorBeltCommand msg;
  msg.set_command(atwork_pb_msgs::START);
  client.send(msg);
}


void on_cb_stop_click()
{
  if (!client.connected()) return;

  atwork_pb_msgs::ConveyorBeltCommand msg;
  msg.set_command(atwork_pb_msgs::STOP);
  client.send(msg);
}


int main(int argc, char **argv)
{
  llsfrb::YamlConfiguration config(CONFDIR);
  config.load("config.yaml");

  protobuf_comm::MessageRegister &message_register = client.message_register();
  message_register.add_message_type<atwork_pb_msgs::BenchmarkState>();


  Glib::RefPtr<Gtk::Application> app = Gtk::Application::create(argc, argv, "org.atwork.controller");
  builder = Gtk::Builder::create_from_file(std::string(SRCDIR) + std::string("/atwork_controller.glade"));

  Gtk::Window *window = 0;
  builder->get_widget("window1", window);
  window->set_title("RoCKIn RefboxController");
  window->show_all();

  Gtk::Button *button_start = 0;
  Gtk::Button *button_pause = 0;
  Gtk::Button *button_stop = 0;
  Gtk::Button *button_shuffle= 0;
  Gtk::Button *button_next = 0;
  Gtk::Button *button_reset = 0;
  Gtk::Button *button_cb_start = 0;
  Gtk::Button *button_cb_stop = 0;
  builder->get_widget("button_start", button_start);
  builder->get_widget("button_pause", button_pause);
  builder->get_widget("button_stop", button_stop);
  builder->get_widget("button_shuffle", button_shuffle);
  builder->get_widget("button_next", button_next);
  builder->get_widget("button_reset", button_reset);
  builder->get_widget("button_cb_start", button_cb_start);
  builder->get_widget("button_cb_stop", button_cb_stop);

  Glib::signal_timeout().connect(sigc::ptr_fun(&timeout_handler), 100);
  button_start->signal_clicked().connect(sigc::ptr_fun(&on_start_click));
  button_pause->signal_clicked().connect(sigc::ptr_fun(&on_pause_click));
  button_stop->signal_clicked().connect(sigc::ptr_fun(&on_stop_click));
  button_shuffle->signal_clicked().connect(sigc::ptr_fun(&on_shuffle_click));
  button_next->signal_clicked().connect(sigc::ptr_fun(&on_next_click));
  button_reset->signal_clicked().connect(sigc::ptr_fun(&on_reset_click));
  button_cb_start->signal_clicked().connect(sigc::ptr_fun(&on_cb_start_click));
  button_cb_stop->signal_clicked().connect(sigc::ptr_fun(&on_cb_stop_click));

  client.signal_received().connect(handle_message);
  client.signal_disconnected().connect(handle_disconnect);
  host = config.get_string("/llsfrb/shell/refbox-host");
  port = config.get_uint("/llsfrb/shell/refbox-port");
  client.async_connect(host.c_str(), port);

  return app->run(*window);
}

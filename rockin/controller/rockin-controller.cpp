#include <chrono>
#include <iomanip>

#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>

#include <config/yaml.h>
#include <utils/system/argparser.h>

#include <protobuf_comm/client.h>
#include <msgs/BenchmarkState.pb.h>
#include <msgs/BenchmarkFeedback.pb.h>
#include <msgs/ConveyorBelt.pb.h>
#include <msgs/DrillingMachine.pb.h>
#include <msgs/ForceFittingMachine.pb.h>

#include <gtkmm.h>
#include <pangomm.h>
#include <glibmm.h>



protobuf_comm::ProtobufStreamClient client;
std::string host;
int port;
Glib::RefPtr<Gtk::Builder> builder;
std::chrono::time_point<std::chrono::system_clock> last_gui_update;
boost::mutex mutex;
std::shared_ptr<rockin_msgs::BenchmarkState> benchmark_state;



void handle_message(uint16_t comp_id, uint16_t msg_type,
      std::shared_ptr<google::protobuf::Message> msg)
{
  // Prevent simultaneous access to the refbox state from gui and network
  boost::mutex::scoped_lock lock(mutex);

  if (std::dynamic_pointer_cast<rockin_msgs::BenchmarkState>(msg)) {
    benchmark_state = std::dynamic_pointer_cast<rockin_msgs::BenchmarkState>(msg);
  }
}



bool idle_handler() {
  if ((std::chrono::system_clock::now() - last_gui_update) < std::chrono::milliseconds(100)) {
    usleep(10000);
    return true;
  }
  last_gui_update = std::chrono::system_clock::now();


  // Prevent simultaneous access to the refbox state from gui and network
  boost::mutex::scoped_lock lock(mutex);

  if (benchmark_state) {
    Gtk::Button *button_start = 0;
    Gtk::Button *button_pause = 0;
    Gtk::Button *button_stop = 0;
    Gtk::Button *button_success = 0;
    Gtk::Button *button_fail = 0;
    builder->get_widget("button_start", button_start);
    builder->get_widget("button_pause", button_pause);
    builder->get_widget("button_stop", button_stop);
    builder->get_widget("button_success", button_success);
    builder->get_widget("button_fail", button_fail);

    switch (benchmark_state->state()) {
      case rockin_msgs::BenchmarkState::STOPPED:
        button_start->set_sensitive(true);
        button_pause->set_sensitive(false);
        button_stop->set_sensitive(false);
      break;

      case rockin_msgs::BenchmarkState::RUNNING:
        button_start->set_sensitive(false);
        button_pause->set_sensitive(true);
        button_stop->set_sensitive(true);
      break;

      case rockin_msgs::BenchmarkState::PAUSED:
        button_start->set_sensitive(true);
        button_pause->set_sensitive(false);
        button_stop->set_sensitive(false);
      break;

      case rockin_msgs::BenchmarkState::FINISHED:
        if (benchmark_state->phase() == rockin_msgs::BenchmarkState::EXECUTION) {
            button_start->set_sensitive(false);
            button_pause->set_sensitive(false);
            button_stop->set_sensitive(false);
        } else {
            button_start->set_sensitive(true);
            button_pause->set_sensitive(false);
            button_stop->set_sensitive(false);
        }
      break;
    }

    // Only activate in FBM2 during STOPPED or FINISHED state
    if ((benchmark_state->scenario().type() == rockin_msgs::BenchmarkScenario::FBM)
        && (benchmark_state->scenario().type_id() == 2)
        && ((benchmark_state->state() == rockin_msgs::BenchmarkState::STOPPED)
         || benchmark_state->state() == rockin_msgs::BenchmarkState::FINISHED)) {
      button_success->set_sensitive(true);
      button_fail->set_sensitive(true);
    } else {
      button_success->set_sensitive(false);
      button_fail->set_sensitive(false);
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

  rockin_msgs::SetBenchmarkTransitionEvent cmd_event;
  cmd_event.set_event(rockin_msgs::SetBenchmarkTransitionEvent::START);
  client.send(cmd_event);
}


void on_pause_click()
{
  if (!client.connected()) return;

  rockin_msgs::SetBenchmarkTransitionEvent cmd_event;
  cmd_event.set_event(rockin_msgs::SetBenchmarkTransitionEvent::PAUSE);
  client.send(cmd_event);
}


void on_stop_click()
{
  if (!client.connected()) return;

  rockin_msgs::SetBenchmarkTransitionEvent cmd_event;
  cmd_event.set_event(rockin_msgs::SetBenchmarkTransitionEvent::STOP);
  client.send(cmd_event);
}


void on_reset_click()
{
  if (!client.connected()) return;

  Gtk::ComboBoxText *combobox_benchmark = 0;
  builder->get_widget("combobox_benchmark", combobox_benchmark);
  std::string benchmark = combobox_benchmark->get_active_text();

  rockin_msgs::SetBenchmarkScenario cmd_scenario;
  if (benchmark == "FBM1") {
    cmd_scenario.mutable_scenario()->set_type(rockin_msgs::BenchmarkScenario::FBM);
    cmd_scenario.mutable_scenario()->set_type_id(1);
  } else if (benchmark == "FBM2") {
    cmd_scenario.mutable_scenario()->set_type(rockin_msgs::BenchmarkScenario::FBM);
    cmd_scenario.mutable_scenario()->set_type_id(2);
  } else if (benchmark == "FBM3") {
    cmd_scenario.mutable_scenario()->set_type(rockin_msgs::BenchmarkScenario::FBM);
    cmd_scenario.mutable_scenario()->set_type_id(3);
  } else if (benchmark == "TBM1") {
    cmd_scenario.mutable_scenario()->set_type(rockin_msgs::BenchmarkScenario::TBM);
    cmd_scenario.mutable_scenario()->set_type_id(1);
  } else if (benchmark == "TBM2") {
    cmd_scenario.mutable_scenario()->set_type(rockin_msgs::BenchmarkScenario::TBM);
    cmd_scenario.mutable_scenario()->set_type_id(2);
  } else if (benchmark == "TBM3") {
    cmd_scenario.mutable_scenario()->set_type(rockin_msgs::BenchmarkScenario::TBM);
    cmd_scenario.mutable_scenario()->set_type_id(3);
  } else if (benchmark == "None") {
    cmd_scenario.mutable_scenario()->set_type(rockin_msgs::BenchmarkScenario::NONE);
    cmd_scenario.mutable_scenario()->set_type_id(0);
  }
  client.send(cmd_scenario);


  rockin_msgs::SetBenchmarkTransitionEvent cmd_event;
  cmd_event.set_event(rockin_msgs::SetBenchmarkTransitionEvent::RESET);
  client.send(cmd_event);
}


void on_success_click()
{
  if (!client.connected()) return;

  rockin_msgs::BenchmarkFeedback msg;
  msg.set_grasp_notification(true);
  msg.set_phase_to_terminate(benchmark_state->phase());
  client.send(msg);
}


void on_fail_click()
{
  if (!client.connected()) return;

  rockin_msgs::BenchmarkFeedback msg;
  msg.set_grasp_notification(false);
  msg.set_phase_to_terminate(benchmark_state->phase());
  client.send(msg);
}


void on_cb_start_click()
{
  if (!client.connected()) return;

  rockin_msgs::ConveyorBeltCommand msg;
  msg.set_command(rockin_msgs::START);
  client.send(msg);
}


void on_cb_stop_click()
{
  if (!client.connected()) return;

  rockin_msgs::ConveyorBeltCommand msg;
  msg.set_command(rockin_msgs::STOP);
  client.send(msg);
}


void on_dm_down_click()
{
  if (!client.connected()) return;

  rockin_msgs::DrillingMachineCommand msg;
  msg.set_command(rockin_msgs::DrillingMachineCommand::MOVE_DOWN);
  client.send(msg);
}


void on_dm_up_click()
{
  if (!client.connected()) return;

  rockin_msgs::DrillingMachineCommand msg;
  msg.set_command(rockin_msgs::DrillingMachineCommand::MOVE_UP);
  client.send(msg);
}


void on_ffm_down_click()
{
  if (!client.connected()) return;

  rockin_msgs::ForceFittingMachineCommand msg;
  msg.set_command(rockin_msgs::ForceFittingMachineCommand::MOVE_DOWN);
  client.send(msg);
}


void on_ffm_up_click()
{
  if (!client.connected()) return;

  rockin_msgs::ForceFittingMachineCommand msg;
  msg.set_command(rockin_msgs::ForceFittingMachineCommand::MOVE_UP);
  client.send(msg);
}


int main(int argc, char **argv)
{
  llsfrb::YamlConfiguration config(CONFDIR);
  config.load("config.yaml");

  protobuf_comm::MessageRegister &message_register = client.message_register();
  message_register.add_message_type<rockin_msgs::BenchmarkState>();


  Glib::RefPtr<Gtk::Application> app = Gtk::Application::create(argc, argv, "org.rockin.controller");
  builder = Gtk::Builder::create_from_file(std::string(SRCDIR) + std::string("/rockin_controller.glade"));

  Gtk::Window *window = 0;
  builder->get_widget("window1", window);
  window->set_title("RoCKIn RefboxController");
  window->show_all();

  Gtk::Button *button_start = 0;
  Gtk::Button *button_pause = 0;
  Gtk::Button *button_stop = 0;
  Gtk::Button *button_success = 0;
  Gtk::Button *button_fail = 0;
  Gtk::Button *button_reset = 0;
  Gtk::Button *button_cb_start = 0;
  Gtk::Button *button_cb_stop = 0;
  Gtk::Button *button_dm_up = 0;
  Gtk::Button *button_dm_down = 0;
  Gtk::Button *button_ffm_up = 0;
  Gtk::Button *button_ffm_down = 0;
  builder->get_widget("button_start", button_start);
  builder->get_widget("button_pause", button_pause);
  builder->get_widget("button_stop", button_stop);
  builder->get_widget("button_success", button_success);
  builder->get_widget("button_fail", button_fail);
  builder->get_widget("button_reset", button_reset);
  builder->get_widget("button_cb_start", button_cb_start);
  builder->get_widget("button_cb_stop", button_cb_stop);
  builder->get_widget("button_dm_up", button_dm_up);
  builder->get_widget("button_dm_down", button_dm_down);
  builder->get_widget("button_ffm_up", button_ffm_up);
  builder->get_widget("button_ffm_down", button_ffm_down);

  Glib::signal_idle().connect(sigc::ptr_fun(&idle_handler));
  button_start->signal_clicked().connect(sigc::ptr_fun(&on_start_click));
  button_pause->signal_clicked().connect(sigc::ptr_fun(&on_pause_click));
  button_stop->signal_clicked().connect(sigc::ptr_fun(&on_stop_click));
  button_success->signal_clicked().connect(sigc::ptr_fun(&on_success_click));
  button_fail->signal_clicked().connect(sigc::ptr_fun(&on_fail_click));
  button_reset->signal_clicked().connect(sigc::ptr_fun(&on_reset_click));
  button_cb_start->signal_clicked().connect(sigc::ptr_fun(&on_cb_start_click));
  button_cb_stop->signal_clicked().connect(sigc::ptr_fun(&on_cb_stop_click));
  button_dm_up->signal_clicked().connect(sigc::ptr_fun(&on_dm_up_click));
  button_dm_down->signal_clicked().connect(sigc::ptr_fun(&on_dm_down_click));
  button_ffm_up->signal_clicked().connect(sigc::ptr_fun(&on_ffm_up_click));
  button_ffm_down->signal_clicked().connect(sigc::ptr_fun(&on_ffm_down_click));

  client.signal_received().connect(handle_message);
  client.signal_disconnected().connect(handle_disconnect);
  host = config.get_string("/llsfrb/shell/refbox-host");
  port = config.get_uint("/llsfrb/shell/refbox-port");
  client.async_connect(host.c_str(), port);

  return app->run(*window);
}

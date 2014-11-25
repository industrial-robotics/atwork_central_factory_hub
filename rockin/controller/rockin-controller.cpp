#include <chrono>
#include <iomanip>

#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>

#include <config/yaml.h>
#include <utils/system/argparser.h>

#include <protobuf_comm/client.h>
#include <msgs/BenchmarkState.pb.h>
#include <msgs/BenchmarkFeedback.pb.h>

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
    return true;
  }
  last_gui_update = std::chrono::system_clock::now();


  // Prevent simultaneous access to the refbox state from gui and network
  boost::mutex::scoped_lock lock(mutex);

  if (benchmark_state) {
    Gtk::Button *button_start = 0;
    Gtk::Button *button_success = 0;
    Gtk::Button *button_fail = 0;
    builder->get_widget("button_start", button_start);
    builder->get_widget("button_success", button_success);
    builder->get_widget("button_fail", button_fail);

    // Only activate in PAUSED state
    if (benchmark_state->state() == rockin_msgs::BenchmarkState::PAUSED) {
      button_start->set_sensitive(true);
    } else {
      button_start->set_sensitive(false);
    }

    // Only activate in FBM2 during PAUSED or FINISHED state
    if ((benchmark_state->phase().type() == rockin_msgs::BenchmarkPhase::FBM)
        && (benchmark_state->phase().type_id() == 2)
        && ((benchmark_state->state() == rockin_msgs::BenchmarkState::PAUSED)
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
  rockin_msgs::SetBenchmarkState cmd_state;
  cmd_state.set_state(rockin_msgs::BenchmarkState::RUNNING);
  client.send(cmd_state);
}


void on_reset_click()
{
  Gtk::ComboBoxText *combobox_benchmark = 0;
  builder->get_widget("combobox_benchmark", combobox_benchmark);
  std::string phase = combobox_benchmark->get_active_text();

  rockin_msgs::SetBenchmarkPhase cmd_phase;
  if (phase == "FBM1") {
    cmd_phase.mutable_phase()->set_type(rockin_msgs::BenchmarkPhase::FBM);
    cmd_phase.mutable_phase()->set_type_id(1);
  } else if (phase == "FBM2") {
    cmd_phase.mutable_phase()->set_type(rockin_msgs::BenchmarkPhase::FBM);
    cmd_phase.mutable_phase()->set_type_id(2);
  } else if (phase == "TBM1") {
    cmd_phase.mutable_phase()->set_type(rockin_msgs::BenchmarkPhase::TBM);
    cmd_phase.mutable_phase()->set_type_id(1);
  } else if (phase == "TBM2") {
    cmd_phase.mutable_phase()->set_type(rockin_msgs::BenchmarkPhase::TBM);
    cmd_phase.mutable_phase()->set_type_id(2);
  } else if (phase == "TBM3") {
    cmd_phase.mutable_phase()->set_type(rockin_msgs::BenchmarkPhase::TBM);
    cmd_phase.mutable_phase()->set_type_id(3);
  } else if (phase == "None") {
    cmd_phase.mutable_phase()->set_type(rockin_msgs::BenchmarkPhase::NONE);
    cmd_phase.mutable_phase()->set_type_id(0);
  }
  client.send(cmd_phase);


  rockin_msgs::SetBenchmarkState cmd_state;
  cmd_state.set_state(rockin_msgs::BenchmarkState::INIT);
  client.send(cmd_state);
}


void on_success_click()
{
  rockin_msgs::BenchmarkFeedback msg;
  msg.set_grasp_notification(true);
  client.send(msg);
}


void on_fail_click()
{
  rockin_msgs::BenchmarkFeedback msg;
  msg.set_grasp_notification(false);
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
  Gtk::Button *button_success = 0;
  Gtk::Button *button_fail = 0;
  Gtk::Button *button_reset = 0;
  builder->get_widget("button_start", button_start);
  builder->get_widget("button_success", button_success);
  builder->get_widget("button_fail", button_fail);
  builder->get_widget("button_reset", button_reset);

  Glib::signal_idle().connect(sigc::ptr_fun(&idle_handler));
  button_start->signal_clicked().connect(sigc::ptr_fun(&on_start_click));
  button_success->signal_clicked().connect(sigc::ptr_fun(&on_success_click));
  button_fail->signal_clicked().connect(sigc::ptr_fun(&on_fail_click));
  button_reset->signal_clicked().connect(sigc::ptr_fun(&on_reset_click));

  client.signal_received().connect(handle_message);
  client.signal_disconnected().connect(handle_disconnect);
  host = config.get_string("/llsfrb/shell/refbox-host");
  port = config.get_uint("/llsfrb/shell/refbox-port");
  client.async_connect(host.c_str(), port);

  return app->run(*window);
}

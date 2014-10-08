#include <chrono>
#include <iomanip>

#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>

#include <config/yaml.h>
#include <utils/system/argparser.h>

#include <protobuf_comm/client.h>
#include <msgs/BenchmarkState.pb.h>
#include <msgs/DrillingMachine.pb.h>
#include <msgs/ForceFittingMachine.pb.h>
#include <msgs/ConveyorBelt.pb.h>
#include <msgs/RobotInfo.pb.h>
#include <msgs/AttentionMessage.pb.h>
#include <msgs/Inventory.pb.h>
#include <msgs/Order.pb.h>

#include <gtkmm.h>
#include <pangomm.h>
#include <glibmm.h>



protobuf_comm::ProtobufStreamClient client;
std::string host;
int port;
Glib::RefPtr<Gtk::Builder> builder;
std::vector<Gtk::Widget *> robot_widgets;
std::chrono::time_point<std::chrono::system_clock> last_gui_update;
boost::mutex mutex;
std::shared_ptr<rockin_msgs::BenchmarkState> benchmark_state;
std::shared_ptr<rockin_msgs::ConveyorBeltStatus> conveyor_belt_state;
std::shared_ptr<rockin_msgs::DrillingMachineStatus> drilling_machine_state;
std::shared_ptr<rockin_msgs::ForceFittingMachineStatus> force_fitting_machine_state;
std::shared_ptr<rockin_msgs::RobotInfo> robot_info;
std::shared_ptr<rockin_msgs::Inventory> inventory;
std::shared_ptr<rockin_msgs::OrderInfo> order_info;
std::deque<std::string> attention_msgs(12);



class RobotInfoFrame : public Gtk::Frame
{
  public:
    RobotInfoFrame(const std::string &caption, const std::string &info, bool is_robot_lost) {
      // Robot name and team
      Pango::FontDescription fd_robot;
      fd_robot.set_family("Cantarell");
      fd_robot.set_size(14 * Pango::SCALE);
      fd_robot.set_weight(Pango::WEIGHT_BOLD);
      Pango::Attribute font_robot = Pango::Attribute::create_attr_font_desc(fd_robot);
      Pango::Attribute fg_color;
      if (is_robot_lost) fg_color = Pango::Attribute::create_attr_foreground(61423, 10537, 10537);
      else fg_color = Pango::Attribute::create_attr_foreground(35466, 57825, 13364);
      Pango::AttrList attr_list_robot;
      attr_list_robot.insert(font_robot);
      attr_list_robot.insert(fg_color);
      label_robot.set_attributes(attr_list_robot);
      label_robot.set_alignment(0.0);
      label_robot.set_text(caption);
      label_robot.show();


      // Robot host and status
      Pango::FontDescription fd_info;
      fd_info.set_family("Cantarell");
      fd_info.set_size(12 * Pango::SCALE);
      // fd_info.set_weight(Pango::WEIGHT_BOLD);
      Pango::Attribute font_info = Pango::Attribute::create_attr_font_desc(fd_info);
      Pango::AttrList attr_list_info;
      attr_list_info.insert(font_info);
      label_info.set_attributes(attr_list_info);
      label_info.set_alignment(0.0);
      label_info.set_text(info);
      label_info.show();


      // Configure this frame
      set_shadow_type(Gtk::SHADOW_NONE);
      set_label_widget(label_robot);
      add(label_info);
      show();
    }


    virtual ~RobotInfoFrame() {}


  private:
    Gtk::Label label_robot;
    Gtk::Label label_info;
};



void handle_message(uint16_t comp_id, uint16_t msg_type,
      std::shared_ptr<google::protobuf::Message> msg)
{
  // Prevent simultaneous access to the refbox state from gui and network
  boost::mutex::scoped_lock lock(mutex);

  if (std::dynamic_pointer_cast<rockin_msgs::BenchmarkState>(msg)) {
    benchmark_state = std::dynamic_pointer_cast<rockin_msgs::BenchmarkState>(msg);
  }

  if (std::dynamic_pointer_cast<rockin_msgs::ConveyorBeltStatus>(msg)) {
    conveyor_belt_state = std::dynamic_pointer_cast<rockin_msgs::ConveyorBeltStatus>(msg);
  }

  if (std::dynamic_pointer_cast<rockin_msgs::DrillingMachineStatus>(msg)) {
    drilling_machine_state = std::dynamic_pointer_cast<rockin_msgs::DrillingMachineStatus>(msg);
  }

  if (std::dynamic_pointer_cast<rockin_msgs::ForceFittingMachineStatus>(msg)) {
    force_fitting_machine_state = std::dynamic_pointer_cast<rockin_msgs::ForceFittingMachineStatus>(msg);
  }

  if (std::dynamic_pointer_cast<rockin_msgs::RobotInfo>(msg)) {
    robot_info = std::dynamic_pointer_cast<rockin_msgs::RobotInfo>(msg);
  }

  if (std::dynamic_pointer_cast<rockin_msgs::Inventory>(msg)) {
    inventory = std::dynamic_pointer_cast<rockin_msgs::Inventory>(msg);
  }

  if (std::dynamic_pointer_cast<rockin_msgs::OrderInfo>(msg)) {
    order_info = std::dynamic_pointer_cast<rockin_msgs::OrderInfo>(msg);
  }

  std::shared_ptr<rockin_msgs::AttentionMessage> am;
  if ((am = std::dynamic_pointer_cast<rockin_msgs::AttentionMessage>(msg))) {
    attention_msgs.push_back(am->message());
    if (attention_msgs.size() > 12) attention_msgs.pop_front();
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
    // Time
    Gtk::Label *label_time = 0;
    builder->get_widget("label_time", label_time);

    std::chrono::seconds time(benchmark_state->benchmark_time().sec());
    std::chrono::minutes minutes = std::chrono::duration_cast<std::chrono::minutes>(time);
    std::chrono::seconds seconds = time - minutes;

    std::stringstream sstr_time;
    sstr_time << std::setfill('0') << std::setw(2) << minutes.count() << ":"
              << std::setfill('0') << std::setw(2) << seconds.count();
    label_time->set_text(sstr_time.str());


    // State
    Gtk::Label *label_state = 0;
    builder->get_widget("label_state", label_state);
    Pango::Attribute fg_color_state;

    Pango::AttrList attr_list_state = label_state->get_attributes();
    switch (benchmark_state->state()) {
      case rockin_msgs::BenchmarkState::RUNNING:
        fg_color_state = Pango::Attribute::create_attr_foreground(35466, 57825, 13364);
        label_state->set_text("Running");
      break;

      case rockin_msgs::BenchmarkState::PAUSED:
        fg_color_state = Pango::Attribute::create_attr_foreground(13364, 25957, 42148);
        label_state->set_text("Paused");
      break;

      case rockin_msgs::BenchmarkState::FINISHED:
        fg_color_state = Pango::Attribute::create_attr_foreground(61423, 10537, 10537);
        label_state->set_text("Finished");
      break;

      case rockin_msgs::BenchmarkState::STOPPED:
        fg_color_state = Pango::Attribute::create_attr_foreground(61423, 10537, 10537);
        label_state->set_text("Stopped");
      break;
    }
    attr_list_state.insert(fg_color_state);
    label_state->set_attributes(attr_list_state);


    // Phase
    Gtk::Label *label_phase = 0;
    builder->get_widget("label_phase", label_phase);
    Pango::Attribute fg_color_phase;

    Pango::AttrList attr_list_phase = label_phase->get_attributes();
    switch (benchmark_state->phase()) {
      case rockin_msgs::BenchmarkState::EXECUTION:
        fg_color_phase = Pango::Attribute::create_attr_foreground(35466, 57825, 13364);
        label_phase->set_text("Execution");
      break;

      case rockin_msgs::BenchmarkState::CALIBRATION:
        fg_color_phase = Pango::Attribute::create_attr_foreground(61423, 10537, 10537);
        label_phase->set_text("Calibration");
      break;

      case rockin_msgs::BenchmarkState::PREPARATION:
        fg_color_phase = Pango::Attribute::create_attr_foreground(13364, 25957, 42148);
        label_phase->set_text("Preparation");
      break;
    }
    attr_list_phase.insert(fg_color_phase);
    label_phase->set_attributes(attr_list_phase);


    // Benchmark scenario
    Gtk::Label *label_scenario = 0;
    builder->get_widget("label_benchmark_scenario", label_scenario);
    std::stringstream sstr_scenario;

    switch (benchmark_state->scenario().type()) {
      case rockin_msgs::BenchmarkScenario::NONE:
          sstr_scenario << "None";
      break;

      case rockin_msgs::BenchmarkScenario::FBM:
          sstr_scenario << "Functionality Benchmark " << benchmark_state->scenario().type_id();
      break;

      case rockin_msgs::BenchmarkScenario::TBM:
          sstr_scenario << "Task Benchmark " << benchmark_state->scenario().type_id();
      break;
    }
    label_scenario->set_text(sstr_scenario.str());
  }


  if (conveyor_belt_state) {
    Gtk::Label *label_conveyor_belt = 0;
    builder->get_widget("label_conveyor_belt", label_conveyor_belt);

    switch (conveyor_belt_state->state()) {
      case rockin_msgs::ConveyorBeltRunMode::START:
        label_conveyor_belt->set_text("Running");
      break;
      case rockin_msgs::ConveyorBeltRunMode::STOP:
        label_conveyor_belt->set_text("Stopped");
      break;
    }
  }


  if (drilling_machine_state) {
    Gtk::Label *label_drilling_machine = 0;
    builder->get_widget("label_drilling_machine", label_drilling_machine);

    switch (drilling_machine_state->state()) {
      case rockin_msgs::DrillingMachineStatus::AT_BOTTOM:
        label_drilling_machine->set_text("At bottom");
      break;
      case rockin_msgs::DrillingMachineStatus::AT_TOP:
        label_drilling_machine->set_text("At top");
      break;
      case rockin_msgs::DrillingMachineStatus::MOVING_DOWN:
        label_drilling_machine->set_text("Moving down");
      break;
      case rockin_msgs::DrillingMachineStatus::MOVING_UP:
        label_drilling_machine->set_text("Moving up");
      break;
      case rockin_msgs::DrillingMachineStatus::UNKNOWN:
        label_drilling_machine->set_text("Unknown");
      break;
    }
  }


  if (force_fitting_machine_state) {
    Gtk::Label *label_force_fitting_machine = 0;
    builder->get_widget("label_force_fitting_machine", label_force_fitting_machine);

    switch (force_fitting_machine_state->state()) {
      case rockin_msgs::ForceFittingMachineStatus::AT_BOTTOM:
        label_force_fitting_machine->set_text("At bottom");
      break;
      case rockin_msgs::ForceFittingMachineStatus::AT_TOP:
        label_force_fitting_machine->set_text("At top");
      break;
      case rockin_msgs::ForceFittingMachineStatus::MOVING_DOWN:
        label_force_fitting_machine->set_text("Moving down");
      break;
      case rockin_msgs::ForceFittingMachineStatus::MOVING_UP:
        label_force_fitting_machine->set_text("Moving up");
      break;
      case rockin_msgs::ForceFittingMachineStatus::UNKNOWN:
        label_force_fitting_machine->set_text("Unknown");
      break;
    }
  }


  if (robot_info) {
    Gtk::Box *box_robots = 0;
    builder->get_widget("box_robots", box_robots);

    // Remove and delete all widgets inside of the box (the next block is
    // reponsible for adding new widgets)
    for (std::size_t i = 0; i < robot_widgets.size(); i++) {
      box_robots->remove(*robot_widgets[i]);
      delete robot_widgets[i];
    }
    robot_widgets.clear();

    // For each robot create a new widget (in the next cycle the widget is
    // deleted by the previous block)
    for (int i = 0; i < robot_info->robots_size(); i++) {
      struct timeval tv;
      gettimeofday(&tv, 0);
      bool is_robot_lost = ((tv.tv_sec - robot_info->robots(i).last_seen().sec()) > 10);

      std::stringstream sstr_name;
      sstr_name << robot_info->robots(i).name() << " (" << robot_info->robots(i).team() << ")";

      std::stringstream sstr_host;
      sstr_host << robot_info->robots(i).host() << std::endl;
      sstr_host << (is_robot_lost ? "Lost" : "Active");

      if (robot_info->robots(i).has_is_logging()) {
        bool is_logging = robot_info->robots(i).is_logging();
        sstr_host << ", " << (is_logging ? "Logging" : "Not Logging");
      }

      RobotInfoFrame *frame = new RobotInfoFrame(sstr_name.str(), sstr_host.str(), is_robot_lost);
      box_robots->pack_end(*frame);

      robot_widgets.push_back(frame);
    }
  }


  if (inventory) {
    Gtk::Label *label_inventory = 0;
    builder->get_widget("label_inventory", label_inventory);

    std::stringstream sstr;
    for (int i = 0; i < inventory->items_size(); i++) {
      const rockin_msgs::Inventory::Item &item = inventory->items(i);
      sstr << item.object().description() << ": ";

      if (inventory->items(i).has_container()) sstr << item.container().description();
      else if (inventory->items(i).has_location()) sstr << item.location().description();

      sstr << std::endl;
    }
    label_inventory->set_text(sstr.str());
  }


  if (order_info) {
    Gtk::Label *label_orders = 0;
    builder->get_widget("label_orders", label_orders);

    std::stringstream sstr;
    for (int i = 0; i < order_info->orders_size(); i++) {
      const rockin_msgs::Order &order = order_info->orders(i);

      if (order.has_quantity_requested()) sstr << order.quantity_requested() << " ";
      sstr << order.object().description() << " -> ";
      if (order.has_destination()) sstr << order.destination().description();
      else if (order.has_container()) sstr << order.container().description();

      if (order.has_processing_team()) sstr << " [" << order.processing_team() << "]";

      sstr << std::endl;
    }

    label_orders->set_text(sstr.str());
  }

  std::stringstream sstr_attention_messages;
  Gtk::Label *label_attention_messages = 0;
  builder->get_widget("label_attention_messages", label_attention_messages);
  for (std::size_t i = 0; i < attention_msgs.size(); i++) {
    sstr_attention_messages << attention_msgs[i] << std::endl;
  }
  label_attention_messages->set_text(sstr_attention_messages.str());

  return true;
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

  protobuf_comm::MessageRegister &message_register = client.message_register();
  message_register.add_message_type<rockin_msgs::BenchmarkState>();
  message_register.add_message_type<rockin_msgs::DrillingMachineStatus>();
  message_register.add_message_type<rockin_msgs::ConveyorBeltStatus>();
  message_register.add_message_type<rockin_msgs::RobotInfo>();
  message_register.add_message_type<rockin_msgs::AttentionMessage>();
  message_register.add_message_type<rockin_msgs::Inventory>();
  message_register.add_message_type<rockin_msgs::OrderInfo>();


  Glib::RefPtr<Gtk::Application> app = Gtk::Application::create(argc, argv, "org.rockin.viewer");
  builder = Gtk::Builder::create_from_file(std::string(SRCDIR) + std::string("/rockin_viewer.glade"));

  Gtk::Window *window = 0;
  builder->get_widget("window1", window);
  window->set_title("RoCKIn RefboxViewer");
  window->show_all();

  Glib::signal_idle().connect(sigc::ptr_fun(&idle_handler));

  client.signal_received().connect(handle_message);
  client.signal_disconnected().connect(handle_disconnect);
  host = config.get_string("/llsfrb/shell/refbox-host");
  port = config.get_uint("/llsfrb/shell/refbox-port");
  client.async_connect(host.c_str(), port);


  return app->run(*window);
}

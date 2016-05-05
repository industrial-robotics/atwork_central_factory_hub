/***************************************************************************
 *  triggered_conveyor_belt_plugin.cpp - Triggered conveyor belt plugin
 *
 *  Created: Mon Oct 12 15:39:11 2015
 *  Copyright  2015 Sven Schneider
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

#include <core/plugin.h>

#include "triggered_conveyor_belt_thread.h"
#include "triggered_conveyor_belt_simulation_thread.h"

using namespace fawkes;

/** Plugin to interface with the triggered conveyor belt device. The triggered
 * conveyor belt is the composition of the conveyor belt (which can be started
 * and stopped via the plugin) and the quality control camera (which can also
 * stop the conveyor belt and provides the detected object to this plugin).
 *
 * @author Sven Schneider
 */
class TriggeredConveyorBeltPlugin: public fawkes::Plugin
{
    public:
        /** Constructor.
         * @param config Fawkes configuration
         */
        TriggeredConveyorBeltPlugin(Configuration *config) :
                Plugin(config)
        {
            if (config->exists("/llsfrb/triggered-conveyor-belt/mode"))
            {
                std::string config_mode = config->get_string("/llsfrb/triggered-conveyor-belt/mode");
                if (config_mode == std::string("real"))
                    thread_list.push_back(new TriggeredConveyorBeltThread());
                else if (config_mode == std::string("simulation"))
                    thread_list.push_back(new TriggeredConveyorBeltSimulationThread());
            }
        }
};

PLUGIN_DESCRIPTION("Plugin to communicate with or simulate the triggered conveyor belt")
EXPORT_PLUGIN(TriggeredConveyorBeltPlugin)

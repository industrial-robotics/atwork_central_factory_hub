/***************************************************************************
 *  quality_control_camera_plugin.cpp - Quality control camera plugin
 *
 *  Created: Mon Nov 12 10:25:11 2014
 *  Copyright  2014 Frederik Hegger
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

#include "quality_control_camera_thread.h"

using namespace fawkes;

/** Plugin to interface with quality control camera
 * @author Frederik Hegger
 */
class QualityControlCameraPlugin: public fawkes::Plugin
{
    public:
        /** Constructor.
         * @param config Fawkes configuration
         */
        QualityControlCameraPlugin(Configuration *config) :
                Plugin(config)
        {
            thread_list.push_back(new QualityControlCameraThread());
        }
};

PLUGIN_DESCRIPTION("Plugin to communicate with the quality control camera")
EXPORT_PLUGIN(QualityControlCameraPlugin)

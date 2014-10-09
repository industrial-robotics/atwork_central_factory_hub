#*****************************************************************************
#               Makefile Build System for Fawkes: LLSF Machine
#
#   Created on Wed Aug 20 17:00:00 2014
#   Copyright (C) 2011 by Tim Niemueller, AllemaniACs RoboCup Team
#
##*****************************************************************************
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#*****************************************************************************

BASEDIR = ../../..

include $(BASEDIR)/etc/buildsys/config.mk
include $(BUILDSYSDIR)/protobuf.mk
include $(BUILDSYSDIR)/clips.mk
include $(BUILDSYSDIR)/boost.mk
include $(BASEDIR)/rockin/rockin.mk

REQ_BOOST_LIBS = thread system

LIBS_conveyor_belt = llsfrbcore llsfrbaspects rockin_msgs zmq
OBJS_conveyor_belt = conveyor_belt_plugin.o conveyor_belt_thread.o

OBJS_all    = $(OBJS_conveyor_belt)

PLUGINS_all = $(PLUGINDIR)/conveyor_belt.$(SOEXT)

CFLAGS  += $(call boost-libs-cflags,$(REQ_BOOST_LIBS))
LDFLAGS += $(call boost-libs-ldflags,$(REQ_BOOST_LIBS))

ifeq ($(HAVE_CLIPS),1)
  CFLAGS  += $(CFLAGS_CLIPS) ${CFLAGS_PROTOBUF}
  LDFLAGS += $(LDFLAGS_CLIPS) ${LDFLAGS_PROTOBUF}
else
  WARN_TARGETS += warning_clips
endif

ifeq ($(OBJSSUBMAKE),1)
all: $(WARN_TARGETS) $(WARN_TARGETS_BOOST)
.PHONY: warning_clips
warning_clips:
	$(SILENT)echo -e "$(INDENT_PRINT)--> $(TRED)Cannot build Conveyor Belt Plugin$(TNORMAL) (clipsmm not found)"
endif

include $(BUILDSYSDIR)/base.mk

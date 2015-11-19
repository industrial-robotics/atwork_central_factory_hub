#*****************************************************************************
#               Makefile Build System for Fawkes: RoCKIn drilling machine
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
include $(BUILDSYSDIR)/libzmq.mk
include $(BASEDIR)/rockin/rockin.mk

REQ_BOOST_LIBS = thread system
HAVE_BOOST_LIBS = $(call boost-have-libs,$(REQ_BOOST_LIBS))

LIBS_drilling_machine = llsfrbcore llsfrbaspects rockin_plugin_msgs
OBJS_drilling_machine = drilling_machine_plugin.o drilling_machine_thread.o \
                        drilling_machine_simulation_thread.o \
                        drilling_machine_mockup_thread.o

ifeq ($(HAVE_CLIPS)$(HAVE_PROTOBUF)$(HAVE_LIBZMQ)$(HAVE_BOOST_LIBS),1111)
  OBJS_all    = $(OBJS_drilling_machine)
  PLUGINS_all = $(PLUGINDIR)/drilling_machine.$(SOEXT)

  CFLAGS  += $(CFLAGS_CLIPS) ${CFLAGS_PROTOBUF} ${CFLAGS_LIBZMQ} \
             $(call boost-libs-cflags,$(REQ_BOOST_LIBS))
  LDFLAGS += $(LDFLAGS_CLIPS) ${LDFLAGS_PROTOBUF} ${LDFLAGS_LIBZMQ} \
             $(call boost-libs-ldflags,$(REQ_BOOST_LIBS))
else
  ifneq ($(HAVE_CLIPS),1)
    WARN_TARGETS += warning_clips
  endif
  ifneq ($(HAVE_PROTOBUF),1)
    WARN_TARGETS += warning_protobuf
  endif
  ifneq ($(HAVE_LIBZMQ),1)
    WARN_TARGETS += warning_libzmq
  endif
  ifneq ($(HAVE_BOOST_LIBS),1)
    WARN_TARGETS_BOOST = $(foreach l,$(REQ_BOOST_LIBS),$(if $(call boost-have-lib,$l),, warning_boost_$l))
  endif
endif

ifeq ($(OBJSSUBMAKE),1)
all: $(WARN_TARGETS) $(WARN_TARGETS_BOOST)
.PHONY: warning_libmodbus warning_clips
warning_libzmq:
	$(SILENT)echo -e "$(INDENT_PRINT)--> $(TRED)Cannot build Drilling Machine Plugin$(TNORMAL) (libzmq not found)"
warning_clips:
	$(SILENT)echo -e "$(INDENT_PRINT)--> $(TRED)Cannot build Drilling Machine Plugin$(TNORMAL) (clipsmm not found)"
warning_protobuf:
	$(SILENT)echo -e "$(INDENT_PRINT)--> $(TRED)Cannot build Drilling Machine Plugin$(TNORMAL) (protobuf not found)"
$(WARN_TARGETS_BOOST): warning_boost_%:
	$(SILENT)echo -e "$(INDENT_PRINT)--> $(TRED)Cannot build Drilling Machine Plugin$(TNORMAL) (Boost library $* not found)"
endif

include $(BUILDSYSDIR)/base.mk

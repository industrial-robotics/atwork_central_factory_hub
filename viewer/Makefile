#*****************************************************************************
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#*****************************************************************************

BASEDIR = ../..

include $(BASEDIR)/etc/buildsys/config.mk
include $(BUILDSYSDIR)/gui.mk
include $(BUILDSYSDIR)/protobuf.mk
include $(BUILDSYSDIR)/boost.mk
include $(BASEDIR)/rockin/rockin.mk

CFLAGS += $(CFLAGS_CPP11)

REQ_BOOST_LIBS = thread asio system signals2
HAVE_BOOST_LIBS = $(call boost-have-libs,$(REQ_BOOST_LIBS))

LIBS_rockin_viewer = stdc++ llsfrbcore llsfrbutils llsfrbconfig llsf_protobuf_comm rockin_msgs
OBJS_rockin_viewer = rockin-viewer.o

ifeq ($(HAVE_PROTOBUF)$(HAVE_BOOST_LIBS)$(HAVE_GTKMM),111)
  OBJS_all += $(OBJS_rockin_viewer)
  BINS_all += $(BINDIR)/rockin-viewer

  CFLAGS += $(CFLAGS_PROTOBUF) $(CFLAGS_GTKMM) \
            $(call boost-libs-cflags,$(REQ_BOOST_LIBS))
  LDFLAGS += $(LDFLAGS_PROTOBUF) $(LDFLAGS_GTKMM) \
             $(call boost-libs-ldflags,$(REQ_BOOST_LIBS))
else
  ifneq ($(HAVE_PROTOBUF),1)
    WARN_TARGETS += warning_protobuf
  endif
  ifneq ($(HAVE_BOOST_LIBS),1)
    WARN_TARGETS_BOOST = $(foreach l,$(REQ_BOOST_LIBS),$(if $(call boost-have-lib,$l),, warning_boost_$l))
  endif
endif

ifeq ($(OBJSSUBMAKE),1)
all: $(WARN_TARGETS) $(WARN_TARGETS_BOOST) $(WARN_TARGETS_GTKMM)
.PHONY: warning_protobuf warning_gtkmm
warning_protobuf:
	$(SILENT)echo -e "$(INDENT_PRINT)--> $(TRED)Cannot build RoCKIn viewer$(TNORMAL) (protobuf not found)"
$(WARN_TARGETS_BOOST): warning_boost_%:
	$(SILENT)echo -e "$(INDENT_PRINT)--> $(TRED)Cannot build RoCKIn viewer$(TNORMAL) (Boost library $* not found)"
endif

include $(BUILDSYSDIR)/base.mk
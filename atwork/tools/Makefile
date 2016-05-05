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
include $(BUILDSYSDIR)/protobuf.mk
include $(BUILDSYSDIR)/boost.mk
include $(BASEDIR)/atwork/atwork.mk

CFLAGS += $(CFLAGS_CPP11)

REQ_BOOST_LIBS = thread asio system signals2
HAVE_BOOST_LIBS = $(call boost-have-libs,$(REQ_BOOST_LIBS))

LIBS_atwork_fake_robot = stdc++ llsfrbcore llsfrbutils llsfrbconfig llsf_protobuf_comm atwork_pb_msgs
OBJS_atwork_fake_robot = atwork-fake-robot.o

LIBS_atwork_device_ctrl = stdc++ llsfrbcore llsfrbutils llsfrbconfig llsf_protobuf_comm atwork_pb_msgs
OBJS_atwork_device_ctrl = atwork-device-ctrl.o

LIBS_atwork_benchmark_ctrl = stdc++ llsfrbcore llsfrbutils llsfrbconfig llsf_protobuf_comm atwork_pb_msgs
OBJS_atwork_benchmark_ctrl = atwork-benchmark-ctrl.o

LIBS_atwork_attention_msg_vis = stdc++ llsfrbcore llsfrbutils llsfrbconfig llsf_protobuf_comm atwork_pb_msgs
OBJS_atwork_attention_msg_vis = atwork-attention-msg-vis.o

ifeq ($(HAVE_PROTOBUF)$(HAVE_BOOST_LIBS),11)
  OBJS_all += $(OBJS_atwork_fake_robot) $(OBJS_atwork_device_ctrl) $(OBJS_atwork_benchmark_ctrl) $(OBJS_atwork_attention_msg_vis)
  BINS_all += $(BINDIR)/atwork-fake-robot $(BINDIR)/atwork-device-ctrl $(BINDIR)/atwork-benchmark-ctrl $(BINDIR)/atwork-attention-msg-vis

  CFLAGS_atwork_fake_robot += $(CFLAGS_PROTOBUF) \
                              $(call boost-libs-cflags,$(REQ_BOOST_LIBS))
  LDFLAGS_atwork_fake_robot += $(LDFLAGS_PROTOBUF) \
                               $(call boost-libs-ldflags,$(REQ_BOOST_LIBS))

  CFLAGS_atwork_device_ctrl += $(CFLAGS_PROTOBUF) \
                               $(call boost-libs-cflags,$(REQ_BOOST_LIBS))
  LDFLAGS_atwork_device_ctrl += $(LDFLAGS_PROTOBUF) \
                                $(call boost-libs-ldflags,$(REQ_BOOST_LIBS))

  CFLAGS_atwork_benchmark_ctrl += $(CFLAGS_PROTOBUF) \
                               $(call boost-libs-cflags,$(REQ_BOOST_LIBS))
  LDFLAGS_atwork_benchmark_ctrl += $(LDFLAGS_PROTOBUF) \
                                $(call boost-libs-ldflags,$(REQ_BOOST_LIBS))

  CFLAGS_atwork_attention_msg_vis += $(CFLAGS_PROTOBUF) \
                               $(call boost-libs-cflags,$(REQ_BOOST_LIBS))
  LDFLAGS_atwork_attention_msg_vis += $(LDFLAGS_PROTOBUF) \
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
all: $(WARN_TARGETS) $(WARN_TARGETS_BOOST)
.PHONY:
$(WARN_TARGETS_BOOST): warning_boost_%:
	$(SILENT)echo -e "$(INDENT_PRINT)--> $(TRED)Cannot build tools $(TNORMAL) (Boost library $* not found)"
endif

include $(BUILDSYSDIR)/base.mk

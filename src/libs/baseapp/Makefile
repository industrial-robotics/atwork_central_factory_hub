#*****************************************************************************
#          Makefile Build System for Fawkes: Base Application Library
#                            -------------------
#   Created on Wed May 04 21:20:58 2011
#   Copyright (C) 2006-2011 by Tim Niemueller, AllemaniACs RoboCup Team
#
#*****************************************************************************
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#*****************************************************************************

BASEDIR = ../../..
include $(BASEDIR)/etc/buildsys/config.mk

LIBS_libllsfrbbaseapp = stdc++ llsfrbcore llsfrbutils llsfrbconfig \
			llsfrbplugin llsfrbaspects llsfrblogging

OBJS_libllsfrbbaseapp =	$(patsubst %.cpp,%.o,$(patsubst qa/%,,$(subst $(SRCDIR)/,,$(realpath $(wildcard $(SRCDIR)/*.cpp)))))
HDRS_libllsfrbbaseapp = $(subst $(SRCDIR)/,,$(wildcard $(SRCDIR)/*.h))

OBJS_all = $(OBJS_libllsfrbbaseapp)
LIBS_all  = $(LIBDIR)/libllsfrbbaseapp.so

include $(BUILDSYSDIR)/base.mk


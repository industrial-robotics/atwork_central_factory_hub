#*****************************************************************************
#                Makefile Build System for Fawkes: ZeroMQ bits
#                            -------------------
#   Created on Thu Oct 09 13:50:00 2014 (Sankt Augustin)
#   Copyright (C) 2014 by Sven Schneider
#
#*****************************************************************************
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#*****************************************************************************

ifndef __buildsys_config_mk_
$(error config.mk must be included before libzmq.mk)
endif

ifndef __buildsys_libzmq_mk_
__buildsys_libzmq_mk_ := 1

LIBZMQ_MIN_VERSION=2.0.0

ifneq ($(PKGCONFIG),)
  HAVE_LIBZMQ = $(if $(shell $(PKGCONFIG) --atleast-version=$(LIBZMQ_MIN_VERSION) 'libzmq'; echo $${?/1/}),1,0)
endif

ifeq ($(HAVE_LIBZMQ),1)
  CFLAGS_LIBZMQ  = -DHAVE_LIBZMQ $(shell $(PKGCONFIG) --cflags 'libzmq')
  LDFLAGS_LIBZMQ = $(shell $(PKGCONFIG) --libs 'libzmq')
endif

endif # __buildsys_libzmq_mk_


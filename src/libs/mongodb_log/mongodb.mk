#*****************************************************************************
#            Makefile Build System for Fawkes: MongoDB Plugin
#                            -------------------
#   Created on Sun Dec 05 23:03:18 2010 (Steelers vs. Baltimore)
#   Copyright (C) 2006-2010 by Tim Niemueller, AllemaniACs RoboCup Team
#
#*****************************************************************************
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#*****************************************************************************

include $(BUILDSYSDIR)/boost.mk

ifneq ($(PKGCONFIG),)
  HAVE_LIBSSL := $(if $(shell $(PKGCONFIG) --exists 'openssl'; echo $${?/1/}),1,0)
endif
ifeq ($(HAVE_LIBSSL),1)
  CFLAGS_LIBSSL  += -DHAVE_LIBSSL $(shell $(PKGCONFIG) --cflags 'openssl')
  LDFLAGS_LIBSSL += $(shell $(PKGCONFIG) --libs 'openssl')
endif

ifneq ($(wildcard /usr/include/mongo/client/dbclient.h /usr/local/include/mongo/client/dbclient.h),)
  ifeq ($(call boost-have-libs,thread system filesystem)$(HAVE_LIBSSL),11)
    HAVE_MONGODB = 1

    # Unfortunately, the MongoDB header files don't include a macro with the
    # version number. Therefore, we try to extract the version from the mongo
    # binary and hope for the best.
    ifneq ($(wildcard /usr/bin/mongo /usr/local/bin/mongo),)
      MONGODB_VERSION_MAJOR = $(shell mongo --version | grep -o "[0-9]\.[0-9]\.[0-9]" | cut -f1 -d\.)
      MONGODB_VERSION_MINOR = $(shell mongo --version | grep -o "[0-9]\.[0-9]\.[0-9]" | cut -f2 -d\.)
      MONGODB_VERSION_PATCH = $(shell mongo --version | grep -o "[0-9]\.[0-9]\.[0-9]" | cut -f3 -d\.)
    endif

    CFLAGS_MONGODB  = -DHAVE_MONGODB \
          -DMONGODB_VERSION_MAJOR=$(MONGODB_VERSION_MAJOR) \
          -DMONGODB_VERSION_MINOR=$(MONGODB_VERSION_MINOR) \
          -DMONGODB_VERSION_PATCH=$(MONGODB_VERSION_PATCH) \
          $(CFLAGS_LIBSSL)
    LDFLAGS_MONGODB = -lm -lpthread -lmongoclient \
		      $(call boost-libs-ldflags,thread system filesystem) \
		      $(LDFLAGS_LIBSSL)
  endif
endif

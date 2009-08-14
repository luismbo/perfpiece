# -*- indent-tabs-mode: t -*-

OSTYPE = $(shell uname)

CC             := gcc
CFLAGS         := -Wall -lpapi -pthread
SHLIB_CFLAGS   := -shared
SHLIB_EXT      := .so

ifneq ($(if $(filter Linux %BSD,$(OSTYPE)),OK), OK)
ifeq ($(OSTYPE), Darwin)
SHLIB_CFLAGS   := -bundle
else
ifeq ($(OSTYPE), SunOS)
CFLAGS         += -c
else
# Let's assume this is win32
SHLIB_EXT      := .dll
endif
endif
endif

ARCH = $(shell uname -m)

#ifneq ($(ARCH), x86_64)
#CFLAGS += -lm
#endif

ifeq ($(ARCH), x86_64)
CFLAGS += -fPIC
endif

# Are all G5s ppc970s?
ifeq ($(ARCH), ppc970)
CFLAGS += -m64
endif

shlib: libppmonitor$(SHLIB_EXT) 

libppmonitor$(SHLIB_EXT): ppmonitor.c
	$(CC) -o $@ $(SHLIB_CFLAGS) $(CFLAGS) $<

clean:
	rm -f *.so *.dylib *.dll *.bundle

# vim: ft=make noet

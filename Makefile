# Variables to override
#
# CC            C compiler. MUST be set if crosscompiling
# CROSSCOMPILE	crosscompiler prefix, if any
# CFLAGS        compiler flags for compiling all C files
# LDFLAGS       linker flags for linking all binaries
# ERTS_DIR      Erlang runtime directory.
# ERL_LDFLAGS   additional linker flags for projects referencing Erlang libraries

# Check that we're on a supported build platform
ifeq ($(CROSSCOMPILE),)
    # Not crosscompiling, so check that we're on Linux.
    ifneq ($(shell uname -s),Linux)
        $(warning this dhcp server only works on Linux, but crosscompilation)
        $(warning is supported by defining $$CROSSCOMPILE and $$ERL_LDFLAGS.)
        $(warning See Makefile for details. If using Nerves,)
        $(warning this should be done automatically.)
        $(warning .)
        $(warning Skipping C compilation unless targets explicitly passed to make.)
	DEFAULT_TARGETS = priv
    endif
endif

ifeq ($(ERTS_DIR),)
ERTS_DIR = $(shell erl -eval "io:format(\"~s/erts-~s~n\", [code:root_dir(), erlang:system_info(version)])" -s init stop -noshell)
ifeq ($(ERTS_DIR),)
   $(error Could not find the Erlang installation. Check to see that 'erl' is in your PATH)
endif
endif

# If not cross-compiling, then run sudo by default
ifeq ($(origin CROSSCOMPILE), undefined)
	SUDO_ASKPASS ?= /usr/bin/ssh-askpass
	SUDO ?= sudo
	else
	# If cross-compiling, then permissions need to be set some build system-dependent way
	SUDO ?= true
endif

DEFAULT_TARGETS ?= priv priv/dhcp_server.so

LDFLAGS += -shared -fPIC
CFLAGS ?= -O2 -Wall -fPIC -std=c99

.PHONY: all clean

all: $(DEFAULT_TARGETS)

c_src/dhcp_server.o:
	$(CC) -c $(CFLAGS) -o $@ -I$(ERTS_DIR)/include  c_src/dhcp_server.c

priv:
	mkdir -p priv

priv/dhcp_server.so: c_src/dhcp_server.o
	$(CC) $^ $(ERL_LDFLAGS) $(LDFLAGS) -o $@
	SUDO_ASKPASS=$(SUDO_ASKPASS) $(SUDO) -- sh -c 'chown root:root $@; chmod +s $@'

clean:
	rm -f priv/dhcp_server.* c_src/*.o

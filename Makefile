# Variables to override
#
# CC            C compiler. MUST be set if crosscompiling
# CROSSCOMPILE	crosscompiler prefix, if any
# CFLAGS        compiler flags for compiling all C files
# LDFLAGS       linker flags for linking all binaries
# ERTS_DIR      Erlang runtime directory.
# ERL_LDFLAGS   additional linker flags for projects referencing Erlang libraries

PREFIX = $(MIX_COMPILE_PATH)/../priv
BUILD = $(MIX_COMPILE_PATH)/../obj

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


DEFAULT_TARGETS ?= $(PREFIX) $(PREFIX)/dhcp_server.so

LDFLAGS += -shared -fPIC
CFLAGS ?= -O2 -Wall -fPIC -std=c99

.PHONY: all clean

all: $(DEFAULT_TARGETS)

$(BUILD)/dhcp_server.o:
	$(CC) -c $(CFLAGS) -o $@ -I$(ERTS_DIR)/include  c_src/dhcp_server.c

$(PREFIX):
	mkdir -p $(BUILD)
	mkdir -p $(PREFIX)

$(PREFIX)/dhcp_server.so: $(BUILD)/dhcp_server.o
	$(CC) $^ $(ERL_LDFLAGS) $(LDFLAGS) -o $@

clean:
	rm -f $(PREFIX)/dhcp_server.* $(BUILD)/*.o

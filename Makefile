SRC_DIR := ./src
OS ?= $(shell uname)
CPU ?= $(shell uname -m)
ifeq ($(CPU),x86_64)
	ABI ?= AMD64
else
	ifeq ($(CPU),riscv64)
		ABI ?= RISCV64
	else
		ifeq ($(CPU),loongarch64)
			CPU := la64
			ABI ?= LA64
		else
			CPU := arm64
			ABI ?= ARM64
		endif
	endif
endif

EXE_EXT ?=

#There is one host program, chrysalisp. What a node has, a GUI, audio, is
#what the host finds as it starts, a node that has none is a terminal's.
#The GUI driver is sdl3, if SDL3 is on this machine, else sdl, which is
#SDL2, else none, a host with no GUI and no audio, which needs nothing.
#Say which with GUI=, one of sdl3 sdl raw3 raw fb none.
ifeq ($(GUI),)
ifneq ($(SDL3_PREFIX)$(shell cat sdl3_prefix 2>/dev/null),)
	GUI := sdl3
else
ifeq ($(shell pkg-config --exists sdl3 2>/dev/null && echo yes),yes)
	GUI := sdl3
else
ifneq ($(shell sdl2-config --version 2>/dev/null),)
	GUI := sdl
else
	GUI := none
endif
endif
endif
endif

HGUI := $(shell echo $(GUI) | tr '[:upper:]' '[:lower:]')

#each GUI driver has its own objects, so a change of GUI= is a build of that driver
HOST_GUI := 0
OBJ_GUI := gui
ifeq ($(HGUI),sdl)
	HOST_GUI := 0
endif
ifeq ($(HGUI),fb)
	HOST_GUI := 1
	OBJ_GUI := gui_fb
#the frame buffer GUI has no SDL under it. Its sound is ALSA, if the
#development files of ALSA are on the machine, else it has none. The two
#are built in folders of their own, every file is built for one or the other.
ifeq ($(shell pkg-config --exists alsa 2>/dev/null && echo yes),yes)
	FB_ALSA := yes
	OBJ_GUI := gui_fb_alsa
endif
endif
ifeq ($(HGUI),raw)
	HOST_GUI := 2
	OBJ_GUI := gui_raw
endif
ifeq ($(HGUI),sdl3)
	HOST_GUI := 3
	OBJ_GUI := gui_sdl3
endif
ifeq ($(HGUI),raw3)
	HOST_GUI := 4
	OBJ_GUI := gui_raw3
endif
ifeq ($(HGUI),none)
	HOST_GUI := none
	OBJ_GUI := gui_none
endif
ifneq ($(HOST_GUI),none)
	GUI_FLAGS := -D_HOST_GUI=$(HOST_GUI)
endif

OBJ_DIR_GUI := ./src/obj/$(CPU)/$(ABI)/$(OS)/$(OBJ_GUI)
#which GUI driver the host was last linked with
GUI_DRIVER := ./src/obj/$(CPU)/$(ABI)/$(OS)/gui_driver

SRC_DIRS := $(shell find $(SRC_DIR) -type d | grep -v "/obj")
OBJ_DIRS := $(patsubst $(SRC_DIR)/%,$(OBJ_DIR_GUI)/%,$(SRC_DIRS))

SRC_FILES_CORE := src/host/main.cpp src/host/vp64.cpp src/host/net.cpp src/host/pii_linux.cpp src/host/pii_windows.cpp src/host/pii_darwin.cpp
SRC_FILES_DRIVERS := src/host/audio_sdl.cpp src/host/audio_sdl3.cpp src/host/audio_alsa.cpp src/host/gui_sdl.cpp src/host/gui_sdl3.cpp src/host/gui_raw.cpp src/host/gui_fb.c src/host/video.cpp src/host/sdl3_load.cpp

OBJ_FILES_CORE_GUI := $(patsubst src/%.cpp,$(OBJ_DIR_GUI)/%.o,$(SRC_FILES_CORE))
OBJ_FILES_DRIVERS_GUI := $(patsubst src/%.cpp,$(OBJ_DIR_GUI)/%.o,$(SRC_FILES_DRIVERS))
OBJ_FILES_DRIVERS_GUI := $(patsubst src/%.c,$(OBJ_DIR_GUI)/%.o,$(OBJ_FILES_DRIVERS_GUI))

OBJ_FILES := $(OBJ_FILES_CORE_GUI) $(OBJ_FILES_DRIVERS_GUI)

CFLAGS ?= -O3 -nostdlib -fno-exceptions -MMD
CPPFLAGS ?= -std=c++14
LDFLAGS ?=
ifneq ($(OS),Windows)
	LDFLAGS += -pthread
endif
ifeq ($(OS),Darwin)
	LDFLAGS += -framework Cocoa -lobjc
endif

ifeq ($(HOST_GUI),none)
	HOST_AUDIO := -1
	AUDIO_FLAGS :=
else
ifneq ($(HOST_GUI),1)
	HOST_AUDIO := 0
ifeq ($(filter 3 4,$(HOST_GUI)),)
	SDL_CFLAGS ?= $(shell sdl2-config --cflags)
	SDL_LIBS ?= $(shell sdl2-config --libs) -lSDL2_mixer
endif
	AUDIO_FLAGS := -D_HOST_AUDIO=$(HOST_AUDIO)
else
ifeq ($(FB_ALSA),yes)
	HOST_AUDIO := 2
	SDL_CFLAGS ?= $(shell pkg-config --cflags alsa)
	SDL_LIBS ?= $(shell pkg-config --libs alsa)
	AUDIO_FLAGS := -D_HOST_AUDIO=$(HOST_AUDIO)
else
	HOST_AUDIO := -1
	SDL_CFLAGS ?= 
	SDL_LIBS ?= 
	AUDIO_FLAGS := -Dhost_audio_funcs=nullptr
endif
endif
endif

#SDL2 and SDL3 share the names of their calls, so one program can not
#link both. The sdl3 and raw3 GUI drivers go with the sdl3 AUDIO driver,
#which does its own mixing, there is no mixer library.
#SDL3 is found with pkg-config. On a machine with no pkg-config, give the
#folder SDL3 was installed to, once, and it is kept in the file sdl3_prefix,
#make GUI=sdl3 SDL3_PREFIX=$$HOME/sdl3_build/install
#SDL3 is not linked, the host loads it as it starts, see src/host/sdl3_load.h,
#the one it was built with first, so a machine with no SDL3 runs it too
ifneq ($(filter 3 4,$(HOST_GUI)),)
	HOST_AUDIO := 1
	AUDIO_FLAGS := -D_HOST_AUDIO=$(HOST_AUDIO)
endif
#a cross build for Windows gives its own SDL flags, see Makefile.mingw
ifneq ($(OS),Windows)
ifneq ($(filter 3 4,$(HOST_GUI)),)
ifeq ($(SDL3_PREFIX),)
	SDL3_PREFIX := $(shell cat sdl3_prefix 2>/dev/null)
else
	SDL3_KEEP := $(shell echo $(SDL3_PREFIX) > sdl3_prefix)
endif
ifneq ($(SDL3_PREFIX),)
	SDL_CFLAGS := -I$(SDL3_PREFIX)/include
	SDL3_LIBDIR := $(SDL3_PREFIX)/lib
else
	SDL_CFLAGS := $(shell pkg-config --cflags sdl3)
	SDL3_LIBDIR := $(shell pkg-config --variable=libdir sdl3)
endif
ifeq ($(OS),Darwin)
	SDL_CFLAGS += -DHOST_SDL3_LIB='"$(SDL3_LIBDIR)/libSDL3.0.dylib"'
	SDL_LIBS :=
else
	SDL_CFLAGS += -DHOST_SDL3_LIB='"$(SDL3_LIBDIR)/libSDL3.so.0"'
	SDL_LIBS :=
endif
endif
endif

HOST := obj/$(CPU)/$(ABI)/$(OS)/chrysalisp$(EXE_EXT)

all:		hostenv host
host:		hostenv $(HOST)
install:	clean hostenv host inst

hostenv:
ifeq ($(HOST_GUI),0)
	@echo Building sdl GUI driver.
endif
ifeq ($(HOST_GUI),1)
	@echo Building fb GUI driver.
endif
ifeq ($(HOST_GUI),2)
	@echo Building raw GUI driver.
endif
ifeq ($(HOST_GUI),3)
	@echo Building sdl3 GUI driver.
endif
ifeq ($(HOST_GUI),4)
	@echo Building raw GUI driver, on SDL3.
endif
ifeq ($(HOST_GUI),none)
	@echo No GUI driver.
endif
ifeq ($(HOST_AUDIO),0)
	@echo Building sdl AUDIO driver.
endif
ifeq ($(HOST_AUDIO),1)
	@echo Building sdl3 AUDIO driver.
endif
ifeq ($(HOST_AUDIO),2)
	@echo Building alsa AUDIO driver.
endif
ifeq ($(HOST_AUDIO),-1)
	@echo No AUDIO driver.
endif
	@rm -f cpu abi os
	@mkdir -p obj/$(CPU)/$(ABI)/$(OS) $(OBJ_DIRS)
	@if [ "`cat $(GUI_DRIVER) 2>/dev/null`" != "$(OBJ_GUI)" ]; then echo $(OBJ_GUI) > $(GUI_DRIVER); rm -f $(HOST); fi

#the snapshot carries the Windows host program, which Windows does not
#build. snapshot_host is the hash of the host source it was built from,
#a line a file, its path and its sha256, and tests/system/test_snapshot.lisp
#fails while src/host is other than that
snapshot:
	@rm -f snapshot.zip
	@for f in `ls src/host | LC_ALL=C sort`; do echo "src/host/$$f `shasum -a 256 < src/host/$$f | cut -d' ' -f1`"; done \
		| shasum -a 256 | cut -d' ' -f1 > snapshot_host
	@zip -9q snapshot.zip \
		obj/vp64/VP64/sys/boot_image \
		`find obj -name "chrysalisp.exe"`

inst:
	@./run_tui.sh -n 0 -i -e -f

#the decoders of films, video.cpp, are the system's own on macOS
ifeq ($(OS),Darwin)
$(HOST): LDFLAGS += -framework VideoToolbox -framework CoreMedia -framework CoreVideo -framework CoreFoundation
endif
#and on Linux VA-API's, in every build, no libva-dev: its libva found as it
#is wanted by dlopen, as SDL3 is, of libc since glibc 2.34 and of libdl before
ifeq ($(OS),Linux)
$(HOST): LDFLAGS += -ldl
endif

$(HOST):	$(OBJ_FILES_CORE_GUI) $(OBJ_FILES_DRIVERS_GUI) $(GUI_DRIVER)
	$(CXX) -o $@ $(filter %.o,$^) $(SDL_LIBS) $(LDFLAGS)

$(GUI_DRIVER):	hostenv

$(OBJ_DIR_GUI)/%.o: $(SRC_DIR)/%.cpp
	$(CXX) -c -o $@ $< $(CFLAGS) $(CPPFLAGS) $(GUI_FLAGS) $(AUDIO_FLAGS) \
		$(SDL_CFLAGS)

$(OBJ_DIR_GUI)/%.o: $(SRC_DIR)/%.c
	$(CC) -c -o $@ $< $(CFLAGS) $(GUI_FLAGS) $(AUDIO_FLAGS) \
		$(SDL_CFLAGS)

clean:
	@rm -rf ./obj/$(CPU)/$(ABI)/$(OS)
	@rm -rf ./src/obj/$(CPU)/$(ABI)/$(OS)
	@unzip -oq snapshot.zip

 -include $(OBJ_FILES:.o=.d)
SRC_DIR := ./src
OS ?= $(shell uname)
CPU ?= $(shell uname -m)
ifeq ($(CPU),x86_64)
	ABI ?= AMD64
else
	ifeq ($(CPU),riscv64)
		ABI ?= RISCV64
	else
		CPU := arm64
		ABI ?= ARM64
	endif
endif

EXE_EXT ?=

OBJ_DIR_GUI := ./src/obj/$(CPU)/$(ABI)/$(OS)/gui
OBJ_DIR_TUI := ./src/obj/$(CPU)/$(ABI)/$(OS)/tui

SRC_DIRS := $(shell find $(SRC_DIR) -type d | grep -v "/obj")
OBJ_DIRS := $(patsubst $(SRC_DIR)/%,$(OBJ_DIR_GUI)/%,$(SRC_DIRS))
OBJ_DIRS += $(patsubst $(SRC_DIR)/%,$(OBJ_DIR_TUI)/%,$(SRC_DIRS))

SRC_FILES_CORE := src/host/main.cpp src/host/vp64.cpp src/host/pii_linux.cpp src/host/pii_windows.cpp src/host/pii_darwin.cpp
SRC_FILES_DRIVERS := src/host/audio_sdl.cpp src/host/gui_sdl.cpp src/host/gui_raw.cpp src/host/gui_fb.c

OBJ_FILES_CORE_GUI := $(patsubst src/%.cpp,$(OBJ_DIR_GUI)/%.o,$(SRC_FILES_CORE))
OBJ_FILES_DRIVERS_GUI := $(patsubst src/%.cpp,$(OBJ_DIR_GUI)/%.o,$(SRC_FILES_DRIVERS))
OBJ_FILES_DRIVERS_GUI := $(patsubst src/%.c,$(OBJ_DIR_GUI)/%.o,$(OBJ_FILES_DRIVERS_GUI))

OBJ_FILES_CORE_TUI := $(patsubst src/%.cpp,$(OBJ_DIR_TUI)/%.o,$(SRC_FILES_CORE))

OBJ_FILES := $(OBJ_FILES_CORE_GUI) $(OBJ_FILES_DRIVERS_GUI) $(OBJ_FILES_CORE_TUI)

CFLAGS ?= -O3 -nostdlib -fno-exceptions -MMD
CPPFLAGS ?= -std=c++14
LDFLAGS ?=
HGUI := $(shell echo $(GUI) | tr '[:upper:]' '[:lower:]')

HOST_GUI := 0
ifeq ($(HGUI),sdl)
	HOST_GUI := 0
endif
ifeq ($(HGUI),fb)
	HOST_GUI := 1
endif
ifeq ($(HGUI),raw)
	HOST_GUI := 2
endif

ifneq ($(HOST_GUI),1)
	HOST_AUDIO := 0
	SDL_CFLAGS ?= $(shell sdl2-config --cflags)
	SDL_LIBS ?= $(shell sdl2-config --libs) -lSDL2_mixer
	AUDIO_FLAGS := -D_HOST_AUDIO=$(HOST_AUDIO)
else
	HOST_AUDIO := -1
	SDL_CFLAGS ?= 
	SDL_LIBS ?= 
	AUDIO_FLAGS := -Dhost_audio_funcs=nullptr
endif

all:		hostenv tui gui
gui:		hostenv obj/$(CPU)/$(ABI)/$(OS)/main_gui$(EXE_EXT)
tui:		hostenv obj/$(CPU)/$(ABI)/$(OS)/main_tui$(EXE_EXT)
install:	clean hostenv tui gui inst

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
ifeq ($(HOST_AUDIO),0)
	@echo Building sdl AUDIO driver.
endif
ifneq ($(OS),Windows)
	@echo $(CPU) > cpu
	@echo $(OS) > os
	@echo $(ABI) > abi
endif
	@mkdir -p obj/$(CPU)/$(ABI)/$(OS) $(OBJ_DIRS)

snapshot:
	@rm -f snapshot.zip
	@zip -9q snapshot.zip \
		obj/vp64/VP64/sys/boot_image \
		`find obj -name "main_gui.exe"` \
		`find obj -name "main_tui.exe"`

inst:
	@./run_tui.sh -i -e -f

obj/$(CPU)/$(ABI)/$(OS)/main_gui$(EXE_EXT):	$(OBJ_FILES_CORE_GUI) $(OBJ_FILES_DRIVERS_GUI)
	$(CXX) -o $@ $^ $(SDL_LIBS) $(LDFLAGS)

obj/$(CPU)/$(ABI)/$(OS)/main_tui$(EXE_EXT):	$(OBJ_FILES_CORE_TUI)
	$(CXX) -o $@ $^ $(LDFLAGS)

$(OBJ_DIR_GUI)/%.o: $(SRC_DIR)/%.cpp
	$(CXX) -c -o $@ $< $(CFLAGS) $(CPPFLAGS) -D_HOST_GUI=$(HOST_GUI) $(AUDIO_FLAGS) \
		$(SDL_CFLAGS)

$(OBJ_DIR_GUI)/%.o: $(SRC_DIR)/%.c
	$(CC) -c -o $@ $< $(CFLAGS) -D_HOST_GUI=$(HOST_GUI) $(AUDIO_FLAGS) \
		$(SDL_CFLAGS)

$(OBJ_DIR_TUI)/%.o: $(SRC_DIR)/%.cpp
	$(CXX) -c -o $@ $< $(CFLAGS) $(CPPFLAGS)

$(OBJ_DIR_TUI)/%.o: $(SRC_DIR)/%.c
	$(CC) -c -o $@ $< $(CFLAGS)

clean:
	@rm -rf ./obj/$(CPU)/$(ABI)/$(OS)
	@rm -rf ./src/obj/$(CPU)/$(ABI)/$(OS)
	@unzip -oq snapshot.zip

 -include $(OBJ_FILES:.o=.d)
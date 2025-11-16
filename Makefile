SRC_DIR := ./src
OBJ_DIR_GUI := ./src/obj/gui
OBJ_DIR_TUI := ./src/obj/tui

SRC_DIRS := $(shell find $(SRC_DIR) -type d | grep -v "/obj")
OBJ_DIRS := $(patsubst $(SRC_DIR)/%,$(OBJ_DIR_GUI)/%,$(SRC_DIRS))
OBJ_DIRS += $(patsubst $(SRC_DIR)/%,$(OBJ_DIR_TUI)/%,$(SRC_DIRS))

SRC_FILES := $(shell find $(SRC_DIR) -name "*.cpp")
SRC_FILES += $(shell find $(SRC_DIR) -name "*.c")

OBJ_FILES_GUI := $(patsubst $(SRC_DIR)/%.cpp,$(OBJ_DIR_GUI)/%.o,$(SRC_FILES))
OBJ_FILES_GUI := $(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR_GUI)/%.o,$(OBJ_FILES_GUI))

OBJ_FILES_TUI := $(patsubst $(SRC_DIR)/%.cpp,$(OBJ_DIR_TUI)/%.o,$(SRC_FILES))
OBJ_FILES_TUI := $(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR_TUI)/%.o,$(OBJ_FILES_TUI))

OBJ_FILES := $(OBJ_FILES_GUI)
OBJ_FILES += $(OBJ_FILES_TUI)

CFLAGS := -O3 -nostdlib -fno-exceptions -MMD
CPPFLAGS := -std=c++14
HGUI := $(shell echo $(GUI) | tr '[:upper:]' '[:lower:]')

OS := $(shell uname)
CPU := $(shell uname -m)
ifeq ($(CPU),x86_64)
	ABI := AMD64
else
	ifeq ($(CPU),riscv64)
		ABI := RISCV64
	else
		CPU := arm64
		ABI := ARM64
	endif
endif

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

HOST_AUDIO := 0
HOST_WASM := 1

all:		hostenv tui gui
gui:		hostenv obj/$(CPU)/$(ABI)/$(OS)/main_gui
tui:		hostenv obj/$(CPU)/$(ABI)/$(OS)/main_tui
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
ifeq ($(HOST_WASM),1)
	@echo Building WASM support (wasm3).
endif
	@echo $(CPU) > cpu
	@echo $(OS) > os
	@echo $(ABI) > abi
	@mkdir -p obj/$(CPU)/$(ABI)/$(OS) $(OBJ_DIRS)

snapshot:
	@rm -f snapshot.zip
	@zip -9q snapshot.zip \
		obj/vp64/VP64/sys/boot_image \
		`find obj -name "main_gui.exe"` \
		`find obj -name "main_tui.exe"`

inst:
	@./run_tui.sh -n 8 -i -e -f

obj/$(CPU)/$(ABI)/$(OS)/main_gui:	$(OBJ_FILES_GUI)
	c++ -o $@ $^ $(shell sdl2-config --libs) -lSDL2_mixer -lm3

obj/$(CPU)/$(ABI)/$(OS)/main_tui:	$(OBJ_FILES_TUI)
	c++ -o $@ $^

$(OBJ_DIR_GUI)/%.o: $(SRC_DIR)/%.cpp
	c++ -c -o $@ $< $(CFLAGS) $(CPPFLAGS) -D_HOST_GUI=$(HOST_GUI) -D_HOST_AUDIO=$(HOST_AUDIO) -D_HOST_WASM=$(HOST_WASM) \
		$(shell sdl2-config --cflags)

$(OBJ_DIR_GUI)/%.o: $(SRC_DIR)/%.c
	cc -c -o $@ $< $(CFLAGS) -D_HOST_GUI=$(HOST_GUI) -D_HOST_AUDIO=$(HOST_AUDIO) -D_HOST_WASM=$(HOST_WASM) \
		$(shell sdl2-config --cflags)

$(OBJ_DIR_TUI)/%.o: $(SRC_DIR)/%.cpp
	c++ -c -o $@ $< $(CFLAGS) $(CPPFLAGS)

$(OBJ_DIR_TUI)/%.o: $(SRC_DIR)/%.c
	cc -c -o $@ $< $(CFLAGS)

clean:
	@rm -rf ./obj
	@rm -rf ./src/obj
	@unzip -oq snapshot.zip

 -include $(OBJ_FILES:.o=.d)

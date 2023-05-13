SRC_DIR := ./src
OBJ_DIR_GUI := ./src/obj/gui
OBJ_DIR_TUI := ./src/obj/tui
dummy_build_gui := $(shell mkdir -p $(OBJ_DIR_GUI))
dummy_build_tui := $(shell mkdir -p $(OBJ_DIR_TUI))
SRC_FILES := $(wildcard $(SRC_DIR)/*.cpp)
SRC_CFILES := $(wildcard $(SRC_DIR)/*.c)
OBJ_FILES_GUI := $(patsubst $(SRC_DIR)/%.cpp,$(OBJ_DIR_GUI)/%.o,$(SRC_FILES))
OBJ_FILES_TUI := $(patsubst $(SRC_DIR)/%.cpp,$(OBJ_DIR_TUI)/%.o,$(SRC_FILES))
OBJ_CFILES_GUI := $(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR_GUI)/%.o,$(SRC_CFILES))
OBJ_CFILES_TUI := $(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR_TUI)/%.o,$(SRC_CFILES))
CFLAGS := -O3 -nostdlib -fno-exceptions -MMD
CPPFLAGS := -std=c++14

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
ifeq ($(GUI),sdl)
	HOST_GUI := 0
endif
ifeq ($(GUI),fb)
	HOST_GUI := 1
endif

all:		hostenv tui gui
gui:		hostenv obj/$(CPU)/$(ABI)/$(OS)/main_gui
tui:		hostenv obj/$(CPU)/$(ABI)/$(OS)/main_tui
install:	clean hostenv tui gui inst

hostenv:
	@echo $(CPU) > cpu
	@echo $(OS) > os
	@echo $(ABI) > abi
	mkdir -p obj/$(CPU)/$(ABI)/$(OS)	

snapshot:
	rm -f snapshot.zip
	zip -9q snapshot.zip \
		obj/vp64/VP64/sys/boot_image \
		`find obj -name "main_gui.exe"` \
		`find obj -name "main_tui.exe"`

inst:
	@./run_tui.sh -n 5 -i -e

obj/$(CPU)/$(ABI)/$(OS)/main_gui:	$(OBJ_FILES_GUI) $(OBJ_CFILES_GUI)
	c++ -o $@ $^ \
		$(shell sdl2-config --libs)

obj/$(CPU)/$(ABI)/$(OS)/main_tui:	$(OBJ_FILES_TUI) $(OBJ_CFILES_TUI)
	c++ -o $@ $^

$(OBJ_DIR_GUI)/%.o: $(SRC_DIR)/%.cpp
	c++ $(CFLAGS) $(CPPFLAGS) -c -D_HOST_GUI=$(HOST_GUI) \
		$(shell sdl2-config --cflags) \
		-o $@ $<

$(OBJ_DIR_TUI)/%.o: $(SRC_DIR)/%.cpp
	c++ $(CFLAGS) $(CPPFLAGS) -c \
		-o $@ $<

$(OBJ_DIR_GUI)/%.o: $(SRC_DIR)/%.c
	cc $(CFLAGS) -c -D_HOST_GUI=$(HOST_GUI) \
		$(shell sdl2-config --cflags) \
		-o $@ $<

$(OBJ_DIR_TUI)/%.o: $(SRC_DIR)/%.c
	cc $(CFLAGS) -c \
		-o $@ $<

clean:
	rm -rf $(OBJ_DIR_GUI)/*
	rm -rf $(OBJ_DIR_TUI)/*
	rm -rf ./obj/
	unzip -oq snapshot.zip

 -include $(OBJ_FILES_GUI:.o=.d)
 -include $(OBJ_FILES_TUI:.o=.d)

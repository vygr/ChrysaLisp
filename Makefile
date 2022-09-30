OS := $(shell uname)
CPU := $(shell uname -m)
ifeq ($(CPU),x86_64)
ABI := AMD64
else
CPU := aarch64
ABI := ARM64
endif

all:		hostenv tui gui
gui:		hostenv obj/$(CPU)/$(ABI)/$(OS)/main_gui
tui:		hostenv obj/$(CPU)/$(ABI)/$(OS)/main_tui
install:	clean hostenv tui gui inst

hostenv:
	@echo $(CPU) > arch
	@echo $(OS) > platform
	@echo $(ABI) > abi

snapshot:
	rm -f snapshot.zip
	zip -9q snapshot.zip \
		obj/x86_64/AMD64/Darwin \
		obj/x86_64/AMD64/Linux \
		obj/aarch64/ARM64/Linux \
		obj/aarch64/ARM64/Darwin \
		obj/vp64/VP64/sys/boot_image \
		`find obj -name "main_gui.exe"` \
		`find obj -name "main_tui.exe"`

inst:
	@./stop.sh
	@./obj/$(CPU)/$(ABI)/$(OS)/main_tui -e obj\vp64\VP64\sys\boot_image -l 005-008 -l 002-008 -l 007-008 -l 006-008 &
	@./obj/$(CPU)/$(ABI)/$(OS)/main_tui -e obj\vp64\VP64\sys\boot_image -l 004-007 -l 001-007 -l 006-007 -l 007-008 &
	@./obj/$(CPU)/$(ABI)/$(OS)/main_tui -e obj\vp64\VP64\sys\boot_image -l 003-006 -l 000-006 -l 006-008 -l 006-007 &
	@./obj/$(CPU)/$(ABI)/$(OS)/main_tui -e obj\vp64\VP64\sys\boot_image -l 002-005 -l 005-008 -l 004-005 -l 003-005 &
	@./obj/$(CPU)/$(ABI)/$(OS)/main_tui -e obj\vp64\VP64\sys\boot_image -l 001-004 -l 004-007 -l 003-004 -l 004-005 &
	@./obj/$(CPU)/$(ABI)/$(OS)/main_tui -e obj\vp64\VP64\sys\boot_image -l 000-003 -l 003-006 -l 003-005 -l 003-004 &
	@./obj/$(CPU)/$(ABI)/$(OS)/main_tui -e obj\vp64\VP64\sys\boot_image -l 002-008 -l 002-005 -l 001-002 -l 000-002 &
	@./obj/$(CPU)/$(ABI)/$(OS)/main_tui -e obj\vp64\VP64\sys\boot_image -l 001-007 -l 001-004 -l 000-001 -l 001-002 &
	@./obj/$(CPU)/$(ABI)/$(OS)/main_tui -e obj\vp64\VP64\sys\boot_image -l 000-006 -l 000-003 -l 000-002 -l 000-001 -run apps/tui/install.lisp

obj/$(CPU)/$(ABI)/$(OS)/main_gui:	obj/$(CPU)/$(ABI)/$(OS)/main_gui.o obj/$(CPU)/$(ABI)/$(OS)/vp64.o
ifeq ($(OS),Darwin)
	c++ -o $@ $@.o \
		obj/$(CPU)/$(ABI)/$(OS)/vp64.o \
		-F/Library/Frameworks \
		-Wl,-framework,SDL2 -Wl
endif
ifeq ($(OS),Linux)
	c++ -o $@ $@.o \
	obj/$(CPU)/$(ABI)/$(OS)/vp64.o \
		$(shell sdl2-config --libs)
endif

obj/$(CPU)/$(ABI)/$(OS)/main_tui:	obj/$(CPU)/$(ABI)/$(OS)/main_tui.o obj/$(CPU)/$(ABI)/$(OS)/vp64.o
ifeq ($(OS),Darwin)
	c++ -o $@ $@.o \
		obj/$(CPU)/$(ABI)/$(OS)/vp64.o
endif
ifeq ($(OS),Linux)
	c++ -o $@ $@.o \
		obj/$(CPU)/$(ABI)/$(OS)/vp64.o
endif

obj/$(CPU)/$(ABI)/$(OS)/main_gui.o:	src/main.cpp src/pii.h Makefile
ifeq ($(OS),Darwin)
	c++ -O3 -c -D_GUI=GUI -nostdlib -fno-exceptions \
		-I/Library/Frameworks/SDL2.framework/Headers/ \
		-o $@ $<
endif
ifeq ($(OS),Linux)
	c++ -O3 -c -D_GUI=GUI -nostdlib -fno-exceptions \
		-I/usr/include/SDL2/ \
		-o $@ $<
endif

obj/$(CPU)/$(ABI)/$(OS)/main_tui.o:	src/main.cpp Makefile
ifeq ($(OS),Darwin)
	c++ -O3 -c -nostdlib -fno-exceptions \
		-o $@ $<
endif
ifeq ($(OS),Linux)
	c++ -O3 -c -nostdlib -fno-exceptions \
		-o $@ $<
endif

obj/$(CPU)/$(ABI)/$(OS)/vp64.o:	src/vp64.cpp Makefile
ifeq ($(OS),Darwin)
	c++ -O3 -c -nostdlib -fno-exceptions \
		-o $@ $<
endif
ifeq ($(OS),Linux)
	c++ -O3 -c -nostdlib -fno-exceptions \
		-o $@ $<
endif

clean:
	rm -rf obj/
	unzip -nq snapshot.zip

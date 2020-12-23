OS := $(shell uname)
CPU := $(shell uname -m)
DTZ := $(shell date "+%Z")
ifeq ($(CPU),x86_64)
ABI := AMD64
else
ABI := ARM64
endif

all:		obj/$(CPU)/$(ABI)/$(OS)/main .hostenv

.hostenv:
ifeq ($(OS), Windows)
	@echo "USER=%USERNAME%" > .hostenv
	@echo "HOME=%HOMEPATH%" >> .hostenv
	@echo "PWD=%CD%" >> .hostenv
else
	@echo "USER=$(USER)" > .hostenv
	@echo "HOME=$(HOME)" >> .hostenv
	@echo "PWD=$(PWD)" >> .hostenv
endif
	@echo "ROOT=$(PWD)" >> .hostenv
	@echo "HE_VER=2" >> .hostenv
	@echo "OS=$(OS)" >> .hostenv
	@echo "CPU=$(CPU)" >> .hostenv
	@echo "ABI=$(ABI)" >> .hostenv
	@echo "TZ=$(DTZ)" >> .hostenv


snapshot:
			rm -f snapshot.zip
			zip -9q snapshot.zip obj/x86_64/AMD64/Darwin obj/x86_64/AMD64/Linux obj/aarch64/ARM64/Linux \
			`find obj -name "boot_image"` `find obj -name "main.exe"`

obj/$(CPU)/$(ABI)/$(OS)/main:	obj/$(CPU)/$(ABI)/$(OS)/main.o obj/$(CPU)/$(ABI)/$(OS)/vp64.o
ifeq ($(OS),Darwin)
			c++ -o $@ $@.o obj/$(CPU)/$(ABI)/$(OS)/vp64.o -F/Library/Frameworks -Wl,-framework,SDL2 -Wl
endif
ifeq ($(OS),Linux)
			c++ -o $@ $@.o obj/$(CPU)/$(ABI)/$(OS)/vp64.o $(shell sdl2-config --libs)
endif

obj/$(CPU)/$(ABI)/$(OS)/main.o:	main.cpp Makefile
			echo $(CPU) > arch
			echo $(OS) > platform
			echo $(ABI) > abi
			unzip -nq snapshot.zip
ifeq ($(OS),Darwin)
			c++ -c -nostdlib -fno-exceptions \
				-I/Library/Frameworks/SDL2.framework/Headers/ \
				-o $@ $<
endif
ifeq ($(OS),Linux)
			c++ -c -nostdlib -fno-exceptions \
				-I/usr/include/SDL2/ \
				-o $@ $<
endif

obj/$(CPU)/$(ABI)/$(OS)/vp64.o:	vp64.cpp Makefile
ifeq ($(OS),Darwin)
			c++ -c -nostdlib -fno-exceptions \
				-o $@ $<
endif
ifeq ($(OS),Linux)
			c++ -c -nostdlib -fno-exceptions \
				-o $@ $<
endif

clean:
			rm -f .hostenv
			rm -rf obj/

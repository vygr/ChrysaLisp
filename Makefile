OS := $(shell uname)
CPU := $(shell uname -m)

all:		obj/$(OS)/$(CPU)/main

undo:		clean
			unzip -oq snapshot_old.zip

backup:
			cp -f snapshot.zip snapshot_old.zip

snapshot:
			rm -f snapshot.zip
			zip -r9ovq -x*.d -x*.o snapshot.zip obj/*

obj/$(OS)/$(CPU)/main:	obj/$(OS)/$(CPU)/main.o
ifeq ($(OS),Darwin)
			cc -o $@ $@.o -Wl,-framework,SDL2 -Wl,-framework,SDL2_ttf
endif
ifeq ($(OS),Linux)
			cc -o $@ $@.o $(shell sdl2-config --libs) -lSDL2_ttf
endif

obj/$(OS)/$(CPU)/main.o:	main.c Makefile
			echo $(CPU) > arch
			echo $(OS) > platform
			unzip -nq snapshot.zip
ifeq ($(OS),Darwin)
			cc -c -nostdlib -fno-exceptions \
				-I/Library/Frameworks/SDL2.framework/Headers/ \
				-I/Library/Frameworks/SDL2_ttf.framework/Headers/ \
				-o $@ $<
endif
ifeq ($(OS),Linux)
			cc -c -nostdlib -fno-exceptions \
				-I/usr/include/SDL2/ \
				-o $@ $<
endif

clean:
			rm -rf obj/

OS := $(shell uname)
CPU := $(shell uname -m)

all:		obj/main

remake:		snapshot clean all

undo:
			unzip -oq $(OS)_old.zip

backup:
			cp -f $(OS).zip $(OS)_old.zip

snapshot:
			rm $(OS).zip
			zip -r9ovq -x*.d -x*.o -xobj/test -xobj/main $(OS).zip obj/*

snapshot_darwin:
			rm Darwin.zip
			zip -r9ovq -x*.d -x*.o -xobj/test -xobj/main Darwin.zip obj/*

snapshot_linux:
			rm Linux.zip
			zip -r9ovq -x*.d -x*.o -xobj/test -xobj/main Linux.zip obj/*

obj/main:	obj/main.o
ifeq ($(OS),Darwin)
			clang -o $@ $@.o -Wl,-framework,SDL2 -Wl,-framework,SDL2_ttf
endif
ifeq ($(OS),Linux)
			clang -o $@ $@.o $(shell sdl2-config --libs) -lSDL2_ttf
endif

obj/main.o:	main.c Makefile
ifeq ($(OS),Darwin)
			clang -c -nostdlib -fno-exceptions -fno-rtti \
				-I/Library/Frameworks/SDL2.framework/Headers/ \
				-I/Library/Frameworks/SDL2_ttf.framework/Headers/ \
				-o $@ $<
endif
ifeq ($(OS),Linux)
			clang -c -nostdlib -fno-exceptions -fno-rtti \
				-I/usr/include/SDL2/ \
				-o $@ $<
endif

clean:
			rm -rf obj/

OS := $(shell uname)
CPU := $(shell uname -m)

all:		obj/main

undo:
			unzip -oq $(OS)_$(CPU)_old.zip

backup:
			cp -f $(OS)_$(CPU).zip $(OS)_$(CPU)_old.zip

snapshot:
			rm -f $(OS)_$(CPU).zip
			zip -r9ovq -x*.d -x*.o -xobj/test -xobj/main $(OS)_$(CPU).zip obj/*

snapshot_darwin:
			rm -f Darwin_x86_64.zip
			zip -r9ovq -x*.d -x*.o -xobj/test -xobj/main Darwin_x86_64.zip obj/*

snapshot_linux:
			rm -f Linux_x86_64.zip
			zip -r9ovq -x*.d -x*.o -xobj/test -xobj/main Linux_x86_64.zip obj/*

obj/main:	obj/main.o
ifeq ($(OS),Darwin)
			clang -o $@ $@.o -Wl,-framework,SDL2 -Wl,-framework,SDL2_ttf
endif
ifeq ($(OS),Linux)
			clang -o $@ $@.o $(shell sdl2-config --libs) -lSDL2_ttf
endif

obj/main.o:	main.c Makefile
			unzip -nq $(OS)_$(CPU).zip
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

OS := $(shell uname)
CPU := $(shell uname -m)

all:		obj/main

remake:		snapshot clean all

undo:
			unzip -nq $(OS)_old.zip
			nasm -dOS=$(OS) -f macho64 -o obj/main.o main.nasm
			clang -o obj/main -e main obj/main.o -Wl,-framework,SDL2 -Wl,-framework,SDL2_ttf

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
			clang -o $@ -e main $@.o -Wl,-framework,SDL2 -Wl,-framework,SDL2_ttf
endif
ifeq ($(OS),Linux)
			clang -o $@ $@.o $(shell sdl2-config --libs) -lSDL2_ttf
endif

obj/main.o:	main.nasm Makefile
			echo $(OS) > platform
			echo $(CPU) > arch
			unzip -nq $(OS).zip
ifeq ($(OS),Darwin)
			nasm -dOS=$(OS) -f macho64 -o $@ $<
endif
ifeq ($(OS),Linux)
			nasm -dOS=$(OS) -f elf64 -o $@ $<
endif

clean:
			rm -rf obj/

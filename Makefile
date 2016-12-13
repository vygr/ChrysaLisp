OS := $(shell uname)

all:		obj/main

snapshot:
			rm $(OS).zip
			zip -ru9ov -x*.d -x*.o -xobj/main $(OS).zip obj/*

obj/main:	obj/main.o
ifeq ($(OS),Darwin)
			clang -o $@ -e main $@.o -Wl,-framework,SDL2 -Wl,-framework,SDL2_ttf
endif
ifeq ($(OS),Linux)
			clang -o $@ -e main $@.o $(shell sdl2-config --libs) -lSDL2_ttf
endif

obj/main.o:	main.nasm Makefile
			echo $(OS) > platform
			unzip -n $(OS).zip
ifeq ($(OS),Darwin)
			nasm -dOS=$(OS) -f macho64 -o $@ $<
endif
ifeq ($(OS),Linux)
			nasm -dOS=$(OS) -f elf64 -o $@ $<
endif

clean:
			rm -rf obj/

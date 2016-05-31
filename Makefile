OS := $(shell uname)
all_srcs := $(shell find . -type f -name '*.nasm')
all_deps := $(shell find . -type f -name '*.d')
all_objects := $(patsubst ./%.nasm, obj/%, $(all_srcs))

all:		make_dirs $(all_objects)

make_dirs:
			mkdir -p $(sort $(dir $(all_objects)))

obj/main:	obj/main.o
ifeq ($(OS),Darwin)
			clang -o obj/main -e main obj/main.o -Wl,-framework,SDL2 -Wl,-framework,SDL2_ttf
endif
ifeq ($(OS),Linux)
			clang -o obj/main -e main obj/main.o $(shell sdl2-config --libs) -lSDL2_ttf
endif

obj/%.o:	%.nasm Makefile \
			obj/sys/load_init obj/sys/load_bind obj/sys/load_statics \
			obj/sys/load_deinit obj/gui/gui_init obj/sys/kernel
ifeq ($(OS),Darwin)
			nasm -dOS=$(OS) -f macho64 -o obj/main.o $< -MD $@.d
endif
ifeq ($(OS),Linux)
			nasm -dOS=$(OS) -f elf64 -o obj/main.o $< -MD $@.d
endif

obj/%:		%.nasm Makefile $(all_incs)
			nasm -dOS=$(OS) -f bin $< -o $@ -MD $@.d

-include	$(all_deps)

clean:
			rm -rf obj/

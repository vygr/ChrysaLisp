OS := $(shell uname)
sdl_ldflags := $(shell sdl2-config --libs)
all_nasms := $(shell find . -type f -name '*.nasm')
all_incs := $(shell find . -type f -name '*.inc')
all_objects := $(patsubst %.nasm, %, $(all_nasms))

all:		$(all_objects)

main:		main.o
ifeq ($(OS),Darwin)
			clang -o main -e main main.o -Wl,-framework,SDL2 -Wl,-framework,SDL2_ttf
endif
ifeq ($(OS),Linux)
			clang -o main -e main main.o $(sdl_ldflags)
endif

%.o:		%.nasm Makefile $(all_incs) \
			sys/load_init sys/load_bind sys/load_statics sys/load_deinit sys/kernel gui/gui_init
ifeq ($(OS),Darwin)
			nasm -dOS=$(OS) -f macho64 $<
endif
ifeq ($(OS),Linux)
			nasm -dOS=$(OS) -f elf64 $<
endif

%:			%.nasm Makefile $(all_incs)
			nasm -dOS=$(OS) -f bin $< -o $@

clean:
			rm *.o $(all_objects)

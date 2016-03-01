OS := $(shell uname)
sdl_ldflags := $(shell sdl2-config --libs)
all_nasms := $(shell find . -type f -name '*.nasm')
all_objects := $(patsubst %.nasm, %, $(all_nasms))

all:		$(all_objects)

main:		main.o
			clang -o main -e main main.o $(sdl_ldflags)

%.o:		%.nasm inc/sdl2.inc inc/gui.inc inc/vp.inc inc/load.inc inc/syscall.inc inc/link.inc \
			sys/load_init sys/load_bind sys/load_statics sys/load_deinit sys/kernel gui/gui_init
ifeq ($(OS),Darwin)
			nasm -dOS=$(OS) -f macho64 $<
endif
ifeq ($(OS),Linux)
			nasm -dOS=$(OS) -f elf64 $<
endif

class/%:	class/%.nasm Makefile class/obj.inc class/ref.inc class/view1.inc
			nasm -dOS=$(OS) -f bin $< -o $@

%:			%.nasm Makefile inc/gui.inc inc/func.inc inc/task.inc inc/list.inc inc/vp.inc \
			inc/code.inc inc/mail.inc inc/syscall.inc inc/heap.inc inc/link.inc
			nasm -dOS=$(OS) -f bin $< -o $@

clean:
			rm *.o $(all_objects)

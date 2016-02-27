OS := $(shell uname)

all:	main sys gui tests

sdl_ldflags := $(shell sdl2-config --libs)

main:	main.o
		clang -o main -e main main.o $(sdl_ldflags)

main.o:	main.nasm sdl2.inc gui.inc vp.inc load.inc syscall.inc link.inc sys/load_init \
		sys/load_bind sys/load_statics \
		sys/load_deinit sys/kernel gui/gui_init
ifeq ($(OS),Darwin)
		nasm -dOS=$(OS) -f macho64 main.nasm
endif
ifeq ($(OS),Linux)
		nasm -dOS=$(OS) -f elf64 main.nasm
endif

sys_objects := $(patsubst %.nasm, %, $(wildcard sys/*.nasm))
gui_objects := $(patsubst %.nasm, %, $(wildcard gui/*.nasm))
tests_objects := $(patsubst %.nasm, %, $(wildcard tests/*.nasm))

sys:	$(sys_objects)
gui:	$(gui_objects)
tests:	$(tests_objects)

%:	%.nasm Makefile gui.inc func.inc task.inc list.inc vp.inc \
		code.inc mail.inc syscall.inc heap.inc link.inc
		nasm -dOS=$(OS) -f bin $< -o $@

clean:
	rm main *.o $(sys_objects) $(gui_objects) $(tests_objects)

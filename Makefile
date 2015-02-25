all:	main gui sys tests

main:	main.o
		ld -macosx_version_min 10.6 -o main -e _main main.o

main.o:	main.nasm vp.inc load.inc syscall.inc link.inc sys/load_init_loader \
		sys/load_function_load sys/load_statics \
		sys/load_deinit_loader sys/kernel
		nasm -f macho64 main.nasm

sdl_ldflags := $(shell sdl2-config --libs)

gui:	gui.o
		clang -o gui -e _main gui.o $(sdl_ldflags)

gui.o:	gui.nasm sdl2.inc vp.inc load.inc syscall.inc link.inc sys/load_init_loader \
		sys/load_function_load sys/load_statics \
		sys/load_deinit_loader sys/kernel
		nasm -f macho64 gui.nasm

sys_objects := $(patsubst %.nasm, %, $(wildcard sys/*.nasm))
tests_objects := $(patsubst %.nasm, %, $(wildcard tests/*.nasm))

sys:	$(sys_objects)
tests:	$(tests_objects)

%:	%.nasm Makefile func.inc task.inc list.inc vp.inc \
		code.inc mail.inc syscall.inc heap.inc link.inc
		nasm -f bin $< -o $@

clean:
	rm main gui *.o $(sys_objects) $(tests_objects)

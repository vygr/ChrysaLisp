all:	main sys tests

main:	main.o
		ld -macosx_version_min 10.6 -o main -e _main main.o

main.o:	main.nasm vp.inc load.inc syscall.inc link.inc sys/load_init_loader \
		sys/load_function_load sys/load_statics \
		sys/load_deinit_loader sys/kernel
		nasm -f macho64 main.nasm

sys:	$(patsubst %.nasm, %, $(wildcard sys/*.nasm))
tests:	$(patsubst %.nasm, %, $(wildcard tests/*.nasm))

%:	%.nasm Makefile func.inc task.inc list.inc vp.inc \
		code.inc mail.inc syscall.inc heap.inc link.inc
		nasm -f bin $< -o $@

clean:
	rm main main.o $(patsubst %.nasm, %, $(wildcard sys/*.nasm)) $(patsubst %.nasm, %, $(wildcard tests/*.nasm))

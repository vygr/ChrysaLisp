all:	main sys tests

main:	main.o
		ld -macosx_version_min 10.6 -o main -e _main main.o

main.o:	main.nasm vp.inc code.inc list.inc heap.inc mail.inc task.inc \
		syscall.inc heap.nasm mail.nasm task.nasm load.nasm func.inc \
		sys/string_compare sys/string_length
		nasm -f macho64 main.nasm

sys:	$(patsubst %.nasm, %, $(wildcard sys/*.nasm))
tests:	$(patsubst %.nasm, %, $(wildcard tests/*.nasm))

%:	%.nasm Makefile sys/write_char.nasm func.inc task.inc list.inc vp.inc \
	code.inc mail.inc syscall.inc heap.inc
	nasm -f bin $< -o $@

clean:
	rm -rf \
	main \
	main.o \
	tests/test1 \
	tests/test2 \
	tests/test3 \
	tests/test4 \
	sys/boot \
	sys/enumerate_backwards \
	sys/enumerate_forwards \
	sys/get_index_of_node \
	sys/get_node_at_index \
	sys/read_line \
	sys/string_compare \
	sys/string_length \
	sys/write_char \
	sys/write_number \
	sys/write_string

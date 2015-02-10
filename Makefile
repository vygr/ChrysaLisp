all : main tests

main: main.o
		ld -macosx_version_min 10.6 -o main -e _main main.o

main.o: main.nasm vp.inc code.inc list.inc heap.inc mail.inc task.inc \
		syscall.inc list.nasm heap.nasm mail.nasm task.nasm load.nasm \
		func.inc util.nasm
		nasm -f macho64 main.nasm

tests:	tests/test1 tests/test2

tests/test1: tests/test1.nasm func.inc task.inc list.inc vp.inc code.inc mail.inc
		nasm -f bin tests/test1.nasm

tests/test2: tests/test2.nasm func.inc task.inc list.inc vp.inc code.inc mail.inc
		nasm -f bin tests/test2.nasm

clean:
	rm -rf main main.o tests/test1 tests/test2

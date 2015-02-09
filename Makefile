all : main test

main: main.o
		ld -macosx_version_min 10.6 -o main -e _main main.o

main.o: main.nasm vp.inc code.inc list.inc heap.inc mail.inc task.inc \
  		syscall.inc list.nasm heap.nasm mail.nasm task.nasm util.nasm
		nasm -f macho64 main.nasm

test: test.nasm func.inc task.inc list.inc vp.inc code.inc mail.inc
		nasm -f bin test.nasm

clean :
	rm -rf main main.o test

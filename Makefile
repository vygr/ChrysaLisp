all : main

main: main.o
	ld -macosx_version_min 10.6 -o main -e _main main.o

main.o: main.nasm code.nasm syscall.nasm code.nasm list.nasm heap.nasm task.nasm util.nasm vp.nasm mail.nasm
	nasm -f macho64 main.nasm

clean :
	rm -rf main main.o

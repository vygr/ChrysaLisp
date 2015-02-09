all : main

main: main.o
	ld -macosx_version_min 10.6 -o main -e _main main.o

main.o: main.nasm task.nasm mail.nasm util.nasm heap.nasm list.nasm code.nasm vp.nasm syscall.nasm
	nasm -f macho64 main.nasm

clean :
	rm -rf main main.o

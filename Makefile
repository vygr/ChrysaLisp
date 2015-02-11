all : main sys tests

main: main.o
		ld -macosx_version_min 10.6 -o main -e _main main.o

main.o: main.nasm vp.inc code.inc list.inc heap.inc mail.inc task.inc \
		syscall.inc heap.nasm mail.nasm task.nasm load.nasm func.inc \
		sys/string_compare sys/string_length
		nasm -f macho64 main.nasm

sys:	sys/enumerate_backwards sys/enumerate_forwards sys/get_index_of_node sys/get_node_at_index \
		sys/write_char sys/write_string sys/read_line sys/write_number sys/string_length sys/string_compare

sys/enumerate_backwards: sys/enumerate_forwards.nasm func.inc task.inc list.inc vp.inc \
		code.inc mail.inc
		nasm -f bin sys/enumerate_backwards.nasm

sys/enumerate_forwards: sys/enumerate_forwards.nasm func.inc task.inc list.inc vp.inc \
		code.inc mail.inc
		nasm -f bin sys/enumerate_forwards.nasm

sys/get_index_of_node: sys/enumerate_forwards.nasm func.inc task.inc list.inc vp.inc \
		code.inc mail.inc
		nasm -f bin sys/get_index_of_node.nasm

sys/get_node_at_index: sys/enumerate_forwards.nasm func.inc task.inc list.inc vp.inc \
		code.inc mail.inc
		nasm -f bin sys/get_node_at_index.nasm

sys/read_line: sys/read_line.nasm func.inc task.inc list.inc vp.inc \
		code.inc mail.inc syscall.inc
		nasm -f bin sys/read_line.nasm

sys/write_char: sys/write_char.nasm func.inc task.inc list.inc vp.inc \
		code.inc mail.inc syscall.inc
		nasm -f bin sys/write_char.nasm

sys/write_string: sys/write_string.nasm func.inc task.inc list.inc vp.inc \
		code.inc mail.inc syscall.inc
		nasm -f bin sys/write_string.nasm

sys/write_number: sys/write_number.nasm func.inc task.inc list.inc vp.inc \
		code.inc mail.inc syscall.inc
		nasm -f bin sys/write_number.nasm

sys/string_length: sys/string_length.nasm func.inc task.inc list.inc vp.inc \
		code.inc mail.inc
		nasm -f bin sys/string_length.nasm

sys/string_compare: sys/string_compare.nasm func.inc task.inc list.inc vp.inc \
		code.inc mail.inc
		nasm -f bin sys/string_compare.nasm

tests:	tests/test1 tests/test2 tests/test3

tests/test1: tests/test1.nasm func.inc task.inc list.inc vp.inc code.inc mail.inc
		nasm -f bin tests/test1.nasm

tests/test2: tests/test2.nasm func.inc task.inc list.inc vp.inc code.inc mail.inc
		nasm -f bin tests/test2.nasm

tests/test3: tests/test3.nasm func.inc task.inc list.inc vp.inc code.inc mail.inc
		nasm -f bin tests/test3.nasm

clean:
	rm -rf main main.o tests/test1 tests/test2 tests/test3

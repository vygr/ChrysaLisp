all:	main sys tests

main:	main.o
		ld -macosx_version_min 10.6 -o main -e _main main.o

main.o:	main.nasm vp.inc code.inc list.inc mail.inc task.inc \
		syscall.inc task.nasm heap.inc load.nasm func.inc \
		sys/string_compare sys/string_length \
		sys/heap_init sys/heap_deinit sys/heap_alloccell \
		sys/mail_mailheap sys/mail_init_mailer \
		sys/mail_deinit_mailer sys/mail_alloc \
		sys/mail_free sys/mail_send sys/mail_read
		nasm -f macho64 main.nasm

sys:	$(patsubst %.nasm, %, $(wildcard sys/*.nasm))
tests:	$(patsubst %.nasm, %, $(wildcard tests/*.nasm))

%:	%.nasm Makefile func.inc task.inc list.inc vp.inc \
		code.inc mail.inc syscall.inc heap.inc
		nasm -f bin $< -o $@

clean:
	rm -rf \
	main \
	main.o \
	sys/boot \
	sys/enumerate_backwards \
	sys/enumerate_forwards \
	sys/get_index_of_node \
	sys/get_node_at_index \
	sys/heap_alloccell \
	sys/heap_deinit \
	sys/heap_freeheap \
	sys/heap_init \
	sys/mail_alloc \
	sys/mail_deinit_mailer \
	sys/mail_free \
	sys/mail_init_mailer \
	sys/mail_mailheap \
	sys/mail_read \
	sys/mail_send \
	sys/read_line \
	sys/string_compare \
	sys/string_length \
	sys/write_char \
	sys/write_number \
	sys/write_string \
	tests/test1 \
	tests/test2 \
	tests/test3 \
	tests/test4

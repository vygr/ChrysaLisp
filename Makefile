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
	rm -rf \
	main \
	main.o \
	sys/kernel \
	sys/link \
	sys/get_cpu_id \
	sys/heap_alloccell \
	sys/heap_deinit \
	sys/heap_freeheap \
	sys/heap_init \
	sys/link_statics \
	sys/link_init_linker \
	sys/list_enumerate_backwards \
	sys/list_enumerate_forwards \
	sys/list_get_index_of_node \
	sys/list_get_node_at_index \
	sys/load_deinit_loader \
	sys/load_function_load \
	sys/load_statics \
	sys/load_init_loader \
	sys/mail_alloc \
	sys/mail_deinit_mailer \
	sys/mail_free \
	sys/mail_statics \
	sys/mail_init_mailer \
	sys/mail_read \
	sys/mail_read_mymail \
	sys/mail_send \
	sys/math_random \
	sys/mem_alloc \
	sys/mem_clear \
	sys/mem_copy \
	sys/mem_grow \
	sys/mem_deinit_allocator \
	sys/mem_free \
	sys/mem_statics \
	sys/mem_init_allocator \
	sys/read_line \
	sys/string_compare \
	sys/string_copy \
	sys/string_length \
	sys/string_parse_int \
	sys/task_deinit_tasker \
	sys/task_deshedule \
	sys/task_statics \
	sys/task_init_tasker \
	sys/task_open \
	sys/task_open_array \
	sys/task_open_child \
	sys/task_open_farm \
	sys/task_open_pipe \
	sys/task_restore \
	sys/task_resume \
	sys/task_sleep \
	sys/task_start \
	sys/task_stop \
	sys/task_suspend \
	sys/write_char \
	sys/write_number \
	sys/write_string \
	tests/test1 \
	tests/test2 \
	tests/test3 \
	tests/test4 \
	tests/test5 \
	tests/test6 \
	tests/test7 \
	tests/test8 \
	tests/test9 \
	tests/test10

all:	main sys tests

main:	main.o
		ld -macosx_version_min 10.6 -o main -e _main main.o

main.o:	main.nasm vp.inc code.inc list.inc mail.inc task.inc heap.inc \
		load.inc syscall.inc func.inc sys/load_init_loader \
		sys/load_function_load sys/load_get_statics \
		sys/load_deinit_loader sys/mail_init_mailer \
		sys/mail_deinit_mailer sys/mail_send sys/mail_read \
		sys/task_get_statics sys/task_init_tasker \
		sys/task_deinit_tasker sys/task_deshedule sys/task_start \
		sys/mem_init_allocator sys/mem_deinit_allocator \
		sys/string_compare sys/string_length sys/string_parse_int \
		sys/get_cpu_id
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
	boot \
	get_cpu_id \
	heap_alloccell \
	heap_deinit \
	heap_freeheap \
	heap_init \
	list_enumerate_backwards \
	list_enumerate_forwards \
	list_get_index_of_node \
	list_get_node_at_index \
	load_deinit_loader \
	load_function_load \
	load_get_statics \
	load_init_loader \
	mail_alloc \
	mail_deinit_mailer \
	mail_free \
	mail_get_statics \
	mail_init_mailer \
	mail_read \
	mail_send \
	math_random \
	mem_alloc \
	mem_clear \
	mem_copy \
	mem_deinit_allocator \
	mem_free \
	mem_get_statics \
	mem_init_allocator \
	read_line \
	string_compare \
	string_copy \
	string_length \
	string_parse_int \
	task_deinit_tasker \
	task_deshedule \
	task_get_statics \
	task_init_tasker \
	task_open \
	task_open_array \
	task_open_child \
	task_open_farm \
	task_open_pipe \
	task_restore \
	task_resume \
	task_sleep \
	task_start \
	task_stop \
	task_suspend \
	write_char \
	write_number \
	write_string \
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

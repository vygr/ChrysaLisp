all:	main sys tests

main:	main.o
		ld -macosx_version_min 10.6 -o main -e _main main.o

main.o:	main.nasm vp.inc code.inc list.inc mail.inc task.inc heap.inc \
		load.inc syscall.inc sys/load_init_loader \
		sys/load_function_load sys/load_get_statics \
		sys/load_deinit_loader sys/mail_init_mailer \
		sys/mail_deinit_mailer sys/mail_send sys/mail_read \
		sys/task_get_statics sys/task_init_tasker \
		sys/task_deinit_tasker sys/task_deshedule sys/task_start \
		sys/mem_init_allocator sys/mem_deinit_allocator
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
	sys/list_enumerate_backwards \
	sys/list_enumerate_forwards \
	sys/list_get_index_of_node \
	sys/list_get_node_at_index \
	sys/heap_alloccell \
	sys/heap_deinit \
	sys/heap_freeheap \
	sys/heap_init \
	sys/load_deinit_loader \
	sys/load_function_load \
	sys/load_get_statics \
	sys/load_init_loader \
	sys/mail_alloc \
	sys/mail_deinit_mailer \
	sys/mail_free \
	sys/mail_get_statics \
	sys/mail_init_mailer \
	sys/mail_read \
	sys/mail_send \
	sys/mem_alloc \
	sys/mem_deinit_allocator \
	sys/mem_free \
	sys/mem_get_statics \
	sys/mem_init_allocator \
	sys/read_line \
	sys/string_compare \
	sys/string_length \
	sys/string_copy \
	sys/task_deinit_tasker \
	sys/task_sleep \
	sys/task_deshedule \
	sys/task_get_statics \
	sys/task_init_tasker \
	sys/task_restore \
	sys/task_resume \
	sys/task_start \
	sys/task_stop \
	sys/task_suspend \
	sys/task_open \
	sys/task_open_child \
	sys/task_open_farm \
	sys/task_open_array \
	sys/task_open_pipe \
	sys/write_char \
	sys/write_number \
	sys/write_string \
	sys/math_random \
	sys/get_cpu_id \
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

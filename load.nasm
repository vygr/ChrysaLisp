%include "vp.inc"
%include "code.inc"
%include "syscall.inc"
%include "func.inc"

;;;;;;;;;;;
; load code
;;;;;;;;;;;

	SECTION .text

	LD_BLOCK_SIZE	equ 16384
	LD_PAGE_SIZE	equ 4096

ld_init_loader:
	;set prebound functions as executable
	vp_lea [rel ld_prebound], r0
	vp_and -LD_PAGE_SIZE, r0
	vp_lea [rel ld_prebounde], r1
	vp_sub r0, r1
	sys_mprotect r0, r1, PROT_READ|PROT_WRITE|PROT_EXEC

	;add all to function list
	vp_lea [rel ld_prebound], r1
	loopstart
		vp_cpy [r1], r0
		breakif r0, ==, 0
		vp_cpy [rel ld_function_list], r0
		vp_cpy r0, [r1]
		vp_cpy r1, [rel ld_function_list]
		vp_add [r1 + FN_HEADER_LENGTH], r1
	loopend

	;bind all function intra references
	vp_cpy [rel ld_function_list], r3
	loopstart
		breakif r3, ==, 0
		vp_cpy r3, r0
		vp_add [r3 + FN_HEADER_LINKS], r0
		loopstart
			vp_cpy [r0], r1
			breakif r1, ==, 0
			vp_lea [r0 + 8], r0
			vp_push r0
			vp_push r3
			vp_call ld_load_function
			vp_cpy r0, r1
			vp_pop r3
			vp_pop r0
			vp_cpy r1, [r0 - 8]
			vp_call ld_string_length + 0x38
			vp_add r1, r0
			vp_add 8, r0	;7 plus string nul !
			vp_and -8, r0
		loopend
		vp_cpy [r3], r3
	loopend
	vp_ret

ld_deinit_loader:
	vp_cpy r0, r2
	vp_cpy [rel ld_block_list], r1
	loopstart
		breakif r1, ==, 0
		vp_cpy [r1], r3
		sys_munmap r1, LD_BLOCK_SIZE
		vp_cpy r3, r1
	loopend
	vp_ret

ld_load_function:
	;input
	;r0 = function path name
	;output
	;r0 = 0 else, function entry pointer
	;trashes
	;r1-r3, r5-r7

	;save pathname
	vp_cpy r0, r7

	;check if function allready present !
	vp_xor r5, r5
	vp_cpy [rel ld_function_list], r6
	repeat
		breakif r6, ==, 0
		vp_cpy r7, r0
		vp_lea [r6 + FN_HEADER_PATHNAME], r1
		vp_call ld_string_compare + 0x38
		if r0, !=, 0
			vp_cpy r6, r5
			vp_add [r6 + FN_HEADER_ENTRY], r5
		endif
		vp_cpy [r6], r6
	until r5, !=, 0
	if r5, !=, 0
		;found function allready loaded
		vp_cpy r5, r0
		vp_ret
	endif

	;get length of function on disk
	sys_stat r7, ld_stat_buffer
	if r0, !=, 0
		;no such file
		xor r0, r0
		vp_ret
	endif

	;ensure space for new function
	vp_cpy [rel ld_block_start], r1
	vp_cpy [rel ld_block_end], r2
	vp_sub r1, r2
	vp_cpy ld_stat_buffer, r0
	vp_cpy [r0 + STAT_FSIZE], r0
	if r2, <, r0
		;not enough so allcate new function buffer
		sys_mmap 0, LD_BLOCK_SIZE, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_PRIVATE|MAP_ANON, -1, 0

		;add to block list for freeing
		vp_cpy [rel ld_block_list], r3
		vp_cpy r3, [r0]
		vp_cpy r0, [rel ld_block_list]

		;set block pointers for loading
		vp_add 8, r0
		vp_cpy r0, [rel ld_block_start]
		vp_add LD_BLOCK_SIZE - 8, r0
		vp_cpy r0, [rel ld_block_end]
	endif

	;open function file
	sys_open r7, O_RDONLY, 0
	vp_cpy r0, r12

	;read into buffer
	vp_cpy [rel ld_block_start], r3
	vp_cpy ld_stat_buffer, r2
	sys_read r12, r3, [r2 + STAT_FSIZE]

	;close function file
	sys_close r12

	;add to function list
	vp_cpy [rel ld_function_list], r0
	vp_cpy r0, [r3]
	vp_cpy r3, [rel ld_function_list]

	;adjust block start
	vp_cpy r3, r0
	vp_add [r2 + STAT_FSIZE], r0
	vp_cpy r0, [rel ld_block_start]

	;load and link function references
	vp_cpy r3, r0
	vp_add [r3 + FN_HEADER_LINKS], r0
	loopstart
		vp_cpy [r0], r1
		breakif r1, ==, 0
		vp_lea [r0 + 8], r0
		vp_push r0
		vp_push r3
		vp_call ld_load_function
		vp_cpy r0, r1
		vp_pop r3
		vp_pop r0
		vp_cpy r1, [r0 - 8]
		vp_call ld_string_length + 0x38
		vp_add r1, r0
		vp_add 8, r0	;7 plus string nul !
		vp_and -8, r0
	loopend

	;return function address
	vp_cpy r3, r0
	vp_add [r3 + FN_HEADER_ENTRY], r0
	vp_ret

;;;;;;;;;;;
; load data
;;;;;;;;;;;

	SECTION .data

ld_function_list:
	dq 0
ld_block_list:
	dq	0
ld_block_start:
	dq	0
ld_block_end:
	dq	0
ld_stat_buffer:
	times STAT_SIZE db 0

	align 8, db 0
ld_prebound:

ld_string_compare:
	incbin	'sys/string_compare'
ld_string_length:
	incbin	'sys/string_length'

ld_heap_init:
	incbin	'sys/heap_init'
ld_heap_deinit:
	incbin	'sys/heap_deinit'
ld_heap_alloccell:
	incbin	'sys/heap_alloccell'

ld_mail_mailheap:
	incbin	'sys/mail_mailheap'
ld_mail_init_mailer:
	incbin	'sys/mail_init_mailer'
ld_mail_deinit_mailer:
	incbin	'sys/mail_deinit_mailer'
ld_mail_alloc:
	incbin	'sys/mail_alloc'
ld_mail_free:
	incbin	'sys/mail_free'
ld_mail_send:
	incbin	'sys/mail_send'
ld_mail_read:
	incbin	'sys/mail_read'

ld_prebounde:
	dq 0

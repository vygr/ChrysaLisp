%include 'inc/func.inc'
%include 'inc/load.inc'

def_func sys/load_init
	;set prebound functions as read/write/executable
	vp_rel _func_start, r0
	vp_cpy r0, r1
	loop_start
	 	vp_cpy_ui [r1 + fn_header_length], r2
		breakif r2, ==, 0
		vp_add r2, r1
	loop_end
	vp_and -ld_page_size, r0
	vp_sub r0, r1
	sys_mprotect r0, r1, prot_read|prot_write|prot_exec

	;get loader statics and bind function !
	vp_rel _func_start, r6
	vp_cpy_ui [r6 + fn_header_length], r0
	vp_add r0, r6
	vp_cpy r6, r5
	vp_cpy_ui [r6 + fn_header_length], r0
	vp_add r0, r6
	vp_cpy_ui [r6 + fn_header_entry], r0
	vp_add r0, r6
	vp_cpy_ui [r5 + fn_header_entry], r0
	vp_add r0, r5

	;init reloc buffer address
	vp_lea [r6 + ld_statics_reloc_buffer], r1
	vp_cpy r1, [r6 + ld_statics_reloc_stack]

	;add all prebound functions to function list
	vp_rel _func_start, r1
	loop_start
	 	vp_cpy_ui [r1 + fn_header_length], r2
		breakif r2, ==, 0
		vp_cpy [r6 + ld_statics_function_flist], r0
		vp_cpy r0, [r1]
		vp_cpy r1, [r6 + ld_statics_function_flist]
		vp_add r2, r1
	loop_end

	;bind all prebound function intra references
	vp_rel _func_start, r2
	loop_start
		vp_cpy_ui [r2 + fn_header_length], r1
		breakif r1, ==, 0
		vp_cpy_ui [r2 + fn_header_links], r0
		vp_add r2, r0
		vp_add r1, r2
		loop_start
			vp_cpy [r0], r1
			breakif r1, ==, 0
			vp_push r0, r2, r5
			vp_add r1, r0
			vp_call r5		;sys/load_bind
			if r0, ==, 0
				;no such function
				vp_rel bind_error, r0
				sys_write_string 2, r0, bind_error_end-bind_error
				vp_cpy [r4 + (ptr_size * 2)], r0
				vp_add [r0], r0
				vp_call string_skip
				vp_lea [r0 - 1], r1
				vp_cpy [r4 + (ptr_size * 2)], r0
				vp_add [r0], r0
				vp_sub r0, r1
				sys_write_string 2, r0, r1
				sys_write_char 2, 10
				sys_exit 1
			endif
			vp_cpy r0, r1
			vp_pop r0, r2, r5
			vp_cpy r1, [r0]
			vp_add ptr_size, r0
		loop_end
	loop_end
	vp_ret

string_skip:
	loop_start
		vp_cpy_ub [r0], r1
		vp_inc r0
	loop_until r1, ==, 0
	ret

bind_error:
	db 'Prebind error: '
bind_error_end:

def_func_end

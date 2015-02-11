%include "vp.inc"
%include "code.inc"
%include "syscall.inc"

;;;;;;;;;;;
; util code
;;;;;;;;;;;

	SECTION .text

write_char:
	;inputs
	;r0 = char
	;r1 = fd

	sys_write_char r1, r0
	vp_ret

write_string:
	;inputs
	;r0 = string
	;r1 = fd
	;trashes
	;r2-r3

	vp_cpy r1, r3
	vp_call string_length
	sys_write_string r3, r0, r1
	vp_ret

write_number:
	;inputs
	;r0 = number
	;r1 = fd
	;trashes
	;r0-r3, r5

	vp_cpy 10, r3	;base
	vp_cpy r4, r5	;stack location
	repeat
		vp_xor r2, r2
		vp_div r3
		vp_push r2
	until r0, ==, 0
	repeat
		vp_pop r0
		vp_add '0', r0
		sys_write_char r1, r0
	until r5, ==, r4
	vp_ret

read_line:
	;inputs
	;r0 = buffer address
	;r1 = buffer size
	;r2 = fd
	;outputs
	;r0 = chars read
	;trashes
	;r0-r3, r5

	vp_cpy r0, r3
	vp_cpy r0, r5
	vp_add r0, r1
	repeat
		breakif r5, e, r1
		sys_read_char r2
		vp_cpy r0l, byte[r5]
		vp_inc r5
	until r0, ==, 10
	vp_cpy r5, r0
	vp_sub r3, r0
	vp_ret

string_compare:
	;inputs
	;r0 = string1
	;r1 = string2
	;outputs
	;r0 = 0 if not same
	;trashes
	;r0-r3

	repeat
		vp_cpy byte[r0], r2l
		vp_cpy byte[r1], r3l
		vp_and 0xff, r2
		vp_and 0xff, r3
		if r2, ==, 0
			if r3, ==, 0
				vp_cpy 1, r0
				vp_ret
			endif
			vp_xor r0, r0
			vp_ret
		endif
		if r3, ==, 0
			if r2, ==, 0
				vp_cpy 1, r0
				vp_ret
			endif
			vp_xor r0, r0
			vp_ret
		endif
		vp_add 1, r0
		vp_add 1, r1
	until r2, !=, r3
	vp_xor r0, r0
	vp_ret

string_length:
	;inputs
	;r0 = string
	;outputs
	;r0 = string
	;r1 = string len
	;trashes
	;r2

	vp_cpy r0, r1
	loopstart
		vp_cpy byte[r1], r2l
		vp_and 0xff, r2
		breakif r2, ==, 0
		vp_add 1, r1
	loopend
	vp_sub r0, r1
	vp_ret

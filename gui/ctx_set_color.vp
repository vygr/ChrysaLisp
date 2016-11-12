%include 'inc/func.ninc'
%include 'inc/sdl2.ninc'

def_func gui/ctx_set_color
	;inputs
	;r0 = ctx
	;r1 = color
	;trashes
	;all but r4

	def_struct local
		long local_old_stack
	def_struct_end

	;align stack to 16 bytes for SDl
	vp_cpy r4, r15
	vp_sub local_size, r4
	vp_and -16, r4
	vp_cpy r15, [r4 + local_old_stack]

	vp_cpy r1, r8
	vp_cpy r1, r9
	vp_cpy r1, r10
	vp_cpy r1, r11
	vp_shr 24, r11
	vp_shr 16, r8
	vp_shr 8, r9
	vp_and 0xff, r8
	vp_and 0xff, r9
	vp_and 0xff, r10
	vp_and 0xff, r11
	sdl_set_render_draw_color [r0 + gui_ctx_sdl_ctx], r8, r9, r10, r11

	vp_cpy [r4 + local_old_stack], r4
	vp_ret

def_func_end

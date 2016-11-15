%include 'inc/func.ninc'
%include 'class/class_button.ninc'
%include 'class/class_flow.ninc'

def_func class/button/layout
	;inputs
	;r0 = button object
	;trashes
	;all but r0, r4

	def_struct local
		ptr local_inst
	def_struct_end

	;save inputs
	vp_sub local_size, r4
	vp_cpy r0, [r4 + local_inst]

	vp_cpy button_border_size, r8
	vp_cpy [r0 + view_w], r10
	vp_cpy [r0 + view_h], r11
	vp_sub button_border_size * 2, r10
	vp_sub button_border_size * 2, r11
	vp_cpy [r0 + button_state], r1
	vp_and button_state_pressed, r1
	vpif r1, !=, 0
		vp_add button_border_size, r8
	endif
	f_call flow, change, {[r0 + label_flow], r8, r8, r10, r11}

	f_call button, opaque, {[r4 + local_inst]}
	vp_add local_size, r4
	vp_ret

def_func_end

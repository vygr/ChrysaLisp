%include 'inc/func.ninc'
%include 'class/class_button.ninc'

def_func class/button/mouse_down
	;inputs
	;r0 = button object
	;r1 = mouse event message
	;trashes
	;all but r0, r4

	vp_cpy [r0 + button_state], r1
	vp_or button_state_pressed, r1
	vp_cpy r1, [r0 + button_state]
	v_call button, layout, {r0}
	f_jmp button, dirty, {r0}

def_func_end

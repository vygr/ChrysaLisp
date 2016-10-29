%include 'inc/func.inc'
%include 'class/class_button.inc'

def_func class/button/sig_pressed
	;inputs
	;r0 = button object
	;outputs
	;r1 = pressed signal list

	vp_lea [r0 + button_pressed_signal], r1
	vp_ret

def_func_end

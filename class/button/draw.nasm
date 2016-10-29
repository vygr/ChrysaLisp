%include 'inc/func.inc'
%include 'inc/font.inc'
%include 'class/class_button.inc'

def_func class/button/draw
	;inputs
	;r0 = button object
	;r1 = ctx object
	;trashes
	;all but r0, r4

	;draw panel
	vp_cpy button_border_size, r3
	vp_cpy [r0 + button_state], r2
	vp_and button_state_pressed, r2
	if r2, !=, 0
		vp_mul -1, r3
	endif
	f_jmp button, draw_panel, {r0, r1, 1, r3}

def_func_end

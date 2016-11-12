%include 'inc/func.ninc'
%include 'class/class_button.ninc'

def_func class/button/deinit
	;inputs
	;r0 = button object
	;trashes
	;all but r0, r4

	;disconnnect all signals
	f_call button, disconnect_sig, {r0, &[r0 + button_pressed_signal]}

	;parent deinit
	s_jmp button, deinit, {r0}

def_func_end

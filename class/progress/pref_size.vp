%include 'inc/func.ninc'
%include 'class/class_progress.ninc'

def_func class/progress/pref_size
	;inputs
	;r0 = progress object
	;outputs
	;r10 = prefered width
	;r11 = prefered height
	;trashes
	;all but r0, r4

	vp_cpy 256, r10
	vp_cpy 10, r11
	vp_ret

def_func_end

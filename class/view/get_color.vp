%include 'inc/func.ninc'
%include 'class/class_view.ninc'

def_func class/view/get_color
	;inputs
	;r0 = view object
	;outputs
	;r0 = view object
	;r1 = color

	;get color info
	vp_cpy [r0 + view_color], r1
	vp_ret

def_func_end

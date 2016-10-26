%include 'inc/func.inc'
%include 'inc/list.inc'

def_func sys/list_init
	;inputs
	;r0 = list head
	;trashes
	;r1

	lh_init r0, r1
	vp_ret

def_func_end

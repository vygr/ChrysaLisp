%include 'inc/func.ninc'
%include 'class/class_grid.ninc'

def_func class/grid/init
	;inputs
	;r0 = grid object
	;r1 = vtable pointer
	;outputs
	;r1 = 0 if error, else ok

	;init parent
	s_call grid, init, {r0, r1}, {r1}
	vpif r1, !=, 0
		;init myself
		vp_cpy 1, r1
		vp_cpy r1, [r0 + grid_width]
		vp_cpy r1, [r0 + grid_height]
	endif
	vp_ret

def_func_end

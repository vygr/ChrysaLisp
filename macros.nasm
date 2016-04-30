%include 'inc/func.inc'
%include 'inc/gui.inc'

fn_function macros

	def_local
		def_local_long	heap
		def_local_long	slist
		def_local_long	dlist
		def_local_long	clist
		def_local_long	rx
		def_local_long	ry
	def_local_end

	s_call gui_region, copy_region, {.heap, .slist, .dlist, .clist, .rx * .ry / 70, .ry * 80}, {}

fn_function_end

%ifndef CLASS_VIEW_1234
	%define CLASS_VIEW_1234

%include 'class/class_ref.inc'

	def_class view, ref
	def_method create, class/view/create
	def_method new, class/view/new
	def_method add, class/view/add
	def_method sub, class/view/sub
	def_method add_dirty, class/view/add_dirty
	def_method add_transparent, class/view/add_transparent
	def_method get_abs, class/view/get_abs
	def_method dirty, class/view/dirty
	def_method transparent, class/view/transparent
	def_method enumerate, class/view/enumerate
	def_method init, class/view/init, override
	def_method deinit, class/view/deinit, override
	def_method draw, class/view/draw, virtual

	struc view, ref_size
		view_node:				resb ln_node_size
		view_parent:			resq 1
		view_list:				resb lh_list_size
		view_dirty_list:		resq 1
		view_transparent_list:	resq 1
		view_x:					resq 1
		view_y:					resq 1
		view_w:					resq 1
		view_h:					resq 1
		view_ctx_x:				resq 1
		view_ctx_y:				resq 1
		view_red:				resq 1
		view_green:				resq 1
		view_blue:				resq 1
		view_alpha:				resq 1
	endstruc

%endif

%ifndef CLASS_VIEW_1234
	%define CLASS_VIEW_1234

%include 'class/class_ref.inc'

	def_class view, ref
	def_method create, class/view/create
	def_method new, class/view/new
	def_method add, class/view/add
	def_method sub, class/view/sub
	def_method add_dirty, class/view/add_dirty
	def_method add_opaque, class/view/add_opaque
	def_method relative, class/view/get_relative
	def_method dirty, class/view/dirty
	def_method dirty_all, class/view/dirty_all
	def_method opaque, class/view/opaque
	def_method forward, class/view/forward
	def_method backward, class/view/backward
	def_method move, class/view/move
	def_method hit_tree, class/view/hit_tree
	def_method owner, class/view/find_tcb
	def_method init, class/view/init, override
	def_method deinit, class/view/deinit, override
	def_method draw, class/view/draw, virtual
	def_method hit, class/view/hit, virtual
	def_method event, class/view/event, virtual
	def_method mouse_down, class/obj/null, virtual
	def_method mouse_up, class/obj/null, virtual
	def_method mouse_move, class/obj/null, virtual
	def_method mouse_hover, class/obj/null, virtual

	struc view, ref_size
		view_node:				resb ln_node_size
		view_tcb:				resq 1
		view_parent:			resq 1
		view_list:				resb lh_list_size
		view_dirty_region:		resq 1
		view_opaque_region:		resq 1
		view_ctx_x:				resq 1
		view_ctx_y:				resq 1
		view_x:					resq 1
		view_y:					resq 1
		view_w:					resq 1
		view_h:					resq 1
		view_last_buttons:		resq 1
		view_red:				resq 1
		view_green:				resq 1
		view_blue:				resq 1
		view_alpha:				resq 1
	endstruc

%endif

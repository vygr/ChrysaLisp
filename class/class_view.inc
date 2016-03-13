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
	def_method forward_tree, class/view/forward_tree
	def_method backward_tree, class/view/backward_tree
	def_method move, class/view/move
	def_method change, class/view/change
	def_method hit_tree, class/view/hit_tree
	def_method owner, class/view/find_tcb
	def_method draw_panel, class/view/draw_panel

	def_method init, class/view/init, override
	def_method deinit, class/view/deinit, override

	def_method draw, class/view/draw, virtual
	def_method hit, class/view/hit, virtual
	def_method pref_size, class/view/pref_size, virtual
	def_method layout, class/obj/null, virtual
	def_method event, class/view/event, virtual
	def_method mouse_down, class/obj/null, virtual
	def_method mouse_up, class/obj/null, virtual
	def_method mouse_move, class/obj/null, virtual
	def_method mouse_hover, class/obj/null, virtual

	def_structure view, ref
		def_struct	view_node, ln_node
		def_struct	view_list, lh_list
		def_long	view_parent
		def_long	view_tcb
		def_long	view_dirty_region
		def_long	view_opaque_region
		def_long	view_ctx_x
		def_long	view_ctx_y
		def_long	view_x
		def_long	view_y
		def_long	view_w
		def_long	view_h
		def_long	view_last_buttons
		def_long	view_red
		def_long	view_green
		def_long	view_blue
		def_long	view_alpha
	def_structure_end

%endif

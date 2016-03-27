%ifndef CLASS_VIEW_1234
%define CLASS_VIEW_1234

%include 'class/class_ref.inc'

	def_class view, ref
	def_method create, class/view/create
	def_method new, class/view/new
	def_method init, class/view/init
	def_method add, class/view/add
	def_method sub, class/view/sub
	def_method add_dirty, class/view/add_dirty
	def_method add_opaque, class/view/add_opaque
	def_method get_relative, class/view/get_relative
	def_method dirty, class/view/dirty
	def_method dirty_all, class/view/dirty_all
	def_method opaque, class/view/opaque
	def_method forward, class/view/forward
	def_method backward, class/view/backward
	def_method forward_tree, class/view/forward_tree
	def_method backward_tree, class/view/backward_tree
	def_method change, class/view/change
	def_method hit_tree, class/view/hit_tree
	def_method set_owner, class/view/set_owner
	def_method find_owner, class/view/find_owner
	def_method draw_panel, class/view/draw_panel
	def_method set_color, class/view/set_color
	def_method get_color, class/view/get_color
	def_method get_bounds, class/view/get_bounds
	def_method set_bounds, class/view/set_bounds
	def_method set_flags, class/view/set_flags

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

	view_flag_solid	equ 1 << 0

	def_structure	view, ref
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
		def_long	view_flags
		def_long	view_color
	def_structure_end

%endif

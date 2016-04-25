%ifndef CLASS_VIEW_1234
%define CLASS_VIEW_1234

%include 'class/class_component.inc'

	def_class view, component
	def_method create, class/view/create
	def_method new, class/view/new
	def_method init, class/view/init
	def_method add, class/view/add, static, {r0, r1}
	def_method sub, class/view/sub, static, {r0}
	def_method add_dirty, class/view/add_dirty, static, {r0, r8, r9, r10, r11}
	def_method add_opaque, class/view/add_opaque, static, {r0, r8, r9, r10, r11}
	def_method get_relative, class/view/get_relative, static, {r0, r1, r8, r9}, {r8, r9}
	def_method dirty, class/view/dirty, static, {r0}
	def_method dirty_all, class/view/dirty_all, static, {r0}
	def_method opaque, class/view/opaque, static, {r0}
	def_method forward, class/view/forward, static, {r0, r1, r2}
	def_method backward, class/view/backward, static, {r0, r1, r2}
	def_method forward_tree, class/view/forward_tree, static, {r0, r1, r2, r3}
	def_method backward_tree, class/view/backward_tree, static, {r0, r1, r2, r3}
	def_method change, class/view/change, static, {r0, r8, r9, r10, r11}
	def_method hit_tree, class/view/hit_tree, static, {r0, r8, r9}, {r1, r8, r9}
	def_method set_owner, class/view/set_owner, static, {r0, r1}
	def_method find_owner, class/view/find_owner, static, {r0}, {r1}
	def_method draw_panel, class/view/draw_panel, static, {r0, r1, r2, r3}
	def_method set_color, class/view/set_color, static, {r0, r1}
	def_method get_color, class/view/get_color, static, {r0}, {r1}
	def_method get_bounds, class/view/get_bounds, static, {r0}, {r8, r9, r10, r11}
	def_method set_bounds, class/view/set_bounds, static, {r0, r8, r9, r10, r11}
	def_method set_flags, class/view/set_flags, static, {r0, r1}
	def_method get_parent, class/view/get_parent, static, {r0}, {r1}

	def_method deinit, class/view/deinit, override

	def_method draw, class/obj/null, virtual, {r0}
	def_method hit, class/view/hit, virtual, {r0, r8, r9}, {r1}
	def_method pref_size, class/view/pref_size, virtual, {r0}, {r10, r11}
	def_method layout, class/obj/null, virtual, {r0}
	def_method event, class/view/event, virtual, {r0, r1}
	def_method mouse_down, class/obj/null, virtual, {r0, r1}
	def_method mouse_up, class/obj/null, virtual, {r0, r1}
	def_method mouse_move, class/obj/null, virtual, {r0, r1}
	def_method mouse_hover, class/obj/null, virtual, {r0, r1}

	view_flag_solid	equ 1 << 0

	def_structure	view, component
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

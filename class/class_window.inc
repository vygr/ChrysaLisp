%ifndef CLASS_WINDOW_1234
%define CLASS_WINDOW_1234

%include 'class/class_view.inc'

	window_border_size	equ 4

	def_bit
		bit window_drag_left
		bit window_drag_right
		bit window_drag_top
		bit window_drag_bottom
	def_bit_end

	def_class window, view
	def_method create, class/window/create
	def_method new, class/window/new
	def_method init, class/window/init
	def_method get_panel, class/window/get_panel, static, {r0}, {r1}
	def_method set_title, class/window/set_title, static, {r0, r1}
	def_method set_status, class/window/set_status, static, {r0, r1}

	def_method pref_size, class/window/pref_size, override
	def_method layout, class/window/layout, override
	def_method draw, class/window/draw, override
	def_method mouse_down, class/window/mouse_down, override
	def_method mouse_move, class/window/mouse_move, override

	def_structure window, view
		ptr window_border
		ptr window_title
		ptr window_status
		ptr window_flow
		ptr window_panel
		long window_offset_x
		long window_offset_y
		ulong window_drag_mode
	def_structure_end

%endif

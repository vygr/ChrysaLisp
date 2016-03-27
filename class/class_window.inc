%ifndef CLASS_WINDOW_1234
%define CLASS_WINDOW_1234

%include 'class/class_view.inc'

%define window_border_size 8
%define window_drag_left 1 << 0
%define window_drag_right 1 << 1
%define window_drag_top 1 << 2
%define window_drag_bottom 1 << 3

	def_class window, view
	def_method create, class/window/create
	def_method new, class/window/new
	def_method init, class/window/init
	def_method get_panel, class/window/get_panel
	def_method set_title, class/window/set_title
	def_method set_status, class/window/set_status

	def_method deinit, class/window/deinit, override
	def_method pref_size, class/window/pref_size, override
	def_method layout, class/window/layout, override
	def_method draw, class/window/draw, override
	def_method mouse_down, class/window/mouse_down, override
	def_method mouse_move, class/window/mouse_move, override

	def_structure	window, view
		def_long	window_border
		def_long	window_title
		def_long	window_status
		def_long	window_flow
		def_long	window_panel
		def_long	window_offset_x
		def_long	window_offset_y
		def_long	window_drag_mode
	def_structure_end

%endif

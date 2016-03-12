%ifndef CLASS_WINDOW_1234
	%define CLASS_WINDOW_1234

%include 'class/class_view.inc'

	def_class window, view
	def_method new, tests/gui/gui1/window/new
	def_method create, tests/gui/gui1/window/create
	def_method mouse_down, tests/gui/gui1/window/mouse_down, override
	def_method mouse_move, tests/gui/gui1/window/mouse_move, override

	def_structure window, view
		def_long window_last_x
		def_long window_last_y
	def_structure_end

%endif

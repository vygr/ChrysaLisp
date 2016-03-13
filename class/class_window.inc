%ifndef CLASS_WINDOW_1234
%define CLASS_WINDOW_1234

%include 'class/class_flow.inc'

	def_class window, flow
	def_method create, class/window/create
	def_method new, class/window/new
	def_method panel, class/window/get_panel

	def_method init, class/window/init, override
	def_method deinit, class/window/deinit, override

	def_structure window, flow
		def_long	window_border
		def_long	window_title
		def_long	window_panel
	def_structure_end

%endif

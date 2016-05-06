%ifndef CLASS_GRID_1234
%define CLASS_GRID_1234

%include 'class/class_view.inc'

	def_class grid, view
	def_method create, class/grid/create
	def_method new, class/grid/new
	def_method init, class/grid/init
	def_method set_grid, class/grid/set_grid, static, {r0, r10, r11}
	def_method set_grid_flags, class/grid/set_grid_flags, static, {r0, r1}

	def_method pref_size, class/grid/pref_size, override
	def_method layout, class/grid/layout, override

	grid_flag	equ 1 << 0

	def_structure grid, view
		long grid_flags
		long grid_width
		long grid_height
	def_structure_end

%endif

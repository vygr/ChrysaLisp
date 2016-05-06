%ifndef CLASS_TITLE_1234
%define CLASS_TITLE_1234

%include 'class/class_label.inc'

	title_border_size	equ 1

	def_class title, label
	def_method create, class/title/create
	def_method new, class/title/new

	def_method pref_size, class/title/pref_size, override
	def_method draw, class/title/draw, override
	def_method layout, class/title/layout, override
	def_method mouse_down, class/title/mouse_down, override
	def_method mouse_move, class/title/mouse_move, override

	def_structure	title, label
		long	title_last_x
		long	title_last_y
	def_structure_end

%endif

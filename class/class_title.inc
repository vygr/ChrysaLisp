%ifndef CLASS_TITLE_1234
%define CLASS_TITLE_1234

%include 'class/class_label.inc'

%define title_border_size 1

	def_class title, label
	def_method create, class/title/create
	def_method new, class/title/new

	def_method pref_size, class/title/pref_size, override
	def_method draw, class/title/draw, override
	def_method mouse_down, class/title/mouse_down, override
	def_method mouse_move, class/title/mouse_move, override

	def_structure	title, label
		def_long	title_last_x
		def_long	title_last_y
	def_structure_end

%endif

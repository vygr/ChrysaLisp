%ifndef CLASS_BUTTON_1234
%define CLASS_BUTTON_1234

%include 'class/class_label.inc'

%define button_border_size 2

	def_class button, label
	def_method create, class/button/create
	def_method new, class/button/new

	def_method pref_size, class/button/pref_size, override
	def_method draw, class/button/draw, override

	def_structure	button, label
	def_structure_end

%endif

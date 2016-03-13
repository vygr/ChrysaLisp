%ifndef CLASS_BUTTON_1234
%define CLASS_BUTTON_1234

%include 'class/class_view.inc'

	def_class button, view
	def_method create, class/button/create
	def_method new, class/button/new
	def_method pref_size, class/button/pref_size, override
	def_method draw, class/button/draw, override

	def_structure button, view
	def_structure_end

%endif

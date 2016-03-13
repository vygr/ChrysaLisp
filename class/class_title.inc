%ifndef CLASS_TITLE_1234
%define CLASS_TITLE_1234

%include 'class/class_view.inc'

	def_class title, view
	def_method create, class/title/create
	def_method new, class/title/new
	def_method pref_size, class/title/pref_size, override

	def_structure title, view
	def_structure_end

%endif

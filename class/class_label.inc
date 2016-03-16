%ifndef CLASS_LABEL_1234
%define CLASS_LABEL_1234

%include 'class/class_view.inc'

	def_class label, view
	def_method create, class/label/create
	def_method new, class/label/new

	def_method pref_size, class/label/pref_size, override
	def_method draw, class/label/draw, override

	def_structure label, view
	def_structure_end

%endif

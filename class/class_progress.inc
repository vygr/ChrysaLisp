%ifndef CLASS_PROGRESS_1234
%define CLASS_PROGRESS_1234

%include 'class/class_view.inc'

%define progress_border_size 1

	def_class progress, view
	def_method create, class/progress/create
	def_method new, class/progress/new

	def_method pref_size, class/progress/pref_size, override
	def_method draw, class/progress/draw, override

	def_structure progress, view
	def_structure_end

%endif

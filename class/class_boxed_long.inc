%ifndef CLASS_BOXED_LONG_1234
%define CLASS_BOXED_LONG_1234

%include 'class/class_boxed_ptr.inc'

	def_class boxed_long, boxed_ptr
	def_method create, class/boxed_long/create
	def_method new, class/boxed_long/new

	def_structure boxed_long, boxed_ptr
	def_structure_end

%endif

%ifndef CLASS_BOXED_LONG_1234
%define CLASS_BOXED_LONG_1234

%include 'class/class_boxed_ptr.inc'

	def_class boxed_long, boxed_ptr
	def_method create, class/boxed_long/create
	def_method new, class/boxed_long/new
	def_method init, class/boxed_long/init
	def_method get_value, class/boxed_long/get_value, static, {r0}, {r1}
	def_method set_value, class/boxed_long/set_value, static, {r0, r1}

	def_structure boxed_long, boxed_ptr
	def_structure_end

%endif

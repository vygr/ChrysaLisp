%ifndef CLASS_BOXED_LONG_1234
%define CLASS_BOXED_LONG_1234

%include 'class/class_ref.inc'

	def_class boxed_long, ref
	def_method create, class/boxed_long/create
	def_method new, class/boxed_long/new
	def_method init, class/boxed_long/init
	def_method get_value, class/boxed_long/get_value, static, {r0}, {r1}
	def_method set_value, class/boxed_long/set_value, static, {r0, r1}

	def_structure boxed_long, ref
		long boxed_long_value
	def_structure_end

%endif

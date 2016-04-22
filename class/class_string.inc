%ifndef CLASS_STRING_1234
%define CLASS_STRING_1234

%include 'class/class_ref.inc'

	def_class string, ref
	def_method create, class/string/create, static, 'r0', 'r0'
	def_method new, class/string/new, static, 'r0', 'r0'
	def_method init, class/string/init, static, 'r0, r1, r2, r3', 'r1'
	def_method init1, class/string/init1, static, 'r0, r1, r2, r3', 'r1'
	def_method add, class/string/add, static, 'r0, r1', 'r0'

	def_structure	string, ref
		def_long	string_length
		def_struct	string_data, null
	def_structure_end

%endif

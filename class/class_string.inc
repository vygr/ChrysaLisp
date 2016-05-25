%ifndef CLASS_STRING_1234
%define CLASS_STRING_1234

%include 'class/class_ref.inc'

	def_class string, ref
	def_method create_from_cstr, class/string/create_from_cstr, static, {r0}, {r0}
	def_method create_from_file, class/string/create_from_file, static, {r0}, {r0}
	def_method new, class/string/new, static, {r0}, {r0}
	def_method init, class/string/init, static, {r0, r1, r2, r3}, {r1}
	def_method init1, class/string/init1, static, {r0, r1, r2, r3}, {r1}
	def_method init2, class/string/init2, static, {r0, r1, r2, r3}, {r1}
	def_method add, class/string/add, static, {r0, r1}, {r0}

	def_method hash, class/string/hash, override

	def_structure string, ref
		ulong string_length
		struct string_data, null
	def_structure_end

%endif

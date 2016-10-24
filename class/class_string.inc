%ifndef CLASS_STRING_1234
%define CLASS_STRING_1234

%include 'class/class_sequence.inc'

	def_class string, sequence
	def_method create_from_buffer, class/string/create_from_buffer, static, {r0, r1}, {r0}
	def_method create_from_cstr, class/string/create_from_cstr, static, {r0}, {r0}
	def_method create_from_file, class/string/create_from_file, static, {r0}, {r0}
	def_method create_from_long, class/string/create_from_long, static, {r0, r1}, {r0}
	def_method append, class/string/append, static, {r0, r1}, {r0}
	def_method new, class/string/new, static, {r0}, {r0}
	def_method init, class/string/init, static, {r0, r1, r2, r3}, {r1}
	def_method init1, class/string/init1, static, {r0, r1, r2, r3}, {r1}
	def_method init2, class/string/init2, static, {r0, r1, r2, r3}, {r1}
	def_method split, class/string/split, static, {r0, r1}, {r1}
	def_method compare, class/string/compare, static, {r0, r1}, {r1}

	def_method hash, class/string/hash, override
	def_method get_length, class/string/get_length, override
	def_method ref_element, class/string/ref_element, override
	def_method slice, class/string/slice, override

	def_structure string, sequence
		ulong string_length
		ulong string_hashcode
		offset string_data
	def_structure_end

%endif

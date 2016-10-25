%ifndef CLASS_VECTOR_1234
%define CLASS_VECTOR_1234

%include 'class/class_sequence.inc'

	def_class vector, sequence
	def_method create, class/vector/create
	def_method new, class/vector/new
	def_method init, class/vector/init
	def_method get_capacity, class/vector/get_capacity, static, {r0}, {r1}
	def_method set_capacity, class/vector/set_capacity, static, {r0, r1}
	def_method ref_back, class/vector/ref_back, static, {r0}, {r1}
	def_method get_back, class/vector/get_back, static, {r0}, {r1}
	def_method get_element, class/vector/get_element, static, {r0, r1}, {r1}
	def_method push_back, class/vector/push_back, static, {r0, r1}
	def_method pop_back, class/vector/pop_back, static, {r0}
	def_method clear, class/vector/clear, static, {r0}
	def_method for_each, class/vector/for_each, static, {r0, r1, r2, r3, r5}, {r1}
	def_method set_element, class/vector/set_element, static, {r0, r1, r2}
	def_method append, class/vector/append, static, {r0, r1, r2, r3}

	def_method deinit, class/vector/deinit, override
	def_method get_length, class/vector/get_length, override
	def_method ref_element, class/vector/ref_element, override
	def_method slice, class/vector/slice, override

	def_structure vector, sequence
		pptr vector_array
		ulong vector_length
		ulong vector_capacity
	def_structure_end

%endif

%ifndef CLASS_UNORDERED_SET_1234
%define CLASS_UNORDERED_SET_1234

%include 'class/class_ref.inc'

	def_class unordered_set, ref
	def_method create, class/unordered_set/create, static, {r0}, {r1}
	def_method new, class/unordered_set/new
	def_method init, class/unordered_set/init, static, {r0, r1, r2}, {r1}
	def_method clear, class/unordered_set/clear, static, {r0}
	def_method for_each, class/unordered_set/for_each, static, {r0, r1, r2}, {r1}

	def_method deinit, class/unordered_set/deinit, override

	def_structure unordered_set, ref
		ptr unordered_set_buckets
		ulong unordered_set_num_buckets
	def_structure_end

%endif

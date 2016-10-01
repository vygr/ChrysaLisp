%ifndef CLASS_UNORDERED_MAP_1234
%define CLASS_UNORDERED_MAP_1234

%include 'class/class_unordered_set.inc'

	def_class unordered_map, unordered_set
	def_method create, class/unordered_map/create
	def_method find, class/unordered_map/find
	def_method insert, class/unordered_map/insert, static, {r0, r1, r2}, {r1, r2}

	def_structure unordered_map, unordered_set
	def_structure_end

%endif

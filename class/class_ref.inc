%ifndef CLASS_REF_1234
%define CLASS_REF_1234

%include 'class/class_obj.inc'

	def_class ref, obj
	def_method ref, class/ref/ref, static, {r0}
	def_method deref, class/ref/deref, static, {r0}
	def_method init, class/ref/init

	def_structure	ref, obj
		long	ref_count
	def_structure_end

%endif

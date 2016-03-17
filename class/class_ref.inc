%ifndef CLASS_REF_1234
%define CLASS_REF_1234

%include 'class/class_obj.inc'

	def_class ref, obj
	def_method ref, class/ref/ref
	def_method deref, class/ref/deref
	def_method init, class/ref/init

	def_structure	ref, obj
		def_long	ref_count
	def_structure_end

%endif

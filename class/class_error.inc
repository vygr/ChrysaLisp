%ifndef CLASS_ERROR_1234
%define CLASS_ERROR_1234

%include 'class/class_ref.inc'

	def_class error, ref
	def_method create, class/error/create, static, {r0, r1}, {r0}
	def_method new, class/error/new
	def_method init, class/error/init, static, {r0, r1, r2, r3}, {r1}
	def_method get_description, class/error/get_description, static, {r0}, {r1}
	def_method get_object, class/error/get_object, static, {r0}, {r1}

	def_method deinit, class/error/deinit, override

	def_structure error, ref
		ptr error_description
		ptr error_object
	def_structure_end

%endif

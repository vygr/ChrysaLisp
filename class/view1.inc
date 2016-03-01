%ifndef CLASS_VIEW1_1234
	%define CLASS_VIEW1_1234

%include 'class/ref.inc'

	redef_method obj_init, class/view1/init
	redef_method obj_deinit, class/view1/deinit

	def_method view1_mul, class/view1/mul
	def_method view1_add, class/view1/add
	def_method view1_sub, class/view1/sub

%endif

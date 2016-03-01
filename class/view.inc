%ifndef CLASS_VIEW_1234
	%define CLASS_VIEW_1234

%include 'class/ref.inc'

	redef_method obj_init, class/view/init
	redef_method obj_deinit, class/view/deinit

	def_method view_add, class/view/add
	def_method view_sub, class/view/sub

%endif

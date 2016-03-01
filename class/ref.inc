%include 'class/obj.inc'

	redef_method obj_init, class/ref/init
	redef_method obj_deinit, class/ref/deinit

	def_method ref_ref, class/ref/ref
	def_method ref_deref, class/ref/deref

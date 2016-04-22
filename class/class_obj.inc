%ifndef CLASS_OBJ_1234
%define CLASS_OBJ_1234

%include 'inc/class.inc'

	def_class obj
	def_method create, class/obj/create, static, '', 'r0'
	def_method destroy, class/obj/destroy, static, 'r0'
	def_method new, class/obj/new, static, '', 'r0'
	def_method init, class/obj/init, static, 'r0, r1', 'r1'

	def_method delete, sys/mem_free, virtual, 'r0'
	def_method deinit, class/obj/null, virtual, 'r0'

	def_structure	obj
		def_long	obj_vtable
	def_structure_end

%endif

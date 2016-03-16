%ifndef CLASS_OBJ_1234
%define CLASS_OBJ_1234

%include 'inc/class.inc'

	def_class obj
	def_method create, class/obj/create
	def_method destroy, class/obj/destroy
	def_method new, class/obj/new
	def_method init, class/obj/init
	def_method delete, sys/mem_free, virtual
	def_method deinit, class/obj/null, virtual

	def_structure obj
		def_long	obj_vtable
	def_structure_end

%endif

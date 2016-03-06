%ifndef CLASS_OBJ_1234
    %define CLASS_OBJ_1234

%include 'inc/class.inc'

    def_class obj
    def_method create, class/obj/create
	def_method new, class/obj/new
	def_method init, class/obj/init, virtual
    def_method destroy, class/obj/destroy, virtual
    def_method delete, sys/mem_free, virtual
    def_method deinit, class/obj/null, virtual

	struc obj
		obj_vtable:		resq 1
	endstruc

%endif

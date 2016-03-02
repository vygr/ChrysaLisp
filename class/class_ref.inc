%ifndef CLASS_REF_1234
    %define CLASS_REF_1234

%include 'class/class_obj.inc'

    def_class ref, obj
	def_method_override ref, init, class/ref/init
	def_method ref, ref, class/ref/ref
	def_method ref, deref, class/ref/deref

	struc REF
		REF_PARENT:		resb OBJ_SIZE
		REF_COUNT:		resq 1
		REF_SIZE:
	endstruc

%endif

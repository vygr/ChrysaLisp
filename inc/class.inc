%ifndef CLASS_1234
	%define CLASS_1234

	%macro vtable_emit 1
		;%1 = class name
		%assign %%_n 0
		%rep _map_%[%1]_size
			%ifidn _map_%[%1]_type_%[%%_n], virtual
				fn_add_link _map_%[%1]_func_%[%%_n]
				;%warning class %1 vtable entry _map_%[%1]_name_%[%%_n]
			%endif
			%assign %%_n %%_n + 1
		%endrep
	%endmacro

	%macro class_add 2
		;%1 = dest class name
		;%2 = source class name
		%ifndef _map_%[%1]_size
			%assign _map_%[%1]_size 0
		%endif
		%ifndef _map_%[%2]_size
			%assign _map_%[%2]_size 0
		%endif
		%assign %%_n 0
		%rep _map_%[%2]_size
			def_method _map_%[%2]_name_%[%%_n], _map_%[%2]_func_%[%%_n], _map_%[%2]_type_%[%%_n]
			%assign %%_n %%_n + 1
		%endrep
	%endmacro

	%macro def_class 1-2 null
		;%1 = class name
		;%2 = super class name
		%ifdef _map_%[%1]_size
			%fatal Class %1 allready exits !
		%endif
		%define _class_ %1
		%define _super_class_%[%1]_ %2
		class_add %1, %2
	%endmacro

	%macro def_method 2-3 nonvirtual
		;%1 = member name
		;%2 = member function
		;%3 = mode (nonvirtual/virtual/override)
		%assign %%_n 0
		%rep _map_%[_class_]_size
			%ifidn _map_%[_class_]_name_%[%%_n], %1
				%exitrep
			%else
				%assign %%_n %%_n + 1
			%endif
		%endrep
		%if %%_n = _map_%[_class_]_size
			;new method
			%ifidn %3, nonvirtual
				%assign _map_%[_class_]_size %%_n + 1
				%define _map_%[_class_]_name_%[%%_n] %1
				%define _map_%[_class_]_func_%[%%_n] %2
				%define _map_%[_class_]_type_%[%%_n] %3
			%elifidn %3, virtual
				%assign _map_%[_class_]_size %%_n + 1
				%define _map_%[_class_]_name_%[%%_n] %1
				%define _map_%[_class_]_func_%[%%_n] %2
				%define _map_%[_class_]_type_%[%%_n] %3
			%elifidn %3, override
				%fatal Method %1 does not exist !
			%else
				%fatal Must use nonvirtual, virtual or override !
			%endif
		%else
			;existing method
			%ifidn %3, nonvirtual
				%ifidn _map_%[_class_]_type_%[%%_n], nonvirtual
					%define _map_%[_class_]_func_%[%%_n] %2
				%else
					%fatal Virtual method %1 allready exists !
				%endif
			%elifidn %3, virtual
				%fatal Method %1 allready exists !, use override ?
			%elifidn %3, override
				%ifidn _map_%[_class_]_type_%[%%_n], virtual
					%define _map_%[_class_]_func_%[%%_n] %2
				%else
					%fatal Nonvirtual %1 allready exists !
				%endif
			%else
				%fatal Must use nonvirtual, virtual or override !
			%endif
		%endif
	%endmacro

	%macro method_lookup 2
		;%1 = class name
		;%2 = member name
		%ifndef _map_%[%1]_size
			%fatal Class %1 not defined !
		%endif
		%assign _virtual_ 0
		%assign _nonvirtual_ 0
		%rep _map_%[%1]_size
			%ifidn _map_%[%1]_name_%[_nonvirtual_], %2
				%exitrep
			%elifidn _map_%[%1]_type_%[_nonvirtual_], virtual
				%assign _virtual_ _virtual_ + 1
				%assign _nonvirtual_ _nonvirtual_ + 1
			%else
				%assign _nonvirtual_ _nonvirtual_ + 1
			%endif
		%endrep
		%if _nonvirtual_ = _map_%[%1]_size
			;no such method
			%fatal No such method %2 in class %1 !
		%endif
	%endmacro

	%macro method_call 2-4 r15, r0
		;%1 = class name
		;%2 = member name
		;%3 = dispatch reg
		;%4 = instance pointer reg

		method_lookup %1, %2
		%ifidn _map_%[%1]_type_%[_nonvirtual_], nonvirtual
			%fatal Class %1 method %2 is nonvirtual ! Use static_call ?
		%endif
		vp_cpy [%4 + obj_vtable], %3
		vp_call [%3 + _virtual_ * 8]
	%endmacro

	%macro method_jmp 2-4 r15, r0
		;%1 = class name
		;%2 = member name
		;%3 = dispatch reg
		;%4 = instance pointer reg

		method_lookup %1, %2
		%ifidn _map_%[%1]_type_%[_nonvirtual_], nonvirtual
			%fatal Class %1 method %2 is nonvirtual ! Use static_jmp ?
		%endif
		vp_cpy [%4 + obj_vtable], %3
		vp_call [%3 + _virtual_ * 8]
	%endmacro

	%macro method_bind 2-4 r15, r0
		;%1 = class name
		;%2 = member name
		;%3 = dispatch reg
		;%4 = instance pointer reg

		method_lookup %1, %2
		%ifidn _map_%[%1]_type_%[_nonvirtual_], nonvirtual
			%fatal Class %1 method %2 is nonvirtual ! Use static_bind ?
		%endif
		vp_cpy [%4 + obj_vtable], %3
		vp_call [%3 + _virtual_ * 8]
	%endmacro

	%macro static_call 2
		;%1 = class name
		;%2 = member name

		method_lookup %1, %2
		%ifidn _map_%[%1]_type_%[_nonvirtual_], virtual
			%ifidn _map_%[%1]_name_%[_nonvirtual_], init
				;exeption for init
			%else
				%fatal Class %1 method %2 is virtual ! Use method_call ?
			%endif
		%endif
		fn_call _map_%[%1]_func_%[_nonvirtual_]
	%endmacro

	%macro static_jmp 2
		;%1 = class name
		;%2 = member name

		method_lookup %1, %2
		%ifidn _map_%[%1]_type_%[_nonvirtual_], virtual
			%fatal Class %1 method %2 is virtual ! Use method_jmp ?
		%endif
		fn_jmp _map_%[%1]_func_%[_nonvirtual_]
	%endmacro

	%macro static_bind 3
		;%1 = class name
		;%2 = member name
		;%3 = reg

		method_lookup %1, %2
		%ifidn _map_%[%1]_type_%[_nonvirtual_], virtual
			%fatal Class %1 method %2 is virtual ! Use method_bind ?
		%endif
		fn_bind _map_%[%1]_func_%[_nonvirtual_], %3
	%endmacro

	%macro super_call 2
		;%1 = class name
		;%2 = member name

		method_lookup _super_class_%[%1]_, %2
		%ifidn _map_%[_super_class_%[%1]_]_type_%[_nonvirtual_], nonvirtual
			%fatal Class _super_class_%[%1]_ method %2 is nonvirtual !
		%endif
		fn_call _map_%[_super_class_%[%1]_]_func_%[_nonvirtual_]
	%endmacro

	%macro super_jmp 2
		;%1 = class name
		;%2 = member name

		method_lookup _super_class_%[%1]_, %2
		%ifidn _map_%[_super_class_%[%1]_]_type_%[_nonvirtual_], nonvirtual
			%fatal Class _super_class_%[%1]_ method %2 is nonvirtual !
		%endif
		fn_jmp _map_%[_super_class_%[%1]_]_func_%[_nonvirtual_]
	%endmacro

	%macro super_bind 3
		;%1 = class name
		;%2 = member name
		;%3 = reg

		method_lookup _super_class_%[%1]_, %2
		%ifidn _map_%[_super_class_%[%1]_]_type_%[_nonvirtual_], nonvirtual
			%fatal Class _super_class_%[%1]_ method %2 is nonvirtual !
		%endif
		fn_bind _map_%[_super_class_%[%1]_]_func_%[_nonvirtual_], %3
	%endmacro

%endif

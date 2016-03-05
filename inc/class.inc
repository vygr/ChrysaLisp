%ifndef CLASS_1234
	%define CLASS_1234

	%macro vtable_emit 1
		;%1 = <class name>
		%assign %%_n 0
		%rep _map_%[%1]_size
			fn_add_link _map_%[%1]_func_%[%%_n]
			%assign %%_n %%_n + 1
		%endrep
	%endmacro

	%macro class_add 2
		;%1 = <dest class name>
		;%2 = <source class name>
		%ifndef _map_%[%1]_size
			%assign _map_%[%1]_size 0
		%endif
		%ifndef _map_%[%2]_size
			%assign _map_%[%2]_size 0
		%endif
		%assign %%_n 0
		%rep _map_%[%2]_size
			def_method _map_%[%2]_name_%[%%_n], _map_%[%2]_func_%[%%_n]
			%assign %%_n %%_n + 1
		%endrep
	%endmacro

	%macro def_class 1-2 null
		;%1 = <class name>
		;%2 = <super class name>
		%ifdef _map_%[%1]_size
			%fatal Class %1 allready exits !
		%endif
		%define _class_ %1
		%define _super_class_%[%1] %2
		class_add %1, %2
	%endmacro

	%macro def_method 2-3 declare
		;%1 = <member name>
		;%2 = <member function>
		;%3 = mode (declare/override)
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
			%ifidn %3, declare
				%assign _map_%[_class_]_size %%_n + 1
				%define _map_%[_class_]_name_%[%%_n] %1
				%define _map_%[_class_]_func_%[%%_n] %2
			%elifidn %3, override
				%fatal Method %1 does not exist, use declare ?
			%else
				%fatal Must use declare or override !
			%endif
		%else
			;existing method
			%ifidn %3, declare
				%fatal Method %1 allready exists, use override ?
			%elifidn %3, override
				;existing method
				%define _map_%[_class_]_name_%[%%_n] %1
				%define _map_%[_class_]_func_%[%%_n] %2
			%else
				%fatal Must use declare or override !
			%endif
		%endif
	%endmacro

	%macro method_lookup 2
		;%1 = <class name>
		;%2 = <member name>
		%assign %%_n 0
		%rep _map_%[%1]_size
			%ifidn _map_%[%1]_name_%[%%_n], %2
				%exitrep
			%else
				%assign %%_n %%_n + 1
			%endif
		%endrep
		%if %%_n = _map_%[%1]_size
			;no such method
			%fatal No such method %2
		%else
			%assign _method_ %%_n
		%endif
	%endmacro

	%macro method_call 2-4 r0, r15
		;%1 = <class name>
		;%2 = <member name>
		;%3 = instance pointer reg
		;%4 = dispatch reg

		method_lookup %1, %2
		vp_cpy [%3 + obj_vtable], %4
		vp_call [%4 + _method_ * 8]
	%endmacro

	%macro method_jmp 2-4 r0, r15
		;%1 = <class name>
		;%2 = <member name>
		;%3 = instance pointer reg
		;%4 = dispatch reg

		method_lookup %1, %2
		vp_cpy [%3 + obj_vtable], %4
		vp_jmp [%4 + _method_ * 8]
	%endmacro

	%macro method_bind 2-4 r0, r15
		;%1 = <class name>
		;%2 = <member name>
		;%3 = instance pointer reg
		;%4 = dispatch reg

		method_lookup %1, %2
		vp_cpy [%3 + obj_vtable], %4
		vp_cpy [%4 + _method_ * 8], %4
	%endmacro

	%macro class_call 2
		;%1 = <class name>
		;%2 = <member name>

		method_lookup %1, %2
		fn_call _map_%[%1]_func_%[_method_]
	%endmacro

	%macro class_jmp 2
		;%1 = <class name>
		;%2 = <member name>

		method_lookup %1, %2
		fn_jmp _map_%[%1]_func_%[_method_]
	%endmacro

	%macro class_bind 3
		;%1 = <class name>
		;%2 = <member name>
		;%3 = reg

		method_lookup %1, %2
		fn_bind _map_%[%1]_func_%[_method_], %3
	%endmacro

	%macro super_call 2
		;%1 = <class name>
		;%2 = <member name>
		class_call _super_class_%[%1], %2
	%endmacro

%endif

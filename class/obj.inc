%include 'inc/func.inc'

	%assign _method_num 0
    %macro def_method 2
		%assign %%_num _method_num
		%assign _method_num _method_num + 1
        %define _method_%[%1]_num %%_num
        %defstr _method_%[%%_num]_function %2
        %warning Def Method Number _method_%[%1]_num
        %warning Def Method Function _method_%[%%_num]_function
    %endmacro

    %macro redef_method 2
        %assign %%_num _method_%[%1]_num
		%deftok _parent_method_%[%%_num]_function _method_%[%%_num]_function
		%defstr _method_%[%%_num]_function %2

		%warning Redef Method Number %%_num
		%warning Redef Old Method Function _parent_method_%[%%_num]_function
		%warning Redef New Method Function _method_%[%%_num]_function
    %endmacro

    %macro method_impl_vtable 0
        %assign %%_num 0
        %rep _method_num
			%deftok %%_function _method_%[%%_num]_function
            %warning Vtable Method Function %%_num %%_function
            fn_jmp %%_function
            %assign %%_num %%_num + 1
        %endrep
	%endmacro

    %macro method_call 1
        dq  _method_%[%1]_num
	%endmacro

    %macro parent_call 1
		%assign %%_num _method_%[%1]_num
        fn_call _parent_method_%[%%_num]_function
	%endmacro

    def_method obj_new, class/obj/new
    def_method obj_delete, class/obj/delete
    def_method obj_init, class/obj/init
    def_method obj_deinit, class/obj/deinit

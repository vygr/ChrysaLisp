%ifndef CLASS_OBJ_1234
    %define CLASS_OBJ_1234

%include 'inc/func.inc'

    %assign _vtable_null_size 0

    %macro vtable_emit 1
        ;%1 = <class name>
        %assign %%_n 0
        %rep _vtable_%[%1]_size
			fn_jmp _vtable_%[%1]_func_%[%%_n]
            %assign %%_n %%_n + 1
        %endrep
    %endmacro

    %macro vtable_copy 2
        ;%1 = <source class name>
        ;%2 = <dest class name>
        %assign %%_n 0
        %rep _vtable_%[%1]_size
            %define _vtable_%[%2]_name_%[%%_n] _vtable_%[%1]_name_%[%%_n]
            %define _vtable_%[%2]_func_%[%%_n] _vtable_%[%1]_func_%[%%_n]
            %assign %%_n %%_n + 1
        %endrep
        %assign _vtable_%[%2]_size _vtable_%[%1]_size
    %endmacro

    %macro def_method 3
        ;%1 = <class name>
        ;%2 = <member name>
        ;%3 = <member function>
        %assign %%_n 0
        %rep _vtable_%[%1]_size
            %ifidn _vtable_%[%1]_name_%[%%_n], %2
                %exitrep
            %else
                %assign %%_n %%_n + 1
            %endif
        %endrep
        %if %%_n = _vtable_%[%1]_size
            ;new method
            %assign _vtable_%[%1]_size %%_n + 1
            %define _vtable_%[%1]_name_%[%%_n] %2
            %define _vtable_%[%1]_func_%[%%_n] %3
        %else
            ;existing method
            %fatal Method %2 allready exists, use def_method_override ?
        %endif
    %endmacro

    %macro def_method_override 3
        ;%1 = <class name>
        ;%2 = <member name>
        ;%3 = <member function>
        %assign %%_n 0
        %rep _vtable_%[%1]_size
            %ifidn _vtable_%[%1]_name_%[%%_n], %2
                %exitrep
            %else
                %assign %%_n %%_n + 1
            %endif
        %endrep
        %if %%_n = _vtable_%[%1]_size
            ;new method
            %fatal Method %2 dose not exist, use def_method ?
        %else
            ;existing method
            %define _vtable_%[%1]_name_%[%%_n] %2
            %define _vtable_%[%1]_func_%[%%_n] %3
        %endif
    %endmacro

    %macro def_class 2
        ;%1 = <class name>
        ;%2 = <parent class name>
        vtable_copy %2, %1
    %endmacro

    %macro method_call 2
        ;%1 = <class name>
        ;%2 = <member name>
        %assign %%_n 0
        %rep _vtable_%[%1]_size
            %ifidn _vtable_%[%1]_name_%[%%_n], %2
                %exitrep
            %else
                %assign %%_n %%_n + 1
            %endif
        %endrep
        %if %%_n = _vtable_%[%1]_size
            ;no such method
            %fatal No such method %2
        %else
            vp_cpy [r0 + OBJ_VTABLE], r15
			vp_call [r15 + (%%_n * 6)]
        %endif
    %endmacro

    %macro function_call 2
        ;%1 = <class name>
        ;%2 = <member name>
        %assign %%_n 0
        %rep _vtable_%[%1]_size
            %ifidn _vtable_%[%1]_name_%[%%_n], %2
                %exitrep
            %else
                %assign %%_n %%_n + 1
            %endif
        %endrep
        %if %%_n = _vtable_%[%1]_size
            ;no such method
            %fatal No such method %2
        %else
            fn_call _vtable_%[%1]_func_%[%%_n]
        %endif
    %endmacro

    def_class obj, null
    def_method obj, create, class/obj/create
    def_method obj, destroy, class/obj/destroy
    def_method obj, new, class/obj/new
    def_method obj, delete, class/obj/delete
    def_method obj, init, class/obj/init
    def_method obj, deinit, class/obj/deinit

	struc OBJ
		OBJ_VTABLE:		resq 1
		OBJ_SIZE:
	endstruc

%endif

%ifndef CLASS_1234
%define CLASS_1234

;;;;;;;;;;;;;;;;;;
; class definition
;;;;;;;;;;;;;;;;;;

	%assign _map_null_slots_ 0

	%macro vtable_emit 1
		;%1 = class name
		%assign %%n 0
		%rep _map_%1_slots_
			%ifidn _map_%1_type_%[%%n], virtual
				fn_add_link _map_%1_slot_%[%%n]
			%endif
			%assign %%n %%n + 1
		%endrep
	%endmacro

	%macro def_class 1-2 null
		;%1 = class name
		;%2 = super class name
		%ifdef _map_%1_slots_
			%fatal Class %1 allready defined !
		%endif
		%ifndef _map_%2_slots_
			%fatal Class %2 not defined !
		%endif
		%assign _map_%1_slots_ 0
		%xdefine _class_ %1
		%xdefine _super_class_%1_ %2
		%assign %%n 0
		%rep _map_%2_slots_
			def_method	_map_%2_name_%[%%n], \
						_map_%2_slot_%[%%n], \
						_map_%2_type_%[%%n], \
						{_map_%2_in_%[%%n]}, \
						{_map_%2_out_%[%%n]}
			%assign %%n %%n + 1
		%endrep
	%endmacro

	%macro def_method 2-5 static, {}, {}
		;%1 = member name
		;%2 = member function
		;%3 = mode (static/virtual/override)
		;%4 = in regs
		;%5 = out regs
		%assign %%n 0
		%rep _map_%[_class_]_slots_
			%ifidn _map_%[_class_]_name_%[%%n], %1
				%exitrep
			%else
				%assign %%n %%n + 1
			%endif
		%endrep
		%if %%n = _map_%[_class_]_slots_
			;new method
			%ifidn %3, static
				%assign _map_%[_class_]_slots_ %%n + 1
				%xdefine _map_%[_class_]_name_%[%%n] %1
				%xdefine _map_%[_class_]_slot_%[%%n] %2
				%xdefine _map_%[_class_]_type_%[%%n] %3
				%xdefine _map_%[_class_]_in_%[%%n] %4
				%xdefine _map_%[_class_]_out_%[%%n] %5
			%elifidn %3, virtual
				%assign _map_%[_class_]_slots_ %%n + 1
				%xdefine _map_%[_class_]_name_%[%%n] %1
				%xdefine _map_%[_class_]_slot_%[%%n] %2
				%xdefine _map_%[_class_]_type_%[%%n] %3
				%xdefine _map_%[_class_]_in_%[%%n] %4
				%xdefine _map_%[_class_]_out_%[%%n] %5
			%elifidn %3, override
				%fatal Method %1 does not exist !
			%else
				%fatal Must use static, virtual or override !
			%endif
		%else
			;existing method
			%ifidn %3, static
				%ifidn _map_%[_class_]_type_%[%%n], static
					%xdefine _map_%[_class_]_slot_%[%%n] %2
					%ifnempty %4
						%xdefine _map_%[_class_]_in_%[%%n] %4
					%endif
					%ifnempty %5
						%xdefine _map_%[_class_]_out_%[%%n] %5
					%endif
				%else
					%fatal Virtual method %1 allready exists !
				%endif
			%elifidn %3, virtual
				%fatal Method %1 allready exists !, use override ?
			%elifidn %3, override
				%ifidn _map_%[_class_]_type_%[%%n], virtual
					%xdefine _map_%[_class_]_slot_%[%%n] %2
					%ifnempty %4
						%xdefine _map_%[_class_]_in_%[%%n] %4
					%endif
					%ifnempty %5
						%xdefine _map_%[_class_]_out_%[%%n] %5
					%endif
				%else
					%fatal Static %1 allready exists !
				%endif
			%else
				%fatal Must use static, virtual or override !
			%endif
		%endif
	%endmacro

;;;;;;;;;;;;;;;
; class calling
;;;;;;;;;;;;;;;

	%macro method_lookup 2
		;%1 = class name
		;%2 = member name
		%ifndef _map_%1_slots_
			%fatal Class %1 not defined !
		%endif
		%assign _virtual_ 0
		%assign _static_ 0
		%rep _map_%1_slots_
			%ifidn _map_%1_name_%[_static_], %2
				%exitrep
			%elifidn _map_%1_type_%[_static_], virtual
				%assign _virtual_ _virtual_ + 1
				%assign _static_ _static_ + 1
			%else
				%assign _static_ _static_ + 1
			%endif
		%endrep
		%if _static_ = _map_%1_slots_
			%fatal No such method %2 in class %1 !
		%endif
	%endmacro

	%macro map_inputs 4
		;%1 = class name
		;%2 = member name
		;%3 = paramaters
		;%4 = slot num

		set_src %3
		set_dst _map_%1_in_%4
		map_src_to_dst
	%endmacro

	%macro map_outputs 4
		;%1 = class name
		;%2 = member name
		;%3 = paramaters
		;%4 = slot num

		set_src _map_%1_out_%4
		set_dst %3
		map_src_to_dst
	%endmacro

	%macro static_call 2-4 {}, {}
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters
		;%4 = out paramaters

		method_lookup %1, %2
		%ifidn _map_%1_type_%[_static_], virtual
			%fatal Class %1 method %2 is virtual ! Use method_call ?
		%endif
		map_inputs %1, %2, {%3}, _static_
		fn_call _map_%1_slot_%[_static_]
		map_outputs %1, %2, {%4}, _static_
	%endmacro

	%macro s_call 2-4 {}, {}
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters
		;%4 = out paramaters

		method_lookup %1, %2
		%ifidn _map_%1_type_%[_static_], virtual
			%fatal Class %1 method %2 is virtual ! Use m_call ?
		%endif
		eval {%3}, {_map_%1_in_%[_static_]}
		fn_call _map_%1_slot_%[_static_]
		retire {_map_%1_out_%[_static_]}, {%4}
	%endmacro

	%macro static_jmp 2-4 {}
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters

		method_lookup %1, %2
		%ifidn _map_%1_type_%[_static_], virtual
			%fatal Class %1 method %2 is virtual ! Use method_jmp ?
		%endif
		map_inputs %1, %2, {%3}, _static_
		fn_jmp _map_%1_slot_%[_static_]
	%endmacro

	%macro s_jmp 2-4 {}
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters

		method_lookup %1, %2
		%ifidn _map_%1_type_%[_static_], virtual
			%fatal Class %1 method %2 is virtual ! Use m_jmp ?
		%endif
		eval {%3}, {_map_%1_in_%[_static_]}
		fn_jmp _map_%1_slot_%[_static_]
	%endmacro

	%macro static_bind 3
		;%1 = class name
		;%2 = member name
		;%3 = reg

		method_lookup %1, %2
		%ifidn _map_%1_type_%[_static_], virtual
			%fatal Class %1 method %2 is virtual ! Use method_bind ?
		%endif
		fn_bind _map_%1_slot_%[_static_], %3
	%endmacro

	%macro super_call 2-4 {}, {}
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters
		;%4 = out paramaters

		method_lookup _super_class_%1_, %2
		%ifidn _map_%[_super_class_%1_]_type_%[_static_], static
			%ifidn _map_%[_super_class_%1_]_name_%[_static_], init
				;exeption for init
			%else
				%fatal Class _super_class_%1_ method %2 is static !
			%endif
		%endif
		map_inputs %1, %2, {%3}, _static_
		fn_call _map_%[_super_class_%1_]_slot_%[_static_]
		map_outputs %1, %2, {%4}, _static_
	%endmacro

	%macro p_call 2-4 {}, {}
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters
		;%4 = out paramaters

		method_lookup _super_class_%1_, %2
		%ifidn _map_%[_super_class_%1_]_type_%[_static_], static
			%ifidn _map_%[_super_class_%1_]_name_%[_static_], init
				;exeption for init
			%else
				%fatal Class _super_class_%1_ method %2 is static !
			%endif
		%endif
		eval {%3}, {_map_%[_super_class_%1_]_in_%[_static_]}
		fn_call _map_%[_super_class_%1_]_slot_%[_static_]
		retire {_map_%[_super_class_%1_]_out_%[_static_]}, {%4}
	%endmacro

	%macro super_jmp 2-4 {}
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters

		method_lookup _super_class_%1_, %2
		%ifidn _map_%[_super_class_%1_]_type_%[_static_], static
			%fatal Class _super_class_%1_ method %2 is static !
		%endif
		map_inputs %1, %2, {%3}, _static_
		fn_jmp _map_%[_super_class_%1_]_slot_%[_static_]
	%endmacro

	%macro p_jmp 2-4 {}
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters

		method_lookup _super_class_%1_, %2
		%ifidn _map_%[_super_class_%1_]_type_%[_static_], static
			%fatal Class _super_class_%1_ method %2 is static !
		%endif
		eval {%3}, {_map_%[_super_class_%1_]_in_%[_static_]}
		fn_jmp _map_%[_super_class_%1_]_slot_%[_static_]
	%endmacro

	%macro super_bind 3
		;%1 = class name
		;%2 = member name
		;%3 = reg

		method_lookup _super_class_%1_, %2
		%ifidn _map_%[_super_class_%1_]_type_%[_static_], static
			%fatal Class _super_class_%1_ method %2 is static !
		%endif
		fn_bind _map_%[_super_class_%1_]_slot_%[_static_], %3
	%endmacro

	%macro method_call 2-5 {}, {}, r15
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters
		;%4 = out paramaters
		;%5 = dispatch reg

		method_lookup %1, %2
		%ifidn _map_%1_type_%[_static_], static
			%fatal Class %1 method %2 is static ! Use static_call ?
		%endif
		map_inputs %1, %2, {%3}, _static_
		vp_cpy [r0 + obj_vtable], %5
		vp_call [%5 + _virtual_ * 8]
		map_outputs %1, %2, {%4}, _static_
	%endmacro

	%macro m_call 2-5 {}, {}, r15
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters
		;%4 = out paramaters
		;%5 = dispatch reg

		method_lookup %1, %2
		%ifidn _map_%1_type_%[_static_], static
			%fatal Class %1 method %2 is static ! Use static_call ?
		%endif
		eval {%5, %3}, {%5, _map_%1_in_%[_static_]}
		vp_cpy [r0 + obj_vtable], %5
		vp_call [%5 + _virtual_ * 8]
		retire {_map_%1_out_%[_static_]}, {%4}
	%endmacro

	%macro method_jmp 2-5 {}, r15
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters
		;%4 = dispatch reg

		method_lookup %1, %2
		%ifidn _map_%1_type_%[_static_], static
			%fatal Class %1 method %2 is static ! Use static_jmp ?
		%endif
		map_inputs %1, %2, {%3}, _static_
		vp_cpy [r0 + obj_vtable], %4
		vp_jmp [%4 + _virtual_ * 8]
	%endmacro

	%macro m_jmp 2-5 {}, r15
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters
		;%4 = dispatch reg

		method_lookup %1, %2
		%ifidn _map_%1_type_%[_static_], static
			%fatal Class %1 method %2 is static ! Use static_jmp ?
		%endif
		eval {%4, %3}, {%4, _map_%1_in_%[_static_]}
		vp_cpy [r0 + obj_vtable], %4
		vp_jmp [%4 + _virtual_ * 8]
	%endmacro

	%macro method_bind 2-3 r15
		;%1 = class name
		;%2 = member name
		;%3 = dispatch reg

		method_lookup %1, %2
		%ifidn _map_%1_type_%[_static_], static
			%fatal Class %1 method %2 is static ! Use static_bind ?
		%endif
		vp_cpy [r0 + obj_vtable], %3
		vp_cpy [%3 + _virtual_ * 8], %3
	%endmacro

	%macro slot_bind 3
		;%1 = class name
		;%2 = slot method name
		;%3 = reg

		method_lookup %1, %2
		fn_bind _map_%1_slot_%[_static_], %3
	%endmacro

	%macro slot_function 2-3 _function_
		;%1 = class name
		;%2 = slot method name
		;%3 = macro name

		method_lookup %1, %2
		%xdefine %3 _map_%1_slot_%[_static_]
	%endmacro

	%macro super_function 2-3 _function_
		;%1 = class name
		;%2 = slot method name
		;%3 = macro name

		method_lookup _super_class_%1_, %2
		%xdefine %3 _map_%[_super_class_%1_]_slot_%[_static_]
	%endmacro

;;;;;;;;;;;;;;;;;
; virtual classes
;;;;;;;;;;;;;;;;;

	def_class class
	def_method obj, class/class_obj
	def_method ref, class/class_ref
	def_method component, class/class_component
	def_method string, class/class_string
	def_method text, class/class_text
	def_method view, class/class_view
	def_method label, class/class_label
	def_method button, class/class_button
	def_method flow, class/class_flow
	def_method grid, class/class_grid
	def_method window, class/class_window
	def_method progress, class/class_progress
	def_method title, class/class_title

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generic class construction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	%macro class_macro_class 1
		;%1 = class name
		fn_function class/class_%1, no_debug_enter
			vtable_emit %1
		fn_function_end
	%endmacro

	%macro class_macro_create 1
		;%1 = class name
		fn_function class/%1/create
			;outputs
			;r0 = 0 if error, else object
			;trashes
			;r1-r3
			static_call %1, new, {}, {r0}
			if r0, !=, 0
				;init the object
				slot_function class, %1
				static_call %1, init, {r0, @_function_}, {r1}
				if r1, ==, 0
					;error with init
					method_call %1, delete, {r0}, {}, r1
					vp_xor r0, r0
				endif
			endif
			vp_ret
		fn_function_end
	%endmacro

	%macro class_macro_new 1
		;%1 = class name
		fn_function class/%1/new
			;outputs
			;r0 = 0 if error, else object
			;trashes
			;r1-r3
			static_call sys_mem, alloc, {%1_size}, {r0, _}
			if r0, !=, 0
				;clear object memory
				vp_cpy r0, r3
				static_call sys_mem, clear, {r0, %1_size}
				vp_cpy r3, r0
			endif
			vp_ret
		fn_function_end
	%endmacro

%endif

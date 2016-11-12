;;;;;;;;;;;;;;;;;;
; class definition
;;;;;;;;;;;;;;;;;;

%assign _map_null_slots_ 0

%macro vtable_emit 1-2
	;%1 = class name
	;%2 = parent class name
	%if %0 = 1
		dq 0
	%else
		fn_add_link %2
	%endif
	%assign %%n 0
	%rep _map_%1_slots_
		%ifidn _map_%1_type_%[%%n], virtual
			fn_add_link _map_%1_slot_%[%%n]
		%endif
		%assign %%n %%n + 1
	%endrep
%endmacro

%macro (def-class '1-2 null)
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

%macro (def-method '2-5 static '{} '{})
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
	%assign _virtual_ ptr_size
	%assign _static_ 0
	%rep _map_%1_slots_
		%ifidn _map_%1_name_%[_static_], %2
			%exitrep
		%elifidn _map_%1_type_%[_static_], virtual
			%assign _virtual_ _virtual_ + ptr_size
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

	(set-src %3)
	(set-dst _map_%1_in_%4)
	(map-src-to-dst)
%endmacro

%macro map_outputs 4
	;%1 = class name
	;%2 = member name
	;%3 = paramaters
	;%4 = slot num

	(set-src _map_%1_out_%4)
	(set-dst %3)
	(map-src-to-dst)
%endmacro

%macro func_path 2-3 _function_
	;%1 = class name
	;%2 = slot method name
	;%3 = macro name

	method_lookup %1, %2
	%xdefine %3 _map_%1_slot_%[_static_]
%endmacro

%macro super_path 2-3 _function_
	;%1 = class name
	;%2 = slot method name
	;%3 = macro name

	method_lookup _super_class_%1_, %2
	%xdefine %3 _map_%[_super_class_%1_]_slot_%[_static_]
%endmacro

;;;;;;;;;;;;;;;;;
; assembler level
;;;;;;;;;;;;;;;;;

%macro f_call 2-4 {}, {}
	;%1 = class name
	;%2 = member name
	;%3 = in paramaters
	;%4 = out paramaters

	method_lookup %1, %2
	%ifidn _map_%1_type_%[_static_], virtual
		%fatal Class %1 method %2 is virtual ! Use v_call ?
	%endif
	map_inputs %1, %2, {%3}, _static_
	fn_call _map_%1_slot_%[_static_]
	map_outputs %1, %2, {%4}, _static_
%endmacro

%macro f_jmp 2-4 {}
	;%1 = class name
	;%2 = member name
	;%3 = in paramaters

	method_lookup %1, %2
	%ifidn _map_%1_type_%[_static_], virtual
		%fatal Class %1 method %2 is virtual ! Use v_jmp ?
	%endif
	map_inputs %1, %2, {%3}, _static_
	fn_jmp _map_%1_slot_%[_static_]
%endmacro

%macro f_bind 3
	;%1 = class name
	;%2 = member name
	;%3 = reg

	method_lookup %1, %2
	%ifidn _map_%1_type_%[_static_], virtual
		%fatal Class %1 method %2 is virtual ! Use v_bind ?
	%endif
	fn_bind _map_%1_slot_%[_static_], %3
%endmacro

%macro s_call 2-4 {}, {}
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
	map_inputs _super_class_%1_, %2, {%3}, _static_
	fn_call _map_%[_super_class_%1_]_slot_%[_static_]
	map_outputs _super_class_%1_, %2, {%4}, _static_
%endmacro

%macro s_jmp 2-4 {}
	;%1 = class name
	;%2 = member name
	;%3 = in paramaters

	method_lookup _super_class_%1_, %2
	%ifidn _map_%[_super_class_%1_]_type_%[_static_], static
		%ifidn _map_%[_super_class_%1_]_name_%[_static_], init
			;exeption for init
		%else
			%fatal Class _super_class_%1_ method %2 is static !
		%endif
	%endif
	map_inputs _super_class_%1_, %2, {%3}, _static_
	fn_jmp _map_%[_super_class_%1_]_slot_%[_static_]
%endmacro

%macro s_bind 3
	;%1 = class name
	;%2 = member name
	;%3 = reg

	method_lookup _super_class_%1_, %2
	%ifidn _map_%[_super_class_%1_]_type_%[_static_], static
		%fatal Class _super_class_%1_ method %2 is static !
	%endif
	fn_bind _map_%[_super_class_%1_]_slot_%[_static_], %3
%endmacro

%macro v_call 2-5 {}, {}, r15
	;%1 = class name
	;%2 = member name
	;%3 = in paramaters
	;%4 = out paramaters
	;%5 = dispatch reg

	method_lookup %1, %2
	%ifidn _map_%1_type_%[_static_], static
		%fatal Class %1 method %2 is static ! Use f_call ?
	%endif
	map_inputs %1, %2, {%3}, _static_
	vp_cpy [r0 + obj_vtable], %5
	vp_call [%5 + _virtual_]
	map_outputs %1, %2, {%4}, _static_
%endmacro

%macro v_jmp 2-5 {}, r15
	;%1 = class name
	;%2 = member name
	;%3 = in paramaters
	;%4 = dispatch reg

	method_lookup %1, %2
	%ifidn _map_%1_type_%[_static_], static
		%fatal Class %1 method %2 is static ! Use f_jmp ?
	%endif
	map_inputs %1, %2, {%3}, _static_
	vp_cpy [r0 + obj_vtable], %4
	vp_jmp [%4 + _virtual_]
%endmacro

%macro v_bind 2-3 r15
	;%1 = class name
	;%2 = member name
	;%3 = dispatch reg

	method_lookup %1, %2
	%ifidn _map_%1_type_%[_static_], static
		%fatal Class %1 method %2 is static ! Use f_bind ?
	%endif
	vp_cpy [r0 + obj_vtable], %3
	vp_cpy [%3 + _virtual_], %3
%endmacro

%macro d_call 2-4 {}, {}
	;%1 = class name
	;%2 = member name
	;%3 = in paramaters
	;%4 = out paramaters

	method_lookup %1, %2
	%ifidn _map_%1_type_%[_static_], static
		%fatal Class %1 method %2 is static ! Use f_call ?
	%endif
	map_inputs %1, %2, {%3}, _static_
	fn_call _map_%1_slot_%[_static_]
	map_outputs %1, %2, {%4}, _static_
%endmacro

%macro d_jmp 2-4 {}
	;%1 = class name
	;%2 = member name
	;%3 = in paramaters

	method_lookup %1, %2
	%ifidn _map_%1_type_%[_static_], static
		%fatal Class %1 method %2 is static ! Use f_jmp ?
	%endif
	map_inputs %1, %2, {%3}, _static_
	fn_jmp _map_%1_slot_%[_static_]
%endmacro

%macro d_bind 3
	;%1 = class name
	;%2 = slot method name
	;%3 = reg

	method_lookup %1, %2
	%ifidn _map_%1_type_%[_static_], static
		%fatal Class %1 method %2 is static ! Use f_bind ?
	%endif
	fn_bind _map_%1_slot_%[_static_], %3
%endmacro

;;;;;;;;;;;;;;
; script level
;;;;;;;;;;;;;;

%macro func_call 2-4 {}, {}
	;%1 = class name
	;%2 = member name
	;%3 = in paramaters
	;%4 = out paramaters

	method_lookup %1, %2
	%ifidn _map_%1_type_%[_static_], virtual
		%fatal Class %1 method %2 is virtual ! Use virt_call ?
	%endif
	eval {%3}, {_map_%1_in_%[_static_]}
	fn_call _map_%1_slot_%[_static_]
	retire {_map_%1_out_%[_static_]}, {%4}
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
	eval {%3}, {_map_%[_super_class_%1_]_in_%[_static_]}
	fn_call _map_%[_super_class_%1_]_slot_%[_static_]
	retire {_map_%[_super_class_%1_]_out_%[_static_]}, {%4}
%endmacro

%macro virt_call 2-5 {}, {}, r15
	;%1 = class name
	;%2 = member name
	;%3 = in paramaters
	;%4 = out paramaters
	;%5 = dispatch reg

	method_lookup %1, %2
	%ifidn _map_%1_type_%[_static_], static
		%fatal Class %1 method %2 is static ! Use func_call ?
	%endif
	eval {RXX, %3}, {%5, _map_%1_in_%[_static_]}
	vp_cpy [r0 + obj_vtable], %5
	vp_call [%5 + _virtual_]
	retire {_map_%1_out_%[_static_]}, {%4}
%endmacro

%macro devirt_call 2-4 {}, {}
	;%1 = class name
	;%2 = member name
	;%3 = in paramaters
	;%4 = out paramaters

	method_lookup %1, %2
	%ifidn _map_%1_type_%[_static_], static
		%fatal Class %1 method %2 is static ! Use func_call ?
	%endif
	eval {%3}, {_map_%1_in_%[_static_]}
	fn_call _map_%1_slot_%[_static_]
	retire {_map_%1_out_%[_static_]}, {%4}
%endmacro

%macro local_call 1-5 {}, {}, {}, {}
	;%1 = label
	;%2 = in paramaters
	;%3 = in regs
	;%4 = out paramaters
	;%5 = out regs

	eval {%2}, {%3}
	vp_call %1
	retire {%4}, {%5}
%endmacro

;;;;;;;;;;;;;;;;;
; virtual classes
;;;;;;;;;;;;;;;;;

(def-class 'class)
(def-method 'obj 'class/class_obj)
(def-method 'ref 'class/class_ref)
(def-method 'component 'class/class_component)
(def-method 'string 'class/class_string)
(def-method 'vector 'class/class_vector)
(def-method 'pair 'class/class_pair)
(def-method 'unordered_set 'class/class_unordered_set)
(def-method 'unordered_map 'class/class_unordered_map)
(def-method 'stream 'class/class_stream)
(def-method 'stream_str 'class/class_stream_str)
(def-method 'stream_msg_out 'class/class_stream_msg_out)
(def-method 'stream_msg_in 'class/class_stream_msg_in)
(def-method 'text 'class/class_text)
(def-method 'view 'class/class_view)
(def-method 'label 'class/class_label)
(def-method 'button 'class/class_button)
(def-method 'flow 'class/class_flow)
(def-method 'grid 'class/class_grid)
(def-method 'window 'class/class_window)
(def-method 'progress 'class/class_progress)
(def-method 'title 'class/class_title)
(def-method 'slave 'class/class_slave)
(def-method 'master 'class/class_master)
(def-method 'boxed_ptr 'class/class_boxed_ptr)
(def-method 'boxed_long 'class/class_boxed_long)
(def-method 'lisp 'class/class_lisp)
(def-method 'symbol 'class/class_symbol)
(def-method 'sequence 'class/class_sequence)
(def-method 'error 'class/class_error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generic class construction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%macro class_macro_class 1
	;%1 = class name
	func_path class, %1
	def_func _function_
		%ifidn _super_class_%1_, null
			vtable_emit %1
		%else
			func_path class, _super_class_%1_
			vtable_emit %1, _function_
		%endif
	def_func_end
%endmacro

%macro class_macro_create 1
	;%1 = class name
	def_func class/%1/create
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3
		f_call %1, new, {}, {r0}
		if r0, !=, 0
			;init the object
			func_path class, %1
			f_call %1, init, {r0, @_function_}, {r1}
			if r1, ==, 0
				;error with init
				v_call %1, delete, {r0}, {}, r1
				(vp-xor-rr r0 r0)
			endif
		endif
		(vp-ret)
	def_func_end
%endmacro

%macro class_macro_new 1
	;%1 = class name
	def_func class/%1/new
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3
		f_call sys_mem, alloc, {%1_size}, {r0, _}
		if r0, !=, 0
			;clear object memory
			(vp-cpy-rr r0 r3)
			f_call sys_mem, clear, {r0, %1_size}, {_}
			(vp-cpy-rr r3 r0)
		endif
		(vp-ret)
	def_func_end
%endmacro

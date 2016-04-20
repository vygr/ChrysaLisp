%ifndef CLASS_1234
%define CLASS_1234

	%assign _map_null_slots_ 0

	%macro vtable_emit 1
		;%1 = class name
		%assign %%_n 0
		%rep _map_%[%1]_slots_
			%ifidn _map_%[%1]_type_%[%%_n], virtual
				fn_add_link _map_%[%1]_slot_%[%%_n]
			%endif
			%assign %%_n %%_n + 1
		%endrep
	%endmacro

	%macro class_add 2
		;%1 = dest class name
		;%2 = source class name
		%assign %%_n 0
		%rep _map_%[%2]_slots_
			def_method	_map_%[%2]_name_%[%%_n], \
						_map_%[%2]_slot_%[%%_n], \
						_map_%[%2]_type_%[%%_n], \
						_map_%[%2]_in_%[%%_n], \
						_map_%[%2]_out_%[%%_n]
			%assign %%_n %%_n + 1
		%endrep
	%endmacro

	%macro def_class 1-2 null
		;%1 = class name
		;%2 = super class name
		%ifdef _map_%[%1]_slots_
			%fatal Class %1 allready defined !
		%endif
		%ifndef _map_%[%2]_slots_
			%fatal Class %2 not defined !
		%endif
		%assign _map_%[%1]_slots_ 0
		%define _class_ %1
		%define _super_class_%[%1]_ %2
		class_add %1, %2
	%endmacro

	%macro def_method 2-5 nonvirtual, '', ''
		;%1 = member name
		;%2 = member function
		;%3 = mode (nonvirtual/virtual/override)
		;%4 = in regs
		;%5 = out regs
		%assign %%_n 0
		%rep _map_%[_class_]_slots_
			%ifidn _map_%[_class_]_name_%[%%_n], %1
				%exitrep
			%else
				%assign %%_n %%_n + 1
			%endif
		%endrep
		%if %%_n = _map_%[_class_]_slots_
			;new method
			%ifidn %3, nonvirtual
				%assign _map_%[_class_]_slots_ %%_n + 1
				%define _map_%[_class_]_name_%[%%_n] %1
				%define _map_%[_class_]_slot_%[%%_n] %2
				%define _map_%[_class_]_type_%[%%_n] %3
				%define _map_%[_class_]_in_%[%%_n] %4
				%define _map_%[_class_]_out_%[%%_n] %5
			%elifidn %3, virtual
				%assign _map_%[_class_]_slots_ %%_n + 1
				%define _map_%[_class_]_name_%[%%_n] %1
				%define _map_%[_class_]_slot_%[%%_n] %2
				%define _map_%[_class_]_type_%[%%_n] %3
				%define _map_%[_class_]_in_%[%%_n] %4
				%define _map_%[_class_]_out_%[%%_n] %5
			%elifidn %3, override
				%fatal Method %1 does not exist !
			%else
				%fatal Must use nonvirtual, virtual or override !
			%endif
		%else
			;existing method
			%ifidn %3, nonvirtual
				%ifidn _map_%[_class_]_type_%[%%_n], nonvirtual
					%define _map_%[_class_]_slot_%[%%_n] %2
					%ifnidn %4, ''
						%define _map_%[_class_]_in_%[%%_n] %4
					%endif
					%ifnidn %5, ''
						%define _map_%[_class_]_out_%[%%_n] %5
					%endif
				%else
					%fatal Virtual method %1 allready exists !
				%endif
			%elifidn %3, virtual
				%fatal Method %1 allready exists !, use override ?
			%elifidn %3, override
				%ifidn _map_%[_class_]_type_%[%%_n], virtual
					%define _map_%[_class_]_slot_%[%%_n] %2
					%ifnidn %4, ''
						%define _map_%[_class_]_in_%[%%_n] %4
					%endif
					%ifnidn %5, ''
						%define _map_%[_class_]_out_%[%%_n] %5
					%endif
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
		%ifndef _map_%[%1]_slots_
			%fatal Class %1 not defined !
		%endif
		%assign _virtual_ 0
		%assign _nonvirtual_ 0
		%rep _map_%[%1]_slots_
			%ifidn _map_%[%1]_name_%[_nonvirtual_], %2
				%exitrep
			%elifidn _map_%[%1]_type_%[_nonvirtual_], virtual
				%assign _virtual_ _virtual_ + 1
				%assign _nonvirtual_ _nonvirtual_ + 1
			%else
				%assign _nonvirtual_ _nonvirtual_ + 1
			%endif
		%endrep
		%if _nonvirtual_ = _map_%[%1]_slots_
			%fatal No such method %2 in class %1 !
		%endif
	%endmacro

	%macro set_src 1-*
		;%1... = paramaters
		%assign _src_cnt 0
		%rep %0
			%define _src_%[_src_cnt] %1
			%assign _src_cnt _src_cnt + 1
			%rotate 1
		%endrep
	%endmacro

	%macro set_dst 1-*
		;%1... = paramaters
		%assign _dst_cnt 0
		%rep %0
			%define _dst_%[_dst_cnt] %1
			%assign _dst_cnt _dst_cnt + 1
			%rotate 1
		%endrep
	%endmacro

	%macro map_print 0
		%warning src => dst
		%assign %%i 0
		%rep _src_cnt
			%warning map entry %%i: _src_%[%%i] => _dst_%[%%i]
			%assign %%i %%i + 1
		%endrep
	%endmacro

	%macro map_rotate 2
		;%1 = dst index
		;%2 = src index
		%assign %%i 0
		%rep _src_cnt
			%xdefine %%s_%[%%i] _src_%[%%i]
			%xdefine %%d_%[%%i] _dst_%[%%i]
			%assign %%i %%i + 1
		%endrep
		%assign %%j %2
		%rep %2 - %1
			%assign %%i %%j - 1
			%define _src_%[%%j] %%s_%[%%i]
			%define _dst_%[%%j] %%d_%[%%i]
			%assign %%j %%i
		%endrep
		%define _src_%[%1] %%s_%[%2]
		%define _dst_%[%1] %%d_%[%2]
	%endmacro

	%macro find_src 1
		;%1 = param to find
		%assign _idx 0
		%rep _src_cnt
			%ifidn _src_%[_idx], %1
				%exitrep
			%else
				%assign _idx _idx + 1
			%endif
		%endrep
	%endmacro

	%macro map_sort 0
		%assign %%c 1000
		%rep %%c
			%assign %%f 0
			%assign %%d 0
			%rep _dst_cnt
				find_src _dst_%[%%d]
				%if _idx != _src_cnt
					%if %%d < _idx
						map_rotate %%d, _idx
						%assign %%f 1
						%exitrep
					%endif
				%else
					%assign %%d %%d + 1
				%endif
			%endrep
			%if %%f = 0
				%exitrep
			%else
				%assign %%c %%c - 1
			%endif
		%endrep
		%if %%c = 0
			%fatal Copy cycle detected !
		%endif
	%endmacro

	%macro map_src_to_dst 0
		%if _dst_cnt != _src_cnt
			%fatal Mismatching number of src/dst paramaters !
		%else
			map_sort
			%assign %%i 0
			%rep _src_cnt
				;%warning _src_%[%%i] => _dst_%[%%i]
				vp_cpy _src_%[%%i], _dst_%[%%i]
				%assign %%i %%i + 1
			%endrep
		%endif
	%endmacro

	%macro map_inputs 4
		;%1 = class name
		;%2 = member name
		;%3 = paramaters
		;%4 = slot num

		%ifnidn %3, ''
			%ifidn _map_%1_in_%4, ''
				%fatal Class %1 method %2 inputs not defined !
			%else
				%deftok %%s %3
				%deftok %%d _map_%1_in_%4
				set_src %%s
				set_dst %%d
				map_src_to_dst
			%endif
		%endif
	%endmacro

	%macro map_outputs 4
		;%1 = class name
		;%2 = member name
		;%3 = paramaters
		;%4 = slot num

		%ifnidn %3, ''
			%ifidn _map_%1_out_%4, ''
				%fatal Class %1 method %2 outputs not defined !
			%else
				%deftok %%s _map_%1_out_%4
				%deftok %%d %3
				set_src %%s
				set_dst %%d
				map_src_to_dst
			%endif
		%endif
	%endmacro

	%macro static_call 2-4 '', ''
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters
		;%4 = out paramaters

		method_lookup %1, %2
		%ifidn _map_%[%1]_type_%[_nonvirtual_], virtual
			%fatal Class %1 method %2 is virtual ! Use method_call ?
		%endif
		map_inputs %1, %2, %3, _nonvirtual_
		fn_call _map_%[%1]_slot_%[_nonvirtual_]
		map_outputs %1, %2, %4, _nonvirtual_
	%endmacro

	%macro static_jmp 2-4 '', ''
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters
		;%4 = out paramaters

		method_lookup %1, %2
		%ifidn _map_%[%1]_type_%[_nonvirtual_], virtual
			%fatal Class %1 method %2 is virtual ! Use method_jmp ?
		%endif
		map_inputs %1, %2, %3, _nonvirtual_
		fn_jmp _map_%[%1]_slot_%[_nonvirtual_]
	%endmacro

	%macro static_bind 3
		;%1 = class name
		;%2 = member name
		;%3 = reg

		method_lookup %1, %2
		%ifidn _map_%[%1]_type_%[_nonvirtual_], virtual
			%fatal Class %1 method %2 is virtual ! Use method_bind ?
		%endif
		fn_bind _map_%[%1]_slot_%[_nonvirtual_], %3
	%endmacro

	%macro super_call 2-4 '', ''
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters
		;%4 = out paramaters

		method_lookup _super_class_%[%1]_, %2
		%ifidn _map_%[_super_class_%[%1]_]_type_%[_nonvirtual_], nonvirtual
			%ifidn _map_%[_super_class_%[%1]_]_name_%[_nonvirtual_], init
				;exeption for init
			%else
				%fatal Class _super_class_%[%1]_ method %2 is nonvirtual !
			%endif
		%endif
		map_inputs %1, %2, %3, _nonvirtual_
		fn_call _map_%[_super_class_%[%1]_]_slot_%[_nonvirtual_]
		map_outputs %1, %2, %4, _nonvirtual_
	%endmacro

	%macro super_jmp 2-4 '', ''
		;%1 = class name
		;%2 = member name
		;%3 = in paramaters
		;%4 = out paramaters

		method_lookup _super_class_%[%1]_, %2
		%ifidn _map_%[_super_class_%[%1]_]_type_%[_nonvirtual_], nonvirtual
			%fatal Class _super_class_%[%1]_ method %2 is nonvirtual !
		%endif
		map_inputs %1, %2, %3, _nonvirtual_
		fn_jmp _map_%[_super_class_%[%1]_]_slot_%[_nonvirtual_]
	%endmacro

	%macro super_bind 3
		;%1 = class name
		;%2 = member name
		;%3 = reg

		method_lookup _super_class_%[%1]_, %2
		%ifidn _map_%[_super_class_%[%1]_]_type_%[_nonvirtual_], nonvirtual
			%fatal Class _super_class_%[%1]_ method %2 is nonvirtual !
		%endif
		fn_bind _map_%[_super_class_%[%1]_]_slot_%[_nonvirtual_], %3
	%endmacro

	%macro method_call 2-3 r15
		;%1 = class name
		;%2 = member name
		;%3 = dispatch reg

		method_lookup %1, %2
		%ifidn _map_%[%1]_type_%[_nonvirtual_], nonvirtual
			%fatal Class %1 method %2 is nonvirtual ! Use static_call ?
		%endif
		vp_cpy [r0 + obj_vtable], %3
		vp_call [%3 + _virtual_ * 8]
	%endmacro

	%macro method_jmp 2-3 r15
		;%1 = class name
		;%2 = member name
		;%3 = dispatch reg

		method_lookup %1, %2
		%ifidn _map_%[%1]_type_%[_nonvirtual_], nonvirtual
			%fatal Class %1 method %2 is nonvirtual ! Use static_jmp ?
		%endif
		vp_cpy [r0 + obj_vtable], %3
		vp_jmp [%3 + _virtual_ * 8]
	%endmacro

	%macro method_bind 2-3 r15
		;%1 = class name
		;%2 = member name
		;%3 = dispatch reg

		method_lookup %1, %2
		%ifidn _map_%[%1]_type_%[_nonvirtual_], nonvirtual
			%fatal Class %1 method %2 is nonvirtual ! Use static_bind ?
		%endif
		vp_cpy [r0 + obj_vtable], %3
		vp_cpy [%3 + _virtual_ * 8], %3
	%endmacro

	%macro slot_bind 3
		;%1 = class name
		;%2 = slot method name
		;%3 = reg

		method_lookup %1, %2
		fn_bind _map_%[%1]_slot_%[_nonvirtual_], %3
	%endmacro

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

%endif

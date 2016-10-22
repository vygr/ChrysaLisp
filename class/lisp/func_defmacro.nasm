%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_defmacro
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = value

		ptr this, args, vars, name
		ulong length

		push_scope
		retire {r0, r1}, {this, args}

		slot_call vector, get_length, {args}, {length}
		if {length == 4}
			static_call vector, get_element, {args, 2}, {vars}
			if {vars->obj_vtable == @class/class_vector}
				static_call vector, get_element, {args, 1}, {name}
				if {name->obj_vtable == @class/class_symbol}
					static_call vector, slice, {args, 2, length}, {args}
					static_call unordered_map, insert, {this->lisp_macros, name, args}, {_, _}
					static_call ref, deref, {args}
					static_call ref, ref, {name}
				else
					static_call error, create, {"(defmacro name vars body) name is not a symbol", args}, {name}
				endif
			else
				static_call error, create, {"(defmacro name vars body) vars is not a list", args}, {name}
			endif
		else
			static_call error, create, {"(defmacro name vars body) wrong numbers of args", args}, {name}
		endif

		eval {this, name}, {r0, r1}
		pop_scope
		return

	def_function_end

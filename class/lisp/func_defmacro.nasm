%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_func class/lisp/func_defmacro
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

		devirt_call vector, get_length, {args}, {length}
		if {length == 3}
			func_call vector, get_element, {args, 1}, {vars}
			if {vars->obj_vtable == @class/class_vector}
				func_call vector, get_element, {args, 0}, {name}
				if {name->obj_vtable == @class/class_symbol}
					devirt_call vector, slice, {args, 1, length}, {args}
					func_call unordered_map, insert, {this->lisp_macros, name, args}, {_, _}
					func_call ref, deref, {args}
					func_call ref, ref, {name}
				else
					func_call error, create, {"(defmacro name vars body) name is not a symbol", args}, {name}
				endif
			else
				func_call error, create, {"(defmacro name vars body) vars is not a list", args}, {name}
			endif
		else
			func_call error, create, {"(defmacro name vars body) wrong numbers of args", args}, {name}
		endif

		eval {this, name}, {r0, r1}
		pop_scope
		return

	def_func_end

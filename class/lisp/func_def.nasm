%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_unordered_map.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_def
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args, var, val, env
	uint length, index

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	vpif {length >= 3 && (length & 1)}
		func_call vector, get_element, {args, 0}, {env}
		vpif {env->obj_vtable == @class/class_unordered_map}
			assign {1, 0}, {index, val}
			loop_start
				func_call ref, deref_if, {val}
				func_call vector, get_element, {args, index}, {var}
				devirt_call vector, ref_element, {args, index + 1}, {val}
				func_call unordered_map, insert, {env, var, val}, {_, _}
				assign {index + 2}, {index}
			loop_until {index == length}
		else
			func_call error, create, {"(def env var val ...) not an enviroment", args}, {val}
		endif
	else
		func_call error, create, {"(def env var val ...) wrong number of args", args}, {val}
	endif

	expr {this, val}, {r0, r1}
	pop_scope
	return

def_func_end

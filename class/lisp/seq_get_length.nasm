%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/seq_get_length
		;inputs
		;r0 = lisp object
		;r1 = seq
		;outputs
		;r0 = lisp object
		;r1 = length

		ptr this, seq, type
		ulong length

		push_scope
		retire {r0, r1}, {this, seq}

		assign {seq->obj_vtable}, {type}
		switch
		case {type == @class/class_vector}
			static_call vector, get_length, {seq}, {length}
			break
		case {type == @class/class_string \
			|| type == @class/class_symbol}
			static_call string, get_length, {seq}, {length}
			break
		case {type == @class/class_unordered_set \
			|| type == @class/class_unordered_map}
			static_call unordered_set, get_length, {seq}, {length}
			break
		default
			abort "seq_get_length: bad sequenece type"
		endswitch

		eval {this, length}, {r0, r1}
		pop_scope
		return

	def_function_end

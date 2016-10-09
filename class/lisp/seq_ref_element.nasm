%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/seq_ref_element
		;inputs
		;r0 = lisp object
		;r1 = seq
		;r2 = index
		;outputs
		;r0 = lisp object
		;r1 = elem

		ptr this, seq, type, elem
		ulong index

		push_scope
		retire {r0, r1, r2}, {this, seq, index}

		assign {seq->obj_vtable}, {type}
		switch
		case {type == @class/class_vector}
			static_call vector, ref_element, {seq, index}, {elem}
			break
		case {type == @class/class_string \
			|| type == @class/class_symbol}
			static_call string, ref_element, {seq, index}, {elem}
			break
		case {type == @class/class_unordered_set \
			|| type == @class/class_unordered_map}
			static_call unordered_set, ref_element, {seq, index}, {elem}
			break
		default
			abort "seq_ref_element: bad sequenece type"
		endswitch

		eval {this, elem}, {r0, r1}
		pop_scope
		return

	def_function_end

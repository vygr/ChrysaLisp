%include 'inc/func.inc'
%include 'class/class_obj.inc'

	def_function class/lisp/seq_is_seq
		;inputs
		;r0 = lisp object
		;r1 = seq
		;outputs
		;r0 = lisp object
		;r1 = 0, else seq vtable

		ptr this, seq, type

		push_scope
		retire {r0, r1}, {this, seq}

		assign {seq->obj_vtable}, {type}
		switch
		breakif {type == @class/class_vector}
		breakif {type == @class/class_string}
		breakif {type == @class/class_symbol}
		breakif {type == @class/class_unordered_set}
		breakif {type == @class/class_unordered_map}
		default
			assign {0}, {type}
		endswitch

		eval {this, type}, {r0, r1}
		pop_scope
		return

	def_function_end

%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'
%include 'class/class_stream.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_prin
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = value

		ptr this, args

		push_scope
		retire {r0, r1}, {this, args}

		static_call vector, for_each, {args, 0, $callback, this}, {_}
		assign {this->lisp_sym_t}, {args}
		static_call ref, ref, {args}

		eval {this, args}, {r0, r1}
		pop_scope
		return

	callback:
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr pdata, elem

		push_scope
		retire {r0, r1}, {pdata, iter}

		assign {*iter}, {elem}
		if {elem->obj_vtable == @class/class_string}
			static_call stream, write, {pdata->lisp_stdout, &elem->string_data, elem->string_length}
		else
			static_call lisp, repl_print, {pdata, pdata->lisp_stdout, elem}
		endif

		eval {1}, {r1}
		pop_scope
		return

	def_function_end

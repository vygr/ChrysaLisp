%include 'inc/func.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_vector.inc'
%include 'class/class_pair.inc'
%include 'class/class_string.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_env
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = enviroment list, with _parent_ filtered out

		def_structure pdata
			ptr pdata_this
			ptr pdata_env
		def_structure_end

		struct pdata, pdata
		ptr args, env, value
		pptr iter

		push_scope
		retire {r0, r1}, {pdata.pdata_this, args}

		assign {pdata.pdata_this->lisp_enviroment}, {env}
		static_call vector, create, {}, {value}
		loop_start
			slot_function string, compare
			static_call unordered_map, create, {@_function_, 1}, {pdata.pdata_env}
			static_call unordered_map, for_each, {env, $callback, &pdata}, {_, _}
			static_call vector, push_back, {value, pdata.pdata_env}
			static_call unordered_map, find, {env, pdata.pdata_this->lisp_sym_parent}, {iter, _}
			breakifnot {iter}
			static_call pair, get_second, {*iter}, {env}
		loop_end

		eval {pdata.pdata_this, value}, {r0, r1}
		pop_scope
		return

	callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr pdata, first, second

		push_scope
		retire {r0, r1}, {iter, pdata}

		static_call pair, get_first, {*iter}, {first}
		if {first != pdata->pdata_this->lisp_sym_parent}
			static_call pair, get_second, {*iter}, {second}
			static_call unordered_map, insert, {pdata->pdata_env, first, second}, {_, _}
		endif
		eval {1}, r1

		pop_scope
		return

	def_function_end

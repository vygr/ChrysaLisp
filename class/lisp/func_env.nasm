%include 'inc/func.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_vector.inc'
%include 'class/class_pair.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_env
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = enviroment list, with _parent_ filtered out

		ptr this, args, env, value, bucket
		pptr iter

		push_scope
		retire {r0, r1}, {this, args}

		assign {this->lisp_enviroment}, {env}
		static_call vector, create, {}, {value}
		loop_start
			static_call unordered_map, copy, {env, 1}, {env}
			static_call vector, push_back, {value, env}
			static_call unordered_map, find, {env, this->lisp_sym_parent}, {iter, bucket}
			breakifnot {iter}
			static_call pair, get_second, {*iter}, {args}
			static_call unordered_map, erase, {env, iter, bucket}
			assign {args}, {env}
		loop_end

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end

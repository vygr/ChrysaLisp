%include 'inc/func.inc'
%include 'class/class_string.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_pair.inc'
%include 'class/class_slave.inc'
%include 'class/class_boxed_ptr.inc'
%include 'class/class_boxed_long.inc'
%include 'inc/string.inc'

	def_class lisp
	def_method error, cmd/lisp/error, static, {r0, r1}
	def_method sym_intern, cmd/lisp/sym_intern, static, {r0, r1}, {r0}
	def_method sym_intern_cstr, cmd/lisp/sym_intern_cstr, static, {r0, r1}, {r0}
	def_method built_in_func, cmd/lisp/built_in_func, static, {r0, r1, r2}
	def_method env_set, cmd/lisp/env_set, static, {r0, r1, r2}
	def_method env_get, cmd/lisp/env_get, static, {r0, r1}, {r0}
	def_method env_find, cmd/lisp/env_find, static, {r0, r1}, {r0, r1}
	def_method env_set_list, cmd/lisp/env_set_list, static, {r0, r1, r2}, {r0}
	def_method env_push, cmd/lisp/env_push, static, {r0}
	def_method env_pop, cmd/lisp/env_pop, static, {r0}
	def_method repl_eval, cmd/lisp/repl_eval, static, {r0, r1}, {r0}
	def_method repl_eval_list, cmd/lisp/repl_eval_list, static, {r0, r1}, {r0}

	def_method def, cmd/lisp/def, static, {r0, r1}, {r0}
	def_method list, cmd/lisp/list, static, {r0, r1}, {r0}
	def_method quote, cmd/lisp/quote, static, {r0, r1}, {r0}

	def_structure lisp
		ptr lisp_symbols
		ptr lisp_enviroment
		ptr lisp_slave

		;same order as built_in_symbols table !
		ptr lisp_sym_parent
		ptr lisp_sym_nil
		ptr lisp_sym_t
		ptr lisp_sym_lambda
		ptr lisp_sym_def
		ptr lisp_sym_quote
		ptr lisp_sym_list
	def_structure_end

%ifndef CLASS_LISP_1234
%define CLASS_LISP_1234

%include 'class/class_ref.inc'

;%xdefine debug_lines

	def_class lisp, ref
	def_method create, cmd/lisp/create, static, {r0, r1, r2}, {r0}
	def_method new, cmd/lisp/new, static, {}, {r0}
	def_method init, cmd/lisp/init, static, {r0, r1, r2, r3, r5}, {r1}
	def_method error, cmd/lisp/error, static, {r0, r1, r2}
	def_method sym_intern, cmd/lisp/sym_intern, static, {r0, r1}, {r1}
	def_method sym_intern_cstr, cmd/lisp/sym_intern_cstr, static, {r0, r1}, {r1}
	def_method built_in_func, cmd/lisp/built_in_func, static, {r0, r1, r2}
	def_method env_set, cmd/lisp/env_set, static, {r0, r1, r2}
	def_method env_get, cmd/lisp/env_get, static, {r0, r1}, {r1}
	def_method env_find, cmd/lisp/env_find, static, {r0, r1}, {r1, r2}
	def_method env_set_list, cmd/lisp/env_set_list, static, {r0, r1, r2}, {r1}
	def_method env_push, cmd/lisp/env_push, static, {r0}
	def_method env_pop, cmd/lisp/env_pop, static, {r0}
	def_method repl, cmd/lisp/repl, static, {r0, r1}
	def_method repl_read, cmd/lisp/repl_read, static, {r0, r1, r2}, {r1, r2}
	def_method repl_read_list, cmd/lisp/repl_read_list, static, {r0, r1}, {r1}
	def_method repl_read_sym, cmd/lisp/repl_read_sym, static, {r0, r1, r2}, {r1, r2}
	def_method repl_read_num, cmd/lisp/repl_read_num, static, {r0, r1, r2}, {r1, r2}
	def_method repl_apply, cmd/lisp/repl_apply, static, {r0, r1}, {r1}
	def_method repl_eval, cmd/lisp/repl_eval, static, {r0, r1}, {r1}
	def_method repl_eval_list, cmd/lisp/repl_eval_list, static, {r0, r1}, {r1}
	def_method repl_print, cmd/lisp/repl_print, static, {r0, r1, r2}
	def_method func_def, cmd/lisp/func_def, static, {r0, r1}, {r1}
	def_method func_list, cmd/lisp/func_list, static, {r0, r1}, {r1}
	def_method func_lambda, cmd/lisp/func_lambda, static, {r0, r1}, {r1}
	def_method func_quote, cmd/lisp/func_quote, static, {r0, r1}, {r1}
	def_method func_add, cmd/lisp/func_add, static, {r0, r1}, {r1}
	def_method func_sub, cmd/lisp/func_sub, static, {r0, r1}, {r1}
	def_method func_mul, cmd/lisp/func_mul, static, {r0, r1}, {r1}
	def_method func_div, cmd/lisp/func_div, static, {r0, r1}, {r1}
	def_method func_mod, cmd/lisp/func_mod, static, {r0, r1}, {r1}
	def_method func_eq, cmd/lisp/func_eq, static, {r0, r1}, {r1}
	def_method func_cond, cmd/lisp/func_cond, static, {r0, r1}, {r1}

	def_method deinit, cmd/lisp/deinit, override

	def_structure lisp, ref
		ptr lisp_stdin
		ptr lisp_stdout
		ptr lisp_stderr
		ptr lisp_symbols
		ptr lisp_enviroment

		;same order as built_in_symbols table !
		ptr lisp_sym_parent
		ptr lisp_sym_nil
		ptr lisp_sym_t
		ptr lisp_sym_lambda
		ptr lisp_sym_def
		ptr lisp_sym_quote
		ptr lisp_sym_list
		ptr lisp_sym_add
		ptr lisp_sym_sub
		ptr lisp_sym_mul
		ptr lisp_sym_div
		ptr lisp_sym_mod
		ptr lisp_sym_eq
		ptr lisp_sym_cond
	def_structure_end

%endif

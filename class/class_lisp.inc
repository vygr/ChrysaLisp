%ifndef CLASS_LISP_1234
%define CLASS_LISP_1234

%include 'class/class_ref.inc'

;%xdefine debug_lines

def_class lisp, ref
def_method create, class/lisp/create, static, {r0, r1, r2}, {r0}
def_method new, class/lisp/new, static, {}, {r0}
def_method init, class/lisp/init, static, {r0, r1, r2, r3, r5}, {r1}
def_method deinit, class/lisp/deinit, override

def_method sym_intern, class/lisp/sym_intern, static, {r0, r1}, {r1}
def_method sym_intern_cstr, class/lisp/sym_intern_cstr, static, {r0, r1}, {r1}
def_method built_in_func, class/lisp/built_in_func, static, {r0, r1, r2, r3}

def_method env_push, class/lisp/env_push, static, {r0}
def_method env_pop, class/lisp/env_pop, static, {r0}
def_method env_find, class/lisp/env_find, static, {r0, r1}, {r1, r2}
def_method env_set, class/lisp/env_set, static, {r0, r1, r2}, {r1}
def_method env_get, class/lisp/env_get, static, {r0, r1}, {r1}
def_method env_bind, class/lisp/env_bind, static, {r0, r1, r2, r3}, {r1}

def_method repl_read, class/lisp/repl_read, static, {r0, r1, r2}, {r1, r2}
def_method repl_read_quote, class/lisp/repl_read_quote, static, {r0, r1, r2}, {r1, r2}
def_method repl_read_qquote, class/lisp/repl_read_qquote, static, {r0, r1, r2}, {r1, r2}
def_method repl_read_unquote, class/lisp/repl_read_unquote, static, {r0, r1, r2}, {r1, r2}
def_method repl_read_splicing, class/lisp/repl_read_splicing, static, {r0, r1, r2}, {r1, r2}
def_method repl_read_list, class/lisp/repl_read_list, static, {r0, r1, r2}, {r1, r2}
def_method repl_read_pair, class/lisp/repl_read_pair, static, {r0, r1, r2}, {r1, r2}
def_method repl_read_sym, class/lisp/repl_read_sym, static, {r0, r1, r2}, {r1, r2}
def_method repl_read_str, class/lisp/repl_read_str, static, {r0, r1, r2}, {r1, r2}
def_method repl_read_num, class/lisp/repl_read_num, static, {r0, r1, r2}, {r1, r2}
def_method repl_eval, class/lisp/repl_eval, static, {r0, r1}, {r1}
def_method repl_eval_list, class/lisp/repl_eval_list, static, {r0, r1, r2}, {r1}
def_method repl_apply, class/lisp/repl_apply, static, {r0, r1, r2}, {r1}
def_method repl_print, class/lisp/repl_print, static, {r0, r1, r2}
def_method repl_expand, class/lisp/repl_expand, static, {r0, r1}, {r1}

def_method func_def, class/lisp/func_def, static, {r0, r1}, {r1}
def_method func_defq, class/lisp/func_defq, static, {r0, r1}, {r1}
def_method func_set, class/lisp/func_set, static, {r0, r1}, {r1}
def_method func_setq, class/lisp/func_setq, static, {r0, r1}, {r1}
def_method func_lambda, class/lisp/func_lambda, static, {r0, r1}, {r1}
def_method func_quote, class/lisp/func_quote, static, {r0, r1}, {r1}
def_method func_qquote, class/lisp/func_qquote, static, {r0, r1}, {r1}
def_method func_add, class/lisp/func_add, static, {r0, r1}, {r1}
def_method func_sub, class/lisp/func_sub, static, {r0, r1}, {r1}
def_method func_mul, class/lisp/func_mul, static, {r0, r1}, {r1}
def_method func_div, class/lisp/func_div, static, {r0, r1}, {r1}
def_method func_mod, class/lisp/func_mod, static, {r0, r1}, {r1}
def_method func_eql, class/lisp/func_eql, static, {r0, r1}, {r1}
def_method func_eq, class/lisp/func_eq, static, {r0, r1}, {r1}
def_method func_ne, class/lisp/func_ne, static, {r0, r1}, {r1}
def_method func_lt, class/lisp/func_lt, static, {r0, r1}, {r1}
def_method func_gt, class/lisp/func_gt, static, {r0, r1}, {r1}
def_method func_le, class/lisp/func_le, static, {r0, r1}, {r1}
def_method func_ge, class/lisp/func_ge, static, {r0, r1}, {r1}
def_method func_cond, class/lisp/func_cond, static, {r0, r1}, {r1}
def_method func_progn, class/lisp/func_progn, static, {r0, r1}, {r1}
def_method func_not, class/lisp/func_not, static, {r0, r1}, {r1}
def_method func_length, class/lisp/func_length, static, {r0, r1}, {r1}
def_method func_while, class/lisp/func_while, static, {r0, r1}, {r1}
def_method func_print, class/lisp/func_print, static, {r0, r1}, {r1}
def_method func_prin, class/lisp/func_prin, static, {r0, r1}, {r1}
def_method func_env, class/lisp/func_env, static, {r0, r1}, {r1}
def_method func_sym, class/lisp/func_sym, static, {r0, r1}, {r1}
def_method func_str, class/lisp/func_str, static, {r0, r1}, {r1}
def_method func_cat, class/lisp/func_cat, static, {r0, r1}, {r1}
def_method func_elem, class/lisp/func_elem, static, {r0, r1}, {r1}
def_method func_macroexpand_1, class/lisp/func_macroexpand_1, static, {r0, r1}, {r1, r2}
def_method func_macroexpand, class/lisp/func_macroexpand, static, {r0, r1}, {r1}
def_method func_defmacro, class/lisp/func_defmacro, static, {r0, r1}, {r1}
def_method func_copy, class/lisp/func_copy, static, {r0, r1}, {r1}
def_method func_gensym, class/lisp/func_gensym, static, {r0, r1}, {r1}
def_method func_filestream, class/lisp/func_filestream, static, {r0, r1}, {r1}
def_method func_strstream, class/lisp/func_strstream, static, {r0, r1}, {r1}
def_method func_readchar, class/lisp/func_readchar, static, {r0, r1}, {r1}
def_method func_readline, class/lisp/func_readline, static, {r0, r1}, {r1}
def_method func_writechar, class/lisp/func_writechar, static, {r0, r1}, {r1}
def_method func_writeline, class/lisp/func_writeline, static, {r0, r1}, {r1}
def_method func_write, class/lisp/func_write, static, {r0, r1}, {r1}
def_method func_char, class/lisp/func_char, static, {r0, r1}, {r1}
def_method func_code, class/lisp/func_code, static, {r0, r1}, {r1}
def_method func_slice, class/lisp/func_slice, static, {r0, r1}, {r1}
def_method func_bshr, class/lisp/func_bshr, static, {r0, r1}, {r1}
def_method func_bashr, class/lisp/func_bashr, static, {r0, r1}, {r1}
def_method func_bshl, class/lisp/func_bshl, static, {r0, r1}, {r1}
def_method func_band, class/lisp/func_band, static, {r0, r1}, {r1}
def_method func_bor, class/lisp/func_bor, static, {r0, r1}, {r1}
def_method func_bxor, class/lisp/func_bxor, static, {r0, r1}, {r1}
def_method func_push, class/lisp/func_push, static, {r0, r1}, {r1}
def_method func_apply, class/lisp/func_apply, static, {r0, r1}, {r1}
def_method func_repl, class/lisp/func_repl, static, {r0, r1}, {r1}
def_method func_eval, class/lisp/func_eval, static, {r0, r1}, {r1}

def_enum
	enum type_args_eval_apply
	enum type_args_apply
	enum type_apply
def_enum_end

def_struct lisp, ref
	ptr lisp_stdin
	ptr lisp_stdout
	ptr lisp_stderr
	ptr lisp_symbols
	ptr lisp_enviroment
	ptr lisp_macros
	ulong lisp_nextsym
	ptr lisp_sym_parent
	ptr lisp_sym_optional
	ptr lisp_sym_rest
	ptr lisp_sym_nil
	ptr lisp_sym_t
	ptr lisp_sym_lambda
	ptr lisp_sym_quote
	ptr lisp_sym_qquote
	ptr lisp_sym_unquote
	ptr lisp_sym_splicing
	ptr lisp_sym_cat
	ptr lisp_sym_list
def_struct_end

%endif

(import "././obj/struct.inc")

(structure +lisp +obj_size
	;this first block of ptrs gets derefed on deinit
	(ptr stdin stdout stderr environment
		sym_underscore sym_optional sym_rest sym_ignore sym_most sym_ampersand sym_nil
		sym_t sym_lambda sym_macro sym_quote sym_qquote sym_unquote sym_trunc
		sym_splicing sym_ffi sym_stack_frame func_ffi func_lambda func_macro func_quote
		func_qquote stream_name script_name)
	(ptr error)
	;a chain of empty lists, for the args of a call, see :lisp :repl_eval
	(ptr args_pool)
	;a chain of empty environments, for the call of a lambda, see :lisp
	;:env_push
	(ptr env_pool)
	(uint seq_idx stream_line nextsym))

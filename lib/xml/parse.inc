;;;;;;;;;;;;
; xml parser
;;;;;;;;;;;;

(import "././consts/chars.inc")

;module
(env-push)

(defq +not_symbol_chars {""=<>}
	+open_chars "</?!-"
	+close_chars "-?/>")

(defmacro is-not-symbol-char? (_)
	(list 'find-rev _ +not_symbol_chars))

(defmacro is-xml-open-char? (_)
	(list 'find-rev _ +open_chars))

(defmacro is-xml-close-char? (_)
	(list 'find-rev _ +close_chars))

(defmacro is-whitespace-char? (_)
	`(<= ,_ +char_space))

(defun XML-parse (_stream _fnc_in _fnc_out _fnc_text)
	; (XML-parse stream fnc_in fnc_out fnc_text)
	;calls back to user code, so _ used for vars that would be in scope
	;break the stream into svg tokens, symbols, strings etc
	(defq _state :text _token nil _token_list (list) _state_list (list))
	(while (defq _cc (read-char _stream))
		(defq _c (char _cc))
		(while _c
			(case _state
				(:text
					(cond
						((is-whitespace-char? _cc))
						((= _cc +char_lt)
							(setq _token (list _c) _state :open))
						((is-xml-close-char? _c)
							(setq _token (list _c) _state :close))
						((= _cc +char_equal)
							(setq _token (list _c) _state :assign))
						((= _cc +char_double_quote)
							(setq _token (list) _state :string))
						(t	(setq _token (list _c) _state :symbol)))
					(setq _c nil))
				(:open
					(task-slice)
					(cond
						((is-xml-open-char? _c)
							(push _token _c)
							(setq _c nil))
						(t  (push _token_list (apply cat _token))
							(push _state_list _state)
							(setq _state :text))))
				(:close
					(cond
						((is-xml-close-char? _c)
							(push _token _c)
							(setq _c nil))
						(t  (push _token_list (apply cat _token))
							(push _state_list _state)
							(setq _state :text))))
				(:symbol
					(cond
						((or (is-whitespace-char? _cc) (is-not-symbol-char? _c))
							(push _token_list (apply cat _token))
							(push _state_list _state)
							(setq _state :text))
						(t	(push _token _c)
							(setq _c nil))))
				(:assign
					(push _token_list (apply cat _token))
					(push _state_list _state)
					(setq _state :text))
				(:string
					(cond
						((= _cc +char_double_quote)
							(push _token_list (apply cat _token))
							(push _state_list _state)
							(setq _state :text))
						(t  (push _token _c)))
					(setq _c nil))
				)))

	;parse the commands and attributes calling back to the user functions
	(defq _stack (list) _keys (list) _vals (list) _state :text)
	(each (lambda (_type _token)
		(while _type
			(case _state
				(:text
					(case _type
						((:symbol)
							(_fnc_text _token)
							(setq _type nil))
						(t  (setq _state :open))))
				(:open
					(case _type
						(:open
							(case _token
								("<!" (setq _state :line_comment1)
									(push _stack _token))
								("<?" (setq _state :line_comment2)
									(push _stack _token))
								("<!--" (setq _state :block_comment)
									(push _stack _token))
								("<" (setq _state :command))
								("</" (setq _state :command))
								(t (throw "Unknown open _token !" _token))))
						(t (throw "Unknown open _type !" _type)))
					(push _stack _token)
					(setq _type nil))
				(:line_comment1
					(case _type
						(:close
							(case _token
								(">" (setq _state :close))))
						(t  (setq _type nil))))
				(:line_comment2
					(case _type
						(:close
							(case _token
								("?>" (setq _state :close))))
						(t  (setq _type nil))))
				(:block_comment
					(case _type
						(:close
							(case _token
								("-->" (setq _state :close))))
						(t  (setq _type nil))))
				(:command
					(case _type
						(:symbol
							(push _stack _token)
							(clear _keys _vals)
							(setq _state :symbol))
						(t (throw "Unknown command _type !" _type)))
					(setq _type nil))
				(:symbol
					(case _type
						(:close
							(when (eql (elem -3 _stack) "<")
								(task-slice)
								(_fnc_in (elem -2 _stack) _keys _vals))
							(setq _state :close))
						(:symbol
							(push _keys _token)
							(setq _state :assign _type nil))
						(t (throw "Unknown symbol _type !" _type))))
				(:assign
					(case _type
						(:assign
							(setq _state :string))
						(t (throw "Unknown assign _type !" _type)))
					(setq _type nil))
				(:string
					(case _type
						(:string
							(push _vals _token)
							(setq _state :symbol))
						(t (throw "Unknown string _type !" _type)))
					(setq _type nil))
				(:close
					(case _type
						(:close
							(case _token
								(("-->" "?>" ">" "/>"))
								(t (throw "Unknown close _token !" _token))))
						(t (throw "Unknown close _type !" _type)))
					(if (or (eql _token "/>") (eql (elem -3 _stack) "</"))
						(_fnc_out (elem -2 _stack)))
					(pop _stack)
					(pop _stack)
					(setq _type nil _state :text))
				))) _state_list _token_list))

;module
(export-symbols XML-parse)
(env-pop)

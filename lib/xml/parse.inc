;;;;;;;;;;;;
; xml parser
;;;;;;;;;;;;

(import "././consts/chars.inc")

;module
(env-push)

(defq +symbol_chars (cat
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		"0123456789-:"
		"abcdefghijklmnopqrstuvwxyz")
	+xml_open_chars "</?!-"
	+xml_close_chars "-?/>")

(defmacro is-symbol-char? (_)
	(list 'find-rev _ +symbol_chars))

(defmacro is-xml-open-char? (_)
	(list 'find-rev _ +xml_open_chars))

(defmacro is-xml-close-char? (_)
	(list 'find-rev _ +xml_close_chars))

(defmacro is-whitespace-char? (_)
	`(<= (code ,_) +char_space))

(defun XML-parse (_stream _fnc_in _fnc_out _fnc_text)
	; (XML-parse stream fnc_in fnc_out fnc_text)
	;calls back to user code, so _ used for vars that would be in scope
	;break the steam into svg tokens, symbols, strings etc
	(defq _state :text1 _token (list) _token_list (list) _state_list (list))
	(each-line (lambda (l) (each (lambda (c)
		(while c
			(case _state
				(:text1
					(cond
						((is-whitespace-char? c))
						((eql c (ascii-char +char_lt))
							(push _token_list _token)
							(push _state_list _state)
							(setq _token (list c) _state :open))
						(t	(push _token c)
							(setq _state :text2)))
					(setq c nil))
				(:text2
					(cond
						((is-whitespace-char? c)
							(push _token " ")
							(setq _state :text1))
						((eql c (ascii-char +char_lt))
							(push _token_list _token)
							(push _state_list _state)
							(setq _token (list c) _state :open))
						(t	(push _token c)))
					(setq c nil))
				(:open
					(cond
						((is-xml-open-char? c)
							(push _token c)
							(setq c nil))
						(t	(push _token_list _token)
							(push _state_list _state)
							(setq _state :attribute?))))
				(:attribute?
					(cond
						((is-whitespace-char? c))
						((eql c (ascii-char +char_equal))
							(setq _token (list c) _state :assign))
						((eql c (ascii-char +char_double_quote))
							(setq _token (list) _state :string))
						((is-xml-close-char? c)
							(setq _token (list c) _state :close))
						((is-symbol-char? c)
							(setq _token (list c) _state :symbol)))
					(setq c nil))
				(:symbol
					(cond
						((is-symbol-char? c)
							(push _token c)
							(setq c nil))
						(t	(push _token_list _token)
							(push _state_list _state)
							(setq _token (list) _state :attribute?))))
				(:assign
					(push _token_list _token)
					(push _state_list _state)
					(setq _token (list) _state :attribute?))
				(:string
					(cond
						((eql c (ascii-char +char_double_quote))
							(push _token_list _token)
							(push _state_list _state)
							(setq _token (list) _state :attribute?))
						(t	(push _token c)))
					(setq c nil))
				(:close
					(cond
						((is-xml-close-char? c)
							(push _token c)
							(setq c nil))
						(t	(push _token_list _token)
							(push _state_list _state)
							(setq _token (list) _state :text1))))
			))) (cat l " "))) _stream)

	;parse the commands and attributes calling back to the user functions
	(defq _stack (list) _keys (list) _vals (list) _state :text)
	(each (lambda (_type _token)
		(if (> (length _token) 0)
			(setq _token (apply cat _token))
			(setq _token ""))
		(while _type
			(case _state
				(:text
					(case _type
						((:text1 :text2)
							(unless (eql _token "") (_fnc_text _token))
							(setq _type nil))
						(t	(setq _state :open))))
				(:open
					(case _type
						(:open
							(case _token
								("<!" (setq _state :line_comment)
									(push _stack _token))
								("<!--" (setq _state :block_comment)
									(push _stack _token))
								("<?" (setq _state :command))
								("<" (setq _state :command))
								("</" (setq _state :command))
								(t (throw "Unknown open _token !" _token))))
						(t (throw "Unknown open _type !" _type)))
					(push _stack _token)
					(setq _type nil))
				(:line_comment
					(case _type
						(:close
							(case _token
								(">" (setq _state :close))))
						(t	(setq _type nil))))
				(:block_comment
					(case _type
						(:close
							(case _token
								("-->" (setq _state :close))))
						(t	(setq _type nil))))
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
							(if (eql (elem -3 _stack) "<")
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

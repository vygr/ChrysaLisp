;;;;;;;;;;;;
; xml parser
;;;;;;;;;;;;

;module
(env-push)

(defun XML-parse (_stream _fnc_in _fnc_out _fnc_text)
	; (XML-parse stream fnc_in fnc_out fnc_text)
	;calls back to user code, so _ used for vars that would be in scope
	;break the stream into svg tokens, symbols, strings etc
	;parse the commands and attributes calling back to the user functions
	(defq _stack (list) _keys (list) _vals (list) _state :text)
	(each! 0 -1 (lambda (_type _token)
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
				))) ((const (ffi xml-parse "lib/xml/parse" 0)) stream)))

;module
(export-symbols XML-parse)
(env-pop)

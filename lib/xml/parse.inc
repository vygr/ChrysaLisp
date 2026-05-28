;;;;;;;;;;;;
; xml parser
;;;;;;;;;;;;

;module
(env-push)

(defun XML-parse (stream fnc_in fnc_out fnc_text)
	; (XML-parse stream fnc_in fnc_out fnc_text)
	;break the stream into svg tokens, symbols, strings etc
	;parse the commands and attributes calling back to the user functions
	(defq stack (list) keys (list) vals (list) state :text e (penv))
	(each! (lambda (type token)
		(while type
			(case state
				(:text
					(case type
						((:symbol)
							(callback fnc_text e token)
							(setq type :nil))
						(:t (setq state :open))))
				(:open
					(case type
						(:open
							(case token
								("<!" (setq state :line_comment1)
									(push stack token))
								("<?" (setq state :line_comment2)
									(push stack token))
								("<!--" (setq state :block_comment)
									(push stack token))
								("<" (setq state :command))
								("</" (setq state :command))
								(:t (throw "Unknown open token !" token))))
						(:t (throw "Unknown open type !" type)))
					(push stack token)
					(setq type :nil))
				(:line_comment1
					(case type
						(:close
							(case token
								(">" (setq state :close))))
						(:t (setq type :nil))))
				(:line_comment2
					(case type
						(:close
							(case token
								("?>" (setq state :close))))
						(:t (setq type :nil))))
				(:block_comment
					(case type
						(:close
							(case token
								("-->" (setq state :close))))
						(:t (setq type :nil))))
				(:command
					(case type
						(:symbol
							(push stack token)
							(clear keys vals)
							(setq state :symbol))
						(:t (throw "Unknown command type !" type)))
					(setq type :nil))
				(:symbol
					(case type
						(:close
							(when (eql (elem-get stack -3) "<")
								(task-slice)
								(callback fnc_in e (last stack) keys vals))
							(setq state :close))
						(:symbol
							(push keys token)
							(setq state :assign type :nil))
						(:t (throw "Unknown symbol type !" type))))
				(:assign
					(case type
						(:assign
							(setq state :string))
						(:t (throw "Unknown assign type !" type)))
					(setq type :nil))
				(:string
					(case type
						(:string
							(push vals token)
							(setq state :symbol))
						(:t (throw "Unknown string type !" type)))
					(setq type :nil))
				(:close
					(case type
						(:close
							(case token
								(("-->" "?>" ">" "/>"))
								(:t (throw "Unknown close token !" token))))
						(:t (throw "Unknown close type !" type)))
					(if (or (eql token "/>") (eql (elem-get stack -3) "</"))
						(callback fnc_out e (last stack)))
					(pop stack)
					(pop stack)
					(setq type :nil state :text))
				))) ((const (ffi (path-to-absolute "./parse"))) stream)))

;module
(export-symbols '(XML-parse))
(env-pop)

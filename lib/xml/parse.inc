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

(defun XML-parse (stream fnc_in fnc_out fnc_text)
	; (XML-parse stream fnc_in fnc_out fnc_text)

	;break the steam into svg tokens, symbols, strings etc
	(defq state :text1 token (list) token_list (list) state_list (list))
	(each-line (lambda (l) (each (lambda (c)
		(while c
			(case state
				(:text1
					(cond
						((is-whitespace-char? c))
						((eql c (ascii-char +char_lt))
							(push token_list token)
							(push state_list state)
							(setq token (list c) state :open))
						(t	(push token c)
							(setq state :text2)))
					(setq c nil))
				(:text2
					(cond
						((is-whitespace-char? c)
							(push token " ")
							(setq state :text1))
						((eql c (ascii-char +char_lt))
							(push token_list token)
							(push state_list state)
							(setq token (list c) state :open))
						(t	(push token c)))
					(setq c nil))
				(:open
					(cond
						((is-xml-open-char? c)
							(push token c)
							(setq c nil))
						(t	(push token_list token)
							(push state_list state)
							(setq state :attribute?))))
				(:attribute?
					(cond
						((is-whitespace-char? c))
						((eql c (ascii-char +char_equal))
							(setq token (list c) state :assign))
						((eql c (ascii-char +char_double_quote))
							(setq token (list) state :string))
						((is-xml-close-char? c)
							(setq token (list c) state :close))
						((is-symbol-char? c)
							(setq token (list c) state :symbol)))
					(setq c nil))
				(:symbol
					(cond
						((is-symbol-char? c)
							(push token c)
							(setq c nil))
						(t	(push token_list token)
							(push state_list state)
							(setq token (list) state :attribute?))))
				(:assign
					(push token_list token)
					(push state_list state)
					(setq token (list) state :attribute?))
				(:string
					(cond
						((eql c (ascii-char +char_double_quote))
							(push token_list token)
							(push state_list state)
							(setq token (list) state :attribute?))
						(t	(push token c)))
					(setq c nil))
				(:close
					(cond
						((is-xml-close-char? c)
							(push token c)
							(setq c nil))
						(t	(push token_list token)
							(push state_list state)
							(setq token (list) state :text1))))
			))) (cat l " "))) stream)

	;parse the commands and attributes calling back to the user functions
	(defq stack (list) keys (list) vals (list) state :text)
	(each (lambda (type token)
		(if (> (length token) 0)
			(setq token (apply cat token))
			(setq token ""))
		(while type
			(case state
				(:text
					(case type
						((:text1 :text2)
							(unless (eql token "") (fnc_text token))
							(setq type nil))
						(t	(setq state :open))))
				(:open
					(case type
						(:open
							(case token
								("<!--" (setq state :comment)
									(push stack token))
								("<?" (setq state :command))
								("<" (setq state :command))
								("</" (setq state :command))
								(t (throw "Unknown open token !" token))))
						(t (throw "Unknown open type !" type)))
					(push stack token)
					(setq type nil))
				(:comment
					(case type
						(:close
							(case token
								("-->" (setq state :close))))
						(t	(setq type nil))))
				(:command
					(case type
						(:symbol
							(push stack token)
							(clear keys vals)
							(setq state :symbol))
						(t (throw "Unknown command type !" type)))
					(setq type nil))
				(:symbol
					(case type
						(:close
							(if (eql (elem -3 stack) "<")
								(fnc_in (elem -2 stack) keys vals))
							(setq state :close))
						(:symbol
							(push keys token)
							(setq state :assign type nil))
						(t (throw "Unknown symbol type !" type))))
				(:assign
					(case type
						(:assign
							(setq state :string))
						(t (throw "Unknown assign type !" type)))
					(setq type nil))
				(:string
					(case type
						(:string
							(push vals token)
							(setq state :symbol))
						(t (throw "Unknown string type !" type)))
					(setq type nil))
				(:close
					(case type
						(:close
							(case token
								(("-->" "?>" ">" "/>"))
								(t (throw "Unknown close token !" token))))
						(t (throw "Unknown close type !" type)))
					(if (or (eql token "/>") (eql (elem -3 stack) "</"))
						(fnc_out (elem -2 stack)))
					(pop stack)
					(pop stack)
					(setq type nil state :text))
				))) state_list token_list))

;module
(export-symbols XML-parse)
(env-pop)

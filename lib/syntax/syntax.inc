;;;;;;;;;;;;;;;;;;;;;;;;;;;
; syntax highlighter object
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/symbols.inc")
(import "gui/canvas/lisp.inc")

(defmacro is_number_char (_)
	(list 'find-rev _ "0123456789"))

(defmacro is_allowed_number_char (_)
	(list 'find-rev _ (const (cat
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		"abcdefghijklmnopqrstuvwxyz"
		".0123456789"))))

(defmacro is_symbol_char (_)
	(list 'find-rev _ (const (cat
		"!@#$%^&*_+-=[];'\:|,./<>?`~"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		"0123456789"
		"abcdefghijklmnopqrstuvwxyz"))))

(class syntax ()
	(defun-bind boot-fncs ()
		(defq out (list) e (penv))
		(while (penv e) (setq e (penv e)))
		(each (lambda ((k v))
			(if (fnc? v) (push out k))) (tolist e))
		(sort cmp out))

	(defun-bind boot-funs ()
		(defq out (list) e (penv))
		(while (penv e) (setq e (penv e)))
		(each (lambda ((k v))
			(if (fun? v) (push out k))) (tolist e))
		(sort cmp out))

	(defun-bind boot-macros ()
		(defq out (list) e (penv))
		(while (penv e) (setq e (penv e)))
		(each (lambda ((k v))
			(if (macro? v) (push out k))) (tolist e))
		(sort cmp out))

	(def this :keywords (defq keywords (env -101)) :state :text
		:ink_fncs (<< (canvas-from-argb32 argb_red 15) 32)
		:ink_funs (<< (canvas-from-argb32 argb_magenta 15) 32)
		:ink_macros (<< (canvas-from-argb32 argb_cyan 15) 32)
		:ink_strings (<< (canvas-from-argb32 argb_yellow 15) 32)
		:ink_numbers (<< (canvas-from-argb32 argb_white 15) 32)
		:ink_text (<< (canvas-from-argb32 argb_green 15) 32))
	(each (# (def keywords %0 (get :ink_fncs this))) (boot-fncs))
	(each (# (def keywords %0 (get :ink_funs this))) (boot-funs))
	(each (# (def keywords %0 (get :ink_macros this))) (boot-macros))

	(method-bind :set_state (this state)
		;(. this :set_state state) -> this
		(set this :state state)
		this)

	(method-bind :colorise (this line)
		;(. this :colorise str) -> array

		;tokenise the string starting with the current state
		;save the state at the end as we may wish to continue
		(defq state (get :state this) token "" state_list (list)
			token_list (list) col_list (list))
		(each (lambda (c)
			(while c
				(case state
					(:text
						(cond
							((is_number_char c)
								(push state_list state)
								(push token_list token)
								(setq token c state :number))
							((is_symbol_char c)
								(push state_list state)
								(push token_list token)
								(setq token c state :symbol))
							((eql c (ascii-char 34))
								(push state_list state)
								(push token_list token)
								(setq token c state :string1))
							((eql c "{")
								(push state_list state)
								(push token_list token)
								(setq token c state :string2))
							(t
								(setq token (cat token c))))
						(setq c nil))
					(:symbol
						(cond
							((is_symbol_char c)
								(setq token (cat token c) c nil))
							(t
								(push state_list state)
								(push token_list token)
								(setq token "" state :text))))
					(:number
						(cond
							((is_allowed_number_char c)
								(setq token (cat token c) c nil))
							(t
								(push state_list state)
								(push token_list token)
								(setq token "" state :text))))
					(:string1
						(setq token (cat token c))
						(cond
							((eql c (ascii-char 34))
								(push state_list state)
								(push token_list token)
								(setq token "" state :text)))
						(setq c nil))
					(:string2
						(setq token (cat token c))
						(cond
							((eql c "}")
								(push state_list state)
								(push token_list token)
								(setq token "" state :text)))
						(setq c nil)))))
			line)
		(push state_list state)
		(push token_list token)
		(set this :state state)

		;assign colours
		(each (lambda (state token)
			(case state
				(:symbol
					(if (defq ink (get (sym token) (get :keywords this)))
						(push col_list ink)
						(push col_list (get :ink_text this))))
				(:string1
					(push col_list (get :ink_strings this)))
				(:string2
					(push col_list (get :ink_strings this)))
				(:number
					(push col_list (get :ink_numbers this)))
				(:text
					(push col_list (get :ink_text this)))))
			state_list token_list)

		;build line array
		(defq line_array (list))
		(each (lambda (token ink)
				(push line_array (apply array (map (# (+ (code %0) ink)) token))))
			token_list col_list)
		(apply cat line_array)))

(undef (env) 'is_number_char 'is_allowed_number_char 'is_symbol_char
	'boot-fncs 'boot-funs 'boot-macros)

;test
(when nil
	(defq coloriser (syntax))
	(print (. coloriser :colorise "This is plain text"))
	(print (. coloriser :colorise "(defq a (+ 56.8 0x45))"))
	(print (. coloriser :colorise "{split a string over"))
	(print (. coloriser :colorise "serveral lines}"))
)

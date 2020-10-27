;;;;;;;;;;;;;;;;;;;;;;;;;;;
; syntax highlighter object
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/symbols.inc")
(import "gui/canvas/lisp.inc")

(defmacro-bind is_number_char (_)
	(list 'find-rev _ "0123456789"))

(defmacro-bind is_allowed_number_char (_)
	(list 'find-rev _ (const (cat
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		"abcdefghijklmnopqrstuvwxyz"
		".0123456789"))))

(defmacro-bind is_symbol_char (_)
	(list 'find-rev _ (const (cat
		"!@#$%^&*_+-=[];\:|,./<>?`~"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		"0123456789"
		"abcdefghijklmnopqrstuvwxyz"))))

(class syntax ()
	;default colours and state
	(def this :keywords (defq keywords (env -101)) :state :text
		:ink_keyword1 (<< (canvas-from-argb32 argb_magenta 15) 32)
		:ink_keyword2 (<< (canvas-from-argb32 argb_red 15) 32)
		:ink_keyword3 (<< (canvas-from-argb32 argb_green 15) 32)
		:ink_strings (<< (canvas-from-argb32 argb_cyan 15) 32)
		:ink_numbers (<< (canvas-from-argb32 argb_yellow 15) 32)
		:ink_keysyms (<< (canvas-from-argb32 argb_cyan 15) 32)
		:ink_constants (<< (canvas-from-argb32 argb_yellow 15) 32)
		:ink_comments (<< (canvas-from-argb32 argb_grey12 15) 32)
		:ink_text (<< (canvas-from-argb32 argb_white 15) 32))
	;default keywords
	(defq e (penv))
	(while (penv e) (setq e (penv e)))
	(each (lambda ((k v))
		(cond
			((fnc? v) (def keywords k (get :ink_keyword1 this)))
			((fun? v) (def keywords k (get :ink_keyword2 this)))
			((macro? v) (def keywords k (get :ink_keyword3 this)))))
		(tolist e))

	(method-bind :set_state (this state)
		;(. this :set_state state) -> this
		(set this :state state)
		this)

	(method-bind :colorise (this line)
		;(. this :colorise str) -> array

		;tokenise the string starting with the current state
		;save the state at the end as we may wish to continue
		(defq state (get :state this) token (list) state_list (list)
			token_list (list) col_list (list))
		(each (lambda (c)
			(while c
				(case state
					(:text
						(cond
							((is_number_char c)
								(push state_list state)
								(push token_list token)
								(setq token (list c) state :number))
							((eql c ":")
								(push state_list state)
								(push token_list token)
								(setq token (list c) state :keysym))
							((eql c ";")
								(push state_list state)
								(push token_list token)
								(setq token (list c) state :comment))
							((is_symbol_char c)
								(push state_list state)
								(push token_list token)
								(setq token (list c) state :symbol))
							((eql c (ascii-char 34))
								(push state_list state)
								(push token_list token)
								(setq token (list c) state :string1))
							((eql c "{")
								(push state_list state)
								(push token_list token)
								(setq token (list c) state :string2))
							(t	(push token c)))
						(setq c nil))
					(:symbol
						(cond
							((is_symbol_char c)
								(push token c)
								(setq c nil))
							(t	(push state_list state)
								(push token_list token)
								(setq token (list) state :text))))
					(:keysym
						(cond
							((is_symbol_char c)
								(push token c)
								(setq c nil))
							(t	(push state_list state)
								(push token_list token)
								(setq token (list) state :text))))
					(:comment
						(push token c)
						(setq c nil))
					(:number
						(cond
							((is_allowed_number_char c)
								(push token c)
								(setq c nil))
							(t	(push state_list state)
								(push token_list token)
								(setq token (list) state :text))))
					(:string1
						(push token c)
						(cond
							((eql c (ascii-char 34))
								(push state_list state)
								(push token_list token)
								(setq token (list) state :text)))
						(setq c nil))
					(:string2
						(push token c)
						(cond
							((eql c "}")
								(push state_list state)
								(push token_list token)
								(setq token (list) state :text)))
						(setq c nil)))))
			line)
		(push state_list state)
		(push token_list token)
		(set this :state (if (find state '(:string1 :string2)) state :text))

		;assign colours
		(each (lambda (state token)
			(case state
				(:symbol
					(cond
						((defq ink (get (sym (apply cat token)) (get :keywords this)))
							;present in keyword map
							(push col_list ink))
						((eql (elem 0 token) "+")
							;is a constant symbol
							(push col_list (get :ink_constants this)))
						((eql (elem 0 token) "-")
							;is a nagative number
							(push col_list (get :ink_numbers this)))
						(t	;default text color)
							(push col_list (get :ink_text this)))))
				(:string1
					(push col_list (get :ink_strings this)))
				(:string2
					(push col_list (get :ink_strings this)))
				(:number
					(push col_list (get :ink_numbers this)))
				(:keysym
					(push col_list (get :ink_keysyms this)))
				(:comment
					(push col_list (get :ink_comments this)))
				(:text
					(push col_list (get :ink_text this)))))
			state_list token_list)

		;build line array
		(clear state_list)
		(each (lambda (token ink)
				(push state_list (apply array (map (# (+ (code %0) ink)) token))))
			token_list col_list)
		(apply cat state_list)))

(undef (env) 'is_number_char 'is_allowed_number_char 'is_symbol_char
	'boot-fncs 'boot-funs 'boot-macros)

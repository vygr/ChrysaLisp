;;;;;;;;;;;;;;;;;;;;;;;;;;;
; syntax highlighter object
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")
(import "././consts/chars.inc")
(import "././consts/colors.inc")

;module
(env-push)

(defq +number_chars
		"0123456789"
	+allowed_number_chars (cat
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		"abcdefghijklmnopqrstuvwxyz"
		".0123456789")
	+symbol_chars (cat
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		"0123456789"
		"!@#$%^&*_+-=[];\:|./<>?`~'"
		"abcdefghijklmnopqrstuvwxyz"))

(defmacro is-number-char? (_)
	(list 'find-rev _ +number_chars))

(defmacro is-allowed-number-char? (_)
	(list 'find-rev _ +allowed_number_chars))

(defmacro is-symbol-char? (_)
	(list 'find-rev _ +symbol_chars))

(defclass Syntax () nil
	; (Syntax) -> syntax
	;default colours and state
	(def this :keywords (defq keywords (env 101)) :state :text
		:ink_keyword1 (<< (canvas-from-argb32 +argb_magenta 15) 32)
		:ink_keyword2 (<< (canvas-from-argb32 +argb_red 15) 32)
		:ink_keyword3 (<< (canvas-from-argb32 +argb_green 15) 32)
		:ink_keyword4 (<< (canvas-from-argb32 +argb_cyan 15) 32)
		:ink_strings (<< (canvas-from-argb32 +argb_orange 15) 32)
		:ink_numbers (<< (canvas-from-argb32 +argb_yellow 15) 32)
		:ink_keysyms (<< (canvas-from-argb32 +argb_cyan 15) 32)
		:ink_constants (<< (canvas-from-argb32 +argb_yellow 15) 32)
		:ink_globals (<< (canvas-from-argb32 +argb_steel 15) 32)
		:ink_comments (<< (canvas-from-argb32 +argb_grey12 15) 32)
		:ink_text (<< (canvas-from-argb32 +argb_white 15) 32))
	;defaults from boot environment
	(each (lambda ((k v))
		(cond
			((func? v) (def keywords k (get :ink_keyword1 this)))
			((lambda? v) (def keywords k (get :ink_keyword2 this)))
			((macro? v) (def keywords k (get :ink_keyword3 this)))))
		(tolist *boot_env*))
	(each (lambda (k)
			(def keywords k (get :ink_keyword4 this)))
		'(t nil &rest &optional
			byte ubyte short ushort int uint long ulong ptr union struct
			pbyte pubyte pshort pushort pint puint plong pulong pptr
			nodeid netid offset enum bit))

	(defmethod :set_state (this state)
		; (. syntax :set_state state) -> syntax
		(lower :state)
		this)

	(defmethod :get_state (this)
		; (. syntax :get_state) -> state
		(get :state this))

	(defmethod :set_colors (this colors)
		; (. syntax :set_colors fmap) -> syntax
		(. colors :each (# (set this %0 (<< (canvas-from-argb32 %1 15) 32))))
		this)

	(defmethod :colorise (this line)
		; (. syntax :colorise str) -> array

		;tokenise the string starting with the current state
		;save the state at the end as we may wish to continue
		(raise :state (token (list) state_list (list) token_list (list) col_list (list)))
		(each (lambda (c)
			(while c
				(case state
					(:text
						(cond
							((defq _ (find-rev c (const (cat (ascii-char 34) "{:;"))))
								(push state_list state)
								(push token_list token)
								(setq token (list c) state (elem _ '(:string1 :string2 :keysym :comment))))
							((is-number-char? c)
								(push state_list state)
								(push token_list token)
								(setq token (list c) state :number))
							((is-symbol-char? c)
								(push state_list state)
								(push token_list token)
								(setq token (list c) state :symbol))
							((push token c)))
						(setq c nil))
					((:symbol :keysym)
						(cond
							((is-symbol-char? c)
								(push token c)
								(setq c nil))
							(t  (push state_list state)
								(push token_list token)
								(setq token (list) state :text))))
					(:comment
						(push token c)
						(setq c nil))
					(:number
						(cond
							((is-allowed-number-char? c)
								(push token c)
								(setq c nil))
							(t  (push state_list state)
								(push token_list token)
								(setq token (list) state :text))))
					(:string1
						(push token c)
						(when (eql c (ascii-char +char_double_quote))
							(push state_list state)
							(push token_list token)
							(setq token (list) state :text))
						(setq c nil))
					(:string2
						(push token c)
						(when (eql c "}")
							(push state_list state)
							(push token_list token)
							(setq token (list) state :text))
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
						((eql (elem 0 token) "*")
							;is a global symbol
							(push col_list (get :ink_globals this)))
						((eql (elem 0 token) "-")
							;is a negative number
							(push col_list (get :ink_numbers this)))
						(t  ;default text color)
							(push col_list (get :ink_text this)))))
				((:string1 :string2)
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
		(apply cat state_list))

	(defmethod :compress_tabs (this string tab_width)
		; (. syntax :compress_tabs string tab_width) -> string
		;tab compression of string
		(cond
			((> (length string) 0)
				(defq whitespace "")
				(apply cat (reduce (#
					(cond
						((eql whitespace "X")
							(push %0 %1))
						((eql %1 " ")
							(setq whitespace (cat whitespace " "))
							(when (= (length whitespace) tab_width)
								(setq whitespace "")
								(push %0 (ascii-char +char_tab)))
							%0)
						(t  (push %0 whitespace %1)
							(setq whitespace "X")
							%0))) string (list))))
			(string)))

	(defmethod :text_flow (this words line_width)
		; (. syntax :text_flow words line_width) -> lines
		;word reflow
		(defq cnt 0 line (clear '()) lines (list))
		(each (lambda (word)
			(cond
				((> (setq cnt (+ cnt (length word) 1)) line_width)
					(task-slice)
					(push lines (join line " "))
					(push (clear line) word)
					(setq cnt (+ (length word) 1)))
				(t  (push line word)))) words)
		(if (> (length line) 0)
			(push lines (join line " ")) lines))
	)

;module
(export-classes
	Syntax)
(env-pop)

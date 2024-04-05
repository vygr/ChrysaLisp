;;;;;;;;;;;;;;;;;;;;;;;;;;;
; syntax highlighter object
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")
(import "././consts/chars.inc")
(import "././consts/colors.inc")

;module
(env-push)

(defq +char_class_not_symbol (char-class " \r\f\v\n\t()")
	+char_class_not_text (char-class (cat (ascii-char 34) ":;{")))

(defmacro is-digit-char? (_) `(bfind ,_ +char_class_digit))
(defmacro is-digit-base-char? (_) `(bfind ,_ +char_class_digit_base))
(defmacro is-symbol-char? (_) `(not (bfind ,_ +char_class_not_symbol)))

(defun is-text-char? (_)
	(cond
		((defq i (bfind _ +char_class_not_text))
			(elem-get '(:string1 :keysym :comment :string2) i))
		((is-digit-char? _) :number)
		((is-symbol-char? _) :symbol)
		(:text)))

(defclass Syntax () :nil
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
			((lambda-func? v) (def keywords k (get :ink_keyword2 this)))
			((macro-func? v) (def keywords k (get :ink_keyword3 this)))
			((func? v) (def keywords k (get :ink_keyword1 this)))))
		(tolist *root_env*))
	(each (# (def keywords %0 (get :ink_keyword4 this)))
		'(:t :nil &rest &optional &most &ignore
		byte ubyte short ushort int uint long ulong ptr union struct
		pbyte pubyte pshort pushort pint puint plong pulong pptr
		nodeid netid offset enum bit))

	(defgetmethod state)
	(defsetmethod state)

	(defmethod :set_colors (colors)
		; (. syntax :set_colors fmap) -> syntax
		(. colors :each (# (set this %0 (<< (canvas-from-argb32 %1 15) 32))))
		this)

	(defmethod :colorise (line)
		; (. syntax :colorise str) -> array

		;tokenise the string starting with the current state
		;save the state at the end as we may wish to continue
		(defq j 0 l (length line) state (get :state this) i 0
			token_list (list) state_list (cap l (list)))
		(while (< j l)
			(case state
				(:text
					(while (and (< j l)
							(eql (setq state (is-text-char? (elem-get line j))) :text))
						(setq j (inc j)))
					(when (/= i j)
						(push state_list :text)
						(push token_list (slice line i (setq i j)))))
				(:symbol
					(push state_list :symbol)
					(push token_list (slice line i (setq j
						(bskipn +char_class_not_symbol line (inc j)) i j)))
					(setq state :text))
				(:keysym
					(push state_list :keysym)
					(push token_list (slice line i (setq j
						(bskipn +char_class_not_symbol line (inc j)) i j)))
					(setq state :text))
				(:number
					(push state_list :number)
					(push token_list (slice line i (setq j
						(bskip +char_class_digit_base line (inc j)) i j)))
					(setq state :text))
				(:comment
					(push state_list :comment)
					(push token_list (slice line i (setq state :text j l))))
				(:string1
					(push state_list :string1)
					(if (setq j (find (ascii-char 34) line (inc i)))
						(push token_list (slice line i (setq state :text j (inc j) i j)))
						(push token_list (slice line i (setq j l)))))
				(:string2
					(push state_list :string2)
					(push token_list (if (setq j (find "}" line (inc i)))
						(slice line i (setq state :text j (inc j) i j))
						(slice line i (setq j l)))))))
		(set this :state state)

		;assign colours
		(defq state (cap (length token_list) (list)))
		(each (lambda (type token)
			(case type
				(:symbol
					(cond
						((defq ink (get (sym token) (get :keywords this)))
							;present in keyword map
							(push state ink))
						((eql (first token) "+")
							;is a constant symbol
							(push state (get :ink_constants this)))
						((eql (first token) "*")
							;is a global symbol
							(push state (get :ink_globals this)))
						((eql (first token) "-")
							;is a negative number
							(push state (get :ink_numbers this)))
						(:t ;default text color)
							(push state (get :ink_text this)))))
				((:string1 :string2)
					(push state (get :ink_strings this)))
				(:number
					(push state (get :ink_numbers this)))
				(:keysym
					(push state (get :ink_keysyms this)))
				(:comment
					(push state (get :ink_comments this)))
				(:text
					(push state (get :ink_text this)))))
			state_list token_list)

		;build line array
		(cond
			((nempty? state_list)
				(clear state_list)
				(each (lambda (token ink)
						(each (# (push state_list (+ (code %0) ink))) token))
					token_list state)
				(apply (const array) state_list))
			((array))))

	(defmethod :compress_tabs (string tab_width)
		; (. syntax :compress_tabs string tab_width) -> string
		;tab compression of string
		(cond
			((> (length string) 0)
				(defq whitespace "")
				(apply (const cat) (reduce (#
					(cond
						((eql whitespace "X")
							(push %0 %1))
						((eql %1 " ")
							(setq whitespace (cat whitespace " "))
							(when (= (length whitespace) tab_width)
								(setq whitespace "")
								(push %0 (ascii-char +char_tab)))
							%0)
						(:t (push %0 whitespace %1)
							(setq whitespace "X")
							%0))) string (list))))
			(string)))

	(defmethod :text_flow (words line_width)
		; (. syntax :text_flow words line_width) -> lines
		;word reflow
		(defq cnt 0 line (clear '()) lines (list))
		(each (lambda (word)
			(cond
				((> (setq cnt (+ cnt (length word) 1)) line_width)
					(task-slice)
					(push lines (apply cat (join line '(" "))))
					(push (clear line) word)
					(setq cnt (+ (length word) 1)))
				(:t (push line word)))) words)
		(if (> (length line) 0)
			(push lines (apply cat (join line '(" ")))) lines))
	)

;module
(export-classes '(Syntax))
(env-pop)

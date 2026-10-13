;;;;;;;;;;;;;;;;;;;;;;;;;;;
; syntax highlighter object
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the inks are pixels as a pixmap has them, that is all of the GUI wanted here
(import "gui/pixmap/lisp.inc")
(import "./tokenize.inc")
(import "././consts/colors.inc")

;module
(env-push)

;the words that have a colour of their own, each with the name of the ink
;it is in. From the boot environment, made once as this is read and the
;same map for every syntax there is: making it is most of what making one
;was, and a scan of many files makes one for each
(defq +syntax_keywords (pmap))
(each (lambda ((k v))
	(pinsert +syntax_keywords k
		(cond
			((lambda-func? v) :ink_keyword2)
			((macro-func? v) :ink_keyword3)
			((func? v) :ink_keyword1))))
	(tolist *root_env*))
(each (# (pinsert +syntax_keywords %0 :ink_keyword4))
	'(:t :nil &rest &optional &most &ignore
	byte ubyte short ushort int uint long real ptr union struct
	pbyte pubyte pshort pushort pint puint plong preal pptr
	nodeid netid offset enum bit))

(defclass Syntax () :nil
	; (Syntax) -> syntax
	;default colours and state
	(def this :keywords +syntax_keywords :state :text
		:ink_keyword1 (num (<< (pixmap-from-argb32 +argb_magenta 15) 32))
		:ink_keyword2 (num (<< (pixmap-from-argb32 +argb_red 15) 32))
		:ink_keyword3 (num (<< (pixmap-from-argb32 +argb_green 15) 32))
		:ink_keyword4 (num (<< (pixmap-from-argb32 +argb_cyan 15) 32))
		:ink_strings (num (<< (pixmap-from-argb32 +argb_orange 15) 32))
		:ink_numbers (num (<< (pixmap-from-argb32 +argb_yellow 15) 32))
		:ink_keysyms (num (<< (pixmap-from-argb32 +argb_cyan 15) 32))
		:ink_constants (num (<< (pixmap-from-argb32 +argb_yellow 15) 32))
		:ink_globals (num (<< (pixmap-from-argb32 +argb_steel 15) 32))
		:ink_comments (num (<< (pixmap-from-argb32 +argb_grey12 15) 32))
		:ink_text (num (<< (pixmap-from-argb32 +argb_white 15) 32)))
	(defgetmethod :keywords)
	(defgetmethod :state)
	(defsetmethod :state)

	(defmethod :set_colors (colors)
		; (. syntax :set_colors fmap) -> syntax
		(. colors :each (# (set this %0 (num (<< (pixmap-from-argb32 %1 15) 32)))))
		this)

	(defmethod :tokenize (line)
		; (. syntax :tokenize line) -> (token_list state_list)
		;the state a line ends in is kept here for the next, see
		;(tokenize-line), which is this without a syntax made
		(bind '(token_list state_list state) (tokenize-line line (get :state this)))
		(set this :state state)
		(list token_list state_list))

	(defmethod :colorise (line)
		; (. syntax :colorise str) -> array
		;assign colours
		(bind '(token_list state_list) (. this :tokenize line))
		(defq state (cap (length token_list) (list)))
		(each (lambda (type token)
			(case type
				(:symbol
					(cond
						((defq ink (pfind +syntax_keywords (sym token)))
							;present in keyword map, the ink of that name
							(push state (get ink this)))
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

	(defmethod :line_expand (line tab_width)
		; (. syntax :line_expand line tab_width) -> line
		(cond
			((empty? line) line)
			((defq idx 0)
			(apply (const cat) (map! (lambda (token state)
					(defq res (if (find state '(:string1 :string2))
						token
						(expand token tab_width idx)))
					(++ idx (length res))
					res)
				(. this :tokenize line))))))

	(defmethod :line_compress (line tab_width)
		; (. syntax :line_compress line tab_width) -> line
		(cond
			((empty? line) line)
			((defq idx 0)
			(apply (const cat) (map! (lambda (token state)
					(defq res (if (find state '(:string1 :string2))
						token
						(compress token tab_width idx)))
					(++ idx (length token))
					res)
				(. this :tokenize line))))))
	)

;module
(export-classes '(Syntax))
(env-pop)

;;;;;;;;;;;;;;;;;;;;
; text buffer object
;;;;;;;;;;;;;;;;;;;;

(import "lib/text/syntax.inc")
(import "lib/consts/chars.inc")
(import "lib/text/substr.inc")

;module
(env-push)

(defun last-line ()
	(when (or (= (length buffer) 0) (not (eql (elem -2 buffer) "")))
		(push buffer "")
		(push buffer_colored "")
		(push buffer_states '(nil nil))))

(defun erase-line (y)
	(setq buffer (erase buffer y (inc y)))
	(setq buffer_colored (erase buffer_colored y (inc y)))
	(setq buffer_states (erase buffer_states y (inc y))))

(defun insert-line (y line)
	(setq buffer (insert buffer y (list line)))
	(setq buffer_colored (insert buffer_colored y (list line)))
	(setq buffer_states (insert buffer_states y '((nil nil)))))

(defun set-line (y line)
	(elem-set y buffer line)
	(elem-set y buffer_colored line)
	(elem-set y buffer_states '(nil nil)))

(defun redo ()
	(defq idx (length undo_stack))
	(case state
		(:break (.-> this (:set_cursor x y) :break))
		(:insert (.-> this (:set_cursor x y) (:insert data)))
		(:delete (.-> this (:set_cursor x y) (:delete data)))
		(:backspace (.-> this (:set_cursor x y) (:backspace data)))
		(:cursor (. this :set_cursor x y) (push undo_stack (list :cursor x y)))
		(:mark (push undo_stack (list :mark x))))
	idx)

(defun undo ()
	(defq idx (redo))
	(until (= idx (length undo_stack))
		(push redo_stack (pop undo_stack))))

(defmacro mutate-buffer (&rest _)
	`(progn
		(bind '(x y) (. this :get_cursor))
		(bind '(x y) (. this :constrain x y))
		(bind '(w h) (. this :get_size))
		(defq buffer (get :buffer this)
			buffer_colored (get :buffer_colored this)
			buffer_states (get :buffer_states this))
		~_
		(clear (get :redo_stack this))
		(last-line)
		(set this :buffer buffer :buffer_colored buffer_colored
			:buffer_states buffer_states :dirty t)
		(. this :set_cursor x y)))

(defmacro get-selection ()
	`(progn
		(bind '(x y) (. this :get_cursor))
		(bind '(sx sy) (. this :constrain x y))
		(bind '(sx1 sy1) (. this :constrain anchor_x anchor_y))
		(if (> sy sy1)
			(defq sx (logxor sx sx1) sx1 (logxor sx sx1) sx (logxor sx sx1)
				sy (logxor sy sy1) sy1 (logxor sy sy1) sy (logxor sy sy1)))
		(and (= sy sy1) (> sx sx1)
			(defq sx (logxor sx sx1) sx1 (logxor sx sx1) sx (logxor sx sx1)))))

(defmacro select-buffer (&rest _)
	`(progn
		(get-selection)
		(cond
			((and (= sx sx1) (= sy sy1)) "")
			(t  (bind '(w h) (. this :get_size))
				~_))))

(defclass Buffer (&optional mode syntax) nil
	; (Buffer [mode syntax]) -> buffer
	(def this :buffer (list "") :buffer_colored (list "") :buffer_states (list '(nil nil))
		:dirty nil :syntax (opt syntax (Syntax)) :undo_stack (list) :redo_stack (list)
		:cursor_x 0 :cursor_y 0 :max_width 0 :tab_width 4 :wrap_width 80
		:mode mode :lps nil :lps_pattern nil :next_mark -1)

	(defmethod :next_mark (this)
		; (. buffer :next_mark) -> mark
		(set this :next_mark (inc (get :next_mark this))))

	(defmethod :push_undo (this &rest records)
		; (. buffer :push_undo record ...) -> buffer
		(defq undo_stack (get :undo_stack this))
		(each (# (push undo_stack %0)) records)
		this)

	(defmethod :set_cursor (this x y)
		; (. buffer :set_cursor x y) -> buffer
		(set this :cursor_x x :cursor_y y)
		this)

	(defmethod :get_cursor (this)
		; (. buffer :get_cursor) -> (x y)
		(list (get :cursor_x this) (get :cursor_y this)))

	(defmethod :get_size (this)
		; (. buffer :get_size) -> (width height)
		(list (get :max_width this) (dec (length (get :buffer this)))))

	(defmethod :get_syntax (this)
		; (. buffer :get_syntax) -> syntax
		(get :syntax this))

	(defmethod :get_wrap_width (this)
		; (. buffer :get_wrap_width) -> wrap_width
		(get :wrap_width this))

	(defmethod :get_tab_width (this)
		; (. buffer :get_tab_width) -> tab_width
		(get :tab_width this))

	(defmethod :get_text_lines (this)
		; (. buffer :get_text_lines) -> lines
		(get :buffer this))

	(defmethod :get_text_line (this y)
		; (. buffer :get_text_line y) -> line
		(elem y (get :buffer this)))

	(defmethod :file_load (this filepath)
		; (. buffer :file_load filepath) -> buffer
		(defq syntax (get :syntax this)
			buffer (list) buffer_colored (list) buffer_states (list)
			max_width 0 tab_width (get :tab_width this))
		(each-line (lambda (line)
				(setq line (. syntax :expand_tabs
						(trim line (const (char +char_cr))) tab_width)
					max_width (max max_width (length line)))
				(push buffer line)
				(push buffer_colored line)
				(push buffer_states '(nil nil)))
			(file-stream filepath))
		(last-line)
		(clear (get :undo_stack this) (get :redo_stack this))
		(set this :buffer buffer :buffer_colored buffer_colored
			:buffer_states buffer_states :max_width max_width
			:cursor_x 0 :cursor_y 0 :dirty t)
		this)

	(defmethod :file_save (this filepath)
		; (. buffer :file_save filepath) -> buffer
		(defq syntax (get :syntax this) tab_width (get :tab_width this)
			stream (file-stream filepath +file_open_write))
		(each! 0 (bind '(w h) (. this :get_size)) (lambda (line)
				(write-line stream (. syntax :compress_tabs line tab_width)))
			(list (get :buffer this)))
		this)

	(defmethod :vdu_load (this vdu scroll_x scroll_y)
		; (. buffer :vdu_load vdu scroll_x scroll_y) -> buffer
		(when (get :dirty this)
			(defq syntax (get :syntax this)
				buffer (get :buffer this)
				buffer_colored (get :buffer_colored this)
				buffer_states (get :buffer_states this)
				state :text max_width 0 mode (get :mode this))
			(each (lambda (line line_array (start_state end_state))
					(setq max_width (max max_width (length line)))
					(cond
						(mode)
						((and (array? line_array) (eql state start_state))
							(setq state end_state))
						(t  (elem-set _ buffer_colored
								(.-> syntax (:set_state state) (:colorise line)))
							(elem-set _ buffer_states
								(list state (setq state (. syntax :get_state)))))))
				buffer buffer_colored buffer_states)
			(set this :max_width max_width :dirty nil))
		(. vdu :load
			(get :buffer_colored this)
			scroll_x scroll_y (get :cursor_x this) (get :cursor_y this))
		this)

	(defmethod :constrain (this x y)
		; (. buffer :constrain x y) -> (x y)
		(bind '(w h) (. this :get_size))
		(defq buffer (get :buffer this))
		(cond
			((< y 0) (setq x 0 y 0))
			((> y h) (setq x 0 y h))
			((< x 0) (setq x 0))
			((> x (length (elem y buffer))) (setq x (length (elem y buffer)))))
		(list x y))

	(defmethod :left (this)
		; (. buffer :left) -> buffer
		(bind '(x y) (. this :get_cursor))
		(if (< (setq x (dec x)) 0)
			(if (< (setq y (dec y)) 0)
				(setq x 0)
				(setq x +max_int)))
		(bind '(x y) (. this :constrain x y))
		(. this :set_cursor x y))

	(defmethod :right (this)
		; (. buffer :right) -> buffer
		(bind '(x y) (. this :get_cursor))
		(bind '(w h) (. this :get_size))
		(defq w (if (> y h) 0 (length (elem y (get :buffer this)))))
		(if (> (setq x (inc x)) w)
			(if (> (setq y (inc y)) h)
				(setq x +max_int)
				(setq x 0)))
		(bind '(x y) (. this :constrain x y))
		(. this :set_cursor x y))

	(defmethod :down (this)
		; (. buffer :down) -> buffer
		(bind '(x y) (. this :get_cursor))
		(bind '(x y) (. this :constrain x (inc y)))
		(. this :set_cursor x y))

	(defmethod :up (this)
		; (. buffer :up) -> buffer
		(bind '(x y) (. this :get_cursor))
		(bind '(x y) (. this :constrain x (dec y)))
		(. this :set_cursor x y))

	(defmethod :break (this)
		; (. buffer :break) -> buffer
		(mutate-buffer
			(defq front (slice 0 x (elem y buffer)) back (slice x -1 (elem y buffer)))
			(set-line y front)
			(insert-line (setq x 0 y (inc y)) back)
			(cond
				((<= y h)
					(. this :push_undo (list :backspace x y)))
				(t  (. this :push_undo
						(list :mark (defq mark (. this :next_mark)))
						(list :backspace x y) (list :delete 0 y)
						(list :mark mark))))))

	(defmethod :backspace (this &optional num)
		; (. buffer :backspace &optional num) -> buffer
		(mutate-buffer
			(setd num 1)
			(cond
				((and (<= x 0) (<= y 0)))
				((= x 0)
					(setq y (dec y) x (length (elem y buffer)))
					(set-line y (cat (elem y buffer) (elem (inc y) buffer)))
					(erase-line (inc y))
					(. this :push_undo (list :break x y)))
				(t  (defq undo (slice (- x num) x (elem y buffer)))
					(set-line y (erase (elem y buffer) (- x num) x))
					(setq x (- x num))
					(. this :push_undo (list :insert x y undo))))))

	(defmethod :delete (this &optional num)
		; (. buffer :delete &optional num) -> buffer
		(mutate-buffer
			(setd num 1)
			(cond
				((>= y h))
				((>= x (length (elem y buffer)))
					(set-line y (cat (elem y buffer) (elem (inc y) buffer)))
					(erase-line (inc y))
					(. this :push_undo (list :break x y)))
				(t  (defq undo (slice x (+ x num) (elem y buffer)))
					(set-line y (erase (elem y buffer) x (+ x num)))
					(. this :push_undo (list :insert x y undo))))))

	(defmethod :insert (this string)
		; (. buffer :insert string) -> buffer
		(mutate-buffer
			(set-line y (insert (elem y buffer) x string))
			(setq x (+ x (length string)))
			(if (>= y h) (. this :push_undo (list :delete 0 y)))
			(. this :push_undo (list :backspace x y (length string)))))

	(defmethod :cut (this anchor_x anchor_y)
		; (. buffer :cut anchor_x anchor_y) -> string
		(select-buffer
			(. this :push_undo
				(list :mark (defq mark (. this :next_mark)))
				(list :cursor x y))
			(defq lines (list))
			(. this :set_cursor sx sy)
			(cond
				((= sy sy1)
					(push lines (slice sx sx1 (elem sy (get :buffer this))))
					(. this :delete (- sx1 sx)))
				(t  (while (<= sy (setq sy1 (dec sy1)))
						(push lines (slice sx -1 (elem sy (get :buffer this))) (ascii-char +char_lf))
						(if (> (defq gap (- (length (elem sy (get :buffer this))) sx)) 0)
							(. this :delete gap))
						(. this :delete))
					(when (and (> sx1 0) (< sy (bind '(w h) (. this :get_size))))
						(push lines (slice sx (+ sx sx1) (elem sy (get :buffer this))))
						(. this :delete sx1))))
			(. this :push_undo (list :mark mark))
			(apply cat lines)))

	(defmethod :copy (this anchor_x anchor_y)
		; (. buffer :copy anchor_x anchor_y) -> string
		(select-buffer
			(defq buffer (get :buffer this))
			(cond
				((= sy sy1)
					(slice sx sx1 (elem sy buffer)))
				(t  (defq lines
						(list (slice sx -1 (elem sy buffer)) (ascii-char +char_lf)))
					(while (< (setq sy (inc sy)) sy1)
						(push lines (elem sy buffer) (ascii-char +char_lf)))
					(if (< sy h)
						(push lines (slice 0 sx1 (elem sy buffer))))
					(apply cat lines)))))

	(defmethod :paste (this string)
		; (. buffer :paste string) -> buffer
		(. this :push_undo (list :mark (defq mark (. this :next_mark))))
		(while (defq idx (find (ascii-char +char_lf) string))
			(when (> idx 0)
				(. this :insert (slice 0 idx string)))
			(. this :break)
			(setq string (slice (inc idx) -1 string)))
		(if (> (length string) 0) (. this :insert string))
		(. this :push_undo (list :mark mark))
		this)

	(defmethod :undo (this)
		; (. buffer :undo) -> buffer
		(when (defq data (pop (defq undo_stack (get :undo_stack this))))
			(defq redo_stack (cat (get :redo_stack this)))
			(bind '(state &optional x y data) data)
			(cond
				((eql state :mark)
					(undo)
					(defq outer x x -1)
					(until (and (eql state :mark) (= x outer))
						(bind '(state &optional x y data) (pop undo_stack))
						(undo)))
				(t  (undo)))
			(set this :redo_stack redo_stack))
		this)

	(defmethod :redo (this)
		; (. buffer :redo) -> buffer
		(when (defq data (pop (defq redo_stack (get :redo_stack this))))
			(defq undo_stack (get :undo_stack this) redo_stack (cat redo_stack))
			(bind '(state &optional x y data) data)
			(cond
				((eql state :mark)
					(redo)
					(defq outer x x -1)
					(until (and (eql state :mark) (= x outer))
						(bind '(state &optional x y data) (pop redo_stack))
						(redo)))
				(t  (redo)))
			(set this :redo_stack redo_stack))
		this)

	(defmethod :left_bracket (this)
		; (. buffer :left_bracket) -> (x y) | (nil nil)
		(bind '(x y) (. this :get_cursor))
		(bind '(x y) (. this :constrain x y))
		(bind '(w h) (. this :get_size))
		(defq buffer (get :buffer this) buffer_colored (get :buffer_colored this)
			ink_text (get :ink_text (get :syntax this)) c 0)
		(when (array? (defq line_colored (elem y buffer_colored)))
			(cond
				((<= x 0))
				((= x (length (defq line (elem y buffer)))) (setq x (dec x)))
				((eql (elem x line) "("))
				((eql (elem x line) ")"))
				((setq x (dec x))))
			(and (< x (length (defq line (elem y buffer))))
				(eql (elem x line) "(")
				(setq x (inc x)))
			(while (and (/= c 1) (>= (setq x (dec x)) 0))
				(cond
					((/= (logand 0xffffff000000 (elem x line_colored)) ink_text))
					((eql (elem x line) "(") (setq c (inc c)))
					((eql (elem x line) ")") (setq c (dec c)))))
			(while (and (/= c 1) (>= (setq y (dec y)) 0)
					(array? (defq line_colored (elem y buffer_colored))))
				(setq x (length (defq line (elem y buffer))))
				(while (and (/= c 1) (>= (setq x (dec x)) 0))
					(cond
						((/= (logand 0xffffff000000 (elem x line_colored)) ink_text))
						((eql (elem x line) "(") (setq c (inc c)))
						((eql (elem x line) ")") (setq c (dec c)))))))
		(if (= c 1) (list x y) '(nil nil)))

	(defmethod :right_bracket (this)
		; (. buffer :right_bracket) -> (x y) | (nil nil)
		(bind '(x y) (. this :get_cursor))
		(bind '(x y) (. this :constrain x y))
		(bind '(w h) (. this :get_size))
		(defq buffer (get :buffer this) buffer_colored (get :buffer_colored this)
			ink_text (get :ink_text (get :syntax this)) c 0)
		(when (array? (defq line_colored (elem y buffer_colored)))
			(cond
				((<= x 0))
				((= x (length (defq line (elem y buffer)))) (setq x (dec x)))
				((eql (elem x line) "("))
				((eql (elem x line) ")"))
				((setq x (dec x))))
			(and (< x (length (defq line (elem y buffer))))
				(eql (elem x line) ")")
				(setq x (dec x)))
			(while (and (/= c 1) (< (setq x (inc x)) (length line)))
				(cond
					((/= (logand 0xffffff000000 (elem x line_colored)) ink_text))
					((eql (elem x line) ")") (setq c (inc c)))
					((eql (elem x line) "(") (setq c (dec c)))))
			(while (and (/= c 1) (< (setq y (inc y)) h)
					(array? (defq line_colored (elem y buffer_colored))))
				(setq x -1)
				(while (and (/= c 1) (< (setq x (inc x)) (length (defq line (elem y buffer)))))
					(cond
						((/= (logand 0xffffff000000 (elem x line_colored)) ink_text))
						((eql (elem x line) ")") (setq c (inc c)))
						((eql (elem x line) "(") (setq c (dec c)))))))
		(if (= c 1) (list x y) '(nil nil)))

	(defmethod :find (this pattern)
		; (. buffer :find pattern) -> ([(x y)] ...)
		(defq out (list) lps (get :lps this))
		(unless (eql (get :lps_pattern this) pattern)
			(set this :lps (setq lps (substr-lps pattern)) :lps_pattern pattern))
		(each! 0 (bind '(w h) (. this :get_size)) (lambda (line)
				(when (> (length line) 0)
					(defq idx _)
					(each (# (push out (list %0 idx))) (substr pattern line lps))))
			(list (get :buffer this))) out)
	)

;module
(export (penv)
	*class_Buffer* Buffer Buffer?)
(env-pop)

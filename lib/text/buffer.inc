;;;;;;;;;;;;;;;;;;;;
; text buffer object
;;;;;;;;;;;;;;;;;;;;

(import "./syntax.inc")
(import "././consts/chars.inc")

;module
(env-push)

(bits +dirty_flag 0
	(bit syntax bracket))

(defun intern (l s)
	(cond ((defq i (find s l)) (elem-get i l)) ((push l s) s)))

(defun push-line (line)
	(each (# (push %0 %1))
		(list buffer_line buffer_syntax buffer_states buffer_found buffer_brackets)
		`(,line ,line :nil :nil :nil)))

(defun set-line (y line)
	(each (# (elem-set y %0 %1))
		(list buffer_line buffer_syntax buffer_states buffer_found buffer_brackets)
		`(,line ,line :nil :nil :nil)))

(defun last-line ()
	(when (or (= (length buffer_line) 0) (not (eql (elem-get -2 buffer_line) "")))
		(push-line "")))

(defun erase-line (y)
	(defq y1 (inc y))
	(each (# (set (penv) %0 (erase (eval %0) y y1)))
		'(buffer_line buffer_syntax buffer_states buffer_found buffer_brackets)))

(defun insert-line (y line)
	(each (# (set (penv) %0 (insert (eval %0) y (list %1))))
		'(buffer_line buffer_syntax buffer_states buffer_found buffer_brackets)
		`(,line ,line :nil :nil :nil)))

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
		(raise :buffer_line :buffer_syntax :buffer_states :buffer_found :buffer_brackets)
		~_
		(clear (get :redo_stack this))
		(last-line)
		(lower :buffer_line :buffer_syntax :buffer_states :buffer_found :buffer_brackets
			(:dirty_flags -1 :modified :t))
		(. this :set_cursor x y)))

(defmacro get-selection ()
	`(progn
		(bind '(x y) (. this :get_cursor))
		(bind '(sx sy) (. this :constrain x y))
		(bind '(sx1 sy1) (. this :constrain anchor_x anchor_y))
		(if (> sy sy1) (defq st sx sx sx1 sx1 st st sy sy sy1 sy1 st))
		(and (= sy sy1) (> sx sx1) (defq st sx sx sx1 sx1 st))))

(defmacro select-buffer (&rest _)
	`(progn
		(get-selection)
		(cond
			((and (= sx sx1) (= sy sy1)) "")
			(:t (bind '(w h) (. this :get_size))
				~_))))

(defun build-syntax (this)
	(raise :dirty_flags)
	(when (/= (logand +dirty_flag_syntax dirty_flags) 0)
		(raise :buffer_line :buffer_syntax :buffer_states
			:mode :syntax_engine (state :text max_width 0))
		(each (lambda (line syntax states)
			(setq max_width (max max_width (length line)))
			(bind '(start_state end_state) (opt states '(:nil :nil)))
			(cond
				((not mode))
				((and (array? syntax) (eql state start_state))
					(setq state end_state))
				(:t (task-slice)
					(elem-set _ buffer_syntax
						(.-> syntax_engine (:set_state state) (:colorise line)))
					(elem-set _ buffer_states
						(list state (setq state (. syntax_engine :get_state)))))))
			buffer_line buffer_syntax buffer_states)
		(lower :max_width (:dirty_flags (logand dirty_flags
			(const (lognot +dirty_flag_syntax))))))
	this)

(defun build-brackets (this)
	(raise :dirty_flags)
	(when (/= (logand +dirty_flag_bracket dirty_flags) 0)
		(when (get :mode this)
			(build-syntax this)
			(raise :buffer_syntax :buffer_brackets)
			(defq ink_text (get :ink_text (get :syntax_engine this)))
			(each (lambda (syntax brackets)
				(unless brackets
					(elem-set _ buffer_brackets (reduce
						(# (+ %0 (cond
							((/= (logand 0xffffff000000 %1) ink_text) 0)
							((eql (logand 0xff %1) (ascii-code "(")) -1)
							((eql (logand 0xff %1) (ascii-code ")")) 1)
							(0))))
						syntax 0))))
				buffer_syntax buffer_brackets))
		(lower (:dirty_flags (logand dirty_flags
			(const (lognot +dirty_flag_bracket))))))
	this)

(defclass Buffer (&optional mode syntax) :nil
	; (Buffer [mode syntax]) -> buffer
	(def this :buffer_line (list "") :buffer_syntax (list "")
		:buffer_states (list :nil) :buffer_brackets (list :nil)
		:buffer_found (list :nil) :dirty_flags -1
		:syntax_engine (opt syntax (Syntax)) :undo_stack (list) :redo_stack (list)
		:cursor_x 0 :cursor_y 0 :max_width 0 :tab_width 4 :wrap_width 80
		:mode mode :next_mark -1 :modified :nil
		:last_pattern :nil :last_wmode :nil :last_rmode :nil)

	(defmethod :next_mark ()
		; (. buffer :next_mark) -> mark
		(lower (:next_mark (inc (get :next_mark this)))))

	(defmethod :push_undo (&rest records)
		; (. buffer :push_undo record ...) -> buffer
		(raise :undo_stack)
		(each (# (push undo_stack %0)) records)
		this)

	(defmethod :clear_undo ()
		; (. buffer :clear_undo) -> buffer
		(clear (get :undo_stack this) (get :redo_stack this))
		this)

	(defmethod :set_cursor (cursor_x cursor_y)
		; (. buffer :set_cursor x y) -> buffer
		(lower :cursor_x :cursor_y) this)

	(defmethod :get_cursor ()
		; (. buffer :get_cursor) -> (x y)
		(list (get :cursor_x this) (get :cursor_y this)))

	(defmethod :get_modified ()
		; (. buffer :get_modified) -> :t | :nil
		(get :modified this))

	(defmethod :get_found ()
		; (. buffer :get_found) -> found
		(get :buffer_found this))

	(defmethod :get_size ()
		; (. buffer :get_size) -> (width height)
		(list (get :max_width this) (dec (length (get :buffer_line this)))))

	(defmethod :get_syntax ()
		; (. buffer :get_syntax) -> syntax
		(get :syntax_engine this))

	(defmethod :get_wrap_width ()
		; (. buffer :get_wrap_width) -> wrap_width
		(get :wrap_width this))

	(defmethod :get_tab_width ()
		; (. buffer :get_tab_width) -> tab_width
		(get :tab_width this))

	(defmethod :get_text_lines ()
		; (. buffer :get_text_lines) -> lines
		(get :buffer_line this))

	(defmethod :get_text_line (y)
		; (. buffer :get_text_line y) -> line
		(elem-get y (get :buffer_line this)))

	(defmethod :file_load (filepath)
		; (. buffer :file_load filepath) -> buffer
		(raise :syntax_engine :tab_width (buffer_line (list) buffer_syntax (list)
			 buffer_states (list) buffer_found (list) buffer_brackets (list)
			 max_width 0))
		(each-line (lambda (line)
				(task-slice)
				(setq line (expand (trim-end line (const (char +char_cr))) tab_width)
					max_width (max max_width (length line)))
				(push-line line))
			(file-stream filepath))
		(last-line)
		(clear (get :undo_stack this) (get :redo_stack this))
		(lower :buffer_line :buffer_syntax :buffer_states :buffer_found :buffer_brackets
			:max_width (:cursor_x 0 :cursor_y 0 :dirty_flags -1 :modified :nil))
		this)

	(defmethod :file_save (filepath)
		; (. buffer :file_save filepath) -> buffer
		(raise :syntax_engine :tab_width (stream (file-stream filepath +file_open_write)))
		(each! 0 (bind '(w h) (. this :get_size)) (lambda (line)
				(task-slice)
				(write-line stream (. syntax_engine :compress_tabs line tab_width)))
			(list (get :buffer_line this)))
		(lower (:modified :nil)) this)

	(defmethod :vdu_load (vdu scroll_x scroll_y)
		; (. buffer :vdu_load vdu scroll_x scroll_y) -> buffer
		(. vdu :load (get :buffer_syntax (build-syntax this))
			scroll_x scroll_y (get :cursor_x this) (get :cursor_y this))
		this)

	(defmethod :constrain (x y)
		; (. buffer :constrain x y) -> (x y)
		(bind '(w h) (. this :get_size))
		(raise :buffer_line)
		(cond
			((< y 0) (setq x 0 y 0))
			((> y h) (setq x 0 y h))
			((< x 0) (setq x 0))
			((> x (length (elem-get y buffer_line)))
				(setq x (length (elem-get y buffer_line)))))
		(list x y))

	(defmethod :left ()
		; (. buffer :left) -> buffer
		(bind '(x y) (. this :get_cursor))
		(if (< (setq x (dec x)) 0)
			(if (< (setq y (dec y)) 0)
				(setq x 0)
				(setq x +max_int)))
		(bind '(x y) (. this :constrain x y))
		(. this :set_cursor x y))

	(defmethod :right ()
		; (. buffer :right) -> buffer
		(bind '(x y) (. this :get_cursor))
		(bind '(w h) (. this :get_size))
		(defq w (if (> y h) 0 (length (elem-get y (get :buffer_line this)))))
		(if (> (setq x (inc x)) w)
			(if (> (setq y (inc y)) h)
				(setq x +max_int)
				(setq x 0)))
		(bind '(x y) (. this :constrain x y))
		(. this :set_cursor x y))

	(defmethod :down ()
		; (. buffer :down) -> buffer
		(bind '(x y) (. this :get_cursor))
		(bind '(x y) (. this :constrain x (inc y)))
		(. this :set_cursor x y))

	(defmethod :up ()
		; (. buffer :up) -> buffer
		(bind '(x y) (. this :get_cursor))
		(bind '(x y) (. this :constrain x (dec y)))
		(. this :set_cursor x y))

	(defmethod :break ()
		; (. buffer :break) -> buffer
		(mutate-buffer
			(defq front (slice 0 x (elem-get y buffer_line))
				back (slice x -1 (elem-get y buffer_line)))
			(setq x 0 y (inc y))
			(cond
				((and (eql "" back) (= y h))
					(. this :push_undo (list :backspace x y)))
				(:t (set-line (dec y) front)
					(insert-line y back)
					(cond
						((<= y h)
							(. this :push_undo (list :backspace x y)))
						(:t (. this :push_undo
								(list :mark (defq mark (. this :next_mark)))
								(list :backspace x y) (list :delete 0 y)
								(list :mark mark))))))))

	(defmethod :backspace (&optional num)
		; (. buffer :backspace &optional num) -> buffer
		(mutate-buffer
			(setd num 1)
			(cond
				((and (<= x 0) (<= y 0)))
				((= x 0)
					(setq y (dec y) x (length (elem-get y buffer_line)))
					(set-line y (cat (elem-get y buffer_line) (elem-get (inc y) buffer_line)))
					(erase-line (inc y))
					(. this :push_undo (list :break x y)))
				(:t (defq undo (slice (- x num) x (elem-get y buffer_line)))
					(set-line y (erase (elem-get y buffer_line) (- x num) x))
					(setq x (- x num))
					(. this :push_undo (list :insert x y undo))))))

	(defmethod :delete (&optional num)
		; (. buffer :delete &optional num) -> buffer
		(mutate-buffer
			(setd num 1)
			(cond
				((>= y h))
				((>= x (length (elem-get y buffer_line)))
					(set-line y (cat (elem-get y buffer_line) (elem-get (inc y) buffer_line)))
					(erase-line (inc y))
					(. this :push_undo (list :break x y)))
				(:t (defq undo (slice x (+ x num) (elem-get y buffer_line)))
					(set-line y (erase (elem-get y buffer_line) x (+ x num)))
					(. this :push_undo (list :insert x y undo))))))

	(defmethod :insert (string)
		; (. buffer :insert string) -> buffer
		(if (eql string "") this
			(mutate-buffer
				(set-line y (insert (elem-get y buffer_line) x string))
				(setq x (+ x (length string)))
				(if (>= y h) (. this :push_undo (list :delete 0 y)))
				(. this :push_undo (list :backspace x y (length string))))))

	(defmethod :cut (anchor_x anchor_y)
		; (. buffer :cut anchor_x anchor_y) -> string
		(select-buffer
			(raise :buffer_line)
			(. this :push_undo
				(list :mark (defq mark (. this :next_mark)))
				(list :cursor x y))
			(defq lines (list))
			(. this :set_cursor sx sy)
			(cond
				((= sy sy1)
					(push lines (slice sx sx1 (elem-get sy buffer_line)))
					(. this :delete (- sx1 sx)))
				(:t (while (<= sy (setq sy1 (dec sy1)))
						(task-slice)
						(push lines (slice sx -1 (elem-get sy buffer_line)) (ascii-char +char_lf))
						(if (> (defq gap (- (length (elem-get sy buffer_line)) sx)) 0)
							(. this :delete gap))
						(. this :delete)
						(raise :buffer_line))
					(when (and (> sx1 0) (< sy (bind '(w h) (. this :get_size))))
						(push lines (slice sx (+ sx sx1) (elem-get sy buffer_line)))
						(. this :delete sx1))))
			(. this :push_undo (list :mark mark))
			(apply cat lines)))

	(defmethod :copy (anchor_x anchor_y)
		; (. buffer :copy anchor_x anchor_y) -> string
		(select-buffer
			(raise :buffer_line)
			(cond
				((= sy sy1)
					(slice sx sx1 (elem-get sy buffer_line)))
				(:t (defq lines
						(list (slice sx -1 (elem-get sy buffer_line)) (ascii-char +char_lf)))
					(while (< (setq sy (inc sy)) sy1)
						(push lines (elem-get sy buffer_line) (ascii-char +char_lf)))
					(if (< sy h)
						(push lines (slice 0 sx1 (elem-get sy buffer_line))))
					(apply cat lines)))))

	(defmethod :paste (string)
		; (. buffer :paste string) -> buffer
		(. this :push_undo (list :mark (defq mark (. this :next_mark))))
		(while (defq idx (find (ascii-char +char_lf) string))
			(when (> idx 0)
				(task-slice)
				(. this :insert (slice 0 idx string)))
			(. this :break)
			(setq string (slice (inc idx) -1 string)))
		(if (> (length string) 0) (. this :insert string))
		(. this :push_undo (list :mark mark))
		this)

	(defmethod :undo ()
		; (. buffer :undo) -> buffer
		(when (defq data (pop (defq undo_stack (get :undo_stack this))))
			(defq redo_stack (cat (get :redo_stack this)))
			(bind '(state &optional x y data) data)
			(cond
				((eql state :mark)
					(undo)
					(defq outer x x -1)
					(until (and (eql state :mark) (= x outer))
						(task-slice)
						(bind '(state &optional x y data) (pop undo_stack))
						(undo)))
				(:t (undo)))
			(set this :redo_stack redo_stack))
		this)

	(defmethod :redo ()
		; (. buffer :redo) -> buffer
		(when (defq data (pop (defq redo_stack (get :redo_stack this))))
			(defq undo_stack (get :undo_stack this) redo_stack (cat redo_stack))
			(bind '(state &optional x y data) data)
			(cond
				((eql state :mark)
					(redo)
					(defq outer x x -1)
					(until (and (eql state :mark) (= x outer))
						(task-slice)
						(bind '(state &optional x y data) (pop redo_stack))
						(redo)))
				(:t (redo)))
			(set this :redo_stack redo_stack))
		this)

	(defmethod :left_bracket ()
		; (. buffer :left_bracket) -> (x y) | (:nil :nil)
		(cond
			((get :mode this)
				(bind '(x y) (.-> (build-brackets this) :get_cursor))
				(bind '(x y) (. this :constrain x y))
				(raise :buffer_syntax :buffer_brackets :buffer_line
					(c 0 ink_text (get :ink_text (get :syntax_engine this))))
				(unless (eql "" (defq line (elem-get y buffer_line)))
					(cond
						((<= x 0) (setq x 0))
						((>= x (length line)) (setq x (dec (length line))))
						((eql ")" (elem-get x line)))
						((eql "(" (elem-get x line)))
						((setq x (dec x))))
					(if (eql ")" (elem-get x line))
						(setq x (dec x))))
				(setq y (some! (inc y) 0 :nil (lambda (syntax brackets)
					(defq oc c)
					(cond
						((< (setq c (+ c (cond
								((= x -1) brackets)
								(:t (defq brackets 0)
									(each! (min (inc x) (length syntax)) 0
										(# (setq brackets (+ brackets (cond
											((/= (logand 0xffffff000000 %0) ink_text) 0)
											((eql (logand 0xff %0) (ascii-code "(")) -1)
											((eql (logand 0xff %0) (ascii-code ")")) 1)
											(0))))) (list syntax)) brackets)))) 0)
							(setq x (some! (if (= x -1) (length syntax)
										(min (inc x) (length syntax))) 0 :nil
								(# (if (< (setq oc (+ oc (cond
									((/= (logand 0xffffff000000 %0) ink_text) 0)
									((eql (logand 0xff %0) (ascii-code "(")) -1)
									((eql (logand 0xff %0) (ascii-code ")")) 1)
									(0)))) 0) _)) (list syntax))) _)
						(:t (setq x -1) :nil)))
					(list buffer_syntax buffer_brackets)))
				(if y (list x y) '(:nil :nil)))
			('(:nil :nil))))

	(defmethod :right_bracket ()
		; (. buffer :right_bracket) -> (x y) | (:nil :nil)
		(cond
			((get :mode this)
				(bind '(x y) (.-> (build-brackets this) :get_cursor))
				(bind '(x y) (. this :constrain x y))
				(defq h (second (. this :get_size)))
				(raise :buffer_syntax :buffer_brackets :buffer_line
					(c 0 ink_text (get :ink_text (get :syntax_engine this))))
				(unless (eql "" (defq line (elem-get y buffer_line)))
					(cond
						((<= x 0) (setq x 0))
						((>= x (length line)) (setq x (dec (length line))))
						((eql ")" (elem-get x line)))
						((eql "(" (elem-get x line)))
						((setq x (dec x))))
					(if (eql "(" (elem-get x line))
						(setq x (inc x))))
				(setq y (some! y h :nil (lambda (syntax brackets)
					(defq oc c)
					(cond
						((> (setq c (+ c (cond
								((= x 0) brackets)
								(:t (defq brackets 0)
									(each! x -1 (# (setq brackets (+ brackets (cond
										((/= (logand 0xffffff000000 %0) ink_text) 0)
										((eql (logand 0xff %0) (ascii-code "(")) -1)
										((eql (logand 0xff %0) (ascii-code ")")) 1)
										(0))))) (list syntax)) brackets)))) 0)
							(setq x (some! x -1 :nil (# (if (> (setq oc (+ oc (cond
								((/= (logand 0xffffff000000 %0) ink_text) 0)
								((eql (logand 0xff %0) (ascii-code "(")) -1)
								((eql (logand 0xff %0) (ascii-code ")")) 1)
								(0)))) 0) _)) (list syntax))) _)
						(:t (setq x 0) :nil)))
					(list buffer_syntax buffer_brackets)))
				(if y (list x y) '(:nil :nil)))
			('(:nil :nil))))

	(defmethod :find (pattern wmode rmode)
		; (. buffer :find pattern wmode rmode) -> buffer_found
		(raise :last_pattern :last_wmode :last_rmode :buffer_found)
		(unless (and (eql wmode last_wmode)
					(eql rmode last_rmode)
					(eql last_pattern pattern))
			(lower (:last_wmode wmode :last_rmode rmode :last_pattern pattern))
			(each (lambda (__) (elem-set _ buffer_found :nil)) buffer_found))
		(unless (eql pattern "")
			(bind '(engine pattern meta) (query pattern wmode rmode))
			(defq allsubs (clear '()))
			(each (lambda (line index)
				(if (> (length line) 0)
					(unless index
						(task-slice)
						(bind '(indexs substrs) (. engine :search line pattern meta))
						(setq substrs (map (# (map (# (intern allsubs %0)) %0)) substrs))
						(elem-set _ buffer_found (list indexs substrs)))
					(elem-set _ buffer_found '(()()))))
				(get :buffer_line this) buffer_found))
		buffer_found)
	)

;module
(export-classes
	Buffer)
(env-pop)

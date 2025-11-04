;;;;;;;;;;;;;;;;;;;;
; text buffer object
;;;;;;;;;;;;;;;;;;;;

(import "./syntax.inc")
(import "././consts/chars.inc")
(import "././task/pipe.inc")

;module
(env-push)

(bits +dirty_flag 0
	(bit syntax bracket))

(defun push-line (line)
	(each (# (push %0 %1))
		(list buffer_line buffer_syntax buffer_states buffer_found buffer_brackets)
		`(,line ,line :nil :nil :nil)))

(defun set-line (y line)
	(each (# (elem-set %0 y %1))
		(list buffer_line buffer_syntax buffer_states buffer_found buffer_brackets)
		`(,line ,line :nil :nil :nil)))

(defun last-line ()
	(when (or (= (length buffer_line) 0) (not (eql (last buffer_line) "")))
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
	(static-qq (progn
		(bind '(x y) (. this :get_cursor))
		(bind '(x y) (. this :constrain x y))
		(bind '(w h) (. this :get_size))
		(raise :buffer_line :buffer_syntax :buffer_states :buffer_found :buffer_brackets)
		~_
		(clear (get :redo_stack this))
		(last-line)
		(lower :buffer_line :buffer_syntax :buffer_states :buffer_found :buffer_brackets
			(:dirty_flags -1 :modified :t))
		(. this :set_cursor x y))))

(defmacro get-selection ()
	(static-qq (progn
		(bind '(x y) (. this :get_cursor))
		(bind '(sx sy) (. this :constrain x y))
		(bind '(sx1 sy1) (. this :constrain anchor_x anchor_y))
		(if (> sy sy1) (defq st sx sx sx1 sx1 st st sy sy sy1 sy1 st))
		(and (= sy sy1) (> sx sx1) (defq st sx sx sx1 sx1 st)))))

(defmacro select-buffer (&rest _)
	(static-qq (progn
		(get-selection)
		(cond
			((and (= sx sx1) (= sy sy1)) "")
			((bind '(w h) (. this :get_size)) ~_)))))

(defun build-syntax (this &optional estate)
	(raise :dirty_flags)
	(when (/= (logand +dirty_flag_syntax dirty_flags) 0)
		(raise :buffer_line :buffer_syntax :buffer_states
			:mode :syntax_engine (state :text max_width 0))
		(each (lambda (line syntax states)
			(setq max_width (max max_width (length line)))
			(bind '(start_state end_state) (ifn states '(:nil :nil)))
			(cond
				((not mode))
				((and (array? syntax) (eql state start_state))
					(setq state end_state))
				(:t (task-slice)
					(elem-set buffer_syntax (!)
						(.-> syntax_engine (:set_state state) (:colorise line)))
					(elem-set buffer_states (!)
						(list state (setq state
							(if estate estate (. syntax_engine :get_state))))))))
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
					(elem-set buffer_brackets (!) (reduce
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

(defun insert-line-wrapped (this line wrap_width)
	(bind '(cx _) (. this :get_cursor))
	(when (> (+ cx (length line)) wrap_width)
		(.-> this (:insert (slice line 0 (setq cx (- wrap_width cx)))) :break)
		(setq line (slice line cx -1)))
	(when (> (length line) wrap_width)
		(defq lines (partition line wrap_width) line (pop lines))
		(each (# (.-> this (:insert %0) :break)) lines))
	(. this :insert line))

(defclass Buffer (&optional mode syntax) :nil
	; (Buffer [mode syntax]) -> buffer
	(def this :buffer_line (list "") :buffer_syntax (list "")
		:buffer_states (list :nil) :buffer_brackets (list :nil)
		:buffer_found (list :nil) :dirty_flags -1
		:syntax_engine (ifn syntax (Syntax)) :undo_stack (list) :redo_stack (list)
		:cursor_x 0 :cursor_y 0 :max_width 0 :tab_width 4 :wrap_width 80
		:mode mode :next_mark -1 :modified :nil
		:last_key :nil)

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
		(elem-get (get :buffer_line this) y))

	(defmethod :file_load (filepath)
		; (. buffer :file_load filepath) -> buffer
		(raise :syntax_engine :tab_width (buffer_line (list) buffer_syntax (list)
			 buffer_states (list) buffer_found (list) buffer_brackets (list)
			 max_width 0))
		(lines! (lambda (line)
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

	(defmethod :file_load_hex (filepath &optional width)
		; (. buffer :file_load_hex filepath [width]) -> buffer
		(raise :syntax_engine :tab_width (buffer_line (list) buffer_syntax (list)
			 buffer_states (list) buffer_found (list) buffer_brackets (list)
			 max_width 0 hex_stream (string-stream "")))
		(pipe-run (str "dump -c " (ifn width 8) " " filepath) (# (write-line hex_stream %0)))
		(lines! (lambda (line)
				(task-slice)
				(setq line (expand (trim-end line (const (char +char_cr))) tab_width)
					max_width (max max_width (length line)))
				(push-line line))
			(string-stream (str hex_stream)))
		(last-line)
		(clear (get :undo_stack this) (get :redo_stack this))
		(lower :buffer_line :buffer_syntax :buffer_states :buffer_found :buffer_brackets
			:max_width (:cursor_x 0 :cursor_y 0 :dirty_flags -1 :modified :nil))
		this)

	(defmethod :file_save (filepath)
		; (. buffer :file_save filepath) -> buffer
		(raise :syntax_engine :tab_width (stream (file-stream filepath +file_open_write)))
		(each! (lambda (line)
				(task-slice)
				(write-line-lf stream (. syntax_engine :compress_tabs line tab_width)))
			(list (get :buffer_line this)) 0 (bind '(w h) (. this :get_size)))
		(lower (:modified :nil)) this)

	(defmethod :vdu_load (vdu scroll_x scroll_y &optional end_state)
		; (. buffer :vdu_load vdu scroll_x scroll_y [end_state]) -> buffer
		(. vdu :load (get :buffer_syntax (build-syntax this end_state))
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
			((> x (length (elem-get buffer_line y)))
				(setq x (length (elem-get buffer_line y)))))
		(list x y))

	(defmethod :left ()
		; (. buffer :left) -> buffer
		(bind '(x y) (. this :get_cursor))
		(if (< (-- x) 0)
			(if (< (-- y) 0)
				(setq x 0)
				(setq x +max_int)))
		(bind '(x y) (. this :constrain x y))
		(. this :set_cursor x y))

	(defmethod :right ()
		; (. buffer :right) -> buffer
		(bind '(x y) (. this :get_cursor))
		(bind '(w h) (. this :get_size))
		(defq w (if (> y h) 0 (length (elem-get (get :buffer_line this) y))))
		(if (> (++ x) w)
			(if (> (++ y) h)
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
			(defq front (slice (elem-get buffer_line y) 0 x)
				back (slice (elem-get buffer_line y) x -1))
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
		; (. buffer :backspace [num]) -> buffer
		(mutate-buffer
			(setd num 1)
			(cond
				((and (<= x 0) (<= y 0)))
				((= x 0)
					(setq y (dec y) x (length (elem-get buffer_line y)))
					(set-line y (cat (elem-get buffer_line y) (elem-get buffer_line (inc y))))
					(erase-line (inc y))
					(. this :push_undo (list :break x y)))
				(:t (defq undo (slice (elem-get buffer_line y) (- x num) x))
					(set-line y (erase (elem-get buffer_line y) (- x num) x))
					(-- x num)
					(. this :push_undo (list :insert x y undo))))))

	(defmethod :delete (&optional num)
		; (. buffer :delete [num]) -> buffer
		(mutate-buffer
			(setd num 1)
			(cond
				((>= y h))
				((>= x (length (elem-get buffer_line y)))
					(set-line y (cat (elem-get buffer_line y) (elem-get buffer_line (inc y))))
					(erase-line (inc y))
					(. this :push_undo (list :break x y)))
				(:t (defq undo (slice (elem-get buffer_line y) x (+ x num)))
					(set-line y (erase (elem-get buffer_line y) x (+ x num)))
					(. this :push_undo (list :insert x y undo))))))

	(defmethod :insert (string)
		; (. buffer :insert string) -> buffer
		(if (eql string "") this
			(mutate-buffer
				(set-line y (insert (elem-get buffer_line y) x string))
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
					(push lines (slice (elem-get buffer_line sy) sx sx1))
					(. this :delete (- sx1 sx)))
				(:t (while (<= sy (setq sy1 (dec sy1)))
						(task-slice)
						(push lines (slice (elem-get buffer_line sy) sx -1) (ascii-char +char_lf))
						(if (> (defq gap (- (length (elem-get buffer_line sy)) sx)) 0)
							(. this :delete gap))
						(. this :delete)
						(raise :buffer_line))
					(when (and (> sx1 0) (< sy (bind '(w h) (. this :get_size))))
						(push lines (slice (elem-get buffer_line sy) sx (+ sx sx1)))
						(. this :delete sx1))))
			(. this :push_undo (list :mark mark))
			(apply (const cat) lines)))

	(defmethod :copy (anchor_x anchor_y)
		; (. buffer :copy anchor_x anchor_y) -> string
		(select-buffer
			(raise :buffer_line)
			(cond
				((= sy sy1)
					(slice (elem-get buffer_line sy) sx sx1))
				(:t (defq lines
						(list (slice (elem-get buffer_line sy) sx -1) (ascii-char +char_lf)))
					(while (< (++ sy) sy1)
						(push lines (elem-get buffer_line sy) (ascii-char +char_lf)))
					(if (< sy h)
						(push lines (slice (elem-get buffer_line sy) 0 sx1)))
					(apply (const cat) lines)))))

	(defmethod :paste (string &optional wrap_width)
		; (. buffer :paste string [wrap_width]) -> buffer
		(setd wrap_width +max_int)
		(. this :push_undo (list :mark (defq mark (. this :next_mark))))
		(defq ss (string-stream string) line (read-line ss))
		(lines! (# (insert-line-wrapped this line wrap_width)
			(. this :break) (setq line %0)) ss)
		(when line
			(insert-line-wrapped this line wrap_width)
			(if (eql (last string) (ascii-char 10)) (. this :break)))
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
				(unless (eql "" (defq line (elem-get buffer_line y)))
					(cond
						((<= x 0) (setq x 0))
						((>= x (length line)) (setq x (dec (length line))))
						((eql ")" (elem-get line x)))
						((eql "(" (elem-get line x)))
						((-- x)))
					(if (eql ")" (elem-get line x))
						(-- x)))
				(setq y (some! (lambda (syntax brackets)
					(defq oc c)
					(cond
						((< (setq c (+ c (cond
								((= x -1) brackets)
								(:t (defq brackets 0)
									(each! (# (setq brackets (+ brackets (cond
										((/= (logand 0xffffff000000 %0) ink_text) 0)
										((eql (logand 0xff %0) (ascii-code "(")) -1)
										((eql (logand 0xff %0) (ascii-code ")")) 1)
										(0))))) (list syntax) (min (inc x) (length syntax)) 0) brackets)))) 0)
							(setq x (some! (# (if (< (setq oc (+ oc (cond
									((/= (logand 0xffffff000000 %0) ink_text) 0)
									((eql (logand 0xff %0) (ascii-code "(")) -1)
									((eql (logand 0xff %0) (ascii-code ")")) 1)
									(0)))) 0) (!)))
								(list syntax) :nil (if (= x -1) (length syntax) (min (inc x) (length syntax))) 0)) (!))
						(:t (setq x -1) :nil)))
					(list buffer_syntax buffer_brackets) :nil (inc y) 0))
				(if (and x y) (list x y) '(:nil :nil)))
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
				(unless (eql "" (defq line (elem-get buffer_line y)))
					(cond
						((<= x 0) (setq x 0))
						((>= x (length line)) (setq x (dec (length line))))
						((eql ")" (elem-get line x)))
						((eql "(" (elem-get line x)))
						((-- x)))
					(if (eql "(" (elem-get line x))
						(++ x)))
				(setq y (some! (lambda (syntax brackets)
					(defq oc c)
					(cond
						((> (setq c (+ c (cond
								((= x 0) brackets)
								(:t (defq brackets 0)
									(each! (# (setq brackets (+ brackets (cond
										((/= (logand 0xffffff000000 %0) ink_text) 0)
										((eql (logand 0xff %0) (ascii-code "(")) -1)
										((eql (logand 0xff %0) (ascii-code ")")) 1)
										(0))))) (list syntax) x) brackets)))) 0)
							(setq x (some! (# (if (> (setq oc (+ oc (cond
								((/= (logand 0xffffff000000 %0) ink_text) 0)
								((eql (logand 0xff %0) (ascii-code "(")) -1)
								((eql (logand 0xff %0) (ascii-code ")")) 1)
								(0)))) 0) (!))) (list syntax) :nil x)) (!))
						(:t (setq x 0) :nil)))
					(list buffer_syntax buffer_brackets) :nil y h))
				(if (and x y) (list x y) '(:nil :nil)))
			('(:nil :nil))))

	(defmethod :find (pattern wmode rmode)
		; (. buffer :find pattern wmode rmode) -> buffer_found
		(raise :last_key :buffer_found)
		(unless (eql (defq key (str (list wmode rmode pattern))) last_key)
			(lower (:last_key key))
			(each (lambda (__) (elem-set buffer_found (!) :nil)) buffer_found))
		(unless (eql pattern "")
			(bind '(engine pattern meta) (query pattern wmode rmode))
			(each (lambda (line index)
				(if (> (length line) 0)
					(unless index
						(task-slice)
						(elem-set buffer_found (!)
							(. engine :search line pattern meta)))
					(elem-set buffer_found (!) '())))
				(get :buffer_line this) buffer_found))
		buffer_found)
	)

;module
(export-classes '(Buffer))
(env-pop)

;;;;;;;;;;;;;;;;;;;
; text buffer class
;;;;;;;;;;;;;;;;;;;

(import "./syntax.inc")

;module
(env-push)

;;;;;;;;;;;;;;
; line editing
;;;;;;;;;;;;;;

(defun push-line (line)
	(each (# (push %0 %1))
		(list buffer_lines buffer_syntax buffer_states
			buffer_found buffer_brackets buffer_index)
		`(,line ,line :nil :nil :nil :nil)))

(defun set-line (y line)
	(each (# (elem-set %0 y %1))
		(list buffer_lines buffer_syntax buffer_states
			buffer_found buffer_brackets buffer_index)
		`(,line ,line :nil :nil :nil :nil)))

(defun last-line ()
	(when (or (= (length buffer_lines) 0) (nql (last buffer_lines) +char_class_lf))
		(push-line +char_class_lf)))

(defun erase-line (y)
	(defq y1 (inc y))
	(each (# (set (penv) %0 (erase (eval %0) y y1)))
		'(buffer_lines buffer_syntax buffer_states
			buffer_found buffer_brackets buffer_index)))

(defun insert-line (y line)
	(each (# (set (penv) %0 (insert (eval %0) y (list %1))))
		'(buffer_lines buffer_syntax buffer_states
			buffer_found buffer_brackets buffer_index)
		`(,line ,line :nil :nil :nil :nil)))

(defmacro mutate-buffer (&rest _)
	(static-qq (progn
		(raise :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets :buffer_index)
		~_
		(clear (get :redo_stack this))
		(last-line)
		(lower :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets :buffer_index
			(:dirty_flags -1 :modified :t)))))

;;;;;;
; undo
;;;;;;

(defmacro undoable (&rest _)
	`(progn
		(. this :push_undo
			(list :mark (defq mark (. this :next_mark)))
			(list :cursors (. this :get_cursors)))
		~_
		(. this :push_undo (list :mark mark))))

(defun redo ()
	(defq idx (length undo_stack))
	(case state
		(:mark (push undo_stack (list :mark _0)))
		(:insert (. this :iinsert _0 _1))
		(:delete (. this :idelete _0 _1))
		(:cursors
			(push undo_stack (list :cursors _0))
			(. this :set_cursors _0)))
	idx)

(defun undo ()
	(defq idx (redo))
	(until (= idx (length undo_stack))
		(push redo_stack (pop undo_stack))))

;;;;;;;;;;;;;;;;;
; cursor morphing
;;;;;;;;;;;;;;;;;

(defun csr-sort-idx (idx)
	(bind '(ci ai sx) idx)
	(if (<= ci ai) idx (list ai ci sx)))

(defun csr-sort-csr (csr)
	(bind '((sx sy) (sx1 sy1)) csr)
	(cond
		((> sy sy1)
			(if (and (= sy sy1) (> sx1 sx))
				`((,sx ,sy1) (,sx1 ,sy))
				`((,sx1 ,sy1) (,sx ,sy))))
		((and (= sy sy1) (> sx sx1))
			`((,sx1 ,sy) (,sx ,sy1)))
		(csr)))

(defun csr-floor (csr)
	(setq csr (csr-sort-csr csr))
	(bind '((sx sy) (sx1 sy1)) csr)
	(cond
		((= sy sy1) (static-qq ((,sx ,sy) (,sx1 ,(inc sy1)))))
		((/= 0 sx1) (static-qq ((,sx ,sy) (,sx1 ,(inc sy1)))))
		(csr)))

;;;;;;;;;;;
; meta data
;;;;;;;;;;;

(bits +dirty_flag 0
	(bit syntax bracket index))

(defun build-syntax (this &optional estate)
	(raise :dirty_flags)
	(when (bits? dirty_flags +dirty_flag_syntax)
		(raise :buffer_lines :buffer_syntax :buffer_states
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
			buffer_lines buffer_syntax buffer_states)
		(lower :max_width (:dirty_flags (logand dirty_flags
			(const (lognot +dirty_flag_syntax))))))
	this)

(defun build-brackets (this)
	(raise :dirty_flags)
	(when (bits? dirty_flags +dirty_flag_bracket)
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

(defun build-index (this)
	(raise :dirty_flags)
	(when (bits? dirty_flags +dirty_flag_index)
		(raise :buffer_lines :buffer_index (idx 0))
		(each (# (elem-set buffer_index (!) idx) (++ idx (length %0))) buffer_lines)
		(lower (:dirty_flags (logand dirty_flags (const (lognot +dirty_flag_index))))))
	this)

;;;;;;;
; class
;;;;;;;

(defclass Buffer (&optional mode syntax) :nil
	; (Buffer [mode syntax]) -> buffer
	(def this :buffer_lines (list +char_class_lf) :buffer_syntax (list +char_class_lf)
		:buffer_states (list :nil) :buffer_brackets (list :nil)
		:buffer_found (list :nil) :buffer_index (list 0) :dirty_flags -1
		:syntax_engine (ifn syntax (Syntax)) :undo_stack (list) :redo_stack (list)
		:max_width 0 :tab_width 4 :wrap_width 80
		:mode mode :next_mark -1 :modified :nil :last_key :nil
		:cursors (list '(0 0 :nil)) :tcursors '())

;;;;;;;;;;;;;;;
; basic get/set
;;;;;;;;;;;;;;;

	(defsetmethod :cursors)
	(defsetmethod :tcursors)

	(defgetmethod :cursors)
	(defgetmethod :tcursors)
	(defgetmethod :modified)
	(defgetmethod :buffer_found)
	(defgetmethod :buffer_lines)
	(defgetmethod :syntax_engine)
	(defgetmethod :wrap_width)
	(defgetmethod :tab_width)

;;;;;;;;;;;;;;;;;;;
; cursor conversion
;;;;;;;;;;;;;;;;;;;

	(defmethod :index_to_cursor (idx)
		; (. buffer :index_to_cursor idx) -> (x y)
		; clips to min/max of buffer
		(build-index this)
		(raise :buffer_index :buffer_lines
			(i 0 j 0 l 0 k (length buffer_index)
			idx (min (max idx 0) (last buffer_index))))
		(while (< j k)
			(defq i (>> (+ j k) 1) l (elem-get buffer_index i))
			(if (<= l idx (+ l (length (elem-get buffer_lines i)) -1))
				(setq k -1)
				(if (> idx l) (setq j (inc i)) (setq k i))))
		(list (- idx l) i))

	(defmethod :cursor_to_index ((x y))
		; (. buffer :cursor_to_index csr) -> idx
		; clips to min/max of buffer
		(build-index this)
		(raise :buffer_index :buffer_lines)
		(defq h (dec (length buffer_index)))
		(cond
			((< y 0)
				(setq x 0 y 0))
			((>= y h)
				(setq x +max_int y h)))
		(+ (min (max x 0) (dec (length (elem-get buffer_lines y))))
			(elem-get buffer_index y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cursor merging
; sorts the lines but does NOT change
; the order of cursor elements !
; merged cursors reset any sticky x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defmethod :merge_cursors (cursors)
		; (. buffer :merge_cursors cursors) -> cursors
		(build-index this)
		(raise :buffer_index)
		(defq mi (last buffer_index)
			cursors (sort (map (# (bind '(ci ai sx) %0)
						(list (max (min ci mi) 0) (max (min ai mi) 0) sx)) cursors)
					(# (- (apply (const min) (most %0)) (apply (const min) (most %1))))))
		(reduce! (# (bind '(ci ai &ignore) (last %0))
				(bind '(ci1 ai1 &ignore) %1)
				(defq mci (min ci ai) mai (max ci ai)
					mci1 (min ci1 ai1) mai1 (max ci1 ai1))
				(if (or (> mai mci1)
						(and (= mci mai) (>= mai mci1))
						(and (= mci1 mai1) (>= mai mci1)))
					(if (<= ci ai)
						(elem-set %0 -2 (list ci (max mai mai1) :nil))
						(elem-set %0 -2 (list (max mai mai1) ai :nil)))
					(push %0 %1))
				%0)
			(list cursors) (slice cursors 0 1) 1 -1))

;;;;;;;;;;;;;;;;;
; cursor creation
;;;;;;;;;;;;;;;;;

	(defmethod :set_icursor (ci &optional ai)
		; (. buffer :set_icursor ci [ai]) -> buffer
		(setd ai ci)
		(lower (:cursors `((,ci ,ai :nil))))
		this)

	(defmethod :add_icursor (ci &optional ai)
		; (. buffer :add_icursor ci [ai]) -> buffer
		(setd ai ci)
		(raise :cursors)
		(setq cursors (. this :merge_cursors (push cursors `(,ci ,ai :nil))))
		(lower :cursors)
		this)

	(defmethod :set_cursor (cx cy &optional ax ay)
		; (. buffer :set_cursor cx cy [ax ay]) -> buffer
		(setd ax cx ay cy)
		(. this :set_icursor
			(. this :cursor_to_index `(,cx ,cy))
			(. this :cursor_to_index `(,ax ,ay))))

	(defmethod :add_cursor (cx cy &optional ax ay)
		; (. buffer :set_cursor cx cy [ax ay]) -> buffer
		(setd ax cx ay cy)
		(. this :add_icursor
			(. this :cursor_to_index `(,cx ,cy))
			(. this :cursor_to_index `(,ax ,ay))))

;;;;;;;;;;;;;;;;;;;
; cursor range info
;;;;;;;;;;;;;;;;;;;

	(defmethod :get_selected ()
		; (. buffer :get_selected) -> (((cx cy) (ax ay)) ...)
		(map (# (list (. this :index_to_cursor (first %0))
					(. this :index_to_cursor (second %0))))
			(sort (cat (get :cursors this) (get :tcursors this))
				(# (- (first %0) (first %1))))))

	(defmethod :get_selected_unsorted ()
		; (. buffer :get_selected_unsorted) -> ((cx cy ax ay) ...)
		(map (# (list (. this :index_to_cursor (first %0))
					(. this :index_to_cursor (second %0))))
			(cat (get :cursors this) (get :tcursors this))))

;;;;;;;;;;;;;;;;;;;;;
; basic text mutation
; undoable !
;;;;;;;;;;;;;;;;;;;;;

	(defmethod :idelete (si ei)
		; (. buffer :idelete si ei) -> buffer
		(build-index this) (task-slice)
		(raise :buffer_index :buffer_lines :cursors)
		(defq mi (last buffer_index) si (max (min si mi) 0) ei (max (min ei mi) 0))
		(when (/= si ei)
			(mutate-buffer
				(bind '(si ei &ignore) (csr-sort-idx (list si ei :nil)))
				(bind '(cx cy) (. this :index_to_cursor si))
				(bind '(ax ay) (. this :index_to_cursor ei))
				(. this :push_undo (list :insert si (. this :icopy si ei)))
				(cond
					((= cy ay)
						(set-line cy (erase (elem-get buffer_lines cy) cx ax)))
					(:t (defq d (slice (elem-get buffer_lines cy) 0 cx))
						(while (<= cy (-- ay)) (task-slice) (erase-line cy))
						(set-line cy (cat d (slice (elem-get buffer_lines cy) ax -1)))))
				(defq d (- ei si)))
			(lower (:cursors (. this :merge_cursors
				(map (lambda ((ci ai sx))
						(list (cond ((<= ci si) ci) ((>= ci ei) (- ci d)) (si))
							(cond ((<= ai si) ai) ((>= ai ei) (- ai d)) (si))
							sx)) cursors)))))
		this)

	(defmethod :iinsert (si text)
		; (. buffer :iinsert si text) -> buffer
		(build-index this) (task-slice)
		(raise :buffer_index :buffer_lines :cursors)
		(defq mi (last buffer_index) si (max (min si mi) 0))
		(when (nql text "")
			(mutate-buffer
				(. this :push_undo (list :delete si (+ si (length text))))
				(bind '(cx cy) (. this :index_to_cursor si))
				(defq line (elem-get buffer_lines cy) i 0 d (length text) breaks (list))
				(while (/= i d) (task-slice)
					(setq i (bskipn +char_class_lf text i))
					(if (/= i d) (push breaks (setq i (inc i)))))
				(set-line cy (defq line (cat (slice line 0 cx) text (slice line cx -1))))
				(when (nempty? breaks)
					(defq cy (inc cy) ei -1)
					(reach (# (task-slice) (insert-line cy (slice line (+ cx %0) ei))
						(setq ei (+ cx %0))) breaks)
					(set-line (dec cy) (slice line 0 ei))))
			(lower (:cursors (. this :merge_cursors
				(map (lambda ((ci ai sx))
						(cond
							((= ci ai)
								(list (cond ((>= ci si) (+ ci d)) (ci))
									(cond ((>= ai si) (+ ai d)) (ai))
									sx))
							((<= ci ai)
								(list (cond ((>= ci si) (+ ci d)) (ci))
									(cond ((> ai si) (+ ai d)) (ai))
									sx))
							((list (cond ((> ci si) (+ ci d)) (ci))
								(cond ((>= ai si) (+ ai d)) (ai))
								sx)))) cursors)))))
		this)

;;;;;;;;;;;;;;;;;
; basic text copy
;;;;;;;;;;;;;;;;;

	(defmethod :icopy (si ei)
		; (. buffer :icopy si ei) -> str
		(build-index this) (task-slice)
		(raise :buffer_index :buffer_lines)
		(defq mi (last buffer_index) text (list)
			si (max (min si mi) 0) ei (max (min ei mi) 0))
		(when (/= si ei)
			(bind '(si ei &ignore) (csr-sort-idx (list si ei :nil)))
			(bind '(cx cy) (. this :index_to_cursor si))
			(bind '(ax ay) (. this :index_to_cursor ei))
			(cond
				((= cy ay)
					(push text (slice (elem-get buffer_lines cy) cx ax)))
				(:t (push text (slice (elem-get buffer_lines cy) cx -1))
					(while (< (++ cy) ay) (task-slice)
						(push text (elem-get buffer_lines cy)))
					(push text (slice (elem-get buffer_lines cy) 0 ax)))))
		(if (empty? text) "" (apply (const cat) text)))

;;;;;;;;;;;;;;;;;;;
; cursor navigation
;;;;;;;;;;;;;;;;;;;

	(defmethod :left ()
		; (. buffer :left) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((ci ai &ignore))
					(list (setq ci (if (= ci ai) (dec ci) (min ci ai)))
						ci (first (. this :index_to_cursor ci))))
				(get :cursors this)))))
		this)

	(defmethod :right ()
		; (. buffer :right) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((ci ai &ignore))
					(list (setq ci (if (= ci ai) (inc ci) (max ci ai)))
						ci (first (. this :index_to_cursor ci))))
				(get :cursors this)))))
		this)

	(defmethod :up ()
		; (. buffer :up) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((ci ai sx))
					(bind '(x y) (. this :index_to_cursor (min ci ai)))
					(if (or (not sx) (/= ci ai)) (setq sx x))
					(list (setq ci (. this :cursor_to_index (list sx (dec y)))) ci sx))
				(get :cursors this)))))
		this)

	(defmethod :down ()
		; (. buffer :down) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((ci ai sx))
					(bind '(x y) (. this :index_to_cursor (max ci ai)))
					(if (or (not sx) (/= ci ai)) (setq sx x))
					(list (setq ci (. this :cursor_to_index (list sx (inc y)))) ci sx))
				(get :cursors this)))))
		this)

	(defmethod :home ()
		; (. buffer :home) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((ci &ignore))
					(list (setq ci (. this :cursor_to_index
						(list 0 (last (. this :index_to_cursor ci))))) ci 0))
				(get :cursors this)))))
		this)

	(defmethod :end ()
		; (. buffer :end) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((ci &ignore))
					(bind '(ci ci) (. this :index_to_cursor ci))
					(defq sx (dec (length (elem-get buffer_lines ci)))
						ci (. this :cursor_to_index (list sx ci)))
					(list ci ci sx))
				(get :cursors this)))))
		this)

	(defmethod :top ()
		; (. buffer :top) -> buffer
		(lower (:cursors `((0 0 0))))
		this)

	(defmethod :bottom ()
		; (. buffer :bottom) -> buffer
		(build-index this)
		(defq ci (last (get :buffer_index this)))
		(lower (:cursors `((,ci ,ci 0))))
		this)

;;;;;;;;;;;;;;;;;;
; cursor selection
;;;;;;;;;;;;;;;;;;

	(defmethod :left_select ()
		; (. buffer :left_select) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((ci ai &ignore))
					(list (-- ci) ai (first (. this :index_to_cursor ci))))
				(get :cursors this)))))
		this)

	(defmethod :right_select ()
		; (. buffer :right_select) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((ci ai &ignore))
					(list (++ ci) ai (first (. this :index_to_cursor ci))))
				(get :cursors this)))))
		this)

	(defmethod :up_select ()
		; (. buffer :up_select) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((ci ai sx))
					(bind '(x ci) (. this :index_to_cursor ci))
					(list (. this :cursor_to_index (list (ifn sx x) (dec ci))) ai (ifn sx x)))
				(get :cursors this)))))
		this)

	(defmethod :down_select ()
		; (. buffer :down_select) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((ci ai sx))
					(bind '(x ci) (. this :index_to_cursor ci))
					(list (. this :cursor_to_index (list (ifn sx x) (inc ci))) ai (ifn sx x)))
				(get :cursors this)))))
		this)

	(defmethod :home_select ()
		; (. buffer :home_select) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((ci ai &ignore))
					(list (. this :cursor_to_index
						(list 0 (last (. this :index_to_cursor ci)))) ai 0))
				(get :cursors this)))))
		this)

	(defmethod :end_select ()
		; (. buffer :end_select) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((ci ai &ignore))
					(bind '(ci ci) (. this :index_to_cursor ci))
					(defq sx (dec (length (elem-get buffer_lines ci))))
					(list (. this :cursor_to_index (list sx ci)) ai sx))
				(get :cursors this)))))
		this)

	(defmethod :top_select ()
		; (. buffer :top_select) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((ai ai &ignore)) (list 0 ai 0))
				(get :cursors this)))))
		this)

	(defmethod :bottom_select ()
		; (. buffer :bottom_select) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((ai ai &ignore)) (list +max_long ai 0))
				(get :cursors this)))))
		this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; multi cursor text mutation
; undoable !
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defmethod :delete ()
		; (. buffer :delete) -> buffer
		(undoable
			(reach (lambda ((ci ai &ignore))
					(if (= ci ai)
						(. this :idelete ci (inc ci))
						(. this :idelete ci ai)))
				(. this :merge_cursors (get :cursors this)))))

	(defmethod :backspace ()
		; (. buffer :backspace) -> buffer
		(undoable
			(reach (lambda ((ci ai &ignore))
					(if (= ci ai)
						(. this :idelete (dec ci) ci)
						(. this :idelete ci ai)))
				(. this :merge_cursors (get :cursors this)))))

	(defmethod :insert (text)
		; (. buffer :insert text) -> buffer
		(undoable
			(reach (lambda ((ci ai &ignore)) (. this :iinsert (min ci ai) text))
				(. this :merge_cursors (get :cursors this)))
			(reach (lambda ((ci ai &ignore)) (. this :idelete ci ai)) (get :cursors this))))

	(defmethod :copy ()
		; (. buffer :copy) -> text
		(defq text (list))
		(each (lambda ((ci ai &ignore)) (push text (. this :icopy ci ai)))
			(. this :merge_cursors (get :cursors this)))
		(join text (const (unescape "\f"))))

	(defmethod :cut ()
		; (. buffer :cut) -> text
		(undoable
			(defq text (list) cursors (. this :merge_cursors (get :cursors this)))
			(each (lambda ((ci ai &ignore)) (push text (. this :icopy ci ai))) cursors)
			(reach (lambda ((ci ai &ignore)) (. this :idelete ci ai)) cursors))
		(join text (const (unescape "\f"))))

	(defmethod :paste (text)
		; (. buffer :paste text) -> buffer
		(undoable
			(defq parts (split text (const (unescape "\f")))
				cursors (. this :merge_cursors (get :cursors this)))
			(cond
				((= (length parts) (length cursors))
					;matching number of parts !
					(reach (lambda ((ci ai &ignore) text) (. this :iinsert (min ci ai) text)) cursors parts)
					(reach (lambda ((ci ai &ignore)) (. this :idelete ci ai)) (get :cursors this)))
				(:t ;none matching parts !
					(defq parts (apply (const cat) (push parts "")))
					(reach (lambda ((ci ai &ignore)) (. this :iinsert (min ci ai) parts)) cursors)
					(reach (lambda ((ci ai &ignore)) (. this :idelete ci ai)) (get :cursors this))))))

;;;;;;;;;;;;;
; buffer info
;;;;;;;;;;;;;

	(defmethod :get_icursor ()
		; (. buffer :get_icursor) -> (ci ai)
		(most (last (get :cursors this))))

	(defmethod :get_cursor ()
		; (. buffer :get_cursor) -> (cx cy ax ay)
		(bind '(cx ax &ignore) (last (get :cursors this)))
		(bind '(cx cy) (. this :index_to_cursor cx))
		(bind '(ax ay) (. this :index_to_cursor ax))
		(list cx cy ax ay))

	(defmethod :get_size ()
		; (. buffer :get_size) -> (width height)
		(list (get :max_width this) (dec (length (get :buffer_lines this)))))

	(defmethod :get_text_line (y)
		; (. buffer :get_text_line y) -> line
		(elem-get (get :buffer_lines this) y))

;;;;;;;;;;;;;;;;;;;;
; file load and save
;;;;;;;;;;;;;;;;;;;;

	(defmethod :file_load (filepath)
		; (. buffer :file_load filepath) -> buffer
		(raise :syntax_engine :tab_width (buffer_lines (list) buffer_syntax (list)
			buffer_states (list) buffer_found (list) buffer_brackets (list)
			buffer_index (list) max_width 0))
		;clear them all now !
		(lower :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets :buffer_index)
		(lines! (lambda (line)
				(task-slice)
				(setq line (cat
						(expand (trim-end line (const (unescape "\r"))) tab_width)
						+char_class_lf)
					max_width (max max_width (length line)))
				(push-line line))
			(file-stream filepath))
		(last-line)
		(clear (get :undo_stack this) (get :redo_stack this))
		(lower :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets :buffer_index
			:max_width (:dirty_flags -1 :modified :nil))
		this)

	(defmethod :file_load_hex (filepath &optional width)
		; (. buffer :file_load_hex filepath [width]) -> buffer
		(raise :syntax_engine :tab_width (buffer_lines (list) buffer_syntax (list)
			buffer_states (list) buffer_found (list) buffer_brackets (list)
			buffer_index (list) max_width 0 width (ifn width 8)
			adr 0 stream (file-stream filepath)))
		;clear them all now !
		(lower :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets :buffer_index)
		(while (defq blk (read-blk stream width))
			(task-slice)
			(defq line (cat
					(int-to-hex-str adr) " "
					(join (partition (hex-encode blk) 2) " " 2)
					(pad "" (* 3 (- width (length blk))) "            ")
					(apply (const cat)
						(map (# (if (bfind %0 +char_class_printable) %0 ".")) blk))
					+char_class_lf)
				max_width (max max_width (length line))
				adr (+ adr width))
			(push-line line))
		(last-line)
		(clear (get :undo_stack this) (get :redo_stack this))
		(lower :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets :buffer_index
			:max_width (:dirty_flags -1 :modified :nil))
		this)

	(defmethod :file_save (filepath)
		; (. buffer :file_save filepath) -> buffer
		(raise :syntax_engine :tab_width (stream (file-stream filepath +file_open_write)))
		(each! (lambda (line)
				(task-slice)
				(write-blk stream (. syntax_engine :compress_tabs line tab_width)))
			(list (get :buffer_lines this)) 0 (bind '(w h) (. this :get_size)))
		(lower (:modified :nil)) this)

;;;;;;;;;;;;;;
; vdu transfer
;;;;;;;;;;;;;;

	(defmethod :vdu_load (vdu scroll_x scroll_y &optional end_state)
		; (. buffer :vdu_load vdu scroll_x scroll_y [end_state]) -> buffer
		(bind '(cx cy &ignore) (. this :get_cursor))
		(. vdu :load (get :buffer_syntax (build-syntax this end_state))
			scroll_x scroll_y cx cy)
		this)

;;;;;;;;;;;;
; undo stack
;;;;;;;;;;;;

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

	(defmethod :undo ()
		; (. buffer :undo) -> buffer
		(when (defq data (pop (defq undo_stack (get :undo_stack this))))
			(defq redo_stack (cat (get :redo_stack this)))
			(bind '(state &optional _0 _1) data)
			(cond
				((eql state :mark)
					(undo)
					(defq outer _0 _0 -1)
					(until (and (eql state :mark) (= _0 outer))
						(task-slice)
						(bind '(state &optional _0 _1) (pop undo_stack))
						(undo)))
				(:t (undo)))
			(set this :redo_stack redo_stack))
		this)

	(defmethod :redo ()
		; (. buffer :redo) -> buffer
		(when (defq data (pop (defq redo_stack (get :redo_stack this))))
			(defq undo_stack (get :undo_stack this) redo_stack (cat redo_stack))
			(bind '(state &optional _0 _1) data)
			(cond
				((eql state :mark)
					(redo)
					(defq outer _0 _0 -1)
					(until (and (eql state :mark) (= _0 outer))
						(task-slice)
						(bind '(state &optional _0 _1) (pop redo_stack))
						(redo)))
				(:t (redo)))
			(set this :redo_stack redo_stack))
		this)

;;;;;;;;;;;;;;;;;;
; bracket matching
;;;;;;;;;;;;;;;;;;

	(defmethod :left_bracket ()
		; (. buffer :left_bracket) -> (x y) | (:nil :nil)
		(cond
			((get :mode this)
				(bind '(x y &ignore) (. (build-brackets this) :get_cursor))
				(raise :buffer_syntax :buffer_brackets :buffer_lines
					(c 0 ink_text (get :ink_text (get :syntax_engine this))))
				(unless (eql "" (defq line (elem-get buffer_lines y)))
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
								((= x -2) brackets)
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
								(list syntax) :nil (if (= x -2) (length syntax) (min (inc x) (length syntax))) 0)) (!))
						(:t (setq x -2) :nil)))
					(list buffer_syntax buffer_brackets) :nil (inc y) 0))
				(if (and x y) (list x y) '(:nil :nil)))
			('(:nil :nil))))

	(defmethod :right_bracket ()
		; (. buffer :right_bracket) -> (x y) | (:nil :nil)
		(cond
			((get :mode this)
				(bind '(x y &ignore) (. (build-brackets this) :get_cursor))
				(raise :buffer_syntax :buffer_brackets :buffer_lines
					(c 0 h (length buffer_lines)
					ink_text (get :ink_text (get :syntax_engine this))))
				(unless (eql "" (defq line (elem-get buffer_lines y)))
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

;;;;;;;;;;;;;;;;
; pattern search
;;;;;;;;;;;;;;;;

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
				(get :buffer_lines this) buffer_found))
		buffer_found)
	)

;module
(export-classes '(Buffer))
(export-symbols '(csr-sort-idx csr-sort-csr csr-floor))
(env-pop)

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
			buffer_found buffer_brackets)
		`(,line ,line :nil :nil :nil)))

(defun set-line (y line)
	(each (# (elem-set %0 y %1))
		(list buffer_lines buffer_syntax buffer_states
			buffer_found buffer_brackets)
		`(,line ,line :nil :nil :nil)))

(defun last-line ()
	(when (or (= (length buffer_lines) 0) (nql (last buffer_lines) +char_class_lf))
		(push-line +char_class_lf)))

(defun erase-line (y)
	(defq y1 (inc y))
	(each (# (set (penv) %0 (erase (eval %0) y y1)))
		'(buffer_lines buffer_syntax buffer_states
			buffer_found buffer_brackets)))

(defun insert-line (y line)
	(each (# (set (penv) %0 (insert (eval %0) y (list %1))))
		'(buffer_lines buffer_syntax buffer_states
			buffer_found buffer_brackets)
		`(,line ,line :nil :nil :nil)))

(defmacro mutate-buffer (&rest _)
	(static-qq (progn
		(raise :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets)
		~_
		(clear (get :redo_stack this))
		(last-line)
		(lower :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets
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
		(:insert (. this :iinsert _0 _1 _2))
		(:delete (. this :idelete _0 _1 _2 _3))
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

(defun csr-sort (csr)
	; sort cursor so (cx cy) <= (ax ay)
	(bind '(cx cy ax ay &ignore) csr)
	(if (or (< cy ay) (and (= cy ay) (<= cx ax)))
		(list cx cy ax ay)
		(list ax ay cx cy)))

(defun csr-floor (csr)
	; floor cursor to line boundaries, avoiding object churn if already floored
	(bind '(cx cy ax ay) (defq csr (csr-sort csr)))
	(cond
		; same line - extend to full line
		((= cy ay)
			(if (and (= cx 0) (= ay (inc cy)))
				csr
				(list 0 cy 0 (inc ay))))
		; multi-line with ax not at start - extend
		((/= 0 ax) (list cx cy 0 (inc ay)))
		; already floored
		(csr)))

;;;;;;;;;;;
; meta data
;;;;;;;;;;;

(bits +dirty_flag 0
	(bit syntax bracket))

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

;;;;;;;
; class
;;;;;;;

(defclass Buffer (&optional mode syntax) :nil
	; (Buffer [mode syntax]) -> buffer
	(def this :buffer_lines (list +char_class_lf) :buffer_syntax (list +char_class_lf)
		:buffer_states (list :nil) :buffer_brackets (list :nil)
		:buffer_found (list :nil) :dirty_flags -1
		:syntax_engine (ifn syntax (Syntax)) :undo_stack (list) :redo_stack (list)
		:max_width 0 :tab_width 4 :wrap_width 80
		:mode mode :next_mark -1 :modified :nil :last_key :nil
		:cursors (list '(0 0 0 0 :nil)) :tcursors '())

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

;;;;;;;;;;;;;;;;;;
; cursor clipping
;;;;;;;;;;;;;;;;;;

	(defmethod :clip_cursor (cx cy ax ay &optional sx)
		; (. buffer :clip_cursor cx cy ax ay [sx]) -> (cx cy ax ay sx)
		; clips coordinates to valid buffer positions
		(raise :buffer_lines)
		(defq h (dec (length buffer_lines)))
		; clip cy
		(list
			(cond
				((< cy 0) (setq cy 0))
				((> cy h) (setq cy h) (dec (length (elem-get buffer_lines h))))
				((min (max cx 0) (dec (length (elem-get buffer_lines cy))))))
			cy
			(cond
				((< ay 0) (setq ay 0))
				((> ay h) (setq ay h) (dec (length (elem-get buffer_lines h))))
				((min (max ax 0) (dec (length (elem-get buffer_lines ay))))))
			ay
			sx))

;;;;;;;;;;;;;;;;;
; cursor creation
;;;;;;;;;;;;;;;;;

	(defmethod :set_cursor (cx cy &optional ax ay)
		; (. buffer :set_cursor cx cy [ax ay]) -> buffer
		(setd ax cx ay cy)
		(lower (:cursors (list (. this :clip_cursor cx cy ax ay))))
		this)

	(defmethod :add_cursor (cx cy &optional ax ay)
		; (. buffer :add_cursor cx cy [ax ay]) -> buffer
		(setd ax cx ay cy)
		(lower (:cursors (. this :merge_cursors
			(push (get :cursors this) (. this :clip_cursor cx cy ax ay)))))
		this)

;;;;;;;;;;;;;
; buffer info
;;;;;;;;;;;;;

	(defmethod :get_cursor ()
		; (. buffer :get_cursor) -> (cx cy ax ay)
		; return last cursor position without sticky x
		(most (last (get :cursors this))))

	(defmethod :get_size ()
		; (. buffer :get_size) -> (width height)
		(list (get :max_width this) (dec (length (get :buffer_lines this)))))

	(defmethod :get_text_line (y)
		; (. buffer :get_text_line y) -> line
		(elem-get (get :buffer_lines this) y))

;;;;;;;;;;;;;;;;;;;
; cursor range info
;;;;;;;;;;;;;;;;;;;

	(defmethod :get_selected ()
		; (. buffer :get_selected) -> ((cx cy ax ay) ...)
		; return cursors sorted by position, without sx
		(map (# (task-slice) (most %0))
			(sort (cat (get :cursors this) (get :tcursors this))
				(# (bind '(cx1 cy1 &ignore) (csr-sort %0))
					(bind '(cx2 cy2 &ignore) (csr-sort %1))
					(if (/= cy1 cy2) (- cy1 cy2) (- cx1 cx2))))))

	(defmethod :get_selected_unsorted ()
		; (. buffer :get_selected_unsorted) -> ((cx cy ax ay) ...)
		; return cursors in original order, without sx
		(map (# (task-slice) (most %0))
			(cat (get :cursors this) (get :tcursors this))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cursor merging
; sorts the lines but does NOT change
; the order of cursor elements !
; merged cursors reset any sticky x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defmethod :merge_cursors (cursors)
		; (. buffer :merge_cursors cursors) -> cursors
		; cursors format: ((cx cy ax ay sx) ...)
		(raise :buffer_lines)
		(defq cursors (sort (map (# (task-slice)
				(bind '(cx cy ax ay sx) %0)
				; clip to valid positions
				(. this :clip_cursor cx cy ax ay sx)) cursors)
					; sort by min(cy, ay) then min(cx, ax)
					(# (task-slice)
						(bind '(cx1 cy1 ax1 ay1) (csr-sort %0))
						(bind '(cx2 cy2 ax2 ay2) (csr-sort %1))
						(if (/= cy1 cy2) (- cy1 cy2) (- cx1 cx2)))))
		(reduce! (# (task-slice)
				(bind '(cx cy ax ay) (csr-sort (last %0)))
				(bind '(cx1 cy1 ax1 ay1) (csr-sort %1))
				; check overlap: last cursor end >= next cursor start
				; touching cursors are only merged if one of them is empty (point cursor)
				(cond
					((or (> ay cy1)
						(and (= ay cy1)
							(or (> ax cx1)
								(and (= ax cx1)
									(or (and (= cx ax) (= cy ay))
										(and (= cx1 ax1) (= cy1 ay1)))))))
						; MERGE: extend last cursor to cover both
						(bind '(ocx ocy oax oay &ignore) (last %0))
						(bind '(ncx ncy nax nay &ignore) %1)
						; determine direction: if old was point, use new direction, else keep old
						(defq fwd (if (and (= ocx oax) (= ocy oay))
								(or (> ncy nay) (and (= ncy nay) (> ncx nax)))
								(or (> ocy oay) (and (= ocy oay) (> ocx oax))))
							; determine max end point
							use_new (or (> ay1 ay) (and (= ay1 ay) (> ax1 ax)))
							ex (if use_new ax1 ax) ey (if use_new ay1 ay))
						; update cursor preserving start/end anchor relationship
						(elem-set %0 -2 (if fwd
							(list ex ey cx cy :nil)
							(list cx cy ex ey :nil))))
					(; NO OVERLAP: push new cursor
						(push %0 %1)))
				%0)
			(list cursors) (slice cursors 0 1) 1))

;;;;;;;;;;;;;;;;;;;;;
; basic text mutation
; undoable !
;;;;;;;;;;;;;;;;;;;;;

	(defmethod :idelete (cx cy ax ay)
		; (. buffer :idelete cx cy ax ay) -> buffer
		; delete text from (cx cy) to (ax ay)
		(task-slice)
		(raise :buffer_lines :cursors)
		(bind '(cx cy ax ay &ignore) (. this :clip_cursor cx cy ax ay))
		(when (or (/= cx ax) (/= cy ay))
			; sort so (cx cy) <= (ax ay)
			(bind '(cx cy ax ay) (csr-sort (list cx cy ax ay)))
			(defq dcy (- ay cy) dcx (- ax cx))
			(mutate-buffer
				(. this :push_undo (list :insert cx cy (. this :icopy cx cy ax ay)))
				(cond
					((= cy ay)
						(set-line cy (erase (elem-get buffer_lines cy) cx ax)))
					(:t (defq d (slice (elem-get buffer_lines cy) 0 cx))
						(defq i ay)
						(while (<= cy (-- i)) (task-slice) (erase-line cy))
						(set-line cy (cat d (slice (elem-get buffer_lines cy) ax -1))))))
			; update cursor positions after delete
			(lower (:cursors (. this :merge_cursors
				(map (lambda ((pcx pcy pax pay psx)) (task-slice)
						; adjust cursor position for deleted text
						(list
							; adjust pcx
							(cond
								((< pcy cy) pcx)
								((> pcy ay) pcx)
								((and (= pcy cy) (< pcx cx)) pcx)
								((= pcy cy ay) (cond ((<= pcx cx) pcx) ((>= pcx ax) (- pcx dcx)) (cx)))
								((= pcy ay) (cond ((>= pcx ax) (+ cx (- pcx ax))) (cx)))
								(cx))
							; adjust pcy
							(cond ((<= pcy cy) pcy) ((> pcy ay) (- pcy dcy)) (cy))
							; adjust pax
							(cond
								((< pay cy) pax)
								((> pay ay) pax)
								((and (= pay cy) (< pax cx)) pax)
								((= pay cy ay) (cond ((<= pax cx) pax) ((>= pax ax) (- pax dcx)) (cx)))
								((= pay ay) (cond ((>= pax ax) (+ cx (- pax ax))) (cx)))
								(cx))
							; adjust pay
							(cond ((<= pay cy) pay) ((> pay ay) (- pay dcy)) (cy))
							psx))
					cursors)))))
		this)

	(defmethod :iinsert (cx cy text)
		; (. buffer :iinsert cx cy text) -> buffer
		; insert text at position (cx cy)
		(task-slice)
		(raise :buffer_lines :cursors)
		(bind '(cx cy &ignore) (. this :clip_cursor cx cy cx cy))
		(when (nql text "")
			(mutate-buffer
				(defq line (elem-get buffer_lines cy) i 0 d (length text)
					breaks (list) icy cy icx cx)
				(while (/= i d) (task-slice)
					(setq i (bskipn +char_class_lf text i))
					(if (/= i d) (push breaks (setq i (inc i)))))
				(defq nlines (length breaks))
				; calculate end position after insert
				(defq ecx (if (= nlines 0) (+ icx d) (- d (last breaks)))
					ecy (+ icy nlines))
				(. this :push_undo (list :delete cx cy ecx ecy))
				(set-line cy (defq line (cat (slice line 0 cx) text (slice line cx -1))))
				(when (nempty? breaks)
					(defq cy (inc cy) ei -1)
					(reach (# (task-slice) (insert-line cy (slice line (+ cx %0) ei))
						(setq ei (+ cx %0))) breaks)
					(set-line (dec cy) (slice line 0 ei))))
			; update cursor positions after insert
			(lower (:cursors (. this :merge_cursors
				(map (lambda ((pcx pcy pax pay psx)) (task-slice)
						; adjust cursor position for inserted text
						(list
							; adjust pcx
							(cond
								((< pcy icy) pcx)
								((> pcy icy) pcx)
								((< pcx icx) pcx)
								; At insertion point/line: check if this is the end of a selection
								((and (= pcx icx)
										(or (> pcy pay)
											(and (= pcy pay) (> pcx pax))))
									pcx)
								((= nlines 0) (+ pcx d))
								((+ ecx (- pcx icx))))
							; adjust pcy
							(cond
								((< pcy icy) pcy)
								((> pcy icy) (+ pcy nlines))
								((< pcx icx) pcy)
								; At insertion point/line: check if this is the end of a selection
								((and (= pcx icx)
										(or (> pcy pay)
											(and (= pcy pay) (> pcx pax))))
									pcy)
								((+ pcy nlines)))
							; adjust pax
							(cond
								((< pay icy) pax)
								((> pay icy) pax)
								((< pax icx) pax)
								; At insertion point/line: check if this is the end of a selection
								((and (= pax icx)
										(or (> pay pcy)
											(and (= pay pcy) (> pax pcx))))
									pax)
								((= nlines 0) (+ pax d))
								((+ ecx (- pax icx))))
							; adjust pay
							(cond
								((< pay icy) pay)
								((> pay icy) (+ pay nlines))
								((< pax icx) pay)
								; At insertion point/line: check if this is the end of a selection
								((and (= pax icx)
										(or (> pay pcy)
											(and (= pay pcy) (> pax pcx))))
									pay)
								((+ pay nlines)))
							psx))
					cursors)))))
		this)

;;;;;;;;;;;;;;;;;
; basic text copy
;;;;;;;;;;;;;;;;;

	(defmethod :icopy (cx cy ax ay)
		; (. buffer :icopy cx cy ax ay) -> str
		; copy text from (cx cy) to (ax ay)
		(task-slice)
		(raise :buffer_lines)
		(bind '(cx cy ax ay &ignore) (. this :clip_cursor cx cy ax ay))
		(defq text (list))
		(when (or (/= cx ax) (/= cy ay))
			; sort so (cx cy) <= (ax ay)
			(bind '(cx cy ax ay) (csr-sort (list cx cy ax ay)))
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
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay &ignore)) (task-slice)
					; if no selection, move cursor left; else collapse to left edge
					(cond
						((and (= cx ax) (= cy ay))
							; move left one char
							(if (> cx 0)
								(list (dec cx) cy (dec cx) cy (dec cx))
								(if (> cy 0)
									(list (setq cx (dec (length (elem-get buffer_lines (setq cy (dec cy))))))
										cy cx cy cx)
									(list 0 0 0 0 0))))
						(:t ; collapse selection to start
							(bind '(cx cy &ignore) (csr-sort (list cx cy ax ay)))
							(list cx cy cx cy cx))))
				(get :cursors this)))))
		this)

	(defmethod :right ()
		; (. buffer :right) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay &ignore)) (task-slice)
					; if no selection, move cursor right; else collapse to right edge
					(cond
						((and (= cx ax) (= cy ay))
							; move right one char
							(setq ax (dec (length (elem-get buffer_lines cy))))
							(if (< cx ax)
								(list (inc cx) cy (inc cx) cy (inc cx))
								(if (< cy (dec (length buffer_lines)))
									(list 0 (inc cy) 0 (inc cy) 0)
									(list ax cy ax cy ax))))
						(:t ; collapse selection to end
							(bind '(ax ax ax ay) (csr-sort (list cx cy ax ay)))
							(list ax ay ax ay ax))))
				(get :cursors this)))))
		this)

	(defmethod :up ()
		; (. buffer :up) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay sx)) (task-slice)
					; move up to previous line, using sticky x
					(bind '(cx cy ax ay) (csr-sort (list cx cy ax ay)))
					(if (or (not sx) (or (/= cx ax) (/= cy ay))) (setq sx cx))
					(if (> cy 0)
						(list (setq cx (min sx (dec (length (elem-get buffer_lines (setq cy (dec cy)))))))
							cy cx cy sx)
						(list 0 0 0 0 sx)))
				(get :cursors this)))))
		this)

	(defmethod :down ()
		; (. buffer :down) -> buffer
		(raise :buffer_lines)
		(defq h (dec (length buffer_lines)))
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay sx)) (task-slice)
					; move down to next line, using sticky x
					(bind '(cx cy ax ay) (csr-sort (list cx cy ax ay)))
					(if (or (not sx) (or (/= cx ax) (/= cy ay))) (setq sx ax))
					(if (< ay h)
						(list (setq ax (min sx (dec (length (elem-get buffer_lines (setq ay (inc ay)))))))
							ay ax ay sx)
						(list (setq ax (dec (length (elem-get buffer_lines h)))) h ax h sx)))
				(get :cursors this)))))
		this)

	(defmethod :home ()
		; (. buffer :home) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay &ignore)) (task-slice)
					; move to start of current line
					(bind '(cx cy &ignore) (csr-sort (list cx cy ax ay)))
					(list 0 cy 0 cy 0))
				(get :cursors this)))))
		this)

	(defmethod :end ()
		; (. buffer :end) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay &ignore)) (task-slice)
					; move to end of current line
					(bind '(cx cy ax ay) (csr-sort (list cx cy ax ay)))
					(setq ax (dec (length (elem-get buffer_lines ay))))
					(list ax ay ax ay ax))
				(get :cursors this)))))
		this)

	(defmethod :top ()
		; (. buffer :top) -> buffer
		(lower (:cursors `((0 0 0 0 0))))
		this)

	(defmethod :bottom ()
		; (. buffer :bottom) -> buffer
		(raise :buffer_lines)
		(defq h (dec (length buffer_lines))
			ex (dec (length (elem-get buffer_lines h))))
		(lower (:cursors `((,ex ,h ,ex ,h 0))))
		this)

;;;;;;;;;;;;;;;;;;
; cursor selection
;;;;;;;;;;;;;;;;;;

	(defmethod :left_select ()
		; (. buffer :left_select) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay &ignore)) (task-slice)
					; extend selection left
					(cond
						((> cx 0) (list (dec cx) cy ax ay (dec cx)))
						((> cy 0)
							(list (setq cx (dec (length (elem-get buffer_lines (setq cy (dec cy))))))
								cy ax ay cx))
						((list 0 0 ax ay 0))))
				(get :cursors this)))))
		this)

	(defmethod :right_select ()
		; (. buffer :right_select) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay sx)) (task-slice)
					; extend selection right
					(setq sx (dec (length (elem-get buffer_lines cy))))
					(cond
						((< cx sx) (list (inc cx) cy ax ay (inc cx)))
						((< cy (dec (length buffer_lines)))
							(list 0 (inc cy) ax ay 0))
						((list sx cy ax ay sx))))
				(get :cursors this)))))
		this)

	(defmethod :up_select ()
		; (. buffer :up_select) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay sx)) (task-slice)
					; extend selection up a line
					(if (not sx) (setq sx cx))
					(if (> cy 0)
						(list (setq cx (min sx (dec (length (elem-get buffer_lines (setq cy (dec cy)))))))
							cy ax ay sx)
						(list 0 0 ax ay sx)))
				(get :cursors this)))))
		this)

	(defmethod :down_select ()
		; (. buffer :down_select) -> buffer
		(raise :buffer_lines)
		(defq h (dec (length buffer_lines)))
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay sx)) (task-slice)
					; extend selection down a line
					(if (not sx) (setq sx cx))
					(if (< cy h)
						(list (setq cx (min sx (dec (length (elem-get buffer_lines (setq cy (inc cy)))))))
							cy ax ay sx)
						(list (setq cx (dec (length (elem-get buffer_lines h)))) h ax ay sx)))
				(get :cursors this)))))
		this)

	(defmethod :home_select ()
		; (. buffer :home_select) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay &ignore)) (task-slice)
					; extend selection to start of line
					(list 0 cy ax ay 0))
				(get :cursors this)))))
		this)

	(defmethod :end_select ()
		; (. buffer :end_select) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay &ignore)) (task-slice)
					; extend selection to end of line
					(setq cx (dec (length (elem-get buffer_lines cy))))
					(list cx cy ax ay cx))
				(get :cursors this)))))
		this)

	(defmethod :top_select ()
		; (. buffer :top_select) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay &ignore)) (task-slice)
					; extend selection to start of buffer
					(list 0 0 ax ay 0))
				(get :cursors this)))))
		this)

	(defmethod :bottom_select ()
		; (. buffer :bottom_select) -> buffer
		(raise :buffer_lines)
		(defq h (dec (length buffer_lines))
			ex (dec (length (elem-get buffer_lines h))))
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay &ignore)) (task-slice)
					; extend selection to end of buffer
					(list ex h ax ay 0))
				(get :cursors this)))))
		this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; multi cursor text mutation
; undoable !
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defmethod :delete ()
		; (. buffer :delete) -> buffer
		(raise :buffer_lines)
		(undoable
			(reach (lambda ((cx cy ax ay &ignore)) (task-slice)
					(when (and (= cx ax) (= cy ay))
						; no selection - move end point forward
						(defq line_len (dec (length (elem-get buffer_lines cy))))
						(cond
							((< cx line_len) (setq ax (inc cx)))
							((< cy (dec (length buffer_lines))) (setq ax 0 ay (inc cy)))))
					(. this :idelete cx cy ax ay))
				(. this :merge_cursors (get :cursors this)))))

	(defmethod :backspace ()
		; (. buffer :backspace) -> buffer
		(raise :buffer_lines)
		(undoable
			(reach (lambda ((cx cy ax ay &ignore)) (task-slice)
					(when (and (= cx ax) (= cy ay))
						; no selection - move start point back
						(cond
							((> cx 0) (setq cx (dec cx)))
							((> cy 0) (setq cy (dec cy) cx (dec (length (elem-get buffer_lines cy)))))))
					(. this :idelete cx cy ax ay))
				(. this :merge_cursors (get :cursors this)))))

	(defmethod :insert (text)
		; (. buffer :insert text) -> buffer
		(undoable
			(reach (lambda (csr) (task-slice)
					; insert at min position (sorted start)
					(bind '(cx cy &ignore) (csr-sort csr))
					(. this :iinsert cx cy text))
				(. this :merge_cursors (get :cursors this)))
			; delete any selections
			(reach (lambda ((cx cy ax ay &ignore)) (task-slice)
				(. this :idelete cx cy ax ay)) (get :cursors this))))

	(defmethod :copy ()
		; (. buffer :copy) -> text
		(defq text (list))
		(each (lambda ((cx cy ax ay &ignore)) (task-slice)
				(push text (. this :icopy cx cy ax ay)))
			(. this :merge_cursors (get :cursors this)))
		(join text (const (unescape "\f"))))

	(defmethod :cut ()
		; (. buffer :cut) -> text
		(undoable
			(defq text (list) cursors (. this :merge_cursors (get :cursors this)))
			(each (lambda ((cx cy ax ay &ignore)) (task-slice)
				(push text (. this :icopy cx cy ax ay))) cursors)
			(reach (lambda ((cx cy ax ay &ignore)) (task-slice)
				(. this :idelete cx cy ax ay)) cursors))
		(join text (const (unescape "\f"))))

	(defmethod :paste (text)
		; (. buffer :paste text) -> buffer
		(undoable
			(defq parts (split text (const (unescape "\f")))
				cursors (. this :merge_cursors (get :cursors this)))
			(cond
				((= (length parts) (length cursors))
					;matching number of parts !
					(reach (lambda (csr text) (task-slice)
						(bind '(cx cy &ignore) (csr-sort csr))
						(. this :iinsert cx cy text)) cursors parts)
					(reach (lambda ((cx cy ax ay &ignore)) (task-slice)
						(. this :idelete cx cy ax ay)) (get :cursors this)))
				(:t ;none matching parts !
					(defq parts (apply (const cat) (push parts "")))
					(reach (lambda (csr) (task-slice)
						(bind '(cx cy &ignore) (csr-sort csr))
						(. this :iinsert cx cy parts)) cursors)
					(reach (lambda ((cx cy ax ay &ignore)) (task-slice)
						(. this :idelete cx cy ax ay)) (get :cursors this))))))

;;;;;;;;;;;;;;;;;;;;
; file load and save
;;;;;;;;;;;;;;;;;;;;

	(defmethod :file_load (filepath)
		; (. buffer :file_load filepath) -> buffer
		(raise :syntax_engine :tab_width (buffer_lines (list) buffer_syntax (list)
			buffer_states (list) buffer_found (list) buffer_brackets (list)
			max_width 0))
		;clear them all now !
		(lower :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets)
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
			:buffer_found :buffer_brackets
			:max_width (:dirty_flags -1 :modified :nil))
		this)

	(defmethod :file_load_hex (filepath &optional width)
		; (. buffer :file_load_hex filepath [width]) -> buffer
		(raise :syntax_engine :tab_width (buffer_lines (list) buffer_syntax (list)
			buffer_states (list) buffer_found (list) buffer_brackets (list)
			max_width 0 width (ifn width 8)
			adr 0 stream (file-stream filepath)))
		;clear them all now !
		(lower :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets)
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
			:buffer_found :buffer_brackets
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
			(bind '(state &optional _0 _1 _2 _3) data)
			(cond
				((eql state :mark)
					(undo)
					(defq outer _0 _0 -1)
					(until (and (eql state :mark) (= _0 outer))
						(task-slice)
						(bind '(state &optional _0 _1 _2 _3) (pop undo_stack))
						(undo)))
				(:t (undo)))
			(set this :redo_stack redo_stack))
		this)

	(defmethod :redo ()
		; (. buffer :redo) -> buffer
		(when (defq data (pop (defq redo_stack (get :redo_stack this))))
			(defq undo_stack (get :undo_stack this) redo_stack (cat redo_stack))
			(bind '(state &optional _0 _1 _2 _3) data)
			(cond
				((eql state :mark)
					(redo)
					(defq outer _0 _0 -1)
					(until (and (eql state :mark) (= _0 outer))
						(task-slice)
						(bind '(state &optional _0 _1 _2 _3) (pop redo_stack))
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
(export-symbols '(csr-sort csr-floor))
(env-pop)
;;;;;;;;;;;;;;;;;;;
; text buffer class
;;;;;;;;;;;;;;;;;;;

(import "./syntax.inc")

;module
(env-push)

;;;;;;;;;;;;;;;
; control flags
;;;;;;;;;;;;;;;

(bits +buffer_flag 0
	(bit syntax undo))

;;;;;;;;;;;;;;
; line editing
;;;;;;;;;;;;;;

(defun push-line (line)
	(each (# (push %0 %1))
		(list buffer_lines buffer_syntax buffer_states
			buffer_found buffer_brackets buffer_buckets)
		`(,line ,line :nil :nil :nil ,(list))))

(defun set-line (y line)
	(each (# (elem-set %0 y %1))
		(list buffer_lines buffer_syntax buffer_states
			buffer_found buffer_brackets buffer_buckets)
		`(,line ,line :nil :nil :nil ,(list))))

(defun last-line ()
	(when (or (= (length buffer_lines) 0) (nql (last buffer_lines) +char_class_lf))
		(push-line +char_class_lf)))

(defun erase-line (y)
	(defq y1 (inc y))
	(each (# (set (penv) %0 (erase (eval %0) y y1)))
		'(buffer_lines buffer_syntax buffer_states
			buffer_found buffer_brackets buffer_buckets)))

(defun insert-line (y line)
	(each (# (set (penv) %0 (insert (eval %0) y (list %1))))
		'(buffer_lines buffer_syntax buffer_states
			buffer_found buffer_brackets buffer_buckets)
		`(,line ,line :nil :nil :nil ,(list))))

(defmacro mutate-buffer (&rest _)
	(static-qq (progn
		(raise :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets :buffer_buckets)
		~_
		(clear (get :redo_stack this))
		(last-line)
		(lower :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets :buffer_buckets
			(:dirty_flags -1 :modified :t)))))

;;;;;;
; undo
;;;;;;

(defmacro undoable (buffer &rest _)
	`(progn
		(. ,buffer :push_undo
			(list :mark (defq mark (. ,buffer :next_mark)))
			(list :cursors (. ,buffer :get_cursors)))
		~_
		(. ,buffer :push_undo (list :mark mark))))

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

;;;;;;;;;;;;;;
; cursor utils
;;;;;;;;;;;;;;

(defun csr-sort (csr)
	; (csr-sort csr) -> csr
	; sort cursor so (cx cy) <= (ax ay)
	(bind '(cx cy ax ay &ignore) csr)
	(if (or (< cy ay) (and (= cy ay) (<= cx ax)))
		(nums cx cy ax ay)
		(nums ax ay cx cy)))

;native version
(ffi (path-to-absolute "./csr_sort") csr-sort)

(defun csr-map-delete (px py cx cy ax ay)
	; (csr-map-delete px py cx cy ax ay) -> (nx ny)
	; map a point (px py) against a deletion from (cx cy) to (ax ay)
	(defq dcx (- ax cx) dcy (- ay cy))
	(nums
		; adjust x
		(cond
			((< py cy) px)
			((> py ay) px)
			((and (= py cy) (< px cx)) px)
			((= py ay) (if (>= px ax) (- px dcx) cx))
			(cx))
		; adjust y
		(cond
			((<= py cy) py)
			((> py ay) (- py dcy))
			(cy))))

;native version
(ffi (path-to-absolute "./csr_map_delete") csr-map-delete)

(defun csr-map-insert (px py icx icy ecx ecy dy ax ay)
	; (csr-map-insert px py icx icy ecx ecy dy ax ay) -> (nx ny)
	; map a point (px py) against an insertion at (icx icy) ending at (ecx ecy)
	(cond
		((< py icy) (nums px py))
		((> py icy) (nums px (+ py dy)))
		((< px icx) (nums px py))
		((= px icx)
			(if (or (> py ay) (and (= py ay) (> px ax)))
				; end of selection stays put
				(nums px py)
				; point cursor or start of selection moves
				(nums ecx ecy)))
		((= dy 0) (nums (+ px (- ecx icx)) py))
		((nums (+ ecx (- px icx)) (+ py dy)))))

;native version
(ffi (path-to-absolute "./csr_map_insert") csr-map-insert)

(defmacro csr-sort-top (csr)
	; (csr-sort-top csr) -> (cx cy)
	; sort cursor so (cx cy) <= (ax ay) return (cx cy)
	`(slice (csr-sort ,csr) 0 2))

(defmacro csr-sort-bot (csr)
	; (csr-sort-bot csr) -> (ax ay)
	; sort cursor so (cx cy) <= (ax ay) return (ax ay)
	`(slice (csr-sort ,csr) 2 -1))

(defun csr-floor (csr)
	; (csr-floor csr) -> csr
	; floor cursor to line boundaries
	(bind '(cy cy ax ay) (defq csr (csr-sort csr)))
	(if (or (= cy ay) (/= 0 ax))
		(nums 0 cy 0 (inc ay))
		(nums 0 cy 0 ay)))

(defun csr-cmp (%0 %1)
	; (csr-cmp csr1 csr2) -> + 0 -
	(bind '(cx1 cy1) (csr-sort-top %0))
	(bind '(cx2 cy2) (csr-sort-top %1))
	(if (/= cy1 cy2) (- cy1 cy2) (- cx1 cx2)))

;native version
(ffi (path-to-absolute "./csr_cmp") csr-cmp)

(defun csr-within (csr1 csr2)
	; (csr-within csr1 csr2) -> :t | :nil
	; returns :t if csr2 is enclosed within (or equal to) csr1
	(bind '(x1 y1 x2 y2) (csr-sort csr1))
	(bind '(x3 y3 x4 y4) (csr-sort csr2))
	(and (or (> y3 y1) (and (= y3 y1) (>= x3 x1)))
		(or (< y4 y2) (and (= y4 y2) (<= x4 x2)))))

(defmacro csr-line-end (y)
	`(dec (length (elem-get buffer_lines ,y))))

(defun blank-line? (line)
	(eql (rbskip +char_class_white_space line -1) 0))

(defun bracket-cursors (cursors start_y end_y)
	; (bracket-cursors cursors start_y end_y) -> (si ei)
	; find range of cursors intersecting [start_y, end_y].
	; we bracket the range by including one extra cursor before and after
	; to ensure merges are handled correctly.
	(ifn (defq len (length cursors)
			si (some! (# (if (>= (last (csr-sort %0)) start_y) (!)))
					(list cursors)))
		; if no cursor intersects or is below, all are above.
		(list len len)
		(progn
			; find end index (ei): First cursor starting strictly after end_y.
			; optimization: scanning from si.
			(defq ei (some! (# (if (> (second (csr-sort %0)) end_y) (!)))
						(list cursors) :nil si))
			; bracket the range: include one neighbor on each side
			(list (max 0 (dec si)) (min len (inc (ifn ei len)))))))

;;;;;;;;;;;
; meta data
;;;;;;;;;;;

(bits +dirty_flag 0
	(bit syntax bracket))

(defun build-syntax (this &optional estate)
	(raise :dirty_flags)
	(when (bits? dirty_flags +dirty_flag_syntax)
		(raise :buffer_lines :buffer_syntax :buffer_states
			:flags :syntax_engine (state :text max_width 0))
		(each (lambda (line syntax states)
			(setq max_width (max max_width (length line)))
			(bind '(start_state end_state) (ifn states '(:nil :nil)))
			(cond
				((not (bits? flags +buffer_flag_syntax)))
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
		(when (bits? (get :flags this) +buffer_flag_syntax)
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

;;;;;;;;;;;;;;;;;;;
; cursor list sorts
;;;;;;;;;;;;;;;;;;;

(defun sort-bucket (this lst fcmp)
	(cond
		;nothing to do
		((<= (length lst) 1) lst)
		;trivial case just sort it and be dambed
		((< (length lst) 32) (sort lst fcmp))
		;ok roll out the buckets
		((defq buffer_buckets (get :buffer_buckets this))
		;fill the buckets
		(each (# (push (elem-get buffer_buckets (min (second %0) (elem-get %0 3))) %0)) lst)
		(clear lst)
		;scan buckets and sort each one, pushing onto result
		(each (# (reduce (const push) (sort %0 fcmp) lst) (clear %0))
			buffer_buckets) lst)))

;;;;;;;
; class
;;;;;;;

(defclass Buffer (&optional flags syntax) :nil
	; (Buffer [flags syntax]) -> buffer
	(setd flags 0 syntax (Syntax))
	(def this :buffer_lines (list +char_class_lf) :buffer_syntax (list +char_class_lf)
		:buffer_states (list :nil) :buffer_brackets (list :nil)
		:buffer_found (list :nil) :buffer_buckets (list (list)) :dirty_flags -1
		:syntax_engine syntax :undo_stack (list) :redo_stack (list)
		:max_width 0 :tab_width 4 :wrap_width 80
		:flags flags :next_mark -1 :modified :nil :last_key :nil
		:cursors (list (nums 0 0 0 0 -1)) :tcursors '())

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

	(defmethod :clip_cursor (cx cy &optional ax ay sx)
		; (. buffer :clip_cursor cx cy [ax ay sx]) -> (cx cy ax ay sx)
		; clips coordinates to valid buffer positions
		(raise :buffer_lines)
		(setd ax cx ay cy)
		(defq h (dec (length buffer_lines)))
		; clip cy
		(nums
			(cond
				((< cy 0) (setq cy 0))
				((> cy h) (setq cy h) (csr-line-end cy))
				((min (max cx 0) (csr-line-end cy))))
			cy
			(cond
				((< ay 0) (setq ay 0))
				((> ay h) (setq ay h) (csr-line-end ay))
				((min (max ax 0) (csr-line-end ay))))
			ay (ifn sx -1)))

;;;;;;;;;;;;;;;;;
; cursor creation
;;;;;;;;;;;;;;;;;

	(defmethod :set_cursor (cx cy &optional ax ay)
		; (. buffer :set_cursor cx cy [ax ay]) -> buffer
		(lower (:cursors (list (. this :clip_cursor cx cy ax ay))))
		this)

	(defmethod :add_cursor (cx cy &optional ax ay)
		; (. buffer :add_cursor cx cy [ax ay]) -> buffer
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
			(sort-bucket this (cat (get :cursors this) (get :tcursors this))
				(const csr-cmp))))

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
		; sort by min(cy, ay) then min(cx, ax)
		(defq cursors (sort-bucket this cursors (const csr-cmp)))
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
							(nums ex ey cx cy -1)
							(nums cx cy ex ey -1))))
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
			(bind '(cx cy ax ay) (csr-sort (nums cx cy ax ay)))
			(mutate-buffer
				(. this :push_undo (list :insert cx cy (sym (. this :icopy cx cy ax ay))))
				(cond
					((= cy ay)
						(set-line cy (erase (elem-get buffer_lines cy) cx ax)))
					((defq d (slice (elem-get buffer_lines cy) 0 cx) i ay)
						(while (<= cy (-- i)) (task-slice) (erase-line cy))
						(set-line cy (cat d (slice (elem-get buffer_lines cy) ax -1))))))
			; scan for affected range
			(when (nempty? cursors)
				(bind '(si ei) (bracket-cursors cursors cy ay))
				(unless (and (= si (length cursors)) (= ei (length cursors)))
					; map tail values if required
					(defq tail (if (= (defq dy (- cy ay)) 0)
						(slice cursors ei -1)
						(progn
							(defq delta (nums 0 dy 0 dy 0))
							(map! (# (nums-add %0 delta))
								(list cursors) (list) ei -1))))
					; map changes for mid cursors
					(defq mid (map! (lambda (csr)
						(bind '(pcx pcy pax pay psx) csr)
						(cat
							(csr-map-delete pcx pcy cx cy ax ay)
							(csr-map-delete pax pay cx cy ax ay)
							(nums psx)))
						(list cursors) (list) si ei))
					; sort and merge mid (plus potentially bracketing cursors)
					(lower (:cursors (cat
						(slice cursors 0 si)
						(. this :merge_cursors mid)
						tail))))))
		this)

	(defmethod :iinsert (cx cy text)
		; (. buffer :iinsert cx cy text) -> buffer
		; insert text at position (cx cy)
		(task-slice)
		(raise :buffer_lines :cursors)
		(bind '(cx cy &ignore) (. this :clip_cursor cx cy))
		(when (nql text "")
			(defq icx cx icy cy d (length text) i 0 breaks (list))
			; calculate line breaks
			(while (/= i d) (task-slice)
				(setq i (bskipn +char_class_lf text i))
				(if (/= i d) (push breaks (setq i (inc i)))))
			; calculate dimensions
			(defq dy (length breaks)
				  ecx (if (= dy 0) (+ icx d) (- d (last breaks)))
				  ecy (+ icy dy))
			(mutate-buffer
				(. this :push_undo (list :delete cx cy ecx ecy))
				(defq line (elem-get buffer_lines cy))
				(set-line cy (defq line (cat (slice line 0 cx) text (slice line cx -1))))
				(when (nempty? breaks)
					(defq cy (inc cy) ei -1)
					(reach (# (task-slice) (insert-line cy (slice line (+ cx %0) ei))
						(setq ei (+ cx %0))) breaks)
					(set-line (dec cy) (slice line 0 ei))))
			; scan for affected range
			(when (nempty? cursors)
				(bind '(si ei) (bracket-cursors cursors icy icy))
				(unless (and (= si (length cursors)) (= ei (length cursors)))
					; map tail values if required
					(defq tail (if (= dy 0)
						(slice cursors ei -1)
						(progn
							(defq delta (nums 0 dy 0 dy 0))
							(map! (# (nums-add %0 delta))
								(list cursors) (list) ei -1))))
					; map changes for mid cursors
					(defq mid (map! (lambda (csr)
						(bind '(pcx pcy pax pay psx) csr)
						(cat
							(csr-map-insert pcx pcy icx icy ecx ecy dy pax pay)
							(csr-map-insert pax pay icx icy ecx ecy dy pcx pcy)
							(nums psx)))
						(list cursors) (list) si ei))
					; sort and merge mid
					(lower (:cursors (cat
						(slice cursors 0 si)
						(. this :merge_cursors mid)
						tail))))))
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
			(bind '(cx cy ax ay) (csr-sort (nums cx cy ax ay)))
			(cond
				((= cy ay)
					(push text (slice (elem-get buffer_lines cy) cx ax)))
				((push text (slice (elem-get buffer_lines cy) cx -1))
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
						; move left one char
						((and (= cx ax) (= cy ay))
							(if (> cx 0)
								(nums (dec cx) cy (dec cx) cy (dec cx))
								(if (> cy 0)
									(nums (setq cx (csr-line-end (setq cy (dec cy))))
										cy cx cy cx)
									(nums 0 0 0 0 0))))
						; collapse selection to start
						((bind '(cx cy) (csr-sort-top (nums cx cy ax ay)))
							(nums cx cy cx cy cx))))
				(get :cursors this)))))
		this)

	(defmethod :right ()
		; (. buffer :right) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay &ignore)) (task-slice)
					; if no selection, move cursor right; else collapse to right edge
					(cond
						; move right one char
						((and (= cx ax) (= cy ay))
							(setq ax (csr-line-end cy))
							(if (< cx ax)
								(nums (inc cx) cy (inc cx) cy (inc cx))
								(if (< cy (dec (length buffer_lines)))
									(nums 0 (inc cy) 0 (inc cy) 0)
									(nums ax cy ax cy ax))))
						; collapse selection to end
						((bind '(ax ay) (csr-sort-bot (nums cx cy ax ay)))
							(nums ax ay ax ay ax))))
				(get :cursors this)))))
		this)

	(defmethod :up ()
		; (. buffer :up) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay sx)) (task-slice)
					; move up to previous line, using sticky x
					(bind '(cx cy ax ay) (csr-sort (nums cx cy ax ay)))
					(if (or (= sx -1) (or (/= cx ax) (/= cy ay))) (setq sx cx))
					(if (> cy 0)
						(nums (setq cx (min sx (csr-line-end (setq cy (dec cy)))))
							cy cx cy sx)
						(nums 0 0 0 0 sx)))
				(get :cursors this)))))
		this)

	(defmethod :down ()
		; (. buffer :down) -> buffer
		(raise :buffer_lines)
		(defq h (dec (length buffer_lines)))
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay sx)) (task-slice)
					; move down to next line, using sticky x
					(bind '(cx cy ax ay) (csr-sort (nums cx cy ax ay)))
					(if (or (= sx -1) (or (/= cx ax) (/= cy ay))) (setq sx ax))
					(if (< ay h)
						(nums (setq ax (min sx (dec (length (elem-get buffer_lines (setq ay (inc ay)))))))
							ay ax ay sx)
						(nums (setq ax (csr-line-end h)) h ax h sx)))
				(get :cursors this)))))
		this)

	(defmethod :home ()
		; (. buffer :home) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda (csr) (task-slice)
					; move to start of current line
					(bind '(cx cy) (csr-sort-top csr))
					(nums 0 cy 0 cy 0))
				(get :cursors this)))))
		this)

	(defmethod :end ()
		; (. buffer :end) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda (csr) (task-slice)
					; move to end of current line
					(bind '(ax ay) (csr-sort-bot csr))
					(setq ax (dec (length (elem-get buffer_lines ay))))
					(nums ax ay ax ay ax))
				(get :cursors this)))))
		this)

	(defmethod :top ()
		; (. buffer :top) -> buffer
		(lower (:cursors (list (nums 0 0 0 0 0))))
		this)

	(defmethod :bottom ()
		; (. buffer :bottom) -> buffer
		(raise :buffer_lines)
		(defq h (dec (length buffer_lines)) ex (csr-line-end h))
		(lower (:cursors (list (nums ex h ex h 0))))
		this)

	(defmethod :left_bracket ()
		; (. buffer :left_bracket) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda (csr) (task-slice)
					(bind '(cx cy) (csr-sort-top csr))
					(if (bind '(cx cy) (. this :find_left_bracket cx cy))
						(nums cx cy cx cy cx) csr))
				(get :cursors this)))))
		this)

	(defmethod :right_bracket ()
		; (. buffer :right_bracket) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda (csr) (task-slice)
					(bind '(cx cy) (csr-sort-bot csr))
					(if (bind '(cx cy) (. this :find_right_bracket cx cy))
						(nums cx cy cx cy cx) csr))
				(get :cursors this)))))
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
						((> cx 0) (nums (dec cx) cy ax ay (dec cx)))
						((> cy 0)
							(nums (setq cx (csr-line-end (setq cy (dec cy))))
								cy ax ay cx))
						((nums 0 0 ax ay 0))))
				(get :cursors this)))))
		this)

	(defmethod :right_select ()
		; (. buffer :right_select) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay sx)) (task-slice)
					; extend selection right
					(setq sx (csr-line-end cy))
					(cond
						((< cx sx) (nums (inc cx) cy ax ay (inc cx)))
						((< cy (dec (length buffer_lines)))
							(nums 0 (inc cy) ax ay 0))
						((nums sx cy ax ay sx))))
				(get :cursors this)))))
		this)

	(defmethod :up_select ()
		; (. buffer :up_select) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay sx)) (task-slice)
					; extend selection up a line
					(if (= sx -1) (setq sx cx))
					(if (> cy 0)
						(nums (setq cx (min sx (csr-line-end (setq cy (dec cy)))))
							cy ax ay sx)
						(nums 0 0 ax ay sx)))
				(get :cursors this)))))
		this)

	(defmethod :down_select ()
		; (. buffer :down_select) -> buffer
		(raise :buffer_lines)
		(defq h (dec (length buffer_lines)))
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay sx)) (task-slice)
					; extend selection down a line
					(if (= sx -1) (setq sx cx))
					(if (< cy h)
						(nums (setq cx (min sx (dec (length (elem-get buffer_lines (setq cy (inc cy)))))))
							cy ax ay sx)
						(nums (setq cx (csr-line-end h)) h ax ay sx)))
				(get :cursors this)))))
		this)

	(defmethod :home_select ()
		; (. buffer :home_select) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay &ignore)) (task-slice)
					; extend selection to start of line
					(nums 0 cy ax ay 0))
				(get :cursors this)))))
		this)

	(defmethod :end_select ()
		; (. buffer :end_select) -> buffer
		(raise :buffer_lines)
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay &ignore)) (task-slice)
					; extend selection to end of line
					(setq cx (csr-line-end cy))
					(nums cx cy ax ay cx))
				(get :cursors this)))))
		this)

	(defmethod :top_select ()
		; (. buffer :top_select) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay &ignore)) (task-slice)
					; extend selection to start of buffer
					(nums 0 0 ax ay 0))
				(get :cursors this)))))
		this)

	(defmethod :bottom_select ()
		; (. buffer :bottom_select) -> buffer
		(raise :buffer_lines)
		(defq h (dec (length buffer_lines)) ex (csr-line-end h))
		(lower (:cursors (. this :merge_cursors
			(map (lambda ((cx cy ax ay &ignore)) (task-slice)
					; extend selection to end of buffer
					(nums ex h ax ay 0))
				(get :cursors this)))))
		this)

	(defmethod :floor_selection ()
		; (. buffer :floor_selection) -> buffer
		(lower (:cursors (. this :merge_cursors
			(map (# (push (csr-floor %0) -1)) (get :cursors this)))))
		this)

	(defmethod :get_selected_extent ()
		; (. buffer :get_selected_extent) -> csr
		(defq max_x 0 min_x +max_int max_y 0 min_y +max_int)
		(each (lambda ((sx sy sx1 sy1))
				(setq max_x (max max_x sx1) min_x (min min_x sx)
					max_y (max max_y sy1) min_y (min min_y sy)))
			(map (const csr-floor) (. this :get_selected)))
		(list min_x min_y max_x max_y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; multi cursor text mutation
; undoable !
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defmethod :delete ()
		; (. buffer :delete) -> buffer
		(raise :buffer_lines)
		(undoable this
			(reach (lambda ((cx cy ax ay &ignore)) (task-slice)
					(when (and (= cx ax) (= cy ay))
						; no selection - move end point forward
						(defq line_len (csr-line-end cy))
						(cond
							((< cx line_len) (setq ax (inc cx)))
							((< cy (dec (length buffer_lines))) (setq ax 0 ay (inc cy)))))
					(. this :idelete cx cy ax ay))
				(get :cursors this))))

	(defmethod :backspace ()
		; (. buffer :backspace) -> buffer
		(raise :buffer_lines)
		(undoable this
			(reach (lambda ((cx cy ax ay &ignore)) (task-slice)
					(when (and (= cx ax) (= cy ay))
						; no selection - move start point back
						(cond
							((> cx 0) (setq cx (dec cx)))
							((> cy 0) (setq cy (dec cy) cx (csr-line-end cy)))))
					(. this :idelete cx cy ax ay))
				(get :cursors this))))

	(defmethod :insert (text)
		; (. buffer :insert text) -> buffer
		(undoable this
			(reach (lambda (csr) (task-slice)
					; insert at min position (sorted start)
					(bind '(cx cy) (csr-sort-top csr))
					(. this :iinsert cx cy text))
				(get :cursors this))
			; delete any selections
			(reach (lambda ((cx cy ax ay &ignore)) (task-slice)
				(. this :idelete cx cy ax ay)) (get :cursors this))))

	(defmethod :copy ()
		; (. buffer :copy) -> text
		(defq text (list))
		(each (lambda ((cx cy ax ay &ignore)) (task-slice)
				(push text (. this :icopy cx cy ax ay)))
			(get :cursors this))
		(join text (const (unescape "\f"))))

	(defmethod :cut ()
		; (. buffer :cut) -> text
		(undoable this
			(defq text (list))
			(each (lambda ((cx cy ax ay &ignore)) (task-slice)
				(push text (. this :icopy cx cy ax ay))) (get :cursors this))
			(reach (lambda ((cx cy ax ay &ignore)) (task-slice)
				(. this :idelete cx cy ax ay)) (get :cursors this)))
		(join text (const (unescape "\f"))))

	(defmethod :paste (text)
		; (. buffer :paste text) -> buffer
		(undoable this
			(defq parts (split text (const (unescape "\f"))))
			(cond
				((= (length parts) (length (get :cursors this)))
					;matching number of parts !
					(reach (lambda (csr text) (task-slice)
						(bind '(cx cy) (csr-sort-top csr))
						(. this :iinsert cx cy text)) (get :cursors this) parts)
					(reach (lambda ((cx cy ax ay &ignore)) (task-slice)
						(. this :idelete cx cy ax ay)) (get :cursors this)))
				(:t ;none matching parts !
					(defq parts (join parts (unescape "\n") 2))
					(reach (lambda (csr) (task-slice)
						(bind '(cx cy) (csr-sort-top csr))
						(. this :iinsert cx cy parts)) (get :cursors this))
					(reach (lambda ((cx cy ax ay &ignore)) (task-slice)
						(. this :idelete cx cy ax ay)) (get :cursors this))))))

;;;;;;;;;;;;;;;;;;;;
; file load and save
;;;;;;;;;;;;;;;;;;;;

	(defmethod :file_load (filepath)
		; (. buffer :file_load filepath) -> buffer
		(raise :syntax_engine :tab_width (buffer_lines (list) buffer_syntax (list)
			buffer_states (list) buffer_found (list) buffer_brackets (list)
			buffer_buckets (list) max_width 0))
		;clear them all now !
		(lower :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets :buffer_buckets)
		(clear (get :undo_stack this) (get :redo_stack this))
		(lines! (lambda (line)
				(task-slice)
				(setq line (cat
						(expand (trim-end line (const (unescape "\r"))) tab_width)
						+char_class_lf)
					max_width (max max_width (length line)))
				(push-line line))
			(file-stream filepath))
		(last-line)
		(lower :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets :buffer_buckets
			:max_width (:dirty_flags -1 :modified :nil))
		this)

	(defmethod :file_load_hex (filepath &optional width)
		; (. buffer :file_load_hex filepath [width]) -> buffer
		(raise :syntax_engine :tab_width (buffer_lines (list) buffer_syntax (list)
			buffer_states (list) buffer_found (list) buffer_brackets (list)
			buffer_buckets (list) max_width 0 width (ifn width 8)
			adr 0 stream (file-stream filepath)))
		;clear them all now !
		(lower :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets :buffer_buckets)
		(clear (get :undo_stack this) (get :redo_stack this))
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
		(lower :buffer_lines :buffer_syntax :buffer_states
			:buffer_found :buffer_brackets :buffer_buckets
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
		(if (bits? (get :flags this) +buffer_flag_undo)
			(each (# (push undo_stack %0)) records))
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

	(defmethod :find_left_bracket (cx cy)
		; (. buffer :find_left_bracket cx cy) -> (cx cy) | (:nil :nil)
		(cond
			((bits? (get :flags this) +buffer_flag_syntax)
				(bind '(x y &ignore) (. (build-brackets this) :clip_cursor cx cy))
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

	(defmethod :find_right_bracket (cx cy)
		; (. buffer :find_right_bracket cx cy) -> (cx cy) | (:nil :nil)
		(cond
			((bits? (get :flags this) +buffer_flag_syntax)
				(bind '(x y &ignore) (. (build-brackets this) :clip_cursor cx cy))
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

	(defmethod :set_found_cursors (buffer_found)
		; (. buffer :set_found_cursors buffer_found) -> buffer
		(defq stage (list))
		(each (# (when %0
			(defq y (!))
			(each (lambda (((x x1) &ignore))
				(push stage (nums x1 y x y 0))) %0))) buffer_found)
		(if (nempty? stage)
			(set this :cursors stage)
			this))

	(defmethod :add_found_cursors (buffer_found)
		; (. buffer :add_found_cursors buffer_found) -> buffer
		(defq stage (list))
		(each (# (when %0
			(defq y (!))
			(each (lambda (((x x1) &ignore))
				(push stage (nums x1 y x y 0))) %0))) buffer_found)
		(when (nempty? stage)
			(set this :cursors (. this :merge_cursors (cat (get :cursors this) stage))))
		this)

	(defmethod :find (pattern wmode rmode)
		; (. buffer :find pattern wmode rmode) -> buffer_found
		(raise :last_key :buffer_found)
		(unless (eql (defq key (str (list wmode rmode pattern))) last_key)
			(lower (:last_key key))
			(each (lambda (__) (elem-set buffer_found (!) :nil)) buffer_found))
		(unless (eql pattern "")
			(bind '(engine pattern meta) (query pattern wmode rmode))
			(each (lambda (line index)
				(if (> (length line) 1)
					(unless index
						(task-slice)
						(elem-set buffer_found (!)
							(. engine :search (slice line 0 -2) pattern meta)))
					(elem-set buffer_found (!) '())))
				(get :buffer_lines this) buffer_found))
		buffer_found)
	)

;module
(export-classes '(Buffer))
(export-symbols '(csr-sort csr-floor csr-within undoable blank-line?
	+buffer_flag_syntax +buffer_flag_undo))
(env-pop)
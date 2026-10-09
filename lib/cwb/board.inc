;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/cwb/board.inc - a whiteboard that is worked on
;
; A board is a document, lib/cwb/doc.inc, with what it takes to change
; it: a stage that pointers go to, lib/cwb/pointer.inc, the bindings that
; say what each pointer is, what is selected, and the steps that can be
; undone.
;
; The stage has, from the back: the surface, which every point is on, and
; where a pointer draws, rubs out, or picks things up and moves them; the
; handles of what is selected, to size and turn it; and whatever else is
; put there, a ruler say, lib/cwb/tools.inc.
;
; Nothing here needs a screen. An app gives it pointers and asks it to
; draw, and so can a test or a script: (. board :pointers events) is the
; whole of how it is driven by hand, and every change a hand can make is
; a method that can be called without one.
;
; Every point is in the space of the document. Sizes that are of the
; screen, how near is near enough, how big a handle is, are divided by
; :zoom, which the app sets.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./doc.inc")
(import "./pointer.inc")

;what has changed since it was last drawn
;the document, all of it. What is over it. And only that items were put on
;top of it, :appended, which can be drawn on what is there
(bits +board_dirty 0
	(bit doc overlay append))

(defq +board_handle 5.0 +board_near 4.0 +board_turn_gap 24.0)

;;;;;;;;;;;;;;;;;;
; a few small parts
;;;;;;;;;;;;;;;;;;

(defun board-copy-items (items)
	; (board-copy-items items) -> items
	;items that can be changed and leave those they were copied from as
	;they were. What a shape is flattened to goes with it, it is the same
	(defq root (list) todo (list (list items root)))
	(while (defq next (pop todo))
		(bind '(from into) next)
		(each (lambda (item)
			(defq copy (cat item))
			(push into copy)
			(when (cwb-group? item)
				(defq kids (list))
				(elem-set copy +cwb_items kids)
				(push todo (list (elem-get item +cwb_items) kids))))
			from))
	root)

(defun board-snap (v step)
	; (board-snap v step) -> v
	;to the nearest step, as it is if the step is 0
	(if (= step 0.0) v
		(* (floor (+ (/ v step) 0.5)) step)))

(defun board-pen-d (points)
	; (board-pen-d points) -> d
	;the path of a line drawn by hand, points is x y x y ... It goes
	;from the first to the last, and between is curved: each point is
	;where the line turns toward, and it passes half way between each two
	(defq pts (partition (map (const n2f) points) 2) n (length pts))
	(cond
		((= n 0) "")
		((= n 1) (cwb-d "M" (first (first pts)) (second (first pts)) "L" (first (first pts)) (second (first pts))))
		((= n 2) (apply (const cwb-d-line) (cat (first pts) (second pts))))
		(:t (defq out (list "M" (first (first pts)) (second (first pts))))
			(each! (lambda ((x y))
				(bind '(nx ny) (elem-get pts (inc (!))))
				(push out "Q" x y (* 0.5 (+ x nx)) (* 0.5 (+ y ny))))
				(list pts) 1 (dec n))
			(push out "L" (first (last pts)) (second (last pts)))
			(apply (const cwb-d) out))))

(defun board-two-point-mat (a0 b0 a1 b1)
	; (board-two-point-mat a0 b0 a1 b1) -> matrix
	;the move, turn and change of size that takes two points to where
	;they are now, as two fingers on a thing do. Each is (x y)
	(defq v0x (- (first b0) (first a0)) v0y (- (second b0) (second a0))
		v1x (- (first b1) (first a1)) v1y (- (second b1) (second a1))
		l0 (max 1.0 (sqrt (+ (* v0x v0x) (* v0y v0y))))
		l1 (max 1.0 (sqrt (+ (* v1x v1x) (* v1y v1y))))
		scale (/ l1 l0) angle (- (path-angle v1x v1y) (path-angle v0x v0y))
		m0x (* 0.5 (+ (first a0) (first b0))) m0y (* 0.5 (+ (second a0) (second b0)))
		m1x (* 0.5 (+ (first a1) (first b1))) m1y (* 0.5 (+ (second a1) (second b1))))
	(cwb-mat-mul (cwb-mat-move m1x m1y)
		(cwb-mat-mul (cwb-mat-turn angle)
			(cwb-mat-mul (cwb-mat-scale scale) (cwb-mat-move (neg m0x) (neg m0y))))))

;;;;;;;;;;;;;
; the surface
;;;;;;;;;;;;;

;what is kept for a pointer that is down on the surface, a list
(enums +stroke 0
	(enum tool x y points item moved last_x last_y snap))

(defclass Surface (board) (Actor)
	; (Surface board) -> surface
	;the actor at the back, every point is on it
	(def this :board board :grab :nil)

	(defmethod :hit (x y) :t)

	(defmethod :things ()
		; (. surface :things) -> ((item matrix) ...)
		;what is selected, each with where it is now, to be moved from there
		(map (# (list %0 (elem-get %0 +cwb_m))) (. (get :board this) :selected_items)))

	(defmethod :holding ()
		; (. surface :holding) -> :nil | id
		;a pointer that has hold of what is selected
		(defq found :nil)
		(. (get :states this) :each (lambda (id state)
			(if (and (eql (elem-get state +stroke_tool) :hand) (elem-get state +stroke_item))
				(setq found id))))
		found)

	(defmethod :begin (event)
		; (. surface :begin event) -> state
		;a pointer has gone down
		(raise :board)
		(bind '(id kind buttons x y &ignore) event)
		(defq tool (. (get :bindings board) :value event :tool :pen))
		;a pen is whatever the board is set to draw, and in select mode it
		;is the hand, so a mouse alone can do everything
		(if (eql tool :pen) (setq tool (get :mode board)))
		(if (eql tool :select) (setq tool :hand))
		(defq state (list tool x y (list x y) :nil :nil x y :nil)
			holding (. this :holding))
		(case tool
			(:hand
				(defq hit (cwb-hit (get :doc board) x y (/ +board_near (get :zoom board))))
				;anywhere in the box round what is selected is on it, a box
				;that is not filled, or a thin line, is hard to be on
				(if (and (not hit) (nempty? (defq items (. board :selected_items)))
						(defq box (cwb-bounds items))
						(>= x (first box)) (>= y (second box)) (<= x (third box)) (<= y (elem-get box 3)))
					(setq hit (list (first items) 0 :nil)))
				;a finger in the box of a thing is on the thing
				(if (and (not hit) (eql kind :touch)) (setq hit (cwb-hit-box (get :doc board) x y)))
				(cond
					((and hit holding (find (elem-get (first hit) +cwb_id) (get :selected board)))
						;a second hand on what the first is holding, the two
						;of them move it, see (:two_hands)
						(elem-set state +stroke_item (. this :things)))
					(holding
						;a second hand that is not on it does nothing
						(elem-set state +stroke_tool :idle))
					(hit (defq id_hit (elem-get (first hit) +cwb_id))
						(unless (find id_hit (get :selected board)) (. board :select (list id_hit)))
						(. board :snapshot)
						(elem-set state +stroke_snap :t)
						(elem-set state +stroke_item (. this :things))
						(. board :float (get :selected board)))
					(:t (. board :select (list)))))
			(:eraser (. board :snapshot))
			(:pan)
			(:text
				;the words the board has are put down where the pointer is,
				;they sit on the line through it, and that is all it does
				(elem-set state +stroke_tool :idle)
				(when (nempty? (defq words (get :text board)))
					(. board :commit (cwb-text words (. board :snap_x x) (. board :snap_y y)
						:fill (. (get :bindings board) :value event :color (get :color board))
						:font (get :font board) :font_size (get :font_size board)))))
			(:t ;something is drawn
				(elem-set state +stroke_x (. board :snap_x x))
				(elem-set state +stroke_y (. board :snap_y y))
				(elem-set state +stroke_points (list (elem-get state +stroke_x) (elem-get state +stroke_y)))
				(elem-set state +stroke_item (. board :new_shape event tool))))
		state)

	(defmethod :shape_d (state x y)
		; (. surface :shape_d state x y) -> d
		;the path of what a pointer is drawing, now it is at a point
		(bind '(tool sx sy points &ignore) state)
		(case tool
			((:line :arrow :arrow2) (cwb-d-line sx sy x y))
			((:rect :frect) (cwb-d-rect sx sy x y))
			((:ellipse :fellipse)
				(cwb-d-ellipse (* 0.5 (+ sx x)) (* 0.5 (+ sy y)) (* 0.5 (abs (- x sx))) (* 0.5 (abs (- y sy)))))
			(:t (board-pen-d points))))

	(defmethod :step (event state)
		; (. surface :step event state) -> state
		;a pointer that is down has moved, or come up
		(raise :board)
		(bind '(id kind buttons x y &ignore) event)
		(bind '(tool sx sy points item moved &ignore) state)
		(case tool
			(:idle)
			(:hand
				(cond
					(item ;what is selected goes with it
						(defq move (cwb-mat-move (- (. board :snap_x x) (. board :snap_x sx))
							(- (. board :snap_y y) (. board :snap_y sy))))
						(each (lambda ((thing m0)) (elem-set thing +cwb_m (cwb-mat-mul move m0))) item)
						(if (or (/= x sx) (/= y sy)) (elem-set state +stroke_moved :t))
						(. board :touch +board_dirty_overlay))
					(:t ;a box is dragged out round things
						(def board :band (list (min sx x) (min sy y) (max sx x) (max sy y)))
						(. board :touch +board_dirty_overlay))))
			(:eraser
				(when (defq hit (cwb-hit (get :doc board) x y (/ +board_near (get :zoom board))))
					(. board :remove (list (elem-get (first hit) +cwb_id)))
					(elem-set state +stroke_moved :t)))
			(:pan
				(def board :pan (list (- x sx) (- y sy))))
			(:t ;something is drawn
				(defq x (. board :snap_x x) y (. board :snap_y y))
				(when (or (/= x (elem-get points -3)) (/= y (elem-get points -2)))
					(push points x y)
					(cwb-set item :d (. this :shape_d state x y))
					(. board :touch +board_dirty_overlay))))
		state)

	(defmethod :end (event state)
		; (. surface :end event state)
		;a pointer has come up
		(raise :board)
		(bind '(id kind buttons x y &ignore) event)
		(bind '(tool sx sy points item moved lx ly snap) state)
		(case tool
			(:idle)
			(:hand
				(cond
					(item (if (and snap (not moved)) (. board :forget))
						;put down, if no other hand has hold of it
						(unless (some (# (and (not (eql %0 state)) (eql (elem-get %0 +stroke_tool) :hand)
								(elem-get %0 +stroke_item)))
								(map (const second) (. (get :states this) :tolist)))
							(. board :float (list))))
					(:t (when (defq band (get :band board))
							(. board :select (map (# (elem-get (first %0) +cwb_id))
								(cwb-in-box (get :doc board) band)))
							(def board :band :nil)
							(. board :touch +board_dirty_overlay)))))
			(:eraser (unless moved (. board :forget)))
			(:pan)
			(:t ;what was drawn goes into the document, if it is anything
				(. board :drop_temp id)
				(cond
					((and (find tool '(:pen)) (>= (length points) 2))
						;a line drawn by hand has the points that matter, and is curved
						(defq kept (if (> (length points) 4) (path-filter 0.5 (apply (const path) points) (path)) points))
						(cwb-set item :d (board-pen-d (map (const identity) kept)))
						(. board :commit item))
					((and (/= (length points) 2) (third (cwb-flat item)))
						(. board :commit item))))))

	(defmethod :pointers (stage events)
		; (. surface :pointers stage events) -> surface
		(raise :board)
		;two fingers, or two of anything that moves things, down on what is
		;selected, move it, turn it and size it together
		(each (lambda (event)
			(bind '(id kind buttons &ignore) event)
			(defq state (. this :state id))
			(cond
				((and (not state) (/= buttons 0))
					(. this :set_state id (. this :begin event)))
				((and state (/= buttons 0))
					(elem-set state +stroke_last_x (elem-get event +pev_x))
					(elem-set state +stroke_last_y (elem-get event +pev_y))
					(unless (get :grab this) (. this :step event state)))
				(state
					(unless (get :grab this) (. this :step event state))
					(. this :end event state)
					(. this :set_state id :nil))))
			events)
		(. this :two_hands)
		this)

	(defmethod :two_hands ()
		; (. surface :two_hands)
		;if two pointers have hold of what is selected, the two of them
		;move it, turn it and size it, by where they were when the second
		;took hold and where they are now. While they do, neither moves it
		;alone. When one lets go the other carries on from where things are
		(raise :board)
		(defq hands (list) grab (get :grab this))
		(. (get :states this) :each (lambda (id state)
			(if (and (eql (elem-get state +stroke_tool) :hand) (elem-get state +stroke_item))
				(push hands (list id state)))))
		(cond
			((< (length hands) 2)
				(when grab
					(def this :grab :nil)
					(each (lambda ((id state))
						(elem-set state +stroke_x (elem-get state +stroke_last_x))
						(elem-set state +stroke_y (elem-get state +stroke_last_y))
						(elem-set state +stroke_item (. this :things))) hands)))
			(:t (defq a (second (first hands)) b (second (second hands))
					pa (list (elem-get a +stroke_last_x) (elem-get a +stroke_last_y))
					pb (list (elem-get b +stroke_last_x) (elem-get b +stroke_last_y)))
				(unless grab
					(def this :grab (setq grab (list pa pb (. this :things)))))
				(bind '(pa0 pb0 things) grab)
				(defq move (board-two-point-mat pa0 pb0 pa pb))
				(each (lambda ((thing m0)) (elem-set thing +cwb_m (cwb-mat-mul move m0))) things)
				(each (lambda ((id state)) (elem-set state +stroke_moved :t)) hands)
				(. board :touch +board_dirty_overlay))))
	)

;;;;;;;;;;;;;
; the handles
;;;;;;;;;;;;;

(defclass Handles (board) (Actor)
	; (Handles board) -> handles
	;the box round what is selected, with a square at each corner and the
	;middle of each side to size it by, and a round one above it to turn
	;it by. It is in front of the surface, and a point is on it only where
	;one of those is
	(def this :board board)

	(defmethod :box ()
		; (. handles :box) -> :nil | (x y x1 y1)
		;the box round what is selected, :nil if nothing is, or the board
		;is not in select mode
		(raise :board)
		(and (eql (get :mode board) :select) (nempty? (get :selected board))
			(cwb-bounds (. board :selected_items))))

	(defmethod :spots ()
		; (. handles :spots) -> ((kind x y fx fy) ...)
		;where each handle is. kind is :size or :turn, fx and fy say which
		;way a size handle pulls, -1 0 or 1 each way
		(raise :board)
		(when (defq box (. this :box))
			(bind '(x y x1 y1) box)
			(defq mx (* 0.5 (+ x x1)) my (* 0.5 (+ y y1)))
			(list (list :size x y -1 -1) (list :size mx y 0 -1) (list :size x1 y 1 -1)
				(list :size x my -1 0) (list :size x1 my 1 0)
				(list :size x y1 -1 1) (list :size mx y1 0 1) (list :size x1 y1 1 1)
				(list :turn mx (- y (/ +board_turn_gap (get :zoom board))) 0 0))))

	(defmethod :spot_at (x y)
		; (. handles :spot_at x y) -> :nil | spot
		(raise :board)
		(defq reach (/ (* 1.6 +board_handle) (get :zoom board)))
		(some (lambda (spot)
			(bind '(kind sx sy &ignore) spot)
			(if (and (<= (abs (- x sx)) reach) (<= (abs (- y sy)) reach)) spot))
			(ifn (. this :spots) (list))))

	(defmethod :hit (x y)
		(if (. this :spot_at x y) :t))

	(defmethod :pointers (stage events)
		(raise :board)
		(each (lambda (event)
			(bind '(id kind buttons x y &ignore) event)
			(defq state (. this :state id))
			(cond
				((and (not state) (/= buttons 0))
					(when (and (defq spot (. this :spot_at x y)) (= (. this :held) 0))
						(. board :snapshot)
						(. board :float (get :selected board))
						(bind '(bx by bx1 by1) (. this :box))
						;the spot, the box as it was, where the pointer went
						;down, and each thing as it was
						(. this :set_state id (list spot (list bx by bx1 by1) x y
							(map (# (list %0 (elem-get %0 +cwb_m))) (. board :selected_items))))))
				(state
					(bind '((skind sx sy fx fy) (bx by bx1 by1) dx dy things) state)
					(defq cx (* 0.5 (+ bx bx1)) cy (* 0.5 (+ by by1)) m :nil)
					(cond
						((eql skind :turn)
							(defq angle (- (path-angle (- x cx) (- y cy)) (path-angle (- dx cx) (- dy cy)))
								step (get :snap_angle board))
							(while (> angle +fp_pi) (setq angle (- angle +fp_2pi)))
							(while (<= angle (neg +fp_pi)) (setq angle (+ angle +fp_2pi)))
							(if (/= step 0.0) (setq angle (board-snap angle step)))
							(setq m (cwb-mat-turn angle cx cy)))
						(:t ;the side or corner across from the handle stays where it is
							(defq ax (cond ((< fx 0) bx1) ((> fx 0) bx) (:t cx))
								ay (cond ((< fy 0) by1) ((> fy 0) by) (:t cy))
								w0 (- sx ax) h0 (- sy ay)
								px (. board :snap_x x) py (. board :snap_y y)
								scx (if (or (= fx 0) (= w0 0.0)) 1.0 (/ (- px ax) w0))
								scy (if (or (= fy 0) (= h0 0.0)) 1.0 (/ (- py ay) h0)))
							;a corner keeps the shape of the box unless the board says not
							(if (and (/= fx 0) (/= fy 0) (get :keep_shape board))
								(setq scx (if (> (abs scx) (abs scy)) scx (* (abs scy) (if (< scx 0.0) -1.0 1.0)))
									scy (* (abs scx) (if (< scy 0.0) -1.0 1.0))))
							(if (= scx 0.0) (setq scx 0.001))
							(if (= scy 0.0) (setq scy 0.001))
							(setq m (cwb-mat-scale scx scy ax ay))))
					(each (lambda ((thing m0)) (elem-set thing +cwb_m (cwb-mat-mul m m0))) things)
					(. board :touch +board_dirty_overlay)
					(when (= buttons 0)
						(. this :set_state id :nil)
						(. board :float (list))))))
			events)
		this)
	)

;;;;;;;;;;;
; the board
;;;;;;;;;;;

(defclass Board (&optional doc) :nil
	; (Board [doc]) -> board
	(def this :doc (ifn doc (cwb-doc)) :stage (Stage) :bindings (bindings-default)
		:mode :pen :layer :nil :selected (list) :undo_stack (list) :redo_stack (list)
		:color 0xff000000 :fill 0 :width 3.0
		:text "" :font "fonts/OpenSans-Regular.ctf" :font_size 32
		:snap 0.0 :snap_angle 0.0 :keep_shape :t :zoom 1.0
		:temp (Fmap 11) :band :nil :pan :nil :dirty (+ +board_dirty_doc +board_dirty_overlay)
		:floating (list) :appended (list) :changes (list :all))
	(def this :surface (Surface this) :handles (Handles this))
	(.-> (get :stage this) (:add (get :surface this)) (:add (get :handles this)))

	;a thing a board has, and a method of the same name, can not both be:
	;a method is found by its name in the board as what it has is
	(defmethod :get_doc () (get :doc this))
	(defmethod :get_stage () (get :stage this))
	(defmethod :get_bindings () (get :bindings this))

	(defmethod :set_doc (doc)
		; (. board :set_doc doc) -> board
		;another document, nothing of the last is kept
		(def this :doc doc :selected (list) :undo_stack (list) :redo_stack (list) :layer :nil :band :nil
			:floating (list) :appended (list))
		(. this :changed :all)
		(. (get :temp this) :empty)
		(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))

	(defmethod :touch (flags)
		; (. board :touch flags) -> board
		;something has changed that is drawn
		(def this :dirty (logior (get :dirty this) flags))
		this)

	(defmethod :dirty? (flags)
		; (. board :dirty? flags) -> :t | :nil
		;has that changed, and it is taken as drawn
		(defq dirty (get :dirty this))
		(def this :dirty (logand dirty (lognot flags)))
		(/= (logand dirty flags) 0))

	(defmethod :float (ids)
		; (. board :float ids) -> board
		;these items are being moved about. They are drawn over the
		;document, not in it, so that only they are drawn as they move
		(. this :changed (get :floating this))
		(def this :floating (cat ids))
		(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))

	(defmethod :changed (ids)
		; (. board :changed ids) -> board
		;these items of the layers are not what they were, or are new, or
		;:all for anything might be. It is kept till it is asked for, for
		;what keeps a copy of the document in step with this one, the nodes
		;that draw it in stripes, lib/cwb/stripes.inc. An item that is gone
		;need not be said, the layers are told whole
		(if (eql ids :all)
			(def this :changes (list :all))
			(unless (find :all (get :changes this))
				(each (# (unless (find %0 (get :changes this)) (push (get :changes this) %0))) ids)))
		this)

	(defmethod :take_changes ()
		; (. board :take_changes) -> :all | ids
		;what has changed since this was last asked
		(defq changes (get :changes this))
		(def this :changes (list))
		(if (find :all changes) :all changes))

	(defmethod :take_appended ()
		; (. board :take_appended) -> items
		;the items put on top since this was last asked
		(defq items (get :appended this))
		(def this :appended (list))
		items)

	(defmethod :snap_x (x) (board-snap x (get :snap this)))
	(defmethod :snap_y (y) (board-snap y (get :snap this)))

	(defmethod :layer_index ()
		; (. board :layer_index) -> index
		;the layer that is drawn on, the top one unless one was chosen
		(defq layers (cwb-layers (get :doc this)) index (get :layer this))
		(if (and index (< index (length layers))) index (dec (length layers))))

	;;;;;;;;;;;;;;
	; undo and redo
	;;;;;;;;;;;;;;

	(defmethod :state ()
		; (. board :state) -> state
		;the layers as they are, to go back to
		(map (lambda ((name flags items)) (list name flags (board-copy-items items)))
			(cwb-layers (get :doc this))))

	(defmethod :snapshot ()
		; (. board :snapshot) -> board
		;what is about to be done can be undone
		(push (get :undo_stack this) (. this :state))
		(clear (get :redo_stack this))
		this)

	(defmethod :forget ()
		; (. board :forget) -> board
		;nothing was done after all, the last (:snapshot) is dropped
		(if (nempty? (get :undo_stack this)) (pop (get :undo_stack this)))
		this)

	(defmethod :put_state (state)
		(. (get :doc this) :insert :layers state)
		(. this :changed :all)
		(def this :selected (list) :floating (list) :appended (list))
		(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))

	(defmethod :undo ()
		; (. board :undo) -> :t | :nil
		(when (nempty? (get :undo_stack this))
			(push (get :redo_stack this) (. this :state))
			(. this :put_state (pop (get :undo_stack this)))
			:t))

	(defmethod :redo ()
		; (. board :redo) -> :t | :nil
		(when (nempty? (get :redo_stack this))
			(push (get :undo_stack this) (. this :state))
			(. this :put_state (pop (get :redo_stack this)))
			:t))

	;;;;;;;;;;;;;;;;;;
	; what is selected
	;;;;;;;;;;;;;;;;;;

	(defmethod :select (ids)
		; (. board :select ids) -> board
		(def this :selected (cat ids))
		(. this :touch +board_dirty_overlay))

	(defmethod :get_selected () (get :selected this))

	(defmethod :selected_items ()
		; (. board :selected_items) -> items
		;the items that are selected, each one of a layer's own, in the
		;order they are in, the back one first
		(defq ids (get :selected this) out (list))
		(each (lambda ((name flags items))
			(each (# (if (find (elem-get %0 +cwb_id) ids) (push out %0))) items))
			(cwb-layers (get :doc this)))
		out)

	(defmethod :select_all ()
		; (. board :select_all) -> board
		(defq ids (list))
		(each (lambda ((name flags items))
			(unless (or (bits? flags 1) (bits? flags 2))
				(each (# (push ids (elem-get %0 +cwb_id))) items)))
			(cwb-layers (get :doc this)))
		(. this :select ids))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; what is being drawn, not yet kept
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defmethod :new_shape (event tool)
		; (. board :new_shape event tool) -> shape
		;a shape for a pointer to draw, with the colour and width that
		;pointer is bound to, or the board's. It is drawn over the
		;document till it is kept
		(defq bindings (get :bindings this)
			color (. bindings :value event :color (get :color this))
			width (. bindings :value event :width (get :width this))
			fill (. bindings :value event :fill (get :fill this))
			shape (cwb-shape "" :stroke color :width width :kind tool))
		(case tool
			((:frect :fellipse) (cwb-set shape :fill color :stroke 0))
			((:rect :ellipse) (cwb-set shape :fill fill :join :miter))
			(:arrow (cwb-set shape :cap1 :butt :cap2 :arrow))
			(:arrow2 (cwb-set shape :cap1 :arrow :cap2 :arrow)))
		(. (get :temp this) :insert (elem-get event +pev_id) shape)
		shape)

	(defmethod :drop_temp (id)
		(. (get :temp this) :erase id)
		(. this :touch +board_dirty_overlay))

	;;;;;;;;;;;;;;;;;;;;;;;;;;
	; changes to the document
	;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defmethod :commit (item)
		; (. board :commit item) -> item
		;put an item on the layer that is drawn on, as a step that can be undone
		(. this :snapshot)
		(cwb-add (get :doc this) item (. this :layer_index))
		(. this :changed (list (elem-get item +cwb_id)))
		;on the top layer it is on top of all there is, and can be drawn there
		(if (= (. this :layer_index) (dec (length (cwb-layers (get :doc this)))))
			(progn (push (get :appended this) item)
				(. this :touch (+ +board_dirty_append +board_dirty_overlay)))
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		item)

	(defmethod :add (item)
		; (. board :add item) -> id
		;as (:commit), and the id of it, for a script
		(elem-get (. this :commit item) +cwb_id))

	(defmethod :remove (ids)
		; (. board :remove ids) -> board
		;take items out, no step is made, see (:delete)
		(each (# (cwb-remove (get :doc this) %0)) ids)
		(def this :selected (filter (# (not (find %0 ids))) (get :selected this)))
		(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))

	(defmethod :delete ()
		; (. board :delete) -> board
		;take out what is selected
		(when (nempty? (get :selected this))
			(. this :snapshot)
			(. this :remove (cat (get :selected this))))
		this)

	(defmethod :clear ()
		; (. board :clear) -> board
		;take out everything
		(. this :snapshot)
		(. this :changed :all)
		(each (lambda (layer) (elem-set layer 2 (list))) (cwb-layers (get :doc this)))
		(def this :selected (list))
		(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))

	(defmethod :transform (m)
		; (. board :transform m) -> board
		;move what is selected by a matrix
		(when (nempty? (defq items (. this :selected_items)))
			(. this :snapshot)
			(. this :changed (map (# (elem-get %0 +cwb_id)) items))
			(each (# (elem-set %0 +cwb_m (cwb-mat-mul m (elem-get %0 +cwb_m)))) items)
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	(defmethod :style (&rest key_vals)
		; (. board :style [key val] ...) -> board
		;set the colour, fill, width and so on of every shape that is
		;selected, those in groups too
		(when (nempty? (defq items (. this :selected_items)))
			(. this :snapshot)
			(. this :changed (map (# (elem-get %0 +cwb_id)) items))
			(cwb-walk items :nil (lambda (shape m) (apply (const cwb-set) (cat (list shape) key_vals))))
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	(defmethod :group ()
		; (. board :group) -> :nil | id
		;what is selected becomes one group, where the top one of them was
		(defq items (. this :selected_items))
		(when (> (length items) 1)
			(. this :snapshot)
			(. this :changed :all)
			(defq doc (get :doc this) top (last items) top_id (elem-get top +cwb_id)
				holder (third (cwb-find doc top_id))
				group (cwb-adopt doc (cwb-group items))
				kept (map (# (if (= (elem-get %0 +cwb_id) top_id) group %0)) holder))
			(clear holder)
			(each (# (push holder %0)) kept)
			(each (# (unless (= (elem-get %0 +cwb_id) top_id) (cwb-remove doc (elem-get %0 +cwb_id)))) items)
			;the group is in the list, the items are now only in the group
			(def this :selected (list (elem-get group +cwb_id)))
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay))
			(elem-get group +cwb_id)))

	(defmethod :ungroup ()
		; (. board :ungroup) -> board
		;each group that is selected becomes its items, where it was, each
		;where it was on the board
		(defq groups (filter (const cwb-group?) (. this :selected_items)))
		(when (nempty? groups)
			(. this :snapshot)
			(. this :changed :all)
			(defq doc (get :doc this) ids (list))
			(each (lambda (group)
				(defq id (elem-get group +cwb_id) holder (third (cwb-find doc id))
					kept (reduce (lambda (out item)
						(if (= (elem-get item +cwb_id) id)
							(each (lambda (kid)
								(elem-set kid +cwb_m (cwb-mat-mul (elem-get group +cwb_m) (elem-get kid +cwb_m)))
								(push ids (elem-get kid +cwb_id))
								(push out kid)) (elem-get group +cwb_items))
							(push out item))
						out) holder (list)))
				(clear holder)
				(each (# (push holder %0)) kept))
				groups)
			(def this :selected (cat ids (filter (# (not (find %0 (map (# (elem-get %0 +cwb_id)) groups))))
				(get :selected this))))
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	(defmethod :order (front)
		; (. board :order front) -> board
		;what is selected goes to the front of its layer, or the back
		(when (nempty? (defq ids (get :selected this)))
			(. this :snapshot)
			(each (lambda (layer)
				(defq items (third layer)
					these (filter (# (find (elem-get %0 +cwb_id) ids)) items)
					rest_of (filter (# (not (find (elem-get %0 +cwb_id) ids))) items))
				(if (nempty? these)
					(elem-set layer 2 (if front (cat rest_of these) (cat these rest_of)))))
				(cwb-layers (get :doc this)))
			(. this :touch +board_dirty_doc))
		this)

	(defmethod :align (how)
		; (. board :align how) -> board
		;line up what is selected, each with the box round all of it. how is
		;:left :right :top :bottom, or :hcenter or :vcenter for the middles
		(defq items (. this :selected_items))
		(when (and (> (length items) 1) (defq all (cwb-bounds items)))
			(. this :snapshot)
			(. this :changed (map (# (elem-get %0 +cwb_id)) items))
			(bind '(ax ay ax1 ay1) all)
			(each (lambda (item)
				(when (defq box (cwb-bounds (list item)))
					(bind '(x y x1 y1) box)
					(defq dx (case how (:left (- ax x)) (:right (- ax1 x1))
							(:hcenter (- (* 0.5 (+ ax ax1)) (* 0.5 (+ x x1)))) (:t 0.0))
						dy (case how (:top (- ay y)) (:bottom (- ay1 y1))
							(:vcenter (- (* 0.5 (+ ay ay1)) (* 0.5 (+ y y1)))) (:t 0.0)))
					(elem-set item +cwb_m (cwb-mat-mul (cwb-mat-move dx dy) (elem-get item +cwb_m)))))
				items)
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	(defmethod :duplicate (&optional dx dy)
		; (. board :duplicate [dx dy]) -> board
		;a copy of what is selected, moved a little, default 16 each way,
		;and the copy is what is selected
		(when (nempty? (defq items (. this :selected_items)))
			(. this :snapshot)
			(defq doc (get :doc this) move (cwb-mat-move (ifn dx 16) (ifn dy 16)) ids (list))
			(each (lambda (copy)
				;a copy and all in it has ids of its own
				(defq todo (list copy))
				(while (defq next (pop todo))
					(elem-set next +cwb_id 0)
					(if (cwb-group? next) (each (# (push todo %0)) (elem-get next +cwb_items))))
				(elem-set copy +cwb_m (cwb-mat-mul move (elem-get copy +cwb_m)))
				(cwb-add doc copy (. this :layer_index))
				(push ids (elem-get copy +cwb_id)))
				(board-copy-items items))
			(def this :selected ids)
			(. this :changed ids)
			(. this :touch (+ +board_dirty_doc +board_dirty_overlay)))
		this)

	;;;;;;;;;;
	; pointers
	;;;;;;;;;;

	(defmethod :pointers (events)
		; (. board :pointers events) -> board
		;a batch of pointer events, each in the space of the document
		(. (get :stage this) :pointers events)
		this)

	;;;;;;;;;
	; drawing
	;;;;;;;;;

	(defmethod :draw (canvas &optional m clip)
		; (. board :draw canvas [m clip]) -> count
		;the document, on a canvas, by the matrix it is seen by
		(cwb-draw canvas (get :doc this) m clip (get :floating this)))

	(defmethod :draw_overlay (canvas &optional m)
		; (. board :draw_overlay canvas [m]) -> board
		;what is over the document and not of it: what is being drawn,
		;the box being dragged out, and the handles of what is selected
		(defq zoom (get :zoom this) thin (/ 1.0 zoom))
		(when (nempty? (defq floating (get :floating this)))
			(each (lambda ((name flags items))
				(cwb-draw-items canvas (filter (# (find (elem-get %0 +cwb_id) floating)) items) m))
				(cwb-layers (get :doc this))))
		(. (get :temp this) :each (lambda (id shape) (cwb-draw-items canvas (list shape) m)))
		(when (defq band (get :band this))
			(bind '(x y x1 y1) band)
			(cwb-draw-items canvas (list (cwb-shape (cwb-d-rect x y x1 y1)
				:stroke 0xff0080ff :width thin :fill 0x200080ff :join :miter)) m))
		(when (nempty? (defq items (. this :selected_items)))
			(when (defq box (cwb-bounds items))
				(bind '(x y x1 y1) box)
				(cwb-draw-items canvas (list (cwb-shape (cwb-d-rect x y x1 y1)
					:stroke 0xff0080ff :width thin :join :miter)) m))
			(when (defq spots (. (get :handles this) :spots))
				(defq r (/ +board_handle zoom))
				(cwb-draw-items canvas (map (lambda ((kind sx sy &ignore))
					(cwb-shape (if (eql kind :turn) (cwb-d-ellipse sx sy r)
							(cwb-d-rect (- sx r) (- sy r) (+ sx r) (+ sy r)))
						:fill 0xffffffff :stroke 0xff0080ff :width thin :join :miter)) spots) m)))
		this)
	)

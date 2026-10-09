(report-header "Whiteboard stripes: a board drawn by the nodes, each with its own copy of the document kept in step")

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/cwb/stripes.inc")

(defun st-bytes (canvas)
	;the pixels of a canvas
	(defq stream (memory-stream))
	(pixmap-write (getf canvas +canvas_pixmap 0) stream 32)
	(stream-seek stream 0 0)
	(read-blk stream 100000000))

(defun st-local (board w h zoom style)
	;the board drawn here, by one task, as the nodes are to draw it
	(defq canvas (Canvas w h 1) doc (. board :get_doc))
	(. canvas :set_canvas_flags +canvas_flag_antialias)
	(cwb-paper canvas w h (. doc :find :background) style (max 2 (n2i (* (n2f (. doc :find :grid)) zoom))))
	(. board :draw canvas (if (= zoom 1.0) :nil (cwb-mat-scale zoom)))
	(st-bytes canvas))

;what a copy needs to be in step, with no nodes at all
(defq st_board (Board (cwb-doc 400 300)) st_doc (. st_board :get_doc) st_log (list))
(. st_board :add (cwb-shape (cwb-d-rect 10 10 60 40) :fill 0xffff0000))
(. st_board :add (cwb-shape (cwb-d-line 0 0 100 100)))
(. st_board :add (cwb-group (list (cwb-shape (cwb-d-ellipse 200 150 30)) (cwb-text "Hi" 250 150))))
(push st_log (list 1 (. st_board :take_changes)))
(defq st_copy (cwb-doc) st_items (Fmap 11))
(assert-eq "a copy that has nothing is sent it all, and is at the version"
	1 (stripes-apply st_copy st_items (stripes-delta st_doc st_log -1 1)))
(assert-list-eq "it has the items, in their order" '(1 2 3) (map (# (elem-get %0 +cwb_id)) (cwb-items st_copy)))
(assert-list-eq "and the size" '(400 300) (list (. st_copy :find :width) (. st_copy :find :height)))
;one item is moved
(. st_board :select (list 2))
(. st_board :transform (cwb-mat-move 50 0))
(push st_log (list 2 (. st_board :take_changes)))
(assert-list-eq "the board says which item changed" '(2) (second (last st_log)))
(defq st_kept (first (cwb-items st_copy)) st_delta (stripes-delta st_doc st_log 1 2))
(cwb-flat st_kept)
(assert-true "a copy one version behind is sent the one item, not the rest"
	(and (found? st_delta "M 0 0 L 100 100") (not (found? st_delta "M 10 10 L 60 10"))))
(assert-eq "and is in step" 2 (stripes-apply st_copy st_items st_delta))
(assert-true "the item that was moved is where it is"
	(every (const =) (cwb-get (second (cwb-items st_copy)) :m) (cwb-get (second (cwb-items st_doc)) :m)))
(assert-true "an item that was not sent again is the one it had, with what it was flattened to"
	(and (eql st_kept (first (cwb-items st_copy))) (elem-get st_kept +cwb_cache)))
;one is taken out, one put on top, the order changed
(. st_board :remove (list 1))
(. st_board :add (cwb-shape (cwb-d-rect 300 200 350 250)))
(. st_board :select (list 2)) (. st_board :order :t)
(push st_log (list 3 (. st_board :take_changes)))
(assert-eq "three versions on" 3 (stripes-apply st_copy st_items (stripes-delta st_doc st_log 2 3)))
(assert-list-eq "what was taken out is out, what was put in is in, and the order is the order" '(3 6 2)
	(map (# (elem-get %0 +cwb_id)) (cwb-items st_copy)))
;undo is not said item by item
(. st_board :undo)
(push st_log (list 4 (. st_board :take_changes)))
(assert-eq "after an undo it is anything" :all (second (last st_log)))
(assert-true "and a copy is sent it all" (found? (stripes-delta st_doc st_log 3 4) ":full 1"))
(assert-true "as is one too far behind for what is kept" (found? (stripes-delta st_doc (list (list 4 (list))) 2 4) ":full 1"))
(assert-true "and one that is in step is sent no items" (not (found? (stripes-delta st_doc st_log 4 4) ":shape")))

;the paper, some of its rows, is those rows of all of it
(defq st_a (Canvas 200 120 1) st_b (Canvas 200 120 1))
(. st_a :fill 0) (. st_b :fill 0)
(cwb-paper st_a 200 120 0 :grid 32)
(.-> st_b (:set_clip 0 0 200 50)) (cwb-paper st_b 200 120 0 :grid 32 0 50)
(.-> st_b (:set_clip 0 50 200 120)) (cwb-paper st_b 200 120 0 :grid 32 50 120)
(assert-true "the paper drawn as two stripes is the paper drawn whole" (eql (st-bytes st_a) (st-bytes st_b)))
(each (lambda (style)
	(. st_a :fill 0) (. st_b :fill 0) (. st_b :set_clip 0 0 200 120)
	(cwb-paper st_a 200 120 0xff102030 style 24)
	(.-> st_b (:set_clip 0 0 200 61)) (cwb-paper st_b 200 120 0xff102030 style 24 0 61)
	(.-> st_b (:set_clip 0 61 200 120)) (cwb-paper st_b 200 120 0xff102030 style 24 61 120)
	(assert-true (cat "and so for " (str style)) (eql (st-bytes st_a) (st-bytes st_b))))
	'(:plain :lines :axis))

;the nodes. A board of many shapes, drawn by them, is what one task draws
(defq st_shared (canvas-shared 640 480 1))
(cond
	((not st_shared) (test-skip "stripes drawn by the nodes" "this host has no shared memory for pixels"))
	((< (length (lisp-nodes :t)) 2) (test-skip "stripes drawn by the nodes" "there is one node"))
	(:t
		(. st_shared :set_canvas_flags +canvas_flag_antialias)
		(defq st_board (Board (cwb-doc 640 480)) st_doc (. st_board :get_doc) st_stripes (Stripes st_board 3)
			st_timer (mail-mbox) st_select (cat (. st_stripes :mboxes) (list st_timer)))
		;240 shapes, of every kind, all over it, and no more than three
		;children, other tests are running
		(each (lambda (i)
			(defq x (% (* i 37) 600) y (% (* i 53) 440) col (+ 0xff000000 (% (* i 2654435761) 0xffffff)))
			(cwb-add st_doc (case (% i 5)
				(0 (cwb-shape (cwb-d-rect x y (+ x 40) (+ y 30) 6) :fill col :stroke 0xff000000 :width 2))
				(1 (cwb-shape (cwb-d-ellipse (+ x 20) (+ y 20) 18 12) :stroke col :width 3))
				(2 (cwb-shape (cwb-d-line x y (+ x 50) (+ y 35)) :stroke col :width 4 :cap2 :arrow))
				(3 (cwb-shape (cwb-d-arc (+ x 20) (+ y 20) 16 0.3 4.0 :t) :fill col))
				(:t (cwb-shape (board-pen-d (list x y (+ x 15) (+ y 25) (+ x 30) y (+ x 45) (+ y 25))) :stroke col :width 3)))))
			(range 0 240))
		(. st_board :changed :all)
		(defun st-pump (done secs)
			;what comes to the stripes is handled, till done says so or it has been too long
			(defq result :nil t0 (pii-time))
			(mail-timeout st_timer (task-timeout secs) 0)
			(until (or result (done))
				(defq msg (mail-read (elem-get st_select (defq idx (mail-select st_select)))))
				(cond
					((= idx 3) (setq result :timeout))
					((defq said (. st_stripes :handle idx msg)) (setq result said))))
			(mail-timeout st_timer 0 0)
			(ifn result :ok))
		(defun st-frame (zoom style)
			;a frame drawn by the nodes, :done when it is
			(. st_shared :fill 0)
			(and (. st_stripes :frame st_shared zoom style)
				(st-pump (lambda () :nil) 20)))
		(. st_stripes :start)
		(assert-eq "the children start" :ok
			(progn (st-pump (lambda () (. st_stripes :ready?)) 30) (if (. st_stripes :ready?) :ok :not)))
		(assert-true "and are in step with the document as it was, so a frame is cheap" (. st_stripes :cheap?))
		(assert-eq "a frame can not be asked for of a canvas whose pixels are its own" :nil
			(. st_stripes :frame (Canvas 64 64 1) 1.0 :grid))
		(. st_stripes :note)
		(assert-eq "a frame is drawn by them" :done (st-frame 1.0 :grid))
		(assert-true "every shape was drawn, some by more than one, where it is in two stripes"
			(>= (get :drawn st_stripes) 240))
		(assert-true "and it is, to the pixel, what one task draws"
			(eql (st-bytes st_shared) (st-local st_board 640 480 1.0 :grid)))
		;changed: one moved, one restyled, one gone, one new. The children are sent only those
		(. st_board :select (list 10 11)) (. st_board :transform (cwb-mat-move 33 21))
		(. st_board :select (list 20)) (. st_board :style :stroke 0xff00ff00 :fill 0xffff00ff)
		(. st_board :remove (list 30 31 32))
		(. st_board :add (cwb-shape (cwb-d-rect 100 100 300 200 20) :fill 0xc0ffffff :stroke 0xff000000 :width 5))
		(. st_stripes :note)
		(assert-list-eq "what changed is said item by item" '(10 11 20 241) (second (last (get :log st_stripes))))
		(assert-true "which is cheap" (. st_stripes :cheap?))
		(assert-eq "drawn again" :done (st-frame 1.0 :lines))
		(assert-true "it is what one task draws of the document as it is now"
			(eql (st-bytes st_shared) (st-local st_board 640 480 1.0 :lines)))
		;what is being moved is left out, it is drawn over the rest by the app
		(def st_board :floating (list 241 40))
		(. st_stripes :note)
		(assert-eq "drawn with two items being moved" :done (st-frame 1.0 :plain))
		(assert-true "they are left out, as one task leaves them out"
			(eql (st-bytes st_shared) (st-local st_board 640 480 1.0 :plain)))
		(def st_board :floating (list))
		;undo, everything might have changed
		(. st_board :undo) (. st_board :undo)
		(. st_stripes :note)
		(assert-eq "after two undos a frame is not cheap, each child would be sent it all" :nil (. st_stripes :cheap?))
		(assert-eq "they can be brought in step with no frame" :warm (progn (. st_stripes :warm) (st-pump (lambda () :nil) 20)))
		(assert-true "and then one is" (. st_stripes :cheap?))
		(assert-eq "after two undos" :done (st-frame 1.0 :axis))
		(assert-true "it is the document as it is" (eql (st-bytes st_shared) (st-local st_board 640 480 1.0 :axis)))
		;another size of canvas, another zoom
		(defq st_shared (canvas-shared 320 240 1))
		(. st_shared :set_canvas_flags +canvas_flag_antialias)
		(. st_stripes :note)
		(assert-eq "at half the size, on another canvas" :done (st-frame 0.5 :grid))
		(assert-true "it is what one task draws at half the size" (eql (st-bytes st_shared) (st-local st_board 320 240 0.5 :grid)))
		(assert-eq "kept, with nothing to do for no time at all, they are left be" :idle
			(progn (. st_stripes :keep) (get :state st_stripes)))
		(def st_stripes :stamp (- (pii-time) 6000000))
		(assert-eq "and after a while are brought in step, which keeps them" :warm
			(progn (. st_stripes :keep) (st-pump (lambda () :nil) 20)))
		(. st_stripes :close)
		(assert-eq "closed, it is not ready" :nil (. st_stripes :ready?))))

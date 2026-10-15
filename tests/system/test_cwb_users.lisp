(report-header "Whiteboard users: who made a thing, who a device is, whose an eraser leaves alone")

(import "gui/lisp.inc")
(import "lib/cwb/board.inc")

;Chris: "students could not erase things the teacher or the other student
;had written". The teacher, the presenter, is 0 and has the mouse; touch is
;anybody, 1; a pen or an eraser is a student, each its own, unless the
;teacher made it the teacher's

(defun us-go (board &rest events) (. board :pointers events))
(defun us-users (board) (map (# (elem-get %0 +cwb_user)) (cwb-items (. board :get_doc))))
(defun us-area (board color)
	(defq sum 0.0)
	(cwb-walk (cwb-items (. board :get_doc)) :nil (lambda (shape m)
		(if (= (elem-get shape +cwb_fill) color)
			(setq sum (+ sum (poly-area (poly-union (cwb-mat-paths m (first (cwb-flat shape))) (list) 0)))))))
	sum)

;who a device is
(defq us (Board (cwb-doc 400 300)))
(assert-eq "the mouse is the teacher's" +board_teacher (. us :user_of (ptr-event 0 :mouse 1 0 0)))
(assert-eq "touch is anybody, any finger" +board_anybody (. us :user_of (ptr-event 0x1000003 :touch 1 0 0)))
(assert-eq "a first pen is the first student" +board_first_student (. us :user_of (ptr-event 0x2000000 :pen 1 0 0)))
(assert-eq "a second pen is the next" (inc +board_first_student) (. us :user_of (ptr-event 0x3000000 :pen 1 0 0)))
(assert-eq "the first again is who it was" +board_first_student (. us :user_of (ptr-event 0x2000000 :pen 0 5 5)))
(assert-eq "an eraser is the teacher's, Chris: always under the control of the teacher" +board_teacher (. us :user_of (ptr-event 0x4000000 :eraser 1 0 0)))
(. us :appoint (ptr-event 0x3000000 :pen 1 0 0))
(assert-eq "the teacher makes the second pen the teacher's" +board_teacher (. us :user_of (ptr-event 0x3000000 :pen 1 0 0)))
(assert-eq "and the first is still a student" +board_first_student (. us :user_of (ptr-event 0x2000000 :pen 1 0 0)))

;what is drawn says who drew it
(us-go us (ptr-event 0 :mouse 1 20 20)) (us-go us (ptr-event 0 :mouse 1 100 40)) (us-go us (ptr-event 0 :mouse 0 100 40))
(us-go us (ptr-event 0x2000000 :pen 1 20 100)) (us-go us (ptr-event 0x2000000 :pen 1 100 140)) (us-go us (ptr-event 0x2000000 :pen 0 100 140))
(assert-eq "a line of the mouse is the teacher's, a line of the first pen the student's"
	(str (list +board_teacher +board_first_student)) (str (us-users us)))
(assert-eq "an item made by a script, with no one said, is the teacher's"
	+board_teacher (elem-get (first (cwb-find (. us :get_doc) (. us :add (cwb-shape (cwb-d-rect 0 0 5 5))))) +cwb_user))

;an eraser leaves what another made
(defun us-board ()
	;a box of the teacher's on the left, one of the first student's on the right
	(defq doc (cwb-doc 400 300))
	(cwb-add doc (cwb-shape (cwb-d-rect 50 50 150 150) :fill 0xffff0000 :stroke 0))
	(cwb-add doc (cwb-shape (cwb-d-rect 250 50 350 150) :fill 0xff0000ff :stroke 0 :user +board_first_student))
	(defq board (Board doc))
	(def board :rub_mode :bite)
	board)
(defq us (us-board))
(assert-true "the first student's eraser drawn through both bites" (. us :bite (path 100.0 100.0 300.0 100.0) 10.0 :nil +board_first_student))
(assert-eq "the teacher's box is whole" 10000.0 (us-area us 0xffff0000))
(assert-true "the student's own has lost a strip" (< (us-area us 0xff0000ff) 9000.0))
(assert-true "and what is left of it is the student's still"
	(every (# (or (/= (elem-get %0 +cwb_fill) 0xff0000ff) (= (elem-get %0 +cwb_user) +board_first_student)))
		(cwb-items (. us :get_doc))))
(defq us (us-board))
(assert-true "another student's eraser" (not (. us :bite (path 100.0 100.0 300.0 100.0) 10.0 :nil (inc +board_first_student))))
(assert-eq "takes nothing, of the teacher's or the first student's" (str (list 10000.0 10000.0)) (str (list (us-area us 0xffff0000) (us-area us 0xff0000ff))))
(assert-true "the teacher's eraser" (. us :bite (path 100.0 100.0 300.0 100.0) 10.0 :nil +board_teacher))
(assert-true "takes from both" (and (< (us-area us 0xffff0000) 9000.0) (< (us-area us 0xff0000ff) 9000.0)))
(defq us (us-board))
(assert-true "and one with no user said, a script, takes from all as it did"
	(and (. us :bite (path 100.0 100.0 300.0 100.0) 10.0) (< (us-area us 0xffff0000) 9000.0)))

;a pen bound to rub out is that pen's user: the first student's pen with
;its barrel button, bound to the eraser by default, leaves the teacher's
;and bites the student's own
(defq us (us-board))
(us-go us (ptr-event 0x2000000 :pen 1 0 0)) (us-go us (ptr-event 0x2000000 :pen 0 0 0))
(us-go us (ptr-event 0x2000000 :pen +pev_right 100 100)) (us-go us (ptr-event 0x2000000 :pen +pev_right 300 100))
(us-go us (ptr-event 0x2000000 :pen 0 300 100))
(assert-eq "a student's pen bound to rub out leaves the teacher's" 10000.0 (us-area us 0xffff0000))
(assert-true "and bites the student's own" (< (us-area us 0xff0000ff) 9000.0))

;the eraser device, as a hand moves it, is the teacher's and takes from all
(defq us (us-board))
(us-go us (ptr-event 0x5000000 :eraser 1 100 100)) (us-go us (ptr-event 0x5000000 :eraser 1 300 100))
(us-go us (ptr-event 0x5000000 :eraser 0 300 100))
(assert-true "the eraser takes from the teacher's and the student's" (and (< (us-area us 0xffff0000) 9000.0) (< (us-area us 0xff0000ff) 9000.0)))

;who made a thing is kept in the file, and the teacher's is not written
(defq us (us-board) us_out (string-stream (cat "")))
(cwb-save (. us :get_doc) us_out)
(assert-eq "only the student's says who made it" 1 (length (filter (# (nempty? (substr %0 ":user"))) (split (str us_out) (ascii-char 10)))))
(assert-eq "read back, each is who made it" (str (list +board_teacher +board_first_student))
	(str (map (# (elem-get %0 +cwb_user)) (cwb-items (cwb-load (string-stream (str us_out)))))))

;a tap, Chris: "a short tap for select mode toggle is really quick tap. Not
;a press and hold". On a thing it takes it in or out of select mode whatever
;the contact is bound to draw; on nothing it is a dot; held, it is a draw
(defun us-tap (board id kind x y held)
	(. board :tick (get :time board))
	(us-go board (ptr-event id kind 1 x y))
	(. board :tick (+ (get :time board) held))
	(us-go board (ptr-event id kind 0 x y)))
(defq us (us-board))
;strokes soon one after another are not put in a group as a word here
(def us :time 1000000 :auto_group 0)
(us-tap us 0x2000000 :pen 100 100 50000)
(assert-eq "a quick tap of a pen bound to draw on the teacher's box selects it" (str (list 1)) (str (. us :get_selected)))
(assert-eq "and draws nothing" 2 (length (cwb-items (. us :get_doc))))
(assert-eq "it is the student's selection, not the teacher's" (str (list (list 1) (list)))
	(str (list (. us :selected_by +board_first_student) (. us :selected_by +board_teacher))))
(us-tap us 0x2000000 :pen 102 101 50000)
(assert-eq "a second tap on it takes it out of select mode" 0 (length (. us :get_selected)))
(us-tap us 0x2000000 :pen 200 250 50000)
(assert-eq "a tap on nothing is a dot, drawn" 3 (length (cwb-items (. us :get_doc))))
(us-tap us 0x2000000 :pen 300 100 400000)
(assert-eq "a press and hold on the student's box is a draw, a dot, not a select"
	(str (list 4 0)) (str (list (length (cwb-items (. us :get_doc))) (length (. us :get_selected)))))
(us-go us (ptr-event 0x2000000 :pen 1 60 60)) (us-go us (ptr-event 0x2000000 :pen 1 140 140)) (us-go us (ptr-event 0x2000000 :pen 0 140 140))
(assert-eq "moved, across the box, it draws its line" 5 (length (cwb-items (. us :get_doc))))

;a binder binds what pressed it, the device and the buttons it had held, and
;the rest of what that device is stays. Chris: "The toolbar should really be
;a place for a device/button combo to have its bound action changed"
(defq us_b (bindings-default))
(. us_b :set :pen 0x2000000 +pev_left :color 0xffff0000)
(assert-eq "the first pen's tip is red" 0xffff0000 (. us_b :value (ptr-event 0x2000000 :pen +pev_left 0 0) :color))
(assert-eq "and still draws" :pen (. us_b :value (ptr-event 0x2000000 :pen +pev_left 0 0) :tool))
(assert-eq "its barrel still rubs out" :eraser (. us_b :value (ptr-event 0x2000000 :pen +pev_right 0 0) :tool))
(assert-eq "another pen is not red" :nil (. us_b :value (ptr-event 0x3000000 :pen +pev_left 0 0) :color))
(. us_b :set :pen 0x2000000 +pev_left :width 9.0)
(assert-eq "set again, the tip is wider and red still" (str (list 9.0 0xffff0000))
	(str (list (. us_b :value (ptr-event 0x2000000 :pen +pev_left 0 0) :width) (. us_b :value (ptr-event 0x2000000 :pen +pev_left 0 0) :color))))
(. us_b :set :touch 0x1000000 :nil :tool :pen)
(assert-eq "touch bound to draw, every finger of the panel" (str (list :pen :pen))
	(str (list (. us_b :value (ptr-event 0x1000001 :touch 1 0 0) :tool) (. us_b :value (ptr-event 0x1000007 :touch 1 0 0) :tool))))
(assert-eq "and the mouse is as it was" :hand (. us_b :value (ptr-event 0 :mouse +pev_right 0 0) :tool))

;two in flight at once, one pushed to the back and one pulled to the front.
;Chris: "pull to front and push to back, by multiple users at the same time,
;so we will have to have 2 inflight layers, back/front and the committed
;layer in the middle"
(defq us (us-board))
(us-go us (ptr-event 0 :mouse +pev_right 100 100))
(us-go us (ptr-event 0x1000001 :touch 1 300 100))
(assert-eq "the teacher's box, taken by the right button, is behind; the student's, by a finger, in front"
	(str (list :t :nil)) (str (list (if (. us :back? 1) :t :nil) (if (. us :back? 2) :t :nil))))
(us-go us (ptr-event 0 :mouse +pev_right 120 110) (ptr-event 0x1000001 :touch 1 320 110))
(assert-eq "moved, each is where its hand took it still" (str (list :t :nil)) (str (list (if (. us :back? 1) :t :nil) (if (. us :back? 2) :t :nil))))
(defq us_c (Canvas 400 300 1))
(. us_c :fill 0) (. us :draw_flight us_c (Mat3x2) :back)
(defq us_s (memory-stream)) (pixmap-write (getf us_c +canvas_pixmap 0) us_s 32) (stream-seek us_s 0 0)
(defq us_d (read-blk us_s 10000000) us_at (- (length us_d) (* 400 300 4)))
(defun us-px (x y) (get-uint us_d (+ us_at (* 4 (+ x (* y 400))))))
(assert-eq "the flight behind has the box at the back and not the one in front"
	(str (list :t :nil)) (str (list (/= 0 (us-px 140 130)) (/= 0 (us-px 340 130)))))

;the file is the committed layer, and that's that, Chris: a document has no
;layers. One of version 4, of two layers, is read as one list, the back
;layer's first; it is written as version 5, the one list
(defq us_v4 (cat "((:Emap 1) :version 4 :width 100 :height 100 :next_id 3 :layers ((:list 2)"
	" ((:list 3) \q[back]\q 0 ((:list 1) ((:list 5) :shape :id 1 :d \q[M 0 0 L 10 10]\q)))"
	" ((:list 3) \q[front]\q 2 ((:list 1) ((:list 5) :shape :id 2 :d \q[M 5 5 L 20 20]\q)))))"))
(defq us_doc (cwb-load (string-stream us_v4)))
(assert-eq "a file of two layers is one list, the back layer's first"
	(str (list 1 (list 1 2))) (str (list (length (cwb-layers us_doc)) (map (# (elem-get %0 +cwb_id)) (cwb-items us_doc)))))
(defq us_out (string-stream (cat "")))
(cwb-save us_doc us_out)
(assert-eq "written, it is version 5, of items, with no layers"
	(str (list :t :t :nil)) (str (list (nempty? (substr (str us_out) ":version 5")) (nempty? (substr (str us_out) ":items"))
		(nempty? (substr (str us_out) ":layers")))))
(assert-eq "and read back it is the same"
	(str (list 1 2)) (str (map (# (elem-get %0 +cwb_id)) (cwb-items (cwb-load (string-stream (str us_out)))))))

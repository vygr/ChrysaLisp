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

(report-header "Whiteboard shared: a teacher's board and two that joined it, by deltas, with no machine")

(import "gui/lisp.inc")
(import "lib/cwb/share.inc")

;the teacher's board, and two students', each joined to the teacher's
;alone. What would go by mail is passed here by hand
(defq sh_t (share-start (Board (cwb-doc 400 300)) :t)
	sh_a (share-start (Board (cwb-doc 400 300)))
	sh_b (share-start (Board (cwb-doc 400 300))))
(share-add sh_t :a) (share-add sh_t :b) (share-add sh_a :t) (share-add sh_b :t)
(share-id-base sh_a 1000000) (share-id-base sh_b 2000000)
(defun sh-ids (board) (str (share-ids (. board :get_doc))))
(defun sh-go (from key to name)
	;what from sends to key, given to to as from name, and answered
	(when (defq text (share-delta from key))
		(share-apply to text name)
		(share-acked from key)
		:t))
(defun sh-round ()
	;each student sends to the teacher, and the teacher to each
	(sh-go sh_a :t sh_t :a) (sh-go sh_b :t sh_t :b)
	(sh-go sh_t :a sh_a :t) (sh-go sh_t :b sh_b :t))

;the teacher draws
(. sh_t :add (cwb-shape (cwb-d-rect 10 10 50 50) :fill 0xffff0000 :stroke 0))
(sh-round)
(assert-eq "what the teacher draws is on both" (str (list "(1)" "(1)" "(1)")) (str (list (sh-ids sh_t) (sh-ids sh_a) (sh-ids sh_b))))

;a student draws, from ids of their own
(. sh_a :add (cwb-shape (cwb-d-rect 100 10 150 50) :fill 0xff00ff00 :stroke 0 :user 2))
(sh-round)
(assert-eq "what a student draws is on the teacher's, and the other student's, by an id of its own"
	(str (list "(1 1000000)" "(1 1000000)" "(1 1000000)")) (str (list (sh-ids sh_t) (sh-ids sh_a) (sh-ids sh_b))))
(assert-eq "and keeps who drew it" 2 (elem-get (last (cwb-items (. sh_b :get_doc))) +cwb_user))

;nothing to send, nothing is sent, and one waiting on its last is sent nothing
(assert-eq "with nothing new, nothing goes" :nil (share-delta sh_t :a))
(. sh_t :add (cwb-shape (cwb-d-rect 200 10 250 50) :fill 0xff0000ff :stroke 0))
(defq sh_one (share-delta sh_t :a))
(. sh_t :add (cwb-shape (cwb-d-rect 300 10 350 50) :fill 0xff0000ff :stroke 0))
(assert-eq "one that has not answered the last is sent nothing more" :nil (share-delta sh_t :a))
(share-apply sh_a sh_one :t) (share-acked sh_t :a)
(sh-round)
(assert-eq "answered, it is sent the rest, all at once" (str (list "(1 1000000 2 3)" "(1 1000000 2 3)")) (str (list (sh-ids sh_a) (sh-ids sh_b))))

;sent again it does nothing more
(defq sh_was (sh-ids sh_a))
(share-apply sh_a sh_one :t)
(assert-eq "a delta given twice does nothing the second time" sh_was (sh-ids sh_a))

;what is gone goes, from either end
(. sh_t :remove (list 2))
(. sh_b :remove (list 1000000))
(sh-round) (sh-round)
(assert-eq "what the teacher took out and what a student did are gone from all" (str (list "(1 3)" "(1 3)" "(1 3)"))
	(str (list (sh-ids sh_t) (sh-ids sh_a) (sh-ids sh_b))))

;the order goes too
(. sh_a :select (list 1)) (. sh_a :order :t)
(sh-round) (sh-round)
(assert-eq "a thing a student brought to the front is at the front on all" (str (list "(3 1)" "(3 1)" "(3 1)"))
	(str (list (sh-ids sh_t) (sh-ids sh_a) (sh-ids sh_b))))

;what is being drawn is seen as it is drawn, by the others and not itself
(def sh_a :mode :pen)
(. sh_a :pointers (list (ptr-event 0x2000000 :pen 1 20 200)))
(. sh_a :pointers (list (ptr-event 0x2000000 :pen 1 80 240)))
(sh-round)
(assert-eq "a line a student is drawing is seen on the teacher's and the other's, as it is"
	(str (list 1 1 0)) (str (list (. (get :remote_temp sh_t) :size) (. (get :remote_temp sh_b) :size) (. (get :remote_temp sh_a) :size))))
(. sh_a :pointers (list (ptr-event 0x2000000 :pen 0 80 240)))
(sh-round) (sh-round)
(assert-eq "let go it is kept, on all, and is no longer being drawn anywhere"
	(str (list 3 3 3 0 0)) (str (list (length (cwb-items (. sh_t :get_doc))) (length (cwb-items (. sh_a :get_doc)))
		(length (cwb-items (. sh_b :get_doc))) (. (get :remote_temp sh_t) :size) (. (get :remote_temp sh_b) :size))))

;one that joins late has it all
(defq sh_c (share-start (Board (cwb-doc 400 300))))
(share-add sh_t :c) (share-add sh_c :t) (share-id-base sh_c 3000000)
(sh-go sh_t :c sh_c :t)
(assert-eq "a board that joins is sent all there is" (sh-ids sh_t) (sh-ids sh_c))

;first come first served between machines, Chris: "first come first served
;for who is manipulating what shapes/groups !"
(defq sh_t (share-start (Board (cwb-doc 400 300)) :t)
	sh_a (share-start (Board (cwb-doc 400 300)))
	sh_b (share-start (Board (cwb-doc 400 300))))
(share-add sh_t :a) (share-add sh_t :b) (share-add sh_a :t) (share-add sh_b :t)
(share-id-base sh_a 1000000) (share-id-base sh_b 2000000)
(. sh_t :add (cwb-shape (cwb-d-rect 100 100 150 150) :fill 0xffff0000 :stroke 0))
(sh-round)
(defun sh-x (board) (n2i (first (cwb-bounds (cwb-items (. board :get_doc))))))
;each takes hold of it with the hand, the right button of a mouse of its own
(. sh_a :pointers (list (ptr-event 0 :mouse +pev_right 120 120)))
(. sh_b :pointers (list (ptr-event 0 :mouse +pev_right 125 125)))
(sh-round)
(assert-eq "two take hold at once: the first the host heard of has it, the other has let it go"
	(str (list :a (list 1) (list))) (str (list (. (get :share_holds sh_t) :find 1) (. sh_a :get_selected) (. sh_b :get_selected))))
(. sh_a :pointers (list (ptr-event 0 :mouse +pev_right 170 120)))
(. sh_b :pointers (list (ptr-event 0 :mouse +pev_right 300 125)))
(sh-round) (sh-round)
(assert-eq "what the one that has it does goes to all, what the other does to it goes nowhere"
	(str (list 150 150 150)) (str (list (sh-x sh_t) (sh-x sh_a) (sh-x sh_b))))
(. sh_a :pointers (list (ptr-event 0 :mouse 0 170 120)))
(. sh_a :select (list))
(sh-round) (sh-round)
(assert-eq "let go and put down, no one has it" :nil (. (get :share_holds sh_t) :find 1))

;a thing selected on one board is held on all, "own only": no other board
;can take it, by a box, a tap or a hand, and each sees it is held
(. sh_a :select (list 1))
(sh-round) (sh-round)
(assert-eq "selected there, it is held for that board" :a (. (get :share_holds sh_t) :find 1))
(assert-list-eq "and the other board can see it is taken" (list 1) (. sh_b :taken))
(. sh_b :select (list 1))
(assert-list-eq "a box there does not take it" (list) (. sh_b :get_selected))
(. sh_b :pick_up 1 0)
(assert-list-eq "nor a tap" (list) (. sh_b :get_selected))
(assert-eq "and it has no one of that board's as its selector" :nil (. sh_b :holder 1))
(assert-list-eq "the teacher can not take it either" (list) (progn (. sh_t :select (list 1)) (. sh_t :get_selected)))
(. sh_a :select (list))
(sh-round) (sh-round)
(assert-list-eq "put down, it is no one's" (list) (. sh_b :taken))
(. sh_b :select (list 1))
(sh-round) (sh-round)
(assert-eq "and the other board may have it now" :b (. (get :share_holds sh_t) :find 1))
(. sh_b :select (list))
(sh-round) (sh-round)

;the paper is each board's own: one may be full screen, one a window
(. (. sh_t :get_doc) :insert :width 1920)
(. sh_t :add (cwb-shape (cwb-d-rect 10 10 20 20) :fill 0xff0000ff :stroke 0))
(sh-round)
(assert-eq "the teacher's paper is not the student's" 400 (. (. sh_a :get_doc) :find :width))

;what another board holds is theirs to the eraser too: it is not bitten
;nor cut here, and so it does not snap back when the host says it is as it
;was. Put down, the eraser has it
(defq sh_t (share-start (Board (cwb-doc 400 300)) :t) sh_a (share-start (Board (cwb-doc 400 300))))
(share-add sh_t :a) (share-add sh_a :t) (share-id-base sh_a 1000000)
(. sh_t :add (cwb-shape (cwb-d-rect 100 100 200 200) :fill 0xffff0000 :stroke 0))
(. sh_t :add (cwb-shape (cwb-d-points (list 10 50 390 50) :nil) :stroke 0xff000000 :width 4))
(sh-go sh_t :a sh_a :t)
(. sh_t :select (list 1 2))
(sh-go sh_t :a sh_a :t)
(defun sh-has (board id) (if (find id (map (# (elem-get %0 +cwb_id)) (cwb-items (. board :get_doc)))) :t :nil))
(defun sh-look (board) (str (cwb-items (. board :get_doc))))
(defq sh_was (sh-look sh_a))
(def sh_a :rub_mode :part)
(. sh_a :bite (path 140.0 150.0 160.0 150.0) 10)
(. sh_a :rub 200 50 6)
(assert-eq "what another holds is not bitten nor cut" sh_was (sh-look sh_a))
(. sh_t :select (list))
(sh-go sh_t :a sh_a :t)
(assert-list-eq "put down, the other board is told, with nothing else changed" (list) (. sh_a :taken))
(. sh_a :bite (path 140.0 150.0 160.0 150.0) 10)
(. sh_a :rub 200 50 6)
(assert-true "and the eraser has it" (nql sh_was (sh-look sh_a)))
(assert-eq "the line cut" :nil (sh-has sh_a 2))

;a board that joined is a student's: its mouse rubs out and deletes only
;what it made, the teacher's are left. Trusted by the host, its teachers
;switch, the mouse there is the teacher, and has anything not held
(defq sh_t (share-start (Board (cwb-doc 400 300)) :t) sh_a (share-start (Board (cwb-doc 400 300))))
(share-add sh_t :a) (share-add sh_a :t) (share-id-base sh_a 1000000) (def sh_a :user_base 100)
(. sh_t :add (cwb-shape (cwb-d-rect 100 100 200 200) :fill 0xffff0000 :stroke 0))
(. sh_t :add (cwb-shape (cwb-d-rect 250 100 350 200) :fill 0xff00ff00 :stroke 0))
(sh-go sh_t :a sh_a :t)
(defq sh_mouse (. sh_a :user_of (ptr-event 0 :mouse 0 10 10)))
(assert-eq "the mouse of a board that joined is not the teacher" 100 sh_mouse)
(. sh_a :select (list 1)) (. sh_a :delete (list 1) sh_mouse)
(assert-eq "it deletes nothing of the teacher's" :t (sh-has sh_a 1))
(. sh_a :select (list))
(defq sh_was (sh-look sh_a))
(. sh_a :bite (path 290.0 150.0 310.0 150.0) 10 :nil sh_mouse)
(assert-eq "nor rubs it out" sh_was (sh-look sh_a))
(def sh_a :trusted :t)
(defq sh_mouse (. sh_a :user_of (ptr-event 0 :mouse 0 10 10)))
(assert-eq "trusted, its mouse is the teacher" +board_teacher sh_mouse)
(. sh_a :select (list 1)) (. sh_a :delete (list 1) sh_mouse)
(assert-eq "and deletes what the teacher made" :nil (sh-has sh_a 1))
(. sh_a :bite (path 290.0 150.0 310.0 150.0) 10 :nil sh_mouse)
(assert-true "and rubs it out" (nql sh_was (sh-look sh_a)))

;the pointers of each board are shown on the others, each board by its
;number for its color, the host passing them on: a mouse that moves, and
;one still for a while that is shown no more
(defq sh_t (share-start (Board (cwb-doc 400 300)) :t)
	sh_a (share-start (Board (cwb-doc 400 300))) sh_b (share-start (Board (cwb-doc 400 300))))
(share-add sh_t :a) (share-add sh_t :b) (share-add sh_a :t) (share-add sh_b :t)
(share-id-base sh_a 1000000) (share-id-base sh_b 2000000) (def sh_a :user_base 100) (def sh_b :user_base 200)
(. sh_a :pointers (list (ptr-event 0 :mouse 0 120 130)))
(sh-round) (sh-round)
(defun sh-pointers (board)
	(defq out (list))
	(. (get :remote_pointers board) :each (lambda (k (from x y kind down who)) (push out (list (n2i x) (n2i y) kind who))))
	out)
(assert-eq "a student's mouse is on the teacher's board, as that board's" (str (list (list 120 130 :mouse 1))) (str (sh-pointers sh_t)))
(assert-eq "and passed on to the other student's" (str (list (list 120 130 :mouse 1))) (str (sh-pointers sh_b)))
(assert-eq "and not back to its own" "()" (str (sh-pointers sh_a)))
;still for longer than is shown
(. (get :pointer_at sh_a) :each (lambda (id (x y kind down when))
	(. (get :pointer_at sh_a) :insert id (list x y kind down (- (pii-time) 10000000)))))
(sh-round) (sh-round)
(assert-eq "a mouse still for a while is shown no more" "()" (str (sh-pointers sh_t)))
(assert-eq "anywhere" "()" (str (sh-pointers sh_b)))

;what is someone else's is drawn in two colors, a line with white dashes
;along it, to be seen over anything
(defun sh-colors (canvas y x x1)
	;the colors along a row of a canvas, of what is more there than not
	(defq stream (memory-stream))
	(pixmap-write (getf canvas +canvas_pixmap 0) stream 32)
	(stream-seek stream 0 0)
	(bind '(w h) (. canvas :pref_size))
	(defq d (read-blk stream 10000000) at (- (length d) (* w h 4)) out (list))
	(each (# (defq c (get-uint d (+ at (* 4 (+ (* y w) %0)))))
			(and (>= (logand c 0xff000000) 0x80000000) (not (find (setq c (logand c 0xffffff)) out)) (push out c)))
		(range x x1))
	out)
;and each in the color of the board that has it, as its pointer is
(defq sh_canvas (Canvas 400 300 1))
(. sh_t :add (cwb-shape (cwb-d-rect 100.0 100.0 200.0 200.0) :fill 0xff000000 :stroke 0))
(sh-round)
(. sh_b :select (list (elem-get (last (cwb-items (. sh_b :get_doc))) +cwb_id)))
(sh-round) (sh-round)
(defun sh-held-look (board)
	(. sh_canvas :fill 0) (. board :draw_overlay sh_canvas :nil :t)
	(defq cols (sh-colors sh_canvas 100 110 190))
	(list (if (find 0xffffff cols) :t) (if (find (elem-get +board_guest_colors 2) cols) :t)))
(assert-list-eq "a thing the second student holds has its line on the teacher's board in that student's color, with white dashes" '(:t :t)
	(sh-held-look sh_t))
(assert-list-eq "and on the other student's" '(:t :t) (sh-held-look sh_a))
(. sh_b :select (list))
(sh-round) (sh-round)
(defq sh_g sh_t)
(. (get :remote_pointers sh_g) :insert "a/0" (list :a 300.0 150.0 :touch :t 1))
(. sh_canvas :fill 0) (. sh_g :draw_actors sh_canvas)
(defq sh_cols (sh-colors sh_canvas 134 290 310))
(assert-list-eq "and a finger of another board has a ring of its color with white dashes" '(:t :t)
	(list (if (find 0xffffff sh_cols) :t) (if (some (# (/= %0 0xffffff)) sh_cols) :t)))

;an order changed on one board is not undone by what the other sends at
;the same time, its pointers moving, with its order as it was: each makes
;its delta before it has the other's
(defq sh_t (share-start (Board (cwb-doc 400 300)) :t) sh_a (share-start (Board (cwb-doc 400 300))))
(share-add sh_t :a) (share-add sh_a :t) (share-id-base sh_a 1000000) (def sh_a :user_base 100)
(defun sh-cross ()
	(defq ta (share-delta sh_a :t) tt (share-delta sh_t :a))
	(when ta (share-apply sh_t ta :a) (share-acked sh_a :t))
	(when tt (share-apply sh_a tt :t) (share-acked sh_t :a)))
(defun sh-busy (n)
	;so many crossings, each pointer moved before each
	(each (lambda (i)
			(. sh_a :pointers (list (ptr-event 0 :mouse 0 (+ 300 i) 250)))
			(. sh_t :pointers (list (ptr-event 0 :mouse 0 (+ 300 i) 260)))
			(sh-cross))
		(range 0 n))
	(list (sh-ids sh_t) (sh-ids sh_a)))
(defun sh-tap (board buttons x y)
	(. board :select (list))
	(. board :pointers (list (ptr-event 0 :mouse buttons x y))) (. board :pointers (list (ptr-event 0 :mouse 0 x y))))
(each (# (. sh_t :add (cwb-shape (cwb-d-rect %0 50 (+ %0 100) 150) :fill 0xffff0000 :stroke 0))) '(50 100 150))
(def sh_t :mode :select) (def sh_a :mode :select)
(sh-busy 2)
(sh-tap sh_t +pev_right 220 100)
(assert-eq "the teacher sends one to the back, and it stays there on both" (str (list "(3 1 2)" "(3 1 2)")) (str (sh-busy 4)))
(sh-tap sh_t +pev_left 80 100)
(assert-eq "brings one to the front, and it stays there on both" (str (list "(3 2 1)" "(3 2 1)")) (str (sh-busy 4)))
(sh-tap sh_a +pev_right 80 100)
(assert-eq "and the student sends one to the back, and so it is on both" (str (list "(1 3 2)" "(1 3 2)")) (str (sh-busy 4)))

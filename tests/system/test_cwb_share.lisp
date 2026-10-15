(report-header "Whiteboard shared: a teacher's board and two that joined it, by deltas, with no machine")

(import "gui/lisp.inc")
(import "lib/cwb/share.inc")

;the teacher's board, and two students', each joined to the teacher's
;alone. What would go by mail is passed here by hand
(defq sh_t (share-start (Board (cwb-doc 400 300)))
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

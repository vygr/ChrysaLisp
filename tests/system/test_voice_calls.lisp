(report-header "Voice calls: started, joined, who is in, mute, left and ended")

(import "service/voice/app.inc")

;two voice services, as two desktops would have, each told by mail
(defq vc_a (open-child "service/voice/app_impl.lisp" +kn_call_pin)
	vc_b (open-child "service/voice/app_impl.lisp" +kn_call_pin)
	vc_wa (mail-mbox) vc_wb (mail-mbox))
(defun vc-tell (svc text) (mail-send svc text))
(defun vc-state (watcher test)
	;the state a watcher is told, the first that passes test, as long as
	;that takes. Watched again, as an app does within the lease, and so told
	;at once, and again whenever nothing has come for a fifth of a second
	(defq got :nil done :nil quiet :t)
	(until done
		(when quiet
			(vc-tell vc_a (cat "W" (hex-encode vc_wa)))
			(vc-tell vc_b (cat "W" (hex-encode vc_wb))))
		(setq quiet :t)
		(when (defq msg (mail-read-timeout watcher 200000))
			(setq got (call-state-read msg) done (test got) quiet :nil)))
	got)
(defun vc-names (state) (sort (map (const first) (ifn (. state :find :members) (list))) (const cmp)))
(vc-tell vc_a (cat "W" (hex-encode vc_wa)))
(vc-tell vc_b (cat "W" (hex-encode vc_wb)))
(assert-eq "a watcher is told at once, in no call" :nil (. (vc-state vc_wa (const identity)) :find :id))
(vc-state vc_wb (const identity))

(vc-tell vc_a "SAnn,Maths  the class")
(defq vc_s (vc-state vc_wa (# (. %0 :find :id))))
(assert-list-eq "a call started is the starter's, of its name, with it alone in it" (list (hex-encode vc_a) "Maths  the class" 1)
	(list (. vc_s :find :id) (. vc_s :find :name) (length (. vc_s :find :members))))
(assert-eq "and is on the mesh by that name" "Maths  the class"
	(some (# (if (eql (first %0) (hex-encode vc_a)) (second %0))) (call-list)))

(vc-tell vc_b (cat "JBob," (hex-encode vc_a)))
(defq vc_s (vc-state vc_wb (# (= (length (ifn (. %0 :find :members) (list))) 2))))
(assert-list-eq "one that joins is in it with its name, both are, by their users" (list (hex-encode vc_a) "Maths  the class" '("Ann" "Bob"))
	(list (. vc_s :find :id) (. vc_s :find :name) (vc-names vc_s)))
(assert-eq "a name of a call has no comma, the mesh keeps one after one" "a b" (call-name "a,b"))
(assert-eq "and the starter is told so" 2 (length (. (vc-state vc_wa (# (= (length (. %0 :find :members)) 2))) :find :members)))

(vc-tell vc_b "R1")
(defun vc-rec (state) (map (# (list (first %0) (elem-get %0 2))) (sort (cat (ifn (. state :find :members) (list))) (# (cmp (first %0) (first %1))))))
(defq vc_bob_rec 1)
(defun vc-bob? (state)
	;a test of a state: Bob is in it, recording as vc_bob_rec says. A list
	;is found by (find) only if it is the very same, so not by what is in it
	(some (# (and (eql (first %0) "Bob") (= (second %0) vc_bob_rec))) (vc-rec state)))
(assert-list-eq "one that records says so, and the others in the call are told it" '(("Ann" 0) ("Bob" 1))
	(vc-rec (vc-state vc_wa vc-bob?)))
(vc-tell vc_b "R0")
(setq vc_bob_rec 0)
(assert-list-eq "and when it stops" '(("Ann" 0) ("Bob" 0)) (vc-rec (vc-state vc_wa vc-bob?)))
(vc-tell vc_b "M1")
(assert-eq "mute is said to the watchers" :t (. (vc-state vc_wb (# (. %0 :find :muted))) :find :muted))
(vc-tell vc_b "M0")
(vc-state vc_wb (# (not (. %0 :find :muted))))

(vc-tell vc_b "L")
(assert-eq "one that leaves is out of it" :nil (. (vc-state vc_wb (# (not (. %0 :find :id)))) :find :id))
(assert-eq "and the starter is alone again" 1 (length (. (vc-state vc_wa (# (= (length (. %0 :find :members)) 1))) :find :members)))

(vc-tell vc_b (cat "JBob," (hex-encode vc_a)))
(vc-state vc_wb (# (= (length (ifn (. %0 :find :members) (list))) 2)))
(vc-tell vc_a "L")
(assert-eq "the starter leaves, and the call is ended for all" :nil (. (vc-state vc_wb (# (not (. %0 :find :id)))) :find :id))
(assert-eq "and is on the mesh no more" :nil (find (hex-encode vc_a) (map (const first) (call-list))))

;an app that showed the call put away: the call is left, unless another
;shows it as its own
(vc-tell vc_a "SAnn,Physics")
(vc-state vc_wa (# (. %0 :find :id)))
(vc-tell vc_b (cat "JBob," (hex-encode vc_a)))
(vc-state vc_wb (# (= (length (ifn (. %0 :find :members) (list))) 2)))
(defq vc_shows (mail-mbox))
(vc-tell vc_b (cat "V" (hex-encode vc_shows)))
(vc-tell vc_b "X")
(task-sleep 300000)
(assert-eq "put away while another app shows it, it stays in the call" 2 (length (. (vc-state vc_wb (const identity)) :find :members)))
(vc-tell vc_b (cat "U" (hex-encode vc_shows)))
(vc-tell vc_b "X")
(assert-eq "put away with nothing else showing it, it leaves" :nil (. (vc-state vc_wb (# (not (. %0 :find :id)))) :find :id))

(vc-tell vc_a "Q") (vc-tell vc_b "Q")
(undef (env) 'vc_a 'vc_b 'vc_wa 'vc_wb 'vc_s 'vc_bob_rec)

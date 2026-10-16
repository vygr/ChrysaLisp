;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the gophers of a member
;
; A gopher is a query sent to an object: a path, the block to begin at,
; what it has, and a function. It goes to the blocks of the object one at
; a time. The function is given what it has, the block, which block it is
; and how many there are, and gives what it has now and the block to go to
; next, or :nil, it is done. Then what it has is sent straight to who
; asked, as text, in a list. If it can not go on, "none" is.
;
; What it has and the function are all there is of it, so it can be sent
; on, as it is now, to wherever the block it wants is. How it is sent on,
; and what is done when that fails, is in ./gopher.inc.
;
; This task is started by a Storage service, and runs every gopher that
; is at that member, for as long as the service does. There is one, so a
; gopher that comes here finds it up, with its files read: it costs a
; mail to send a gopher on, and no task is started. It does a block of
; each gopher it has in turn, and reads its mail between, so none waits
; for another to be done, and one that is being sent on, and is waiting
; to hear it got there, holds up nothing.
;
; A function is not run as it was sent. Wherever it can go round, the
; test of a while or an until and the body of a lambda, a call of
; (gopher-tick) is put in: it gives way to the other tasks of the node,
; and once the function has had its time for the one block, a second by
; the clock, it says no, and every loop of the function ends, and every
; function of it given to a map or an each does nothing more. So the
; function comes back by itself, soon, and is done with, who asked is
; told none. Nothing is thrown: in a release build what an error is
; passed back up by is not there, and an error thrown in a loop would be
; a value it went round with. Chris: "it should be the case that
; (time-slice) call if forced or injected into the gopher code", and, of
; the throw it was first: "release mode is for fully tested code, it's
; should never throw !"
;
; It is mailed its service, and then each gopher as the service is sent
; it: who to tell, a line, who asked, a line, - if that is who to tell,
; and then the one form, (path at state function trail skip version
; copies). trail is where it has been, skip the members that did not
; answer it, version the name of what the object was when it set out, and
; copies how many members were to keep its blocks then, neither yet if it
; has just set out. One that was sent on says so, to who sent it,
; before anything else is done with it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./app.inc")
(import "./gopher.inc")

;a gopher that is here. how is what is to be tried when the block it
;wants is not: 0 where it is worked out to be, 1 those that say they
;have it, 2 nothing more. pass is there while it is being sent on
(enums +gopher 0
	(enum asker name at state source visit names version copies trail skip how pass pinned tag budget))

;how many objects it keeps the blocks of, between gophers, and how often
;it looks while one is being sent on and nothing else is to be done
(defq +gopher_kept 16 +gopher_look 20000)

;how long a function may have for a block, and when the one that is
;running has had it
;a step is tried first with so long, +gopher_first, and one that is not
;done in that has its turn again after the others here, with all it may
;have, +gopher_allowed
(defq +gopher_allowed 1000000 +gopher_first 50000 *gopher_until* 0 *gopher_late* :nil *gopher_check* 0
	;in the slow lane, where the quick runner says if it has blocks to do,
	;and if it has
	*gopher_hold* :nil *gopher_held* :nil
	;and how many lists a function may be made of, its macros done
	+gopher_lists 20000)

(defun gopher-tick-past ()
	;the time to look again has come: rest with the bots of the node if
	;that is due, and say no if the block's second is up
	(defq now (pii-time))
	(bot-breathe now)
	;in the slow lane, while the quick runner has blocks to do, it waits,
	;and the time it waits is not of its second
	(when *gopher_hold*
		(while (mail-poll (list *gopher_hold*)) (setq *gopher_held* (eql (mail-read *gopher_hold*) "1")))
		(when *gopher_held*
			(while *gopher_held*
				(setq *gopher_held* (eql (mail-read *gopher_hold*) "1"))
				(while (mail-poll (list *gopher_hold*)) (setq *gopher_held* (eql (mail-read *gopher_hold*) "1"))))
			(setq *gopher_until* (+ *gopher_until* (- (pii-time) now)) now (pii-time))))
	(cond
		(*gopher_late* :nil)
		((> now *gopher_until*) (setq *gopher_late* :t) :nil)
		(:t (setq *gopher_check* (min *gopher_until* (if (< now *bot_rest_end*) *bot_rest_end* *bot_next*)))
			:t)))

(defun gopher-tick ()
	; (gopher-tick) -> :t | :nil
	;what is put into a function wherever it can go round: it gives way,
	;and says if the function may go on. Once it may not, it says so till
	;the next block.
	;
	;It is in every loop of a function, a million times in a big job, and
	;a name looked up from deep in one walks a long way, so the most it
	;does is one look at the clock against one time of the runner's own,
	;*gopher_check*, the sooner of when the block's second is up and when
	;the bots of the node rest next. Only past that is more looked at
	(cond
		((< (pii-time) *gopher_check*) (task-slice) :t)
		((gopher-tick-past))))

(defun gopher-ticked (form)
	; (gopher-ticked form) -> form
	;the function with (gopher-tick) in the test of each while and until
	;of it, and round the body of each lambda, its macros done so that
	;none is hidden in one, and with forms that are not macros, if and
	;progn. What is quoted is not code and is left.
	;
	;Nothing that is there already is written to. A macro may put into the
	;form a list that is not the form's own, and what is run has its names
	;bound in place: a tick put into such a list would be in every use of
	;it, on the whole node. So each list that is gone into is a copy, made
	;as it is come to, and a tick is a new list each time. A form that
	;has no end, a list that holds itself, is thrown out
	(defq hold (list (macroexpand form)) todo (list (list hold 0)) lists 0
		;a macro may have put in the thing itself, where the source has its name
		round (list 'while 'until 'lambda (const while) (const until) (const lambda))
		quoted (list 'quote (const quote)))
	(while (defq at (pop todo))
		(bind '(parent idx) at)
		(defq form (elem-get parent idx))
		(when (and (list? form) (nempty? form) (not (find (defq head (first form)) quoted)))
			(if (> (++ lists) +gopher_lists) (throw "a gopher's function is too big" lists))
			(defq from 0)
			(cond
				((and (defq which (find head round)) (> (length form) 1))
					(setq form (case (% which 3)
						;the test, which says no once the time is up
						(0 (cat (list head (list 'if (list 'gopher-tick) (second form) :nil)) (slice form 2 -1)))
						(1 (cat (list head (list 'if (list 'gopher-tick) (second form) :t)) (slice form 2 -1)))
						;the body, done only while there is time, the args of a
						;lambda are names, not code
						(:t (setq from 2)
							(list head (second form) (list 'if (list 'gopher-tick) (cat (list 'progn) (slice form 2 -1))))))))
				((setq form (cat form))))
			(elem-set parent idx form)
			(each! (lambda (_) (push todo (list form (!)))) (list form) from)))
	(first hold))

(defun gopher-names (svc kept version)
	; (gopher-names svc kept version) -> :nil | names
	;the blocks of an object, by the name of what it is. A name is of the
	;one thing for ever, so what was read for one gopher is kept for the
	;next: a gopher is at a member many times in a walk
	(cond
		((. kept :find version))
		((and (defq text (storage-block (hex-decode version) svc))
				(defq lines (filter (const nempty?) (split text +storage_lf)))
				(> (length lines) 1))
			(defq names (map! (const hex-decode) (list lines) 2 -1))
			(if (>= (. kept :size) +gopher_kept) (. kept :empty))
			(. kept :insert version names)
			names)))

(defun gopher-take (svc member all kept msg)
	; (gopher-take svc member all kept msg) -> :nil | gopher
	;a gopher that has come, as it is from now on. :nil if nothing can be
	;made of it, and who asked is told
	(defq lines (split msg +storage_lf) reply (hex-decode (first lines)) from (second lines)
		asker (if (eql from "-") reply (hex-decode from))
		;the rest is the one form, it may have line ends of its own in it
		text (slice msg (+ (length (first lines)) (length from) 2) -1))
	;it is here, with all of itself: the member that sent it on can let it go
	(if (nql from "-") (mail-send reply svc))
	(catch (progn
			(defq form (first (read (string-stream text))))
			(bind '(name at state source trail skip) (slice form 0 6))
			(defq version (if (> (length form) 6) (elem-get form 6))
				copies (if (> (length form) 7) (elem-get form 7))
				;who holds all of it, they have it pinned: asked of this member
				;as it sets out, which is told as every member is, and then
				;it goes with the gopher
				pinned (if (> (length form) 8)
					(filter (const nempty?) (split (hex-decode (slice (str (elem-get form 8)) 1 -1)) " "))
					(ifn (storage-pinned-by name svc) (list))))
			;what the object is now, if it has just set out: it is of that all
			;the way. And how many keep its blocks, which is the path's to say
			(and (not version) (defq entry (storage-stat name svc))
				(setq version (hex-encode (elem-get entry +store_entry_hash))
					copies (elem-get entry +store_entry_copies)))
			(if (or (not copies) (= copies 0)) (setq copies all))
			(cond
				((and version (defq names (gopher-names svc kept version)))
					;where it has been: each member, and the block it came to it for
					(push trail (list member at))
					;the function as it was sent is kept, to send on with what it
					;has then. What is run is a copy of it: to be run a form has
					;its macros done and its names bound, in the form itself,
					;Chris: "keep the original source so you can send it on with
					;the state", and one that had been would not read back
					(list asker name at state source (exec (gopher-ticked (copy source))) names version copies trail skip
						;it was sent here for a block: if that is not here, what was
						;worked out is not so, and it asks
						(if (> (length trail) 1) 1 0) :nil pinned
						;what it is known by in a recording, the same at every member
						;it comes to: of who it answers, made only when recorded
						(if (storage-recording? recorder) (slice (hex-encode (sha256 asker)) 0 8) "")
						;how long its next step is first tried with
						+gopher_first))
				(:t (mail-send asker "none") :nil)))
		(progn (print "Gopher: " _) (mail-send asker "none") :t)))

(defun gopher-turn (svc member g)
	; (gopher-turn svc member g) -> :keep | :back | :drop
	;a step of a gopher: a block, if the one it wants is here, or a step
	;of sending it on. :drop when it is done with here: it answered, it
	;went, or it can not go on and said so
	(bind '(asker name at state source visit names version copies trail skip how pass pinned tag budget) g)
	(defq count (length names))
	(cond
		(pass
			(case (defq went (gopher-pass-turn pass skip))
				(:wait :keep)
				(:lost
					;none of them took it: the next thing is tried
					(elem-set g +gopher_how (inc how))
					(elem-set g +gopher_pass :nil)
					:keep)
				(:t :drop)))
		((not (and at (< -1 at count)))
			;it is done, it said so or went off the end
			(storage-record recorder member "gopher-done" (cat tag " " name))
			(mail-send asker (str (list state trail)))
			:drop)
		((defq data (storage-block-at (defq want (elem-get names at)) svc))
			;it has its budget from now, and is asked when it comes back too
			(setq *gopher_until* (+ (pii-time) budget) *gopher_late* :nil
				*gopher_check* (min *gopher_until* *bot_next*))
			(storage-record recorder member "gopher-step" (cat (str at) " " (storage-record-name want) " " tag " " name))
			;what it has is not changed in place by a function, so a step that
			;is tried again is tried from what it had
			(bind '(next_state next_at) (visit state data at count))
			;one that had its time came back by itself, with what it had
			;then, which is not an answer
			(cond
				((not (or *gopher_late* (> (pii-time) *gopher_until*)))
					(elem-set g +gopher_state next_state)
					(elem-set g +gopher_at next_at)
					(elem-set g +gopher_how 0)
					:keep)
				((< budget +gopher_allowed)
					;not done in its first try: it goes to the back of the queue,
					;the slow lane, to be tried again from where it was, with all
					;it may have. The others here are not held behind it
					(storage-record recorder member "gopher-back" (cat tag " " name))
					(elem-set g +gopher_budget +gopher_allowed)
					:back)
				(:t (storage-record recorder member "gopher-late" (cat tag " " name))
					(mail-send asker "none")
					:drop)))
		((< how 2)
			;the block is not kept here. The gopher goes to it: all there is
			;of it, where it is, what it has, its function and where it has
			;been, is sent to a member that has the block, to carry on there
			(defq peers (map (# (list (first %0) (hex-decode (second %0))))
					(filter (# (= (length %0) 2)) (map (# (split %0 " "))
						(split (ifn (storage-ask svc +storage_type_peers) "") +storage_lf))))
				holders (if (= how 0) (gopher-route names at member peers copies skip pinned)
					(gopher-order names at (map (const second) peers) skip)))
			(elem-set g +gopher_pass (defq pass (gopher-pass-begin holders asker name at state source trail skip :nil version copies pinned)))
			(storage-record recorder member "gopher-pass" (cat (str at) " " tag " " name))
			;and is sent to the first of them now
			(gopher-pass-turn pass skip)
			:keep)
		(:t (storage-record recorder member "gopher-none" (cat tag " " name))
			(mail-send asker "none") :drop)))

(defun main ()
	;a bot, as its service is: the gophers here run before the apps of the
	;node, and after the GUI
	(task-band :bot)
	(defq first_msg (split (mail-read (task-mbox)) +storage_lf) svc (hex-decode (first first_msg))
		select (list (task-mbox) (mail-mbox))
		gophers (list) kept (Fmap 31) member "?" copies 3 running :t asked :nil
		;the slow lane: a runner of its own, that this starts, and that a
		;gopher not done in its first try is sent to, so that the others
		;here are not held behind it. It gives each all it may have, and is
		;told it is the slow lane, and who started it, it starts none
		slow (= (length first_msg) 2)
		lane (unless slow (open-child "service/storage/gopher.lisp" +kn_call_pin))
		;where the lane is told if this has blocks to do, and what it was told
		lane_hold :nil lane_held :nil
		;who it tells what it does, if anyone
		recorder (storage-record-new))
	(if (and lane (/= 0 (get-long lane 0)))
		(mail-send lane (cat (first first_msg) +storage_lf (hex-encode (task-mbox))))
		(setq lane :nil))
	;the back of the queue: the lane waits while the quick have blocks to
	;do, it is told where to be told so
	(when slow
		(setq *gopher_hold* (mail-mbox))
		(mail-send (hex-decode (second first_msg)) (cat "hold " (hex-encode *gopher_hold*))))
	(defun take (msg)
		(cond
			((eql msg "stop") (setq running :nil)
				;a lane that is held is let go, to hear it
				(if lane_hold (mail-send lane_hold "0"))
				(if lane (mail-send lane msg)))
			((starts-with "record " msg) (storage-record-to recorder (slice msg 7 -1)) (if lane (mail-send lane msg)))
			((starts-with "hold " msg) (setq lane_hold (hex-decode (slice msg 5 -1))))
			(:t ;who this member is, asked the once, when there is first a need
				(unless asked
					(defq info (storage-info svc))
					(setq asked :t member (if info (. info :find :member) "?") copies (if info (. info :find :copies) 3)))
				(when (list? (defq g (gopher-take svc member copies kept msg)))
					(if slow (elem-set g +gopher_budget +gopher_allowed))
					;one that has just set out, or one come on from another member
					;or one that was late here, to the slow lane, the same gopher
					(storage-record recorder member
						(cond (slow "gopher-lane") ((> (length (elem-get g +gopher_trail)) 1) "gopher-on") ("gopher"))
						(cat (elem-get g +gopher_tag) " " (elem-get g +gopher_name)))
					(push gophers g)))))
	(defun to-lane (g)
		;a gopher to the slow lane, as one is sent on to another member, from
		;where it was, with who asked to answer and no member to tell it got
		;there. It is here still, so where it has been is as it was
		(bind '(asker name at state source visit names version copies trail skip how pass pinned &ignore) g)
		(mail-send lane (cat (hex-encode asker) +storage_lf "-" +storage_lf
			(str (list name at state source (slice trail 0 -2) skip version copies
				(cat "p" (hex-encode (join pinned " "))))))))
	(defun turn (g)
		;a step of a gopher. One whose function throws is done with, and says so
		(catch (gopher-turn svc member g)
			(progn (print "Gopher: " _)
				(storage-record recorder member "gopher-threw" (cat (elem-get g +gopher_tag) " " (elem-get g +gopher_name)))
				(mail-send (elem-get g +gopher_asker) "none") :drop)))
	(while running
		(cond
			((some (# (not (elem-get %0 +gopher_pass))) gophers)
				;there are blocks to do: what mail there is is read, and no more
				(while (mail-poll (list (task-mbox))) (take (mail-read (task-mbox)))))
			(:t ;nothing to do but wait: for a gopher, or to hear of one sent on
				(when (nempty? gophers)
					(mail-timeout (second select) 0 0)
					(mail-timeout (second select) +gopher_look 0))
				(defq boxes (cat select (map (# (elem-get (elem-get %0 +gopher_pass) +gopher_pass_mbox)) gophers))
					idx (mail-select boxes))
				;what one sent on to said is read by its own step
				(if (< idx 2) (defq msg (mail-read (elem-get boxes idx))))
				(if (= idx 0) (take msg))))
		;a step of each, and one that was late goes to the slow lane, or, if
		;there is none, is tried here again, with all it may have
		(setq gophers (filter (lambda (g)
			(case (turn g)
				(:keep :t)
				(:back (cond (lane (to-lane g) :nil) (:t))))) gophers))
		;the lane is told when this comes to have blocks to do, and when it
		;has none
		(when lane_hold
			(defq busy (some (# (not (elem-get %0 +gopher_pass))) gophers))
			(unless (eql (not busy) (not lane_held))
				(setq lane_held (if busy :t :nil))
				(mail-send lane_hold (if busy "1" "0"))))
		(storage-record-send recorder))
	(mail-timeout (second select) 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a gopher is sent on
;
; The block it wants is not kept where it is. Who keeps a block is worked
; out of its name and who the members are, (store-rank), so where to go is
; worked out, and nobody is asked: of the members that are to have it, the
; one that is to have the longest run of the blocks after it too, so that
; it is not sent on again till it must be. All there is of the gopher is
; mailed to that member, and it says it has it. Then, and not before, it
; is done with here.
;
; Who is to keep a block and who does are not the same while a store is
; being spread, or a member has just come or gone. A gopher that gets to a
; member that has not the block it came for asks, as it did before there
; was a sum to do: each member that may have it is asked what it has of
; the blocks ahead, (gopher-order).
;
; Sending it can fail: the member is not there, the mail does not get
; there, nothing is said back. If the first has not said it has it in a
; while, it is sent to the next as well, and so on, and the first of them
; to say so, whichever it is, is where it went. They all say so to the one
; mailbox.
;
; If one that was slow to say so had it all the same, there are two
; gophers. Both come back to who asked, who takes the first: for one that
; only reads that is all it is. One that writes must be such that it can
; be done twice.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./app.inc")

;how many blocks ahead a run is looked for, and the waits, in seconds: for
;a member to say what it has, and to say it has a gopher sent to it
(defq +gopher_ahead 32 +gopher_has_wait 1 +gopher_ack_wait 2)

;a gopher that is being sent on: where those sent it say so, who it may
;go to, how many of them have been sent it, when the next is, all there
;is of it as it is sent, and how long each is given
(enums +gopher_pass 0
	(enum mbox holders next due text wait))

(defun gopher-route (names at me peers copies skip)
	; (gopher-route names at me peers copies skip) -> (netid ...)
	;of the other members, ((member netid) ...), those that are to keep
	;the block wanted, the one that is to keep the longest run of the
	;blocks after it first. Nobody is asked: it is the sum every member
	;does, of the members that are there, me and them. One that is in
	;skip, in hex, did not answer this gopher before and is left out
	(defq among (cat (list me) (map (const first) peers))
		kept (map (# (store-rank %0 among copies)) (slice names at (min (length names) (+ at +gopher_ahead))))
		runs (list))
	(each (lambda ((who where))
		(unless (find (hex-encode where) skip)
			(if (> (defq run (ifn (some (# (unless (find who %0) (!))) kept) (length kept))) 0)
				(push runs (list run where)))))
		peers)
	(map (const second) (sort runs (# (- (first %1) (first %0))))))

(defun gopher-order (names at members skip)
	; (gopher-order names at members skip) -> (netid ...)
	;of the members, those that have the block wanted, the one with the
	;longest run of the blocks after it first. Each is asked the one
	;thing, which of the next so many it has, and is not waited for long.
	;One that does not say is put in skip, in hex, and one that is in skip
	;is not asked: skip goes with the gopher, so a member that is not
	;there is waited for the once, not at every block
	(defq ahead (apply (const cat) (cat (list "") (slice names at (min (length names) (+ at +gopher_ahead)))))
		runs (list))
	(each (lambda (where)
		(unless (find (defq id (hex-encode where)) skip)
			(cond
				((defq has (storage-ask where +storage_type_has ahead +gopher_has_wait))
					(if (> (defq run (ifn (find "0" has) (length has))) 0) (push runs (list run where))))
				((push skip id)))))
		members)
	(map (const second) (sort runs (# (- (first %1) (first %0))))))

(defun gopher-pass-begin (holders asker name at state source trail skip &optional wait version)
	; (gopher-pass-begin holders asker name at state source trail skip [wait version]) -> pass
	;a gopher is to be sent on to one of the holders, the first of them
	;if it can be. Nothing is sent yet: (gopher-pass-turn) does it all, a
	;step each time it is called, and never waits. version is the name of
	;what the object was when the gopher set out, so that it is of the one
	;thing all the way. wait, seconds, is for a test
	(list (mail-mbox) holders 0 0
		(cat (hex-encode asker) +storage_lf (str (list name at state source trail skip version)))
		(task-timeout (ifn wait +gopher_ack_wait))))

(defun gopher-pass-turn (pass skip)
	; (gopher-pass-turn pass skip) -> :wait | :lost | netid
	;a step of sending a gopher on. Which of the holders has it, when one
	;has said so; :lost when all of them were sent it and none did in the
	;time; :wait till then.
	;
	;It is sent to the first. If that has not said it has it in a while
	;it is sent to the next as well, and so on, and the first to say so,
	;whichever it is, is the one: Chris, "we don't have to do this
	;serialy ? When any replies come in, we can go with that, even if it
	;was a late reply for the original choice." Those that were sent it
	;and did not say are put in skip
	(bind '(mbox holders next due text wait) pass)
	(cond
		((mail-poll (list mbox))
			(defq went (mail-read mbox))
			(each (# (unless (eql %0 went) (push skip (hex-encode %0)))) (slice holders 0 next))
			went)
		((< (pii-time) due) :wait)
		((< next (length holders))
			(mail-send (elem-get holders next) (cat (setf-> (str-alloc +storage_rpc_size)
				(+storage_rpc_reply_id mbox) (+storage_rpc_type +storage_type_walk)) text))
			(elem-set pass +gopher_pass_next (inc next))
			(elem-set pass +gopher_pass_due (+ (pii-time) wait))
			:wait)
		(:t (each (# (push skip (hex-encode %0))) holders)
			:lost)))

(defun gopher-pass (holders asker name at state source trail skip &optional wait version)
	; (gopher-pass holders asker name at state source trail skip [wait version]) -> :nil | netid
	;a gopher is sent on to one of the holders, and it is waited for.
	;Which it was, :nil if none of them took it
	(defq pass (gopher-pass-begin holders asker name at state source trail skip wait version) went :wait)
	(while (eql (setq went (gopher-pass-turn pass skip)) :wait) (task-sleep 1000))
	(if (eql went :lost) :nil went))

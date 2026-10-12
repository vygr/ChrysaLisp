;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a gopher is sent on
;
; The block it wants is not kept where it is. Of the members that have it,
; it goes to the one with the longest run of the blocks after it too, so
; that it is not sent on again till it must be. A gopher task is started
; there, all there is of this one is mailed to it, and it says it has it.
; Then, and not before, this one is done.
;
; Any of that can fail: the member is not there, its node does not start
; the task, the mail does not get there, nothing is said back. Then one is
; started at the next member that has the block as well, and the next, and
; the first of them all to say it is there is sent the gopher, till one
; has it or there are none.
;
; If one took it and what it said back was lost, the next is tried all the
; same, and there are two gophers. Both come back to who asked, who takes
; the first: for one that only reads that is all it is. One that writes
; must be such that it can be done twice.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./app.inc")

;how many blocks ahead a member is asked of, and the waits, in seconds: for
;a member to say what it has, for a node to start a gopher, for that one
;to say it has what it was sent, and, by one started, to be sent anything
(defq +gopher_ahead 32 +gopher_has_wait 1 +gopher_start_wait 2 +gopher_ack_wait 2 +gopher_state_wait 10)

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

(defun gopher-pass (holders asker name at state source trail skip &optional wait)
	; (gopher-pass holders asker name at state source trail skip [wait]) -> :nil | netid
	;all there is of a gopher is sent on to one of the holders, the first
	;of them if it can be. Which it was.
	;
	;A gopher is started at the first. If it has not said it is there in
	;a while, one is started at the next as well, and so on, and the
	;first of them to say so, whichever it is, is the one: Chris, "we
	;don't have to do this serialy ? When any replies come in, we can go
	;with that, even if it was a late reply for the original choice."
	;They all say so to the one mailbox. Only the one is sent anything.
	;The others, started and sent nothing, go in their own time.
	;
	;The one sent it says it has it, to another mailbox. If it does not,
	;the next to say it is there is sent it. Those that never said, and
	;those that did not say they had it, are put in skip. wait, seconds,
	;is for a test, in place of the two waits
	(defq said (mail-mbox) ack (mail-mbox) started (list) at_holder 0 went :nil more :t
		start_wait (task-timeout (ifn wait +gopher_start_wait)) ack_wait (task-timeout (ifn wait +gopher_ack_wait)))
	(while (and more (not went))
		(when (< at_holder (length holders))
			(defq next (elem-get holders at_holder))
			(open-task "service/storage/gopher.lisp" (slice next +mailbox_id_size -1) +kn_call_pin 0 said)
			(push started next)
			(++ at_holder))
		(cond
			((defq reply (mail-read-timeout said start_wait))
				;which of them that is: one started on the node it is on
				(when (and (/= 0 (get-long (defq there (getf reply +kn_msg_reply_id)) 0))
						(defq next (some (# (if (eql (slice %0 +mailbox_id_size -1) (slice there +mailbox_id_size -1)) %0)) started)))
					(setq started (filter (# (not (eql %0 next))) started))
					(mail-send there (cat (hex-encode asker) +storage_lf (hex-encode next) +storage_lf (hex-encode ack) +storage_lf
						(str (list name at state source trail skip))))
					(if (mail-read-timeout ack ack_wait) (setq went next) (push skip (hex-encode next)))))
			;none said so in the time, and there are no more to start
			((>= at_holder (length holders)) (setq more :nil))))
	(each (# (push skip (hex-encode %0))) started)
	went)

(report-header "Lock Edges: contention, shared reads, key hierarchy, waiting claims, history cap")

;a claim that can not be granted waits this long, then gives :nil
(defq le_wait 30000)

(defun le-claim (key &optional mode)
	(lock-claim-rpc key mode le_wait))

(defun le-ask (key &optional mode)
	; send a claim and do not wait for it, gives the mailbox the grant comes to
	(defq mbox (mail-mbox))
	(mail-send (ensure-lock-service) (setf-> (cat (str-alloc +lock_claim_size) key)
		(+lock_rpc_reply_id mbox)
		(+lock_rpc_type +lock_type_claim)
		(+lock_rpc_mode (ifn mode +lock_mode_write))
		(+lock_rpc_timeout 2000000)))
	mbox)

(defun le-granted? (mbox)
	(if (mail-read-timeout mbox le_wait) :t :nil))

; --- a write lock is exclusive ---
(test-cases
	(le-claim "le/w") :t
	(le-claim "le/w") :nil
	(le-claim "le/w" +lock_mode_read) :nil
	(lock-release-rpc "le/w") :t
	(le-claim "le/w") :t
	(lock-release-rpc "le/w") :t)

; --- read locks are shared, and each must be released before a write ---
(test-cases
	(le-claim "le/r" +lock_mode_read) :t
	(le-claim "le/r" +lock_mode_read) :t
	(le-claim "le/r") :nil
	(lock-release-rpc "le/r") :t
	(le-claim "le/r") :nil
	(lock-release-rpc "le/r") :t
	(le-claim "le/r") :t
	(lock-release-rpc "le/r") :t)

; --- keys are paths, a lock covers what is above and below it ---
(test-cases
	(le-claim "le/p/a") :t
	(le-claim "le/p/a/b") :nil
	(le-claim "le/p") :nil
	;a sibling is free
	(le-claim "le/p/b") :t
	(lock-release-rpc "le/p/b") :t
	(lock-release-rpc "le/p/a") :t
	(le-claim "le/p") :t
	(lock-release-rpc "le/p") :t
	;empty parts of a path are ignored
	(le-claim "le//q") :t
	(le-claim "le/q") :nil
	(lock-release-rpc "le//q") :t
	;a key can hold spaces
	(le-claim "le/a b") :t
	(lock-release-rpc "le/a b") :t)

;the empty key is the root, so covers every key
(test-cases
	(le-claim "") :t
	(le-claim "le/under_root") :nil
	(lock-release-rpc "") :t
	(le-claim "le/under_root") :t
	(le-claim "") :nil
	(lock-release-rpc "le/under_root") :t)

; --- releasing a key that is not held is harmless ---
(test-cases
	(lock-release-rpc "le/never_held") :t
	(lock-release-rpc "le/never_held") :t
	(le-claim "le/never_held") :t
	(lock-release-rpc "le/never_held") :t)

; --- a claim that is waiting is granted when the lock is released ---
(le-claim "le/q1")
(defq le_mbox (le-ask "le/q1"))
(assert-eq "waiting claim, not yet" :nil (le-granted? le_mbox))
(lock-release-rpc "le/q1")
(assert-eq "waiting claim, granted on release" :t (le-granted? le_mbox))
(lock-release-rpc "le/q1")

;a waiting write is not overtaken by a later read
(le-claim "le/q2" +lock_mode_read)
(defq le_write (le-ask "le/q2") le_read (le-ask "le/q2" +lock_mode_read))
(assert-list-eq "queue, both wait" '(:nil :nil) (list (le-granted? le_write) (le-granted? le_read)))
(lock-release-rpc "le/q2")
(assert-list-eq "queue, write first" '(:t :nil) (list (le-granted? le_write) (le-granted? le_read)))
(lock-release-rpc "le/q2")
(assert-eq "queue, then the read" :t (le-granted? le_read))
(lock-release-rpc "le/q2")

; --- the with-lock macros ---
(assert-eq "with-lock gives the body result" 42 (with-lock ("le/m" :nil le_wait) 42))
(assert-eq "with-lock with no key does nothing" :nil (with-lock (:nil) 42))
(assert-eq "nested read locks on one key" 7
	(with-read-lock ("le/m" le_wait) (with-read-lock ("le/m" le_wait) 7)))
(assert-eq "nested write locks on other keys" 8
	(with-write-lock ("le/m" le_wait) (with-write-lock ("le/m2" le_wait) 8)))
;a second write lock on the same key can never be granted, so times out
(assert-eq "nested write locks on one key" :nil
	(with-write-lock ("le/m" le_wait) (with-write-lock ("le/m" le_wait) 9)))

;when the lock is not got the body is not run
(le-claim "le/m3")
(defq le_ran :nil)
(assert-eq "with-lock times out" :nil (with-lock ("le/m3" :nil le_wait) (setq le_ran :t) 42))
(assert-eq "with-lock body not run" :nil le_ran)
(lock-release-rpc "le/m3")

; --- the history holds only the last +lock_max_history entries ---
(times 100 (le-claim "le/h") (lock-release-rpc "le/h"))
(le-claim "le/h_last")
(lock-release-rpc "le/h_last")
(defq le_history (lock-history-rpc))
(assert-eq "history is capped" +lock_max_history (length le_history))
(assert-eq "history ends with the newest" "le/h_last (unlock write)" (last le_history))
(assert-eq "history is in order" "le/h_last (lock write)" (elem-get le_history -3))

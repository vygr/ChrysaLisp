(report-header "Lock Edges: contention, shared reads, key hierarchy, waiting claims, history cap")

;a claim that should be granted is given plenty of time, as the service
;can be on another node, it comes back as soon as it is granted. A claim
;that can not be granted waits only le_wait, then gives :nil.
(defq le_wait 30000 le_long 2000000)

(defun le-claim (key &optional mode)
	; claim a lock that should be free
	(lock-claim-rpc key mode le_long))

(defun le-blocked (key &optional mode)
	; try for a lock that should be held, :t if it could not be got
	(not (lock-claim-rpc key mode le_wait)))

(defun le-ask (key &optional mode)
	; send a claim and do not wait for it, gives the mailbox the grant comes to
	(defq mbox (mail-mbox))
	(mail-send (ensure-lock-service) (setf-> (cat (str-alloc +lock_claim_size) key)
		(+lock_rpc_reply_id mbox)
		(+lock_rpc_type +lock_type_claim)
		(+lock_rpc_mode (ifn mode +lock_mode_write))
		(+lock_rpc_timeout 2000000)))
	mbox)

(defun le-granted? (mbox &optional wait)
	(if (mail-read-timeout mbox (ifn wait le_long)) :t :nil))

(defun le-waiting? (mbox)
	; :t if a claim sent with le-ask has not been granted yet
	(not (le-granted? mbox le_wait)))

; --- a write lock is exclusive ---
;a claim that times out must not be left held, the later claims of the
;same key here would then fail
(test-cases
	(le-claim "le/w") :t
	(le-blocked "le/w") :t
	(le-blocked "le/w" +lock_mode_read) :t
	(lock-release-rpc "le/w") :t
	(le-claim "le/w") :t
	(lock-release-rpc "le/w") :t)

; --- read locks are shared, and each must be released before a write ---
(test-cases
	(le-claim "le/r" +lock_mode_read) :t
	(le-claim "le/r" +lock_mode_read) :t
	(le-blocked "le/r") :t
	(lock-release-rpc "le/r") :t
	(le-blocked "le/r") :t
	(lock-release-rpc "le/r") :t
	(le-claim "le/r") :t
	(lock-release-rpc "le/r") :t)

; --- keys are paths, a lock covers what is above and below it ---
(test-cases
	(le-claim "le/p/a") :t
	(le-blocked "le/p/a/b") :t
	(le-blocked "le/p") :t
	;a sibling is free
	(le-claim "le/p/b") :t
	(lock-release-rpc "le/p/b") :t
	(lock-release-rpc "le/p/a") :t
	(le-claim "le/p") :t
	(lock-release-rpc "le/p") :t
	;empty parts of a path are ignored
	(le-claim "le//q") :t
	(le-blocked "le/q") :t
	(lock-release-rpc "le//q") :t
	;a key can hold spaces
	(le-claim "le/a b") :t
	(lock-release-rpc "le/a b") :t)

;the empty key is the root, so covers every key
(test-cases
	(le-claim "") :t
	(le-blocked "le/under_root") :t
	(lock-release-rpc "") :t
	(le-claim "le/under_root") :t
	(le-blocked "") :t
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
(assert-eq "waiting claim, not yet" :t (le-waiting? le_mbox))
(lock-release-rpc "le/q1")
(assert-eq "waiting claim, granted on release" :t (le-granted? le_mbox))
(lock-release-rpc "le/q1")

;a waiting write is not overtaken by a later read
(le-claim "le/q2" +lock_mode_read)
(defq le_write (le-ask "le/q2") le_read (le-ask "le/q2" +lock_mode_read))
(assert-list-eq "queue, both wait" '(:t :t) (list (le-waiting? le_write) (le-waiting? le_read)))
(lock-release-rpc "le/q2")
(assert-list-eq "queue, write first" '(:t :t) (list (le-granted? le_write) (le-waiting? le_read)))
(lock-release-rpc "le/q2")
(assert-eq "queue, then the read" :t (le-granted? le_read))
(lock-release-rpc "le/q2")

; --- the with-lock macros ---
(assert-eq "with-lock gives the body result" 42 (with-lock ("le/m" :nil le_long) 42))
(assert-eq "with-lock with no key does nothing" :nil (with-lock (:nil) 42))
(assert-eq "nested read locks on one key" 7
	(with-read-lock ("le/m" le_long) (with-read-lock ("le/m" le_long) 7)))
(assert-eq "nested write locks on other keys" 8
	(with-write-lock ("le/m" le_long) (with-write-lock ("le/m2" le_long) 8)))
;a second write lock on the same key can never be granted, so times out
(assert-eq "nested write locks on one key" :nil
	(with-write-lock ("le/m" le_long) (with-write-lock ("le/m" le_wait) 9)))

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

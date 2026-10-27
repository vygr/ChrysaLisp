(report-header "Lock Service: History & Non-Spawning Ensure")
(import "service/lock/app.inc")

; --- Ensure lock service ---
(defq svc (ensure-lock-service))
(assert-true "ensure-lock-service returns netid" (and (str? svc) (= (length svc) 24)))

; --- Claim and release write lock ---
(assert-true "lock-claim-rpc write" (lock-claim-rpc "test/file/write_key" +lock_mode_write))
(assert-true "lock-release-rpc write" (lock-release-rpc "test/file/write_key"))

; --- Claim and release read lock ---
(assert-true "lock-claim-rpc read" (lock-claim-rpc "test/file/read_key" +lock_mode_read))
(assert-true "lock-release-rpc read" (lock-release-rpc "test/file/read_key"))

; --- Lock history retrieval ---
(defq history (lock-history-rpc))
(assert-true "lock-history-rpc returns list" (seq? history))
(assert-true "lock-history contains write lock"
	(nempty? (some (# (if (eql %0 "test/file/write_key (lock write)") %0)) history)))
(assert-true "lock-history contains write unlock"
	(nempty? (some (# (if (eql %0 "test/file/write_key (unlock write)") %0)) history)))
(assert-true "lock-history contains read lock"
	(nempty? (some (# (if (eql %0 "test/file/read_key (lock read)") %0)) history)))
(assert-true "lock-history contains read unlock"
	(nempty? (some (# (if (eql %0 "test/file/read_key (unlock read)") %0)) history)))

; --- Test save and cat command locking ---
(import "lib/task/pipe.inc")
(pipe-run "echo lock_test_data | save obj/tmp_lock_test.txt" (lambda (&) :nil))
(pipe-run "cat obj/tmp_lock_test.txt" (lambda (&) :nil))
(pipe-run "rm obj/tmp_lock_test.txt" (lambda (&) :nil))

(defq hist2 (lock-history-rpc))
(assert-true "lock-history contains save write lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_lock_test.txt (lock write)") %0)) hist2)))
(assert-true "lock-history contains save write unlock"
	(nempty? (some (# (if (eql %0 "obj/tmp_lock_test.txt (unlock write)") %0)) hist2)))
(assert-true "lock-history contains cat read lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_lock_test.txt (lock read)") %0)) hist2)))
(assert-true "lock-history contains cat read unlock"
	(nempty? (some (# (if (eql %0 "obj/tmp_lock_test.txt (unlock read)") %0)) hist2)))

; --- Test cp, mv, dump, rle, lz4, rm locking ---
(pipe-run "echo cp_data | save obj/tmp_cp_src.txt" (lambda (&) :nil))
(pipe-run "cp obj/tmp_cp_src.txt obj/tmp_cp_dst.txt" (lambda (&) :nil))
(pipe-run "mv obj/tmp_cp_dst.txt obj/tmp_mv_dst.txt" (lambda (&) :nil))
(pipe-run "dump obj/tmp_mv_dst.txt" (lambda (&) :nil))
(pipe-run "rle obj/tmp_mv_dst.txt" (lambda (&) :nil))
(pipe-run "lz4 obj/tmp_mv_dst.txt" (lambda (&) :nil))
(pipe-run "rm obj/tmp_cp_src.txt" (lambda (&) :nil))
(pipe-run "rm obj/tmp_mv_dst.txt" (lambda (&) :nil))

(defq hist3 (lock-history-rpc))
(assert-true "lock-history contains cp src read lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_cp_src.txt (lock read)") %0)) hist3)))
(assert-true "lock-history contains cp dst write lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_cp_dst.txt (lock write)") %0)) hist3)))
(assert-true "lock-history contains mv src write lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_cp_dst.txt (lock write)") %0)) hist3)))
(assert-true "lock-history contains mv dst write lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_mv_dst.txt (lock write)") %0)) hist3)))
(assert-true "lock-history contains dump read lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_mv_dst.txt (lock read)") %0)) hist3)))
(assert-true "lock-history contains rm write lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_mv_dst.txt (lock write)") %0)) hist3)))

; --- Test edit command locking ---
(pipe-run "echo original | save obj/tmp_edit.txt" (lambda (&) :nil))
(pipe-run "edit -q -c (edit-delete) obj/tmp_edit.txt" (lambda (&) :nil))
(pipe-run "rm obj/tmp_edit.txt" (lambda (&) :nil))

(defq hist4 (lock-history-rpc))
(assert-true "lock-history contains edit read lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_edit.txt (lock read)") %0)) hist4)))
(assert-true "lock-history contains edit write lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_edit.txt (lock write)") %0)) hist4)))

; --- Test diff, patch, sed, huff locking ---
(pipe-run "echo line1 | save obj/tmp_da.txt" (lambda (&) :nil))
(pipe-run "echo line2 | save obj/tmp_db.txt" (lambda (&) :nil))
(pipe-run "diff obj/tmp_da.txt obj/tmp_db.txt" (lambda (&) :nil))
(pipe-run "patch obj/tmp_da.txt obj/tmp_db.txt" (lambda (&) :nil))
(pipe-run "sed -e line1 -r changed obj/tmp_da.txt" (lambda (&) :nil))
(pipe-run "huff obj/tmp_da.txt" (lambda (&) :nil))
(pipe-run "rm obj/tmp_da.txt" (lambda (&) :nil))
(pipe-run "rm obj/tmp_db.txt" (lambda (&) :nil))

(defq hist5 (lock-history-rpc))
(assert-true "lock-history contains diff read lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_da.txt (lock read)") %0)) hist5)))
(assert-true "lock-history contains patch read lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_db.txt (lock read)") %0)) hist5)))
(assert-true "lock-history contains sed read lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_da.txt (lock read)") %0)) hist5)))
(assert-true "lock-history contains huff read lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_da.txt (lock read)") %0)) hist5)))
(assert-true "lock-history contains rm da write lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_da.txt (lock write)") %0)) hist5)))
(assert-true "obj/tmp_da.txt removed" (not (file-stream "obj/tmp_da.txt")))
(assert-true "obj/tmp_db.txt removed" (not (file-stream "obj/tmp_db.txt")))

; --- Test trace command locking ---
(pipe-run "trace sys/task/dump" (lambda (&) :nil))
(defq hist6 (lock-history-rpc))
(assert-true "lock-history contains trace read lock"
	(nempty? (some (# (if (eql %0 "obj/vp/ (lock read)") %0)) hist6)))

; --- Test head and tail command locking ---
(pipe-run "echo test_line | save obj/tmp_head_tail.txt" (lambda (&) :nil))
(pipe-run "head obj/tmp_head_tail.txt" (lambda (&) :nil))
(pipe-run "tail obj/tmp_head_tail.txt" (lambda (&) :nil))
(pipe-run "rm obj/tmp_head_tail.txt" (lambda (&) :nil))

(defq hist9 (lock-history-rpc))
(assert-true "lock-history contains head read lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_head_tail.txt (lock read)") %0)) hist9)))
(assert-true "lock-history contains head read unlock"
	(nempty? (some (# (if (eql %0 "obj/tmp_head_tail.txt (unlock read)") %0)) hist9)))
(assert-true "lock-history contains tail read lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_head_tail.txt (lock read)") %0)) hist9)))
(assert-true "lock-history contains tail read unlock"
	(nempty? (some (# (if (eql %0 "obj/tmp_head_tail.txt (unlock read)") %0)) hist9)))

; --- Test ctf command locking ---
(pipe-run "ctf fonts/Chess.ctf" (lambda (&) :nil))
(defq hist10 (lock-history-rpc))
(assert-true "lock-history contains ctf read lock"
	(nempty? (some (# (if (eql %0 "fonts/Chess.ctf (lock read)") %0)) hist10)))
(assert-true "lock-history contains ctf read unlock"
	(nempty? (some (# (if (eql %0 "fonts/Chess.ctf (unlock read)") %0)) hist10)))

(pipe-run "cp fonts/Chess.ctf obj/tmp_font.ctf" (lambda (&) :nil))
(pipe-run "ctf -c obj/tmp_font.ctf" (lambda (&) :nil))
(defq hist11 (lock-history-rpc))
(assert-true "lock-history contains ctf convert write lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_font.ctf (lock write)") %0)) hist11)))
(assert-true "lock-history contains ctf convert write unlock"
	(nempty? (some (# (if (eql %0 "obj/tmp_font.ctf (unlock write)") %0)) hist11)))
(pipe-run "rm obj/tmp_font.ctf" (lambda (&) :nil))

; --- Test with-lock macros ---
(defq res_write (with-write-lock "obj/tmp_macro_write.txt"
	(save "macro_data" "obj/tmp_macro_write.txt")
	42))
(assert-eq "with-write-lock returns body result" 42 res_write)

(defq res_read (with-read-lock "obj/tmp_macro_write.txt"
	(load "obj/tmp_macro_write.txt")))
(assert-eq "with-read-lock returns body result" "macro_data" res_read)

(pipe-run "rm obj/tmp_macro_write.txt" (lambda (&) :nil))

(defq hist12 (lock-history-rpc))
(assert-true "lock-history contains macro write lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_macro_write.txt (lock write)") %0)) hist12)))
(assert-true "lock-history contains macro write unlock"
	(nempty? (some (# (if (eql %0 "obj/tmp_macro_write.txt (unlock write)") %0)) hist12)))
(assert-true "lock-history contains macro read lock"
	(nempty? (some (# (if (eql %0 "obj/tmp_macro_write.txt (lock read)") %0)) hist12)))
(assert-true "lock-history contains macro read unlock"
	(nempty? (some (# (if (eql %0 "obj/tmp_macro_write.txt (unlock read)") %0)) hist12)))

(report-header "Lock Edges: contention, shared reads, key hierarchy, waiting claims, history cap")

;a claim that should be granted is waited for as long as it takes, it comes
;back as soon as it is granted: le_long is longer than the suite waits for a
;module, so one that never comes is the suite's to report, not a check that
;fails. A claim that can not be granted waits only le_wait, then gives :nil.
(defq le_wait 30000 le_long (task-timeout 600))

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
		(+lock_rpc_timeout le_long)))
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

;the empty key, the root, is tested on its own, tests/solo/test_lock_root.lisp

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

;a cancel can come before its claim, mail has no order over more than one
;route. The claim is dropped as it comes: granted when the lock is let go,
;it would be held by nobody, and the next claim would wait out its lease
(le-claim "le/race")
(defq le_late (mail-mbox))
(mail-send (ensure-lock-service) (setf-> (cat (str-alloc +lock_cancel_size) "le/race")
	(+lock_rpc_reply_id le_late)
	(+lock_rpc_type +lock_type_cancel)
	(+lock_rpc_mode +lock_mode_write)))
(mail-send (ensure-lock-service) (setf-> (cat (str-alloc +lock_claim_size) "le/race")
	(+lock_rpc_reply_id le_late)
	(+lock_rpc_type +lock_type_claim)
	(+lock_rpc_mode +lock_mode_write)
	(+lock_rpc_timeout le_long)))
(lock-release-rpc "le/race")
(assert-eq "a claim that comes after its cancel is not granted" :t (le-claim "le/race"))
(assert-eq "nor told it was" :nil (mail-read-timeout le_late 100000))
(lock-release-rpc "le/race")

;a waiting write is not overtaken by a later read. Two messages sent one
;after the other can arrive the other way round, where there is more than
;one route between the nodes, so see the write is waiting before the read
;is sent.
(le-claim "le/q2" +lock_mode_read)
(defq le_write (le-ask "le/q2") le_write_waits (le-waiting? le_write)
	le_read (le-ask "le/q2" +lock_mode_read))
(assert-list-eq "queue, both wait" '(:t :t) (list le_write_waits (le-waiting? le_read)))
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
;the service is the one for the whole network, and another task may
;claim a lock while this looks, a shader being assembled does. So the
;last claim made here is looked for near the end, not just at it
(defq le_unlock (rfind "le/h_last (unlock write)" le_history)
	le_lock (rfind "le/h_last (lock write)" le_history))
(assert-true "history has the newest, near its end"
	(and le_unlock (> le_unlock (- (length le_history) 64))))
(assert-true "history is in order" (and le_lock le_unlock (< le_lock le_unlock)))

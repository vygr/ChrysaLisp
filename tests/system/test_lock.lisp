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
(pipe-run "echo lock_test_data | save tmp_lock_test.txt" (lambda (_) :nil))
(pipe-run "cat tmp_lock_test.txt" (lambda (_) :nil))
(pipe-run "rm tmp_lock_test.txt" (lambda (_) :nil))

(defq hist2 (lock-history-rpc))
(assert-true "lock-history contains save write lock"
	(nempty? (some (# (if (eql %0 "tmp_lock_test.txt (lock write)") %0)) hist2)))
(assert-true "lock-history contains save write unlock"
	(nempty? (some (# (if (eql %0 "tmp_lock_test.txt (unlock write)") %0)) hist2)))
(assert-true "lock-history contains cat read lock"
	(nempty? (some (# (if (eql %0 "tmp_lock_test.txt (lock read)") %0)) hist2)))
(assert-true "lock-history contains cat read unlock"
	(nempty? (some (# (if (eql %0 "tmp_lock_test.txt (unlock read)") %0)) hist2)))

; --- Test cp, mv, dump, rle, lz4, rm locking ---
(pipe-run "echo cp_data | save tmp_cp_src.txt" (lambda (_) :nil))
(pipe-run "cp tmp_cp_src.txt tmp_cp_dst.txt" (lambda (_) :nil))
(pipe-run "mv tmp_cp_dst.txt tmp_mv_dst.txt" (lambda (_) :nil))
(pipe-run "dump tmp_mv_dst.txt" (lambda (_) :nil))
(pipe-run "rle tmp_mv_dst.txt" (lambda (_) :nil))
(pipe-run "lz4 tmp_mv_dst.txt" (lambda (_) :nil))
(pipe-run "rm tmp_cp_src.txt" (lambda (_) :nil))
(pipe-run "rm tmp_mv_dst.txt" (lambda (_) :nil))

(defq hist3 (lock-history-rpc))
(assert-true "lock-history contains cp src read lock"
	(nempty? (some (# (if (eql %0 "tmp_cp_src.txt (lock read)") %0)) hist3)))
(assert-true "lock-history contains cp dst write lock"
	(nempty? (some (# (if (eql %0 "tmp_cp_dst.txt (lock write)") %0)) hist3)))
(assert-true "lock-history contains mv src write lock"
	(nempty? (some (# (if (eql %0 "tmp_cp_dst.txt (lock write)") %0)) hist3)))
(assert-true "lock-history contains mv dst write lock"
	(nempty? (some (# (if (eql %0 "tmp_mv_dst.txt (lock write)") %0)) hist3)))
(assert-true "lock-history contains dump read lock"
	(nempty? (some (# (if (eql %0 "tmp_mv_dst.txt (lock read)") %0)) hist3)))
(assert-true "lock-history contains rm write lock"
	(nempty? (some (# (if (eql %0 "tmp_mv_dst.txt (lock write)") %0)) hist3)))

; --- Test edit command locking ---
(pipe-run "echo original | save tmp_edit.txt" (lambda (_) :nil))
(pipe-run "edit -q -c (edit-delete) tmp_edit.txt" (lambda (_) :nil))
(pipe-run "rm tmp_edit.txt" (lambda (_) :nil))

(defq hist4 (lock-history-rpc))
(assert-true "lock-history contains edit read lock"
	(nempty? (some (# (if (eql %0 "tmp_edit.txt (lock read)") %0)) hist4)))
(assert-true "lock-history contains edit write lock"
	(nempty? (some (# (if (eql %0 "tmp_edit.txt (lock write)") %0)) hist4)))

; --- Test diff, patch, sed, huff locking ---
(pipe-run "echo line1 | save tmp_da.txt" (lambda (_) :nil))
(pipe-run "echo line2 | save tmp_db.txt" (lambda (_) :nil))
(pipe-run "diff tmp_da.txt tmp_db.txt" (lambda (_) :nil))
(pipe-run "patch tmp_da.txt tmp_db.txt" (lambda (_) :nil))
(pipe-run "sed -e line1 -r changed tmp_da.txt" (lambda (_) :nil))
(pipe-run "huff tmp_da.txt" (lambda (_) :nil))
(pipe-run "rm tmp_da.txt" (lambda (_) :nil))
(pipe-run "rm tmp_db.txt" (lambda (_) :nil))

(defq hist5 (lock-history-rpc))
(assert-true "lock-history contains diff read lock"
	(nempty? (some (# (if (eql %0 "tmp_da.txt (lock read)") %0)) hist5)))
(assert-true "lock-history contains patch read lock"
	(nempty? (some (# (if (eql %0 "tmp_db.txt (lock read)") %0)) hist5)))
(assert-true "lock-history contains sed read lock"
	(nempty? (some (# (if (eql %0 "tmp_da.txt (lock read)") %0)) hist5)))
(assert-true "lock-history contains huff read lock"
	(nempty? (some (# (if (eql %0 "tmp_da.txt (lock read)") %0)) hist5)))
(assert-true "lock-history contains rm da write lock"
	(nempty? (some (# (if (eql %0 "tmp_da.txt (lock write)") %0)) hist5)))
(assert-true "tmp_da.txt removed" (not (file-stream "tmp_da.txt")))
(assert-true "tmp_db.txt removed" (not (file-stream "tmp_db.txt")))

; --- Test trace command locking ---
(pipe-run "trace sys/task/dump" (lambda (_) :nil))
(defq hist6 (lock-history-rpc))
(assert-true "lock-history contains trace read lock"
	(nempty? (some (# (if (eql %0 "obj/vp/ (lock read)") %0)) hist6)))

; --- Test lib/files/ files-scan locking ---
(import "lib/files/files.inc")
(pipe-run "echo (import \qtest.inc\q) | save tmp_files_test.lisp" (lambda (_) :nil))
(files-scan "tmp_files_test.lisp" (lambda (&rest _) :nil))
(defq hist8 (lock-history-rpc))
(assert-true "lock-history contains files-scan read lock"
	(nempty? (some (# (if (eql %0 "tmp_files_test.lisp (lock read)") %0)) hist8)))
(assert-true "lock-history contains files-scan read unlock"
	(nempty? (some (# (if (eql %0 "tmp_files_test.lisp (unlock read)") %0)) hist8)))
(pipe-run "rm tmp_files_test.lisp" (lambda (_) :nil))

; --- Test head and tail command locking ---
(pipe-run "echo test_line | save tmp_head_tail.txt" (lambda (_) :nil))
(pipe-run "head tmp_head_tail.txt" (lambda (_) :nil))
(pipe-run "tail tmp_head_tail.txt" (lambda (_) :nil))
(pipe-run "rm tmp_head_tail.txt" (lambda (_) :nil))

(defq hist9 (lock-history-rpc))
(assert-true "lock-history contains head read lock"
	(nempty? (some (# (if (eql %0 "tmp_head_tail.txt (lock read)") %0)) hist9)))
(assert-true "lock-history contains head read unlock"
	(nempty? (some (# (if (eql %0 "tmp_head_tail.txt (unlock read)") %0)) hist9)))
(assert-true "lock-history contains tail read lock"
	(nempty? (some (# (if (eql %0 "tmp_head_tail.txt (lock read)") %0)) hist9)))
(assert-true "lock-history contains tail read unlock"
	(nempty? (some (# (if (eql %0 "tmp_head_tail.txt (unlock read)") %0)) hist9)))

; --- Test ctf command locking ---
(pipe-run "ctf fonts/Chess.ctf" (lambda (_) :nil))
(defq hist10 (lock-history-rpc))
(assert-true "lock-history contains ctf read lock"
	(nempty? (some (# (if (eql %0 "fonts/Chess.ctf (lock read)") %0)) hist10)))
(assert-true "lock-history contains ctf read unlock"
	(nempty? (some (# (if (eql %0 "fonts/Chess.ctf (unlock read)") %0)) hist10)))

(pipe-run "cp fonts/Chess.ctf tmp_font.ctf" (lambda (_) :nil))
(pipe-run "ctf -c tmp_font.ctf" (lambda (_) :nil))
(defq hist11 (lock-history-rpc))
(assert-true "lock-history contains ctf convert write lock"
	(nempty? (some (# (if (eql %0 "tmp_font.ctf (lock write)") %0)) hist11)))
(assert-true "lock-history contains ctf convert write unlock"
	(nempty? (some (# (if (eql %0 "tmp_font.ctf (unlock write)") %0)) hist11)))
(pipe-run "rm tmp_font.ctf" (lambda (_) :nil))

; --- Test with-lock macros ---
(defq res_write (with-write-lock "tmp_macro_write.txt"
	(save "macro_data" "tmp_macro_write.txt")
	42))
(assert-eq "with-write-lock returns body result" 42 res_write)

(defq res_read (with-read-lock "tmp_macro_write.txt"
	(load "tmp_macro_write.txt")))
(assert-eq "with-read-lock returns body result" "macro_data" res_read)

(pipe-run "rm tmp_macro_write.txt" (lambda (_) :nil))

(defq hist12 (lock-history-rpc))
(assert-true "lock-history contains macro write lock"
	(nempty? (some (# (if (eql %0 "tmp_macro_write.txt (lock write)") %0)) hist12)))
(assert-true "lock-history contains macro write unlock"
	(nempty? (some (# (if (eql %0 "tmp_macro_write.txt (unlock write)") %0)) hist12)))
(assert-true "lock-history contains macro read lock"
	(nempty? (some (# (if (eql %0 "tmp_macro_write.txt (lock read)") %0)) hist12)))
(assert-true "lock-history contains macro read unlock"
	(nempty? (some (# (if (eql %0 "tmp_macro_write.txt (unlock read)") %0)) hist12)))

(report-header "Lock Edges: contention, shared reads, key hierarchy, waiting claims, history cap")

;a claim that should be granted is given plenty of time, as the service
;can be on another node, it comes back as soon as it is granted. How long
;that is depends on the machine, the emulator is slow, (task-timeout). A claim
;that can not be granted waits only le_wait, then gives :nil.
(defq le_wait 30000 le_long (task-timeout 2))

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
(assert-eq "history ends with the newest" "le/h_last (unlock write)" (last le_history))
(assert-eq "history is in order" "le/h_last (lock write)" (elem-get le_history -3))

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

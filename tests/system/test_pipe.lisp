(report-header "Pipe: Stdin Mailbox, Close & Abort")
(import "lib/task/pipe.inc")

(defun pipe-test-child (reply_mbox body)
	; (pipe-test-child reply_mbox body) -> pipe
	;child runs body, then reports to the reply mailbox when it gets past it
	(Pipe (cat "lisp -r " body
		" (mail-send (hex-decode {" (hex-encode reply_mbox) "}) {done})")))

; --- Command stdin has its own mailbox, leaving the task mailbox free ---
(defq pipe_out (list))
(pipe-run "lisp -r (print (if (eql (in-mbox (io-stream 'stdin)) (task-mbox)) {shared} {free}))"
	(# (push pipe_out %0)))
(assert-true "stdin not task mailbox" (find "free" (apply (const cat) pipe_out)))

; --- Close, the stdin EOF path, of a command blocked on stdin ---
(defq reply_mbox (mail-mbox)
	pipe (pipe-test-child reply_mbox "(read-line (io-stream 'stdin))"))
(assert-eq "pipe close blocked" :nil (mail-read-timeout reply_mbox 100000))
(. pipe :close)
(assert-eq "pipe close stdin" "done" (mail-read-timeout reply_mbox))

; --- Eof, the end of stdin with the pipe left open, what the command
; --- says after it is still read, and a write after it is dropped ---
(defq pipe (Pipe "lisp -r (read-line (io-stream 'stdin)) (print {after eof})")
	pipe_out (list))
(. pipe :eof)
(. pipe :write "dropped")
(while (defq data (. pipe :read))
	(unless (eql data :t) (push pipe_out data)))
(. pipe :close)
(assert-true "pipe eof, then its output" (find "after eof" (apply (const cat) pipe_out)))

; --- Abort, the signal path, signals are a debug build only feature ---
(defq reply_mbox (mail-mbox)
	pipe (pipe-test-child reply_mbox "(mail-read (task-mbox))")
	child_mbox (first (get :ids pipe)))
(assert-eq "pipe abort blocked" :nil (mail-read-timeout reply_mbox 100000))
(. pipe :abort)
(cond
	((defq reply (mail-read-timeout reply_mbox))
		(assert-eq "pipe abort mailbox" "done" reply)
		(defq reply_mbox (mail-mbox)
			pipe (pipe-test-child reply_mbox "(read-line (io-stream 'stdin))"))
		(assert-eq "pipe abort stdin blocked" :nil (mail-read-timeout reply_mbox 100000))
		(. pipe :abort)
		(assert-eq "pipe abort stdin" "done" (mail-read-timeout reply_mbox))
		;reader errors, also debug build only, an unbalanced form must be
		;reported and must not crash the node
		(each (lambda ((name cmdline err))
				(defq pipe_out (list))
				(pipe-run cmdline (# (push pipe_out %0)))
				(assert-true name (find err (apply (const cat) pipe_out))))
			'(("read missing )" "lisp -r (print (+ 1 2)" "missing )")
			("read unexpected )" "lisp -r (print 1))" "unexpected )"))))
	(:t ;release build, so wake the child to let it exit
		(test-skip "pipe abort and reader errors" "needs an error checked build")
		(mail-send child_mbox "")
		(assert-eq "pipe abort child exit" "done" (mail-read-timeout reply_mbox))))

; --- A farm of commands, and one of them that never answers in time ---
(import "lib/task/cmd.inc")
(defq farmed (pipe-farm (list "echo one" "echo two" "echo three")))
(assert-list-eq "pipe farm, every command answered" '("one" "three" "two")
	(sort (map (# (first (split (second %0) (ascii-char 10)))) farmed)))
(assert-eq "pipe farm, nothing to do" 0 (length (pipe-farm (list))))
;the slow one is given out three times, then the farm stops, with the
;result of the quick one and none for the slow
(defq farmed (pipe-farm (list "echo quick"
	(cat "lisp -r (task-sleep " (str (* 5 (task-timeout 1))) ")")) 400000))
(assert-list-eq "pipe farm, a command that is too slow is given up on" '("echo quick")
	(map (const first) farmed))

;a pipe that is told its input has ended, read till it stops, and closed,
;closes at once. A stderr can stop, and be read as stopped, while the last
;of the stdout is still coming, and the close then waited for it to stop
;again, which it never would, till the abort timer of 2 seconds went
(each (lambda (cmdline)
	(defq pc (Pipe cmdline) pc_out (list))
	(. pc :eof)
	(while (defq pc_data (. pc :read)) (if (str? pc_data) (push pc_out pc_data)))
	(defq pc_t0 (pii-time))
	(. pc :close)
	(assert-true (cat "closes at once after eof, " cmdline)
		(< (- (pii-time) pc_t0) (/ (task-timeout 1) 2)))
	(assert-true (cat "and all it said was read, " cmdline) (nempty? pc_out)))
	(list "echo one two" "files cmd/ .lisp" "make all boot" "files cmd/ | head -n 3"))

(undef (env) 'pipe 'pipe_out 'reply_mbox 'child_mbox 'reply 'farmed)

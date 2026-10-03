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
		(assert-eq "pipe abort stdin" "done" (mail-read-timeout reply_mbox)))
	(:t ;release build, so wake the child to let it exit
		(print "[SKIP] pipe abort, signals need a debug build")
		(mail-send child_mbox "")
		(assert-eq "pipe abort child exit" "done" (mail-read-timeout reply_mbox))))

(undef (env) 'pipe 'pipe_out 'reply_mbox 'child_mbox 'reply)

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
(assert-true "stdin not task mailbox" (found? (apply (const cat) pipe_out) "free"))

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
(assert-true "pipe eof, then its output" (found? (apply (const cat) pipe_out) "after eof"))

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
				(assert-true name (found? (apply (const cat) pipe_out) err)))
			'(("read missing )" "lisp -r (print (+ 1 2)" "missing )")
			("read unexpected )" "lisp -r (print 1))" "unexpected )"))))
	(:t ;release build, so wake the child to let it exit
		(test-skip "pipe abort and reader errors" "needs an error checked build")
		(mail-send child_mbox "")
		(assert-eq "pipe abort child exit" "done" (mail-read-timeout reply_mbox))))

; --- Abort of a command that never reads or writes. It is signalled, and
; --- (task-sleep) and (task-slice) are where it finds that it was, an
; --- error there
(defun pipe-told (reply_mbox body)
	; (pipe-told reply_mbox body) -> pipe
	;a command that says it has started, runs body, and says so if it is
	;stopped by an error. It is waited for to start, how long that takes
	;is the machine's affair
	(defq pipe (Pipe (cat "lisp -r (progn (mail-send (hex-decode {" (hex-encode reply_mbox)
		"}) {started}) (catch " body " (progn (mail-send (hex-decode {" (hex-encode reply_mbox)
		"}) (if (found? (str _) {task_aborted}) {aborted} {other})) :nil)))")))
	(assert-eq "the command starts" "started" (mail-read-timeout reply_mbox (task-timeout 10)))
	pipe)
;a task is stopped by an error, so this is of a build that checks for them
(cond
	((not *test_checked*) (test-skip "abort of a command that never reads or writes" "needs an error checked build"))
	(:t
	(defq reply_mbox (mail-mbox)
		pipe (pipe-told reply_mbox (cat "(task-sleep " (str (* 30 (task-timeout 1))) ")")))
	(assert-eq "a command that only sleeps says no more" :nil (mail-read-timeout reply_mbox 200000))
	(defq t0 (pii-time))
	(. pipe :abort)
	(assert-eq "aborted, it is woken and stopped, it does not sleep on" "aborted"
		(mail-read-timeout reply_mbox (task-timeout 5)))
	(assert-true "and long before its sleep was over" (< (- (pii-time) t0) (* 10 (task-timeout 1))))
	(defq reply_mbox (mail-mbox) pipe (pipe-told reply_mbox "(while :t (task-slice))"))
	(assert-eq "a command that only works, and lets others run, says no more" :nil (mail-read-timeout reply_mbox 200000))
	(. pipe :abort)
	(assert-eq "aborted, it is stopped" "aborted" (mail-read-timeout reply_mbox (task-timeout 5)))
	(defq reply_mbox (mail-mbox) pipe (pipe-told reply_mbox
		(cat "(progn (catch (task-sleep " (str (* 30 (task-timeout 1))) ") :t) (while :t (task-sleep 1000)))")))
	(task-sleep 200000)
	(. pipe :abort)
	(assert-eq "one that catches the error and sleeps again is stopped again" "aborted"
		(mail-read-timeout reply_mbox (task-timeout 5)))))
;a task that is not aborted sleeps and wakes as it did
(defq t0 (pii-time))
(task-sleep 50000)
(assert-true "a sleep is still a sleep" (>= (- (pii-time) t0) 50000))

; --- A farm of commands, and one of them that never answers in time ---
(import "lib/task/cmd.inc")
(defq farmed (pipe-farm (list "echo one" "echo two" "echo three")))
(assert-list-eq "pipe farm, every command answered" '("one" "three" "two")
	(sort (map (# (first (split (second %0) (ascii-char 10)))) farmed)))
(assert-eq "pipe farm, nothing to do" 0 (length (pipe-farm (list))))
;the slow one is given out three times, then the farm stops, with the
;result of the quick one and none for the slow
(defq farmed (pipe-farm (list "echo quick"
	(cat "lisp -r (task-sleep " (str (* 10 (task-timeout 1))) ")")) 1000000))
(assert-list-eq "pipe farm, a command that is too slow is given up on" '("echo quick")
	(map (const first) farmed))

;a pipe that is told its input has ended, read till it stops, and closed,
;closes at once. A stderr can stop, and be read as stopped, while the last
;of the stdout is still coming, and the close then waited for it to stop
;again, which it never would, till the abort timer of 2 seconds went.
;
;A close now waits for every stream to stop, with no timer, so that it
;comes back is what is held to. It was a time, then the abort timer, and a
;Pi with every test running failed each now and then: a time says how busy
;the nodes were, not whether the close was right
(each (lambda (cmdline)
	(defq pc (Pipe cmdline) pc_out (list))
	(. pc :eof)
	(while (defq pc_data (. pc :read)) (if (str? pc_data) (push pc_out pc_data)))
	(assert-eq (cat "closes after eof, " cmdline) pc (. pc :close))
	(assert-true (cat "and all it said was read, " cmdline) (nempty? pc_out)))
	(list "echo one two" "files cmd/ .lisp" "make all boot" "files cmd/ | head -n 3"))

;an outfun that goes wrong is an error the caller has, and not a task that
;waits for ever on the pipe it let go of
;only a checked build has the error, a release build skips the two
(defq pipe_t0 (pii-time) pipe_err :nil)
(assert-error "an outfun of the wrong number of args is an error" (pipe-run "echo one two" (# :nil)))
;at once is not for ever, a small machine with every test running takes a while
(assert-true "and it is had at once" (< (- (pii-time) pipe_t0) (task-timeout 10)))
(setq pipe_out (list))
(assert-error "an outfun that throws is an error" (pipe-run "files cmd/ .lisp" (# (throw "mine" %0))))
(pipe-run "echo one two" (# (push pipe_out %0)))
(assert-true "and a pipe run after it is as ever" (nempty? pipe_out))

;a pipe let go of while open ends its stdin and goes, it is not waited on
;for ever. One that had not been read, one mid way, and one written to.
;
;Gone is when its commands have ended, and not for ever is all that is
;held to: there is no timer in letting go, so no time that would mean a
;fault short of never. It was a second by the clock. On a Pi with every
;test running, 48 of these over 12 runs were 12 to 110ms but for those of
;files | sort, the one with work to finish: 226, 263, 274 and 885ms. And
;so over the second once in some twenty runs, which is what a soak showed
(each (lambda (cmdline)
	(defq pd (Pipe cmdline) pipe_t0 (pii-time))
	(if (eql cmdline "cat") (. pd :write "abc"))
	(if (eql cmdline "echo one two") (. pd :read))
	(setq pd :nil)
	(assert-true (cat "a pipe let go of open is gone, " cmdline) (< (- (pii-time) pipe_t0) (task-timeout 5))))
	(list "echo one two" "echo one | cat" "cat" "files cmd/ .lisp | sort"))
(setq pipe_out (list))
(pipe-run "echo one two" (# (push pipe_out %0)))
(assert-true "and a pipe run after them is as ever" (nempty? pipe_out))
;an abort, before and after the end of stdin
(each (lambda (ended)
	(defq pd (Pipe "lisp -r (while :t (task-sleep 100000))") pipe_t0 (pii-time))
	(if ended (. pd :eof))
	(. pd :abort)
	(setq pd :nil)
	(assert-true (cat "a pipe aborted is gone, stdin ended " (str ended)) (< (- (pii-time) pipe_t0) 1000000)))
	(list :nil :t))

;a command that does not exist is no pipe, and is said to be none at once.
;The kernel starts no task for a .lisp file that is not there, and says so
(defq pipe_t0 (pii-time))
(assert-eq "a command that does not exist is no pipe" :nil (Pipe "no_such_command_zyz"))
(assert-eq "nor one with it in the middle" :nil (Pipe "echo one | no_such_command_zyz | cat"))
(assert-eq "nor one with it at the end" :nil (Pipe "echo one | no_such_command_zyz"))
(assert-true "and it is known at once" (< (- (pii-time) pipe_t0) (task-timeout 10)))
(defq pipe_ask (mail-mbox))
(open-task "cmd/no_such_command_zyz.lisp" (task-nodeid) +kn_call_pin 0 pipe_ask)
(defq pipe_said (mail-read pipe_ask))
(assert-eq "the kernel gives no task for a file that is not there" 0
	(get-long (getf pipe_said +kn_msg_reply_id) 0))
;an id of 0 does not say who could not, so the kernel that answers does
(assert-eq "and says it is this node that did not" (hex-encode (task-nodeid))
	(hex-encode (getf pipe_said +kn_msg_reply_node)))
(defq pipe_far (last (lisp-nodes :t)))
(open-task "cmd/no_such_command_zyz.lisp" pipe_far +kn_call_pin 0 pipe_ask)
(setq pipe_said (mail-read pipe_ask))
(assert-eq "another node asked for it gives none, and says it is that node" (hex-encode (cat (char 0 +long_size) pipe_far))
	(hex-encode (cat (slice (getf pipe_said +kn_msg_reply_id) 0 +long_size) (getf pipe_said +kn_msg_reply_node))))
(open-task "(task-sleep 1)" pipe_far +kn_call_pin 0 pipe_ask)
(setq pipe_said (mail-read pipe_ask))
(assert-eq "one that is started says who started it too, the node its id has" (hex-encode (slice (getf pipe_said +kn_msg_reply_id) +long_size -1))
	(hex-encode (getf pipe_said +kn_msg_reply_node)))
;the commands of such a pipeline that did start are told to go, and do
(setq pipe_out (list))
(times 20 (Pipe "cat README.md | no_such_command_zyz | cat"))
(pipe-run "echo one two" (# (push pipe_out %0)))
(assert-true "and a pipe run after them is as ever" (nempty? pipe_out))

;a command whose file is there and throws as it loads. Its task has no
;main, and went with the pipe still waiting to hear it was up. It is
;started as a form that catches the load, takes its stdio and says why
(cond
	((not *test_checked*) (test-skip "a command that throws as it loads" "needs an error checked build"))
	(:t (save "(defq tmp_pipe_load (tmp-pipe-no-such-function 1))\n(defun main ()\n\t(print {never}))\n"
			"cmd/tmp_pipe_bad_load.lisp")
		(setq pipe_out (list) pipe_t0 (pii-time))
		(pipe-run "tmp_pipe_bad_load" (# (push pipe_out %0)))
		(defq pipe_said (apply (const cat) (cat (list "") pipe_out)))
		(assert-true "a command that throws as it loads is over at once" (< (- (pii-time) pipe_t0) (task-timeout 10)))
		(assert-true "and the pipe is told why" (nempty? (substr pipe_said "symbol_not_bound")))
		(assert-true "with the name of what was not there" (nempty? (substr pipe_said "tmp-pipe-no-such-function")))
		(assert-true "its main is not run" (empty? (substr pipe_said "never")))
		(setq pipe_out (list))
		(pipe-run "echo one | tmp_pipe_bad_load | cat" (# (push pipe_out %0)))
		(assert-true "in the middle of a pipeline it is the same"
			(nempty? (substr (apply (const cat) (cat (list "") pipe_out)) "symbol_not_bound")))
		(pii-remove "cmd/tmp_pipe_bad_load.lisp")
		(setq pipe_out (list))
		(pipe-run "echo one two" (# (push pipe_out %0)))
		(assert-true "and a pipe run after it is as ever" (nempty? pipe_out))))

(undef (env) 'pipe_ask 'pipe_t0 'pipe_err 'pipe 'pipe_out 'reply_mbox 'child_mbox 'reply 'farmed)

;what a command says on its stderr and what it says on its stdout are two
;streams, and come in either order. A pipe that took the stdout stopping
;for the end lost the stderr that came after it, the error a command ended
;with, one time in some dozens on a machine with a lot to do. A command
;says its stderr is stopping too now, and the pipe is closed when all it
;says has stopped. Many times over, to see
(defq pipe_lost 0 pipe_both 0)
(times 60
	(defq pipe_out (list))
	(pipe-run "lisp -r (write-line (io-stream (quote stderr)) {oops, on stderr}) (print {said})" (# (push pipe_out %0)))
	(defq pipe_said (apply (const cat) (cat (list "") pipe_out)))
	(unless (found? pipe_said "oops, on stderr") (++ pipe_lost))
	(and (found? pipe_said "oops, on stderr") (found? pipe_said "said") (++ pipe_both)))
(assert-eq "of 60 commands that say a thing on their stderr as they end, what not one says is lost" 0 pipe_lost)
(assert-eq "and each says what it said on its stdout as well" 60 pipe_both)

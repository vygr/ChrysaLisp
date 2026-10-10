;;;;;;;;;;;;;;;;;;
; pipe task object
;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +pipe_abort_delay 2000000 +pipe_stderr_delay 50000)

(defun pipe-split (cmdline)
	; (pipe-split cmdline) -> ((mode cmd) ...)
	(defq i 0 out (list) quotes (partition (substr cmdline "\q") 2) mode +kn_call_run)
	(each (lambda (((x x1)))
		(unless (some (lambda ((((qx &)) ((& qx1)))) (<= qx x x1 qx1)) quotes)
			(defq j i) (setq i x1)
			(push out (list mode (trim (slice cmdline j x))))
			(setq mode (if (eql (slice cmdline x x1) "!") +kn_call_pin +kn_call_run))))
		(push (sort (cat (substr cmdline "|") (substr cmdline "!"))
			(# (- (first (first %0)) (first (first %1))))) '((-1 0))))
	out)

(defun pipe-task (file)
	; (pipe-task file) -> form
	;a command is started as a form that loads its file, not as the file,
	;so that a load that throws can be caught there and the pipe told,
	;(import-cmd) in class/lisp/root.inc
	(cat "(import-cmd \q" file "\q)"))

(defclass Pipe (cmdline &optional user_select) :nil
	; (Pipe cmdline [user_select]) -> pipe | :nil
	(setd user_select '())
	(bind '(cmdmodes cmdpipe args) (reduce (#
		(bind '(mode cmd_str) %1)
		(bind '(((x x1))) (matches cmd_str "^\\S+"))
		(push (first %0) mode)
		(push (second %0) (cat "cmd/" (slice cmd_str x x1) ".lisp"))
		(push (third %0) cmd_str) %0) (pipe-split cmdline) (lists 3)))
	(cond
		;a command that is not there ?
		((some (# (= 0 (age %0))) cmdpipe)
			(setq this :nil))
		;error with pipe element ?
		((some (# (= 0 (get-long %0 0))) (defq mboxs (open-pipe (map (const pipe-task) cmdpipe) cmdmodes)))
			;send abort to any started pipe elements
			(each (# (if (/= 0 (get-long %0 0)) (mail-send %0 ""))) mboxs)
			(setq this :nil))
		(:t ;wire up pipe and send args
			(bind '(streams select) (reduce (lambda (%0 &)
				(push (first %0) (defq in (in-stream)))
				(push (second %0) (in-mbox in)) %0) mboxs (lists 2)))
			(defq stdout (in-stream) stdout_mbox (in-mbox stdout) ack_mbox (mail-mbox))
			(reach (lambda (mbox arg stderr_mbox)
				(mail-send mbox (setf-> (cat (str-alloc +stdio_msg_init_size) arg)
					(+stdio_msg_init_stdout stdout_mbox)
					(+stdio_msg_init_stderr stderr_mbox)
					(+stdio_msg_init_ack ack_mbox)))
				(setq stdout_mbox (getf (mail-read ack_mbox) +stdio_msg_init_stdout))) mboxs args select)
			;the stdin stream goes first of the streams, with what the pipe
			;says after it. A list lets go of its first element first, so a
			;pipe that is let go of while open ends its stdin, and then waits
			;for what the commands say to stop, as they do when their stdin
			;has. The other way round each side waits for the other, for ever
			(setq streams (cat (list (out-stream stdout_mbox)) streams (list stdout)))
			(push select (in-mbox stdout))
			(def this :select (cat user_select select) :user_select user_select
				:streams streams :stdout stdout :ids mboxs :state :t)))

	(defmethod :poll ()
		; (. pipe :poll) -> :nil | :t
		(when (get :state this)
			(mail-poll (get :select this))))

	(defmethod :read ()
		; (. pipe :read) -> :nil | :t | data
		;:nil if pipe closed
		;:t if user select
		;
		;What a pipe says is its stdout and the stderr of each of its
		;commands. They are streams of their own and what is on them comes
		;in any order: a stderr can stop before the last of the stdout has
		;been read, and the stdout can stop before what a command said on
		;its stderr has come, the error it ended with. To close on the
		;first to stop lost the rest of the others, and to close when the
		;stdout did lost that error, now and then, on a machine with a lot
		;to do.
		;
		;So the pipe is closed when its stdout has stopped and every stderr
		;has. A command says its stderr is stopping as it ends,
		;class/stdio/class.vp. A system of before it did only says so of
		;its stdout, and this file may be run by one, as a system is built
		;from a snapshot: so once the stdout has stopped a stderr is waited
		;for no longer than a moment with nothing said, +pipe_stderr_delay
		(defq result :nil)
		(while (and (not result) (get :state this))
			(defq user_select_len (length (get :user_select this)) select (get :select this)
				draining (eql (get :state this) :draining))
			(when draining
				(mail-timeout (defq timer (mail-mbox)) +pipe_stderr_delay 0)
				(setq select (cat select (list timer))))
			(cond
				((< (defq idx (mail-select select)) user_select_len)
					(setq result :t))
				((and draining (= idx (dec (length select))))
					;nothing more was said in that moment
					(def this :state :nil))
				(:t ;must be stdout or one of the stderr's
					(in-next-msg (defq msg_in (elem-get (get :streams this) (inc (- idx user_select_len)))))
					(cond
						((every (# (/= (in-get-state %0) +stream_mail_state_started)) (rest (get :streams this)))
							(def this :state :nil))
						((/= (in-get-state (get :stdout this)) +stream_mail_state_started)
							(def this :state :draining)))
					(setq result (read-avail msg_in))))
			(if draining (mail-timeout timer 0 0)))
		result)

	(defmethod :write (string)
		; (. pipe :write string) -> pipe
		;first stream is :out to first pipe element
		(unless (get :eof this)
			(defq stream (first (get :streams this)))
			(write-blk stream string)
			(stream-flush stream))
		this)

	(defmethod :eof ()
		; (. pipe :eof) -> pipe
		;the end of the pipe's stdin. The stdin stream is cleared, which
		;sends stopping and stopped into the pipe, and what the pipe has
		;yet to say can still be read
		(unless (get :eof this)
			(def this :eof :t)
			(elem-set (get :streams this) 0 :nil))
		this)

	(defmethod :abort ()
		; (. pipe :abort) -> pipe
		;flag all streams as aborted, wake the in streams
		(raise :select :streams)
		(each (# (case (pop (type-of %0))
				(:out (out-set-state %0 +stream_mail_state_aborted))
				(:in (in-set-state %0 +stream_mail_state_aborted))))
			streams)
		;signal kernel of node(id) targeting mbox(id)
		(each (lambda (id)
			(mail-send (cat (char 0 +long_size) (slice id +long_size -1))
				(setf-> (str-alloc +kn_msg_signal_size)
					(+kn_msg_func +kn_call_signal)
					(+kn_msg_signal_mbox (get-long id 0))
					(+kn_msg_signal_flags +tk_flag_aborted))))
			(get :ids this))
		;now destroy all the streams
		(while (nempty? streams) (if (pop streams) (pop select)))
		this)

	(defmethod :close ()
		; (. pipe :close) -> pipe
		;clear the stdin stream, which will send
		;stopping and stopped into the pipe
		(raise :select :streams)
		(def this :eof :t)
		(elem-set streams 0 :nil)
		;what is left to wait for, the stdout and stderr streams
		(setq streams (rest streams))
		;wait for stdout and stderr streams to stop
		;if it goes on too long, then abort !
		(mail-timeout (last (push select (mail-mbox))) +pipe_abort_delay 0)
		(defq user_select_len (length (get :user_select this)) at (length streams))
		;a stream that has stopped already has no more to say, and is not
		;waited for. After (. pipe :eof) a stderr can stop, and be read as
		;stopped, while the last of the stdout is still coming. To wait for
		;it then was to wait for the abort timer, every time
		(while (>= (setq at (dec at)) 0)
			(if (>= (in-get-state (elem-get streams at)) +stream_mail_state_stopped)
				(setq streams (erase streams at (inc at))
					select (erase select (+ at user_select_len) (+ at user_select_len 1)))))
		(while (and (nempty? streams)
					(< (defq mboxs (slice select user_select_len -1) idx (mail-select mboxs))
						(dec (length mboxs))))
			(if (>= (in-get-state (in-next-msg (elem-get streams idx))) +stream_mail_state_stopped)
				(setq streams (erase streams idx (inc idx))
					select (erase select (+ idx user_select_len) (+ idx user_select_len 1)))))
		;did we break out due to the abort timer ?
		(when (nempty? streams)
			(lower :select :streams)
			(. this :abort))
		this)
	)

(defun pipe-run (cmdline &optional out)
	; (pipe-run cmdline [outfun])
	(task-count -1) ;we will be only collating results !
	(setd out print)
	(defq pipe (Pipe cmdline))
	;an error from the outfun closes the pipe on its way by. A pipe let go
	;of while still open waits for ever on a command that has not stopped,
	;and the error is never seen
	(catch (while pipe
			(defq data (. pipe :read))
			(cond
				((eql data :nil)
					;pipe is closed
					(. pipe :close)
					(setq pipe :nil))
				(:t ;string from pipe
					(callback out (penv) data))))
		(progn (. pipe :close) (task-count 1) :nil))
	(task-count 1)) ;we where only collating results !

;module
(export-classes '(Pipe))
(export-symbols '(pipe-run pipe-split))
(env-pop)

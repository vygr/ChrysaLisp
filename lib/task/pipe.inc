;;;;;;;;;;;;;;;;;;
; pipe task object
;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +pipe_abort_delay 2000000)

(defun pipe-split (cmdline)
	; (pipe-split cmdline) -> (e0 [e1] ...)
	(defq i 0 out (list) quotes (partition (substr cmdline (ascii-char 34)) 2))
	(each (lambda (((x x1)))
		(unless (some (lambda ((((qx &)) ((& qx1)))) (<= qx x x1 qx1)) quotes)
			(defq j i) (setq i x1) (push out (trim (slice cmdline j x)))))
		(push (substr cmdline " | ") '((-1 0))))
	out)

(defclass Pipe (cmdline &optional user_select) :nil
	; (Pipe cmdline [user_select]) -> pipe | :nil
	(setd user_select '())
	(bind '(cmdpipe args) (reduce (#
		(bind '(((x x1))) (matches %1 "^\\S+"))
		(push (first %0) (cat "cmd/" (slice %1 x x1) ".lisp"))
		(push (second %0) %1) %0) (pipe-split cmdline) (lists 2)))
	(cond
		;error with pipe element ?
		((some (# (= 0 (get-long %0 0))) (defq mboxs (open-pipe cmdpipe)))
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
			(push streams stdout (out-stream stdout_mbox))
			(push select (in-mbox stdout))
			(def this :select (cat user_select select) :user_select user_select
				:streams streams :ids mboxs :state :t)))

	(defmethod :poll ()
		; (. pipe :poll) -> :nil | :t
		(when (get :state this)
			(mail-poll (get :select this))))

	(defmethod :read ()
		; (. pipe :read) -> :nil | :t | data
		;:nil if pipe closed
		;:t if user select
		(when (get :state this)
			(defq user_select_len (length (get :user_select this)))
			(cond
				((< (defq idx (mail-select (get :select this))) user_select_len))
				(:t ;must be stdout or one of the stderr's
					(if (/= (in-get-state (in-next-msg (defq msg_in
								(elem-get (get :streams this) (- idx user_select_len)))))
							+stream_mail_state_started)
						(def this :state :nil))
					(read-avail msg_in)))))

	(defmethod :write (string)
		; (. pipe :write string) -> pipe
		(defq stream (last (get :streams this)))
		(write-blk stream string)
		(stream-flush stream)
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
		(while (pop streams) (pop select))
		this)

	(defmethod :close ()
		; (. pipe :close) -> pipe
		;clear the stdin stream, which will send stopping and stopped
		(raise :select :streams)
		(pop streams) (pop select)
		;wait for stdout and stderr streams to stop
		;if it goes on too long, then abort !
		(mail-timeout (last (push select (mail-mbox))) +pipe_abort_delay 0)
		(defq user_select_len (length (get :user_select this)))
		(while (and (> (length select) (inc user_select_len))
					(< (defq mboxs (slice select user_select_len -1) idx (mail-select mboxs))
						(dec (length mboxs))))
			(if (>= (in-get-state (in-next-msg (elem-get streams idx))) +stream_mail_state_stopped)
				(setq streams (erase streams idx (inc idx))
					select (erase select (+ idx user_select_len) (+ idx user_select_len 1)))))
		;did we break out due to the abort timer ?
		(pop select)
		(when (> (length select) user_select_len)
			(lower :select :streams)
			(. this :abort))
		this)
	)

(defun pipe-run (cmdline &optional out)
	; (pipe-run cmdline [outfun])
	(task-count -1)	;we will be only collating results !
	(setd out print)
	(defq pipe (Pipe cmdline))
	(while pipe
		(defq data (. pipe :read))
		(cond
			((eql data :nil)
				;pipe is closed
				(. pipe :close)
				(setq pipe :nil))
			(:t ;string from pipe
				(callback out (penv) data))))
	(task-count 1))	;we where only collating results !

;module
(export-classes '(Pipe))
(export-symbols '(pipe-run pipe-split))
(env-pop)

;;;;;;;;;;;;;;;;;;
; pipe task object
;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")
(import "class/lisp.inc")

(defclass Pipe (cmds &optional user_select) nil
	; (Pipe cmds &optional user_select) -> pipe | nil
	(setd user_select '())
	(defq cmdpipe (list) args (map (lambda (cmd)
		(defq cmd (split cmd " "))
		(push cmdpipe (cat "cmd/" (elem-get 0 cmd) ".lisp"))
		(slice 0 -2 (apply cat (map (lambda (_) (cat _ " ")) cmd)))) (split cmds "|")))
	(cond
		;error with pipe element ?
		((some (lambda (_) (= 0 (get-long _ 0))) (defq mboxs (open-pipe cmdpipe)))
			;send abort to any started pipe elements
			(each (lambda (_) (if (/= 0 (get-long _ 0)) (mail-send _ ""))) mboxs)
			(setq this nil))
		(t  ;wire up pipe and send args
			(defq select (list) streams (list))
			(each (lambda (_)
				(push streams (in-stream))
				(push select (in-mbox (elem-get -2 streams)))) mboxs)
			(defq stdout (in-stream) stdout_mbox (in-mbox stdout) ack_mbox (mail-alloc-mbox))
			(each-rev (lambda (mbox arg stderr_mbox)
				(mail-send mbox (setf-> (cat (str-alloc +stdio_msg_init_size) arg)
					(+stdio_msg_init_stdout stdout_mbox)
					(+stdio_msg_init_stderr stderr_mbox)
					(+stdio_msg_init_ack ack_mbox)))
				(setq stdout_mbox (getf (mail-read ack_mbox) +stdio_msg_init_stdout))) mboxs args select)
			(push streams stdout (out-stream stdout_mbox))
			(push select (in-mbox stdout))
			(mail-free-mbox ack_mbox)
			(def this :select (cat user_select select) :user_select user_select
				:streams streams :state t)))

	(defmethod :poll (this)
		; (. pipe :poll) -> nil | t
		(when (get :state this)
			(mail-poll (get :select this))))

	(defmethod :read (this)
		; (. pipe :read) -> nil | t | data
		;nil if pipe closed
		;t if user select
		(when (get :state this)
			(defq user_select_len (length (get :user_select this)))
			(cond
				((< (defq idx (mail-select (get :select this))) user_select_len))
				(t  ;must be stdout or one of the stderr's
					(if (/= (in-get-state (in-next-msg (defq msg_in
								(elem-get (- idx user_select_len) (get :streams this)))))
							+stream_mail_state_started)
						(def this :state nil))
					(read-avail msg_in)))))

	(defmethod :write (this string)
		; (. pipe :write string) -> pipe
		(stream-flush (write (elem-get -2 (get :streams this)) string))
		this)

	(defmethod :close (this)
		; (. pipe :close) -> pipe
		;clear the stdin stream, which will send stopping and stopped
		(pop (get :select this))
		(pop (get :streams this))
		;wait for stdout and stderr streams to stop
		(defq user_select_len (length (get :user_select this)))
		(while (> (length (get :select this)) user_select_len)
			(if (= (in-get-state (in-next-msg (elem-get (defq idx (mail-select
						(slice user_select_len -1 (get :select this)))) (get :streams this))))
					+stream_mail_state_stopped)
				(def this
					:streams (erase (get :streams this) idx (inc idx))
					:select (erase (get :select this) (+ idx user_select_len) (+ idx user_select_len 1)))))
		this)
	)

(defun pipe-run (cmdline &optional out)
	; (pipe-run cmdline &optional outfun)
	(setd out print)
	(defq pipe (Pipe cmdline))
	(while pipe
		(defq data (. pipe :read))
		(cond
			((eql data nil)
				;pipe is closed
				(. pipe :close)
				(setq pipe nil))
			(t  ;string from pipe
				(out data)))))

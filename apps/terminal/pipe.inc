;;;;;;;;;;;;
; pipe utils
;;;;;;;;;;;;

(import 'sys/lisp.inc)
(import 'class/lisp.inc)

;pipe list element structure
(structure 'pipe 0
	(byte 'state 'select 'streams))

(defun-bind pipe-open (cmds)
	(defq cmdpipe (list) args (map (lambda (cmd)
		(defq cmd (split cmd " "))
		(push cmdpipe (cat "cmd/" (elem 0 cmd) ".lisp"))
		(slice 0 -2 (apply cat (map (lambda (_) (cat _ " ")) cmd)))) (split cmds "|")))
	(cond
		;error with pipe element ?
		((some (lambda (_) (= 0 _)) (defq mboxs (open-pipe cmdpipe)))
			;send abort to any started pipe elements
			(each (lambda (_) (if (/= 0 _) (mail-send "" _))) mboxs)
			nil)
		(t	;wire up pipe and send args
			(defq select (array) pipe_streams (list))
			(each (lambda (_)
				(push pipe_streams (msg-in-stream))
				(push select (msg-in-mbox (elem -2 pipe_streams)))) mboxs)
			(defq stdout (msg-in-stream) stdout_mbox (msg-in-mbox stdout)
				ack (msg-in-stream) ack_mbox (msg-in-mbox ack))
			(each-rev (lambda (mbox arg stderr_mbox)
				(mail-send (cat
					(char stdout_mbox (const long_size))
					(char stderr_mbox (const long_size))
					(char ack_mbox (const long_size))
					(char 0 (const int_size)) arg) mbox)
				(setq stdout_mbox (+ (logand mbox 0xffffffff00000000)
					(get-int (mail-read ack_mbox) (const (* 3 long_size)))))) mboxs args select)
			(push pipe_streams stdout (msg-out-stream stdout_mbox))
			(push select (msg-in-mbox stdout) (task-mailbox))
			(list t select pipe_streams))))

(defun-bind pipe-close (pipe)
	;send stopped
	(stream-flush (stream-next (msg-out-set-state
		;send stopping
		(stream-flush (stream-next (msg-out-set-state
				;flush remaining
				(stream-flush (elem -2 (elem (const pipe_streams) pipe)))
			(const stream_mail_state_stopping))))
		(const stream_mail_state_stopped))))
	;wait for all stopped, starting with the pipe output, then the error streams
	(each! -2 0 (lambda (_)
		(while (/= (msg-in-get-state _) (const stream_mail_state_stopped))
			(msg-in-next-msg _))) (list (elem (const pipe_streams) pipe)))
	(clear pipe))

(defun-bind pipe-read (pipe)
	(cond
		;nil if pipe closed
		((elem (const pipe_state) pipe)
			;pipe is open
			(defq idx (mail-select (elem (const pipe_select) pipe)))
			(cond
				;t if task mailbox
				((= (- idx (length (elem (const pipe_select) pipe))) -1))
				(t	;must be stdout or one of the stderr's
					(if (/= (msg-in-get-state
								(msg-in-next-msg
									(defq msg_in (elem idx (elem (const pipe_streams) pipe)))))
							(const stream_mail_state_started))
						(elem-set (const pipe_state) pipe nil))
					(read-avail msg_in))))))

(defun-bind pipe-write (pipe s)
	(stream-flush (write (elem -2 (elem (const pipe_streams) pipe)) s)))

;;;;;;;;;;;;
; farm utils
;;;;;;;;;;;;

(import 'sys/lisp.inc)
(import 'class/lisp.inc)

;farm list element structure
(structure 'farm 0
	(byte 'select 'streams 'mboxs))

(defun-bind farm-open (path num_child &optional flag)
	(cond
		((some (lambda (_) (= 0 _)) (defq mboxs (open-farm path num_child kn_call_child)))
			;error send abort to any started children
			(each (lambda (_) (if (/= 0 _) (mail-send "" _))) mboxs)
			nil)
		(t	;wire up farm
			(defq select (array (if flag 0 (task-mailbox))) streams (list)
				ack (in-stream) ack_mbox (in-mbox ack))
			(each (lambda (_)
				(push streams (in-stream))
				(push select (in-mbox (elem -2 streams)))
				(mail-send (cat (char ack_mbox (const long_size))
					(char (elem -2 select) (const long_size))) _)) mboxs)
			;wait for acks
			(each (lambda (_) (mail-read ack_mbox)) mboxs)
			(in-set-state ack stream_mail_state_stopped)
			(list select streams mboxs))))

(defun-bind farm-close (farm)
	(bind '(select streams mboxs) farm) (clear farm)
	;send close to all children
	(each (lambda (_) (mail-send "" _)) mboxs)
	;wait for all children to stop
	(defq main (slice 0 1 select) select (slice 1 -1 select))
	(while (/= (length select) 0)
		(defq stream (elem (defq idx (mail-select select)) streams))
		(while (/= (in-get-state (in-next-msg stream)) (const stream_mail_state_stopped)))
		(setq streams (erase streams idx (inc idx)) select (erase select idx (inc idx))))
	(push farm main '() (array)))

(defun-bind farm-read (farm)
	(cond
		;t if task mailbox
		((= (defq idx (mail-select (elem (const farm_select) farm))) 0))
		(t	;must be child output stream
			(elem (dec idx) (elem (const farm_streams) farm)))))

(defun-bind farm-size (farm)
	(length (elem (const farm_mboxs) farm)))

(defun-bind farm-child (farm idx)
	(elem idx (elem (const farm_mboxs) farm)))

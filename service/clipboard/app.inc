;clipboard RPC calls

(enums +clip_type 0
	(enum put get))

(structure +clip_rpc 0
	(netid reply_id)
	(uint type))

(structure +clip_get +clip_rpc_size)
(structure +clip_put +clip_rpc_size)

(defun clip-service ()
	; (clip-service) -> :nil | net_id
	;the clipboard service to use. Each desktop has one of its own, on its
	;node, it talks to the clipboard of the host through the GUI driver of
	;the node. An app with a window is on its desktop's node and has that
	;one, so a desktop that quits does not take the clipboard of another.
	;On a host with a clipboard of its own they are all that one clipboard.
	;A task on a node with no desktop has any one there is on the machine
	(defq me (slice (task-mbox) +mailbox_id_size -1) found :nil)
	(each (lambda (entry)
		(defq id (hex-decode (second (split entry ","))))
		(if (or (not found) (eql (slice id +mailbox_id_size -1) me)) (setq found id)))
		(mail-enquire "@Clipboard,"))
	found)

(defun clip-put-rpc (string)
	; (clip-put-rpc string)
	(when (defq service (clip-service))
		(defq mbox (mail-mbox))
		(mail-send service (setf-> (cat (str-alloc +clip_put_size) string)
			(+clip_rpc_reply_id mbox)
			(+clip_rpc_type +clip_type_put)))
		(mail-read-timeout mbox)))

(defun clip-get-rpc ()
	; (clip-get-rpc) -> string
	(defq string "")
	(when (defq service (clip-service))
		(defq mbox (mail-mbox))
		(mail-send service (setf-> (str-alloc +clip_get_size)
			(+clip_rpc_reply_id mbox)
			(+clip_rpc_type +clip_type_get)))
		(setq string (or (mail-read-timeout mbox) "")))
	string)

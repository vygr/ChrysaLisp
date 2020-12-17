;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/options/options.inc")

(structure '+event 0
	(byte 'close+)
	(byte 'deliver+ 'paste+))

(defun main ()
	(defq id t select (list (task-mailbox) (mail-alloc-mbox)) clipboard (list) +req_get+ 0 +req_put+ 1)
	;Start a new clipboard service, only if one isn't currently running.
	(defq clip_service (mail-declare (elem 1 select) "CLIPBOARD_SERVICE" "Clipboard Service 0.1"))
	(while id
		(defq idx (mail-select select) msg (mail-read (elem idx select)))
		(cond
			((/= idx 0)
				(cond
					((eql (defq req_type (first msg)) "GET")
						(unless (empty? clipboard)
							(defq reply (last clipboard))
							(mail-send (second msg) reply)))
					((eql req_type "PUT")
						(push clipboard (second msg)))))
			((= (setq id (get-long msg ev_msg_target_id)) +event_close+)
				;close
				(setq id nil))))
	;shutdown, if requested.
	(mail-free-mbox (pop select))
	(mail-forget clip_service))

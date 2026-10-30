(report-header "Leave: a desktop that quits takes the nodes it started with it")

;this changes the network, so it is in tests/solo/, and runs on its own.
;A hidden desktop starts three nodes of its own, as nodes -a in a terminal
;on it would, its GUI is told to quit, and the three go with it, (node-leave)

(defq lv_before (lisp-nodes :t) lv_pid (first (node-spawn 1 :gui_hidden "service/gui/app.lisp"))
	lv_reply (mail-mbox) lv_node :nil lv_pids :nil)

(defun lv-find ()
	;the desktop's node, :nil if not seen yet
	(some (lambda (node)
			(unless (find node lv_before)
				(open-task (str `(mail-send (hex-decode ,(hex-encode lv_reply)) (str (pii-pid))))
					node +kn_call_pin 0 (mail-mbox))
				(if (eql (mail-read-timeout lv_reply (task-timeout 1)) (str lv_pid)) node)))
		(lisp-nodes :t)))

(assert-true "a desktop is started" (> lv_pid 0))
(until (setq lv_node (lv-find)) (task-sleep 10000))

;once its GUI is up, three nodes started by the desktop's node, noted as
;its own. "Gui" is the desktop's node's own, so that is looked for there
(open-task (str `(progn (until (mail-first "Gui") (task-sleep 10000))
		(mail-send (hex-decode ,(hex-encode lv_reply)) (str (node-spawn 3)))))
	lv_node +kn_call_pin 0 (mail-mbox))
(setq lv_pids (first (read (string-stream (mail-read lv_reply)))))
(assert-eq "it starts three nodes of its own" 3 (length (filter (# (> %0 0)) lv_pids)))
(assert-true "and they are running" (every (const pii-alive) lv_pids))

;its GUI quits, its node goes, and theirs with it
(open-task "(progn (import {service/gui/lisp.inc}) (gui-quit-rpc))" lv_node +kn_call_pin 0 (mail-mbox))
(while (pii-alive lv_pid) (task-sleep 10000))
(assert-eq "the desktop's process ends" :nil (pii-alive lv_pid))
(while (some (const pii-alive) lv_pids) (task-sleep 10000))
(assert-true "and so do those it started" (notany (const pii-alive) lv_pids))
(assert-eq "its note is cleared away" :nil (load (cat "/tmp/chrysalisp_" (str lv_pid) ".session")))
(while (/= (length (lisp-nodes :t)) (length lv_before)) (task-sleep 10000))
(assert-eq "and the network is as it was" (length lv_before) (length (lisp-nodes :t)))

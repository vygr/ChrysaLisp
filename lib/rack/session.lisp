;a session of a rack run, started by a node of this machine with
;(pii-spawn), so on the boot image as it is on disk now. It sizes itself
;to the machine, runs each command line it was left, leaves what they said,
;and then stops every node it started and goes. No launch script watches
;over it, so it clears up after itself: each node it started is sent a task
;that exits it, and the files of its links are removed.
(import "./rack.inc")
;the node that started it gave the run a name, its files end with it
(defq out (list) me (task-nodeid) t0 (pii-time) run (ifn (get '*rack_run*) ""))
(catch (progn
		(node-auto)
		(each (lambda (cmdline)
			(when (nempty? cmdline)
				(pipe-run cmdline (# (push out %0)))))
			(split (ifn (load (cat +rack_cmds run)) "") (ascii-char 10))))
	(progn (push out (cat "SESSION ERROR " (str _) (ascii-char 10))) :t))
(push out (cat "[" (str (/ (- (pii-time) t0) 1000000)) "s]" (ascii-char 10)))
(each (lambda (node) (unless (eql node me) (open-task "(pii-exit)" node +kn_call_pin 0 (mail-mbox)))) (lisp-nodes :t))
(defq file (cat "/tmp/chrysalisp_" (str (pii-pid)) ".session"))
(when (defq text (load file))
	(each (lambda (line) (if (starts-with "link " line) (pii-remove (cat "/tmp/" (slice line 5 -1)))))
		(split text (ascii-char 10)))
	(pii-remove file))
(task-sleep 200000)
(save (apply (const cat) (cat (list "") out)) (cat +rack_out run))
(pii-exit)

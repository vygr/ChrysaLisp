;a session of a rack run, started by a node of this machine with
;(pii-spawn), so on the boot image as it is on disk now. It sizes itself
;to the machine, runs each command line it was left, leaves what they said
;as they say it, and then stops every node it started and goes. No launch
;script watches over it, so it clears up after itself, (rack-session-clear).
;And if what waits for it has gone it stops in the middle, ./watch.lisp.
(import "./rack.inc")
;the node that started it gave the run a name, its files end with it
(defq me (task-nodeid) t0 (pii-time) run (ifn (get '*rack_run*) "")
	out (file-stream (cat +rack_out run) +file_open_write))
(defun said (text)
	;what was said, there as it comes, for what waits to pass on
	(write-blk out text)
	(stream-flush out))
(mail-send (open-child "lib/rack/watch.lisp" +kn_call_pin) run)
(catch (progn
		(node-auto)
		;a desktop: the GUI on this node, the host's, as run.sh -f has it,
		;and up before the command lines, which may look at it
		(when (get '*rack_gui*)
			(open-child "service/gui/app.lisp" +kn_call_pin)
			(defq gui_t0 (pii-time))
			(while (and (empty? (mail-enquire "@Gui,")) (< (- (pii-time) gui_t0) (task-timeout 20)))
				(task-sleep 100000)))
		(each (lambda (cmdline)
			(when (nempty? cmdline)
				(pipe-run cmdline (const said))))
			(split (ifn (load (cat +rack_cmds run)) "") "\n")))
	(progn (said (cat "SESSION ERROR " (str _) "\n")) :t))
(rack-session-clear)
(task-sleep 200000)
;the last line, how long it took, is what says it is all there
(said (cat "[" (str (/ (- (pii-time) t0) 1000000)) "s]" "\n"))
(pii-exit)

;a session of a rack run, started by a node of this machine with
;(pii-spawn), so on the boot image as it is on disk now. It sizes itself
;to the machine, runs each command line it was left, leaves what they said,
;and then stops every node it started and goes. No launch script watches
;over it, so it clears up after itself: each node it started is sent a task
;that exits it, and the files of its links are removed, and the notes and
;links of whatever those nodes started in turn.
(import "./rack.inc")
;the node that started it gave the run a name, its files end with it
(defq out (list) me (task-nodeid) t0 (pii-time) run (ifn (get '*rack_run*) ""))
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
				(pipe-run cmdline (# (push out %0)))))
			(split (ifn (load (cat +rack_cmds run)) "") (ascii-char 10))))
	(progn (push out (cat "SESSION ERROR " (str _) (ascii-char 10))) :t))
(push out (cat "[" (str (/ (- (pii-time) t0) 1000000)) "s]" (ascii-char 10)))
(each (lambda (node) (unless (eql node me) (open-task "(pii-exit)" node +kn_call_pin 0 (mail-mbox)))) (lisp-nodes :t))
;its note, and the note of every node it names, and of those they name: a
;node of this session that started nodes of its own left one, a test that
;starts a node which starts six more, and nothing else clears it away.
;The files of the links they name go with them. A list of those still to
;look at, this does not call itself
(defq todo (list (pii-pid)) seen (list))
(while (defq pid (pop todo))
	(unless (find pid seen)
		(push seen pid)
		(defq file (cat "/tmp/chrysalisp_" (str pid) ".session"))
		(when (defq text (load file))
			(each (lambda (line)
				(defq parts (split line " "))
				(cond
					((< (length parts) 2))
					((eql (first parts) "link") (pii-remove (cat "/tmp/" (second parts))))
					((find (first parts) '("pid" "front")) (push todo (str-as-num (second parts))))))
				(split text (ascii-char 10)))
			(pii-remove file))))
(task-sleep 200000)
(save (apply (const cat) (cat (list "") out)) (cat +rack_out run))
(pii-exit)

;a member of the mesh, one node that stays up: it finds the other machines
;and links to them, and it takes a sync. It is this machine's place in the
;mesh, what a rack run, the rack command, reaches it by. It builds nothing
;and tests nothing itself, a session is started for that. rack.sh starts
;and stops it.
;
;It is also a way in from a shell. A command line left in the file
;/tmp/chrysalisp_rack_job is run here, and what it said is left in
;/tmp/chrysalisp_rack_result, with DONE as its last line.
(import "./rack.inc")
(when (empty? (mail-enquire "@Net,")) (open-child "service/net/app.lisp" +kn_call_run) (task-sleep 200000))
(net-quiet 300000 4)
(save (str (pii-pid)) "/tmp/chrysalisp_rack.pid")
(pipe-run "link -l 3333 -a" (const prin))
(pipe-run "link -a" (const prin))
(pipe-run "sync -a" (const prin))
(stream-flush (io-stream "stdout"))
(while :t
	(when (defq text (load "/tmp/chrysalisp_rack_job"))
		(pii-remove "/tmp/chrysalisp_rack_job")
		(defq out (list))
		(catch (each (lambda (cmdline)
				(if (nempty? cmdline) (pipe-run cmdline (# (push out %0)))))
				(split text (ascii-char 10)))
			(progn (push out (cat "JOB ERROR " (str _) (ascii-char 10))) :t))
		(save (apply (const cat) (cat (list "") out (list (ascii-char 10) "DONE" (ascii-char 10))))
			"/tmp/chrysalisp_rack_result"))
	(task-sleep 100000))

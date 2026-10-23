;a member of the mesh, one node that stays up: it finds the other machines
;and links to them, and it takes a sync. It is this machine's place in the
;mesh, what a rack run, the rack command, reaches it by. It builds nothing
;and tests nothing itself, a session is started for that. rack.sh starts
;and stops it.
;
;It is also a way in from a shell. A command line left in the file
;/tmp/chrysalisp_rack_job is run here, and what it said is left in
;/tmp/chrysalisp_rack_result, with DONE as its last line.
;
;A tree with no .git that the herd has taken to a newer waypoint is run
;by what was built before it: the member leaves /tmp/chrysalisp_rack_update
;and ends, and rack.sh does make install and starts it again. One with a
;.git is someone's own, kept by git, and is left to them, unless it is
;followed, rack.sh up -f: then rack.sh takes it on from git, and the member
;notes the waypoint as it starts, for the herd to take on to the others.
;
;So is one whose boot image was made from other VP source than the tree,
;at its waypoint, has now, as it starts: an image built here by hand, or
;by an install that did not finish. Once for a tree, so one that make
;install does not mend is not built for ever
(import "./rack.inc")
(import "././boot/id.inc")
(when (empty? (mail-enquire "@Net,")) (open-child "service/net/app.lisp" +kn_call_run) (task-sleep 200000))
(net-quiet 300000 4)
(save (str (pii-pid)) "/tmp/chrysalisp_rack.pid")
;the Net service joins the mesh by itself, if this is the first session
;of the machine. This asks again, for one that started without
(pipe-run "link -l 3333 -a" (const prin))
(pipe-run "link -a" (const prin))
(pipe-run "sync -a" (const prin))
;followed, the tree is at a commit of its branch, noted if it is clean
(and (pii-fstat ".git") (pii-fstat "/tmp/chrysalisp_rack_follow")
	(pipe-run "waypoint" (const prin)))
(stream-flush (io-stream "stdout"))
(defq at_start (first (sync-waypoint ".")) looked (pii-time)
	kept (sync-kept "."))
(and (not (pii-fstat ".git")) (sync-clean "." kept)
	(defq want (boot-id-tree)) (nql want (boot-id))
	(cond
		((eql want (load "/tmp/chrysalisp_rack_tried"))
			(print "The boot image is not of this tree's VP source, and make install did not mend it."))
		(:t (save want "/tmp/chrysalisp_rack_tried")
			(save (cat "build " want) "/tmp/chrysalisp_rack_update")
			(pii-exit))))
(while :t
	;a newer waypoint taken by the herd, looked for each few seconds
	(when (> (- (pii-time) looked) 5000000)
		(setq looked (pii-time))
		(and (not (pii-fstat ".git")) (defq at (sync-waypoint "."))
			(nql (first at) at_start) (sync-clean "." kept)
			(progn (save (first at) "/tmp/chrysalisp_rack_update") (pii-exit))))
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

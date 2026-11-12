;a member of the mesh, one node that stays up: it finds the other machines
;and links to them, and it takes a sync. It is this machine's place in the
;mesh, what a rack run, the rack command, reaches it by. It builds nothing
;and tests nothing itself, a session is started for that. rack.sh starts
;and stops it.
;
;It is also a way in from a shell. A command line left in a file
;/tmp/chrysalisp_rack_job_<tag> is run here, and what it said is left in
;/tmp/chrysalisp_rack_result_<tag>, with DONE as its last line. Each job
;has its own tag, rack.sh run's, and they are run one at a time, the
;oldest first, so one is never given another's answer.
;
;It is stopped by leaving /tmp/chrysalisp_rack_stop, as rack.sh does: the
;stores it keeps put what they have on their volumes first, and so does
;it when it ends to be updated. A node killed part way through a write
;leaves its volume to be mended.
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
(import "service/storage/app.inc")

(defun member-leave (&optional update)
	;the stores kept here are closed, then what rack.sh is to do, if
	;anything, is left for it, and the node ends
	(storage-stop-here)
	(if update (save update "/tmp/chrysalisp_rack_update"))
	(pii-exit))

(defun job-next ()
	; (job-next) -> :nil | tag
	;the tag of the oldest job left for this member, the first by name of
	;those left in the same second
	(defq jobs (list))
	(each! (lambda (name type)
			(and (eql type "f") (starts-with "chrysalisp_rack_job_" name)
				(defq info (pii-fstat (cat "/tmp/" name)))
				(push jobs (list (first info) name))))
		(dir-list "/tmp"))
	(when (nempty? jobs)
		(slice (second (first (sort jobs (lambda ((ta na) (tb nb))
				(if (= ta tb) (cmp na nb) (- ta tb)))))) (length "chrysalisp_rack_job_") -1)))

(mail-await (# (mail-first "@Net")) "service/net/app.lisp" +kn_call_run (task-timeout 2))
;and a lock, as any session has: a kernel a child sent here builds is
;made under one, two children building it at once would collide
(mail-await lock-service "service/lock/app.lisp" +kn_call_run (task-timeout 2))
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
			(member-leave (cat "build " want)))))
(while :t
	;a newer waypoint taken by the herd, looked for each few seconds
	(when (> (- (pii-time) looked) 5000000)
		(setq looked (pii-time))
		(and (not (pii-fstat ".git")) (defq at (sync-waypoint "."))
			(nql (first at) at_start) (sync-clean "." kept)
			(member-leave (first at))))
	;asked to stop, rack.sh down or a commit followed
	(when (pii-fstat "/tmp/chrysalisp_rack_stop")
		(pii-remove "/tmp/chrysalisp_rack_stop")
		(member-leave))
	;a job is taken whatever is in it, one that can not be read is not
	;looked at again and again before the others
	(when (and (defq tag (job-next))
			(progn (defq job (cat "/tmp/chrysalisp_rack_job_" tag) text (load job))
				(pii-remove job) text))
		;what it says is written as it is said, so rack.sh run shows a long
		;job as it goes, DONE last
		(defq out (file-stream (cat "/tmp/chrysalisp_rack_result_" tag) +file_open_write))
		(catch (each (lambda (cmdline)
					(if (nempty? cmdline) (pipe-run cmdline (# (write-blk out %0) (stream-flush out)))))
				(split text "\n"))
			(progn (write-blk out (cat "JOB ERROR " (str _) "\n")) :t))
		(write-blk out "\nDONE\n")
		(stream-flush out)
		(setq out :nil))
	(task-sleep 100000))

(report-header "Source Service: a task's files read from the system that sent it")
(import "service/source/app.inc")
(import "apps/system/netmon/app.inc")

;one to a system, which only its own tasks look up: what it is is given
;to a task that is sent, it is said to nobody else
(open-child "service/source/app.lisp" +kn_call_run)
(until (source-service) (task-sleep 10000))
(defq so_svc (source-service))
(assert-eq "it is up, as @Source" 1 (length (mail-enquire "@Source,")))
(assert-eq "and is not said to the mesh" 0 (length (mail-enquire "*Source,")))

;it gives the Lisp of the tree, and nothing else of it: the keys are in
;the tree, mesh_key and keys/, and a user's own folder
(defq so_inc "apps/system/netmon/app.inc")
(assert-eq "it gives a file that is imported, as it is" (load so_inc) (source-ask so_svc so_inc))
(assert-eq "and usr/env.inc, which is the system's" (load "usr/env.inc") (source-ask so_svc "usr/env.inc"))
(assert-list-eq "not the key, the keyring, a user's own, out of the tree, or one not there"
	'(:nil :nil :nil :nil :nil :nil)
	(map (# (source-ask so_svc %0))
		'("mesh_key" "keys/rig" "usr/Test/env.inc" "apps/../usr/Test/env.inc" "/etc/hosts" "no/such.lisp")))

;a stream of a file of a system: of the file on this one, asked of the
;service where the system is another, the same either way
(defq so_here (source-stream (hex-encode (system-id)) so_svc so_inc)
	so_there (source-stream "0" so_svc so_inc))
(assert-eq "a file of this system is read as a file" (load so_inc) (read-blk so_here 100000))
(assert-eq "of another, asked of its service, the same" (load so_inc) (read-blk so_there 100000))
(assert-eq "and none if it will not give it" :nil (source-stream "0" so_svc "mesh_key"))

;NetMon's child, sent as what source-task makes, replies; and again with
;the system said to be another, so that each import, the child's own of
;./app.inc too, is asked of the service as on another machine
(defq so_task (source-task "apps/system/netmon/child.lisp"))
(assert-true "a task to send is Lisp to run, not a path" (starts-with "(" so_task))
(defun so-child (task)
	(defq id (open-child task +kn_call_pin) mbox (mail-mbox))
	(mail-send id mbox)
	(defq msg (mail-read mbox))
	(mail-send id "")
	(getf msg +reply_task_count))
(assert-true "it runs, and replies" (> (so-child so_task) 0))
(assert-true "and from another system, its imports asked of the service"
	(> (so-child (cat "(import {service/source/app.inc})" (ascii-char 10)
		"(source-import {0} {" (hex-encode so_svc) "})" (ascii-char 10)
		"(import {apps/system/netmon/child.lisp})")) 0))

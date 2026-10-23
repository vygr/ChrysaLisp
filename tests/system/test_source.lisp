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

;a native the node has not built, of another system: its VP asked of the
;service, translated here into the task's server folder, bound by the
;task's own function server, which goes when the task does. The VP is
;NetSpeed's vops under names of this run's own
(defq so_pid (str (pii-pid)) so_mode (source-mode) so_sid (cat "T" so_pid)
	so_dir (cat "obj/vp/" so_mode "/tests/source_" so_pid "/")
	so_name (cat "tests/source_" so_pid "/vops") so_bad (cat "tests/source_" so_pid "/bad")
	so_folder (cat (load-path) "source/" so_sid "/"))
(defun so-renamed (name &optional from to)
	;the VP of vops under another name, and with a link renamed
	(defq entry (first (read (file-stream (cat "obj/vp/" so_mode "/apps/system/netspeed/lisp.vp"))))
		old (first entry)
		code (map (lambda (inst)
				(if (list? inst) (map (# (cond ((eql %0 old) name) ((eql %0 from) to) (%0))) inst) inst))
			(second entry)))
	(str (cat (list name code) (slice entry 2 -1))))
(save (so-renamed so_name) (cat so_dir "lisp.vp"))
(save (so-renamed so_bad "sys/pii/time" (cat "tests/source_" so_pid "/none")) (cat so_dir "bad.vp"))
(defun so-native (fn)
	;what a task of the other system has of its servers, and of the native
	(defq mbox (mail-mbox))
	(open-child (cat "(import {service/source/app.inc})" (ascii-char 10)
		"(source-import {" so_sid "} {" (hex-encode so_svc) "})" (ascii-char 10)
		"(mail-send (hex-decode {" (hex-encode mbox) "}) (str (list (task-server)"
		" (catch (progn (ffi {" fn "} f) (length (f))) :failed))))") +kn_call_pin)
	(mail-read mbox))
(defq so_server (list (cat "source/" so_sid "/")))
(assert-eq "a native this node has not built, asked, translated, bound by the task's server, and run"
	(str (list so_server 3)) (so-native so_name))
(assert-true "it is in the server's folder" (/= (age (cat so_folder so_name)) 0))
(assert-eq "not where the node's own are" 0 (age (cat (load-path) so_name)))
;a release build does not check a bind, the bad one would be called
(if *test_checked*
	(assert-eq "one that links to a function there is not fails its bind, and the task goes on"
		(str (list so_server :failed)) (so-native so_bad))
	(test-skip "a native that links to nothing" "needs an error checked build"))
(until (empty? (task-server)) (task-sleep 10000))
(assert-list-eq "the server goes when its tasks have" '() (task-server))
(each (const pii-remove) (list (cat so_dir "lisp.vp") (cat so_dir "bad.vp") (cat so_folder so_name) (cat so_folder so_bad)))

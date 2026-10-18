(report-header "Apps on a desktop: each, opened and closed, leaves its node as it was")

;A release test, tests -a. A desktop of its own is started, hidden and
;silent, and each app is opened on its node and closed as a user would,
;twice. What the node holds is read after each, with what a task's end
;lets go of let go of first, (kernel-stats :t). The first time may leave
;what is kept once; after it each time is to leave no more than the one
;before. One that leaves more each time, the second and the third, leaks
;each time it is used: a view that holds its own frame, a buffer never
;given back. Once only is a cache, or a moment the node was busy. What
;tests -e finds of a module this finds of an app on a desktop

(import "usr/env.inc")
(import "lib/files/files.inc")
(import "service/gui/lisp.inc")

;not those that are the desktop's own, nor those that end it or log in.
;Not Raymarch: it makes its whole film, seconds of the GPU or every node.
;Not Benchmark: its work is a build of the whole system, spread over the
;nodes, whose passing allocations move the node by tens of KB either way;
;four builds in one task hold no more than one
(defq al_skip (cat (list "system/login" "system/logout" "system/launcher" "system/wallpaper" "demos/raymarch" "tools/benchmark")
		(get :launcher_auto_apps *env*))
	al_apps (sort (filter (lambda (app) (not (find app al_skip)))
		(map (# (slice %0 5 -10)) (filter (# (= (length (split %0 "/")) 4)) (files-all "apps" '("app.lisp"))))))
	al_before (lisp-nodes) al_pid (first (node-spawn 1 :gui_hidden "service/gui/app.lisp"))
	al_t0 (pii-time) al_gui :nil al_node :nil)

(defun al-ask (form)
	; (al-ask form) -> :nil | str
	;form run on the desktop's node, what it gives back as text
	(defq reply (mail-mbox))
	(open-task (str `(mail-send (hex-decode ,(hex-encode reply)) (str ,form)))
		al_node +kn_call_pin 0 (mail-mbox))
	(mail-read-timeout reply (task-timeout 5)))

(defun al-used ()
	; (al-used) -> num
	;what the desktop's node holds, once what a task's end lets go of is
	(if (defq text (al-ask '(progn (defq s (kernel-stats :t)) (- (elem-get s 1) (elem-get s 2)))))
		(str-as-num text) 0))

(defun al-tasks ()
	; (al-tasks) -> num
	;how many tasks the desktop's node has, the one that asks among them
	(if (defq text (al-ask '(elem-get (kernel-stats) 0))) (str-as-num text) 0))

(defun al-alive (mbox)
	; (al-alive mbox) -> :t | :nil
	(eql (al-ask `(if (mail-validate (hex-decode ,(hex-encode mbox))) "t" "nil")) "t"))

(defun al-windows ()
	; (al-windows) -> num
	(length (ifn (gui-owners-rpc) (list))))

(defun al-run (app desk)
	; (al-run app desk)
	;the app opened on the desktop's node, its window waited for, closed as
	;a user would, and waited for till its task and its window have gone,
	;and the tasks it started there: a child that finishes what it was
	;doing first, a build or a farm's part, still holds what it uses
	(defq tasks (al-tasks)
		mbox (open-remote (cat "apps/" app "/app.lisp") al_node +kn_call_pin) t0 (pii-time))
	(while (and (<= (al-windows) desk) (< (- (pii-time) t0) (task-timeout 10)))
		(task-sleep 50000))
	(task-sleep 300000)
	(mail-send mbox (setf-> (str-alloc +ev_msg_action_size)
		(+ev_msg_type +ev_type_action)
		(+ev_msg_target_id +event_close)
		(+ev_msg_action_source_id 0)))
	(setq t0 (pii-time))
	(while (and (or (al-alive mbox) (> (al-windows) desk) (> (al-tasks) tasks))
			(< (- (pii-time) t0) (task-timeout 60)))
		(task-sleep 100000)))

(while (and (empty? (setq al_gui (mail-enquire "@Gui,"))) (< (- (pii-time) al_t0) (task-timeout 10)))
	(task-sleep 50000))
(cond
	((starts-with "obj/vp64" (load-path))
		;what an app holds is the same however its VP is run, and the
		;emulator takes minutes to open each app three times
		(test-skip "apps on a desktop" "the emulator, leaks are measured by the native hosts"))
	((empty? al_gui)
		;a host with no display, an emulator say, can not make a desktop
		(test-skip "apps on a desktop" "no desktop could be started on this host"))
	(:t (setq al_node (slice (hex-decode (second (split (first al_gui) ","))) +long_size -1))
		;the desktop's own windows first
		(defq t0 (pii-time) was -1 desk (al-windows))
		(while (and (/= was desk) (< (- (pii-time) t0) (task-timeout 10)))
			(task-sleep 500000)
			(setq was desk desk (al-windows)))
		(defq al_grew (list))
		(each (lambda (app)
				(al-run app desk)
				(defq u1 (al-used))
				(al-run app desk)
				(defq u2 (al-used))
				(al-run app desk)
				(defq u3 (al-used) grew (list (- u2 u1) (- u3 u2)))
				(if (every (# (> %0 16384)) grew) (push al_grew (cat app " " (str grew)))))
			al_apps)
		(assert-list-eq "no app leaves its node holding more each time it is opened and closed" '() al_grew)))

;the desktop goes, and the network is as it was
(when al_pid
	(when (defq al_new (some (# (unless (find %0 al_before) %0)) (lisp-nodes)))
		(open-task "(pii-exit)" al_new +kn_call_pin 0 (mail-mbox)))
	(setq al_t0 (pii-time))
	(while (and (pii-alive al_pid) (< (- (pii-time) al_t0) (task-timeout 5)))
		(task-sleep 10000))
	(if (pii-alive al_pid) (pii-kill al_pid))
	(setq al_t0 (pii-time))
	(while (and (/= (length (lisp-nodes)) (length al_before)) (< (- (pii-time) al_t0) (task-timeout 5)))
		(task-sleep 10000)))

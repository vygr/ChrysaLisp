(report-header "Apps on a desktop: each, opened and closed, leaves its node as it was")

;A release test, tests -a. A desktop of its own is started, hidden and
;silent, and each app is opened on its node and closed as a user would,
;five times. What the node holds is read after each, with what a task's
;end lets go of let go of first, (kernel-stats :t). The first time may
;leave what is kept once. After it, one that leaves more each of the four
;times, by more than a little, leaks each time it is used: a view that
;holds its own frame, a buffer never given back. On a slow machine what an
;app let go of is still being let go of as the node is read, and it reads
;more one time and less the next: that is not a leak, and it goes down as
;often as up. What tests -e finds of a module this finds of an app on a
;desktop

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
	al_t0 (pii-time) al_gui :nil al_node :nil al_probe :nil)

(defun al-probe-start ()
	; (al-probe-start) -> :nil | mbox
	;a task on the desktop's node that stays, and answers what it is asked.
	;A task started to ask each time is a task more on the node while it is
	;read, one that has not yet ended a task more still, 6KB a task, more
	;one time and less the next. u what the node holds, once what a task's
	;end lets go of is, the cells in use: the total less what is free has
	;in it the ends of blocks, and moves by them as a heap takes a block or
	;gives one back; t how many tasks it has; a mailbox, is it there
	(defq reply (mail-mbox))
	(open-task (str `(progn
			(defq in (mail-mbox))
			(mail-send (hex-decode ,(hex-encode reply)) in)
			(while :t
				(defq msg (mail-read in) to (slice msg 0 +net_id_size) ask (slice msg +net_id_size -1))
				(mail-send to (str (cond
					((eql ask "u") (elem-get (kernel-stats :t) 5))
					((eql ask "t") (elem-get (kernel-stats) 0))
					((mail-validate (hex-decode ask)) "t")
					("nil")))))))
		al_node +kn_call_pin 0 (mail-mbox))
	(mail-read-timeout reply (task-timeout 5)))

(defun al-ask (ask)
	; (al-ask ask) -> :nil | str
	;what the probe on the desktop's node says to ask
	(defq reply (mail-mbox))
	(mail-send al_probe (cat reply ask))
	(mail-read-timeout reply (task-timeout 5)))

(defun al-used ()
	; (al-used) -> num
	;what the desktop's node holds, once what a task's end lets go of is
	(if (defq text (al-ask "u")) (str-as-num text) 0))

(defun al-settled ()
	; (al-settled) -> num
	;what the node holds once it has stopped letting go of things: what an
	;app shared with tasks on other nodes, the maps of Planets, is let go
	;of as they end, later when the machine is busy
	(defq was (al-used) now (progn (task-sleep 300000) (al-used)) n 0)
	(while (and (< (- now was) -4096) (< (++ n) 20))
		(setq was now now (progn (task-sleep 300000) (al-used))))
	now)

(defun al-tasks ()
	; (al-tasks) -> num
	;how many tasks the desktop's node has, the one that asks among them
	(if (defq text (al-ask "t")) (str-as-num text) 0))

(defun al-alive (mbox)
	; (al-alive mbox) -> :t | :nil
	(eql (al-ask (hex-encode mbox)) "t"))

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
		(task-sleep 100000))
	;the views of the frame being drawn as it went are let go of with that
	;frame, at the next
	(task-sleep 250000))

(while (and (empty? (setq al_gui (mail-enquire "@Gui,"))) (< (- (pii-time) al_t0) (task-timeout 10)))
	(task-sleep 50000))
(cond
	((starts-with "obj/vp64" (load-path))
		;what an app holds is the same however its VP is run, and the
		;emulator takes minutes to open each app five times
		(test-skip "apps on a desktop" "the emulator, leaks are measured by the native hosts"))
	((empty? al_gui)
		;a host with no display, an emulator say, can not make a desktop
		(test-skip "apps on a desktop" "no desktop could be started on this host"))
	((not (and (setq al_node (slice (hex-decode (second (split (first al_gui) ","))) +long_size -1))
			(setq al_probe (al-probe-start))))
		(test-skip "apps on a desktop" "no probe could be started on the desktop's node"))
	(:t
		;the desktop's own windows first
		(defq t0 (pii-time) was -1 desk (al-windows))
		(while (and (/= was desk) (< (- (pii-time) t0) (task-timeout 10)))
			(task-sleep 500000)
			(setq was desk desk (al-windows)))
		(defq al_grew (list))
		(each (lambda (app)
				(al-run app desk)
				(defq was (al-settled) grew (list))
				(times 4
					(al-run app desk)
					(push grew (- (defq now (al-settled)) was))
					(setq was now))
				(if (every (# (> %0 4096)) grew) (push al_grew (cat app " " (str grew)))))
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

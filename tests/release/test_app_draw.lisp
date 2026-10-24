(report-header "Apps on a desktop: each opens its window, and draws its words")

;A release test, tests -a, Chris: "that draw test is very expensive, so
;really only a release tag test". A desktop of its own is started, hidden
;and silent, and every app is opened on it at once. A window is to come
;for each, its words to be drawn, and no view of it to have a plain
;property found as nothing. What only a desktop shows: the chess game's
;default config broke its board and it had no window, every Text failed to
;draw, the words of a text field in a page of Docs had no ink, and no test
;of the suite could see any of those. Each window's tree is looked at as
;text, as widgets gives it, not a picture. Chris: "Even all the apps
;should not take more than a few seconds"

(import "usr/env.inc")
(import "lib/files/files.inc")
(import "service/gui/lisp.inc")

;not those that are the desktop's own, open from the start, the wallpaper
;that is the desktop and starts them, nor those that end it or log in. Not
;Raymarch: it makes its whole film, seconds of the GPU or every node, and
;its window draws only that
(defq ad_skip (cat (list "system/login" "system/logout" "system/launcher" "system/wallpaper" "demos/raymarch")
		(get :launcher_auto_apps *env*))
	ad_apps (sort (filter (lambda (app) (not (find app ad_skip)))
		(map (# (slice %0 5 -10)) (filter (# (= (length (split %0 "/")) 4)) (files-all "apps" '("app.lisp"))))))
	ad_before (lisp-nodes :t) ad_pid (first (node-spawn 1 :gui_hidden "service/gui/app.lisp"))
	ad_t0 (pii-time) ad_gui :nil)

(defun ad-screen (&optional depth keys)
	; (ad-screen [depth keys]) -> :nil | tree
	;the screen and its windows, a level down if not said
	(and (str?? (defq text (gui-tree-rpc "" :nil :nil (ifn depth 1) (ifn keys (list :type)))))
		(starts-with "(" text)
		(tree-load (string-stream text))))

(defun ad-windows ()
	; (ad-windows) -> num
	(if (defq screen (ad-screen)) (length (ifn (. screen :find :children) (list))) 0))

(defun ad-nodes (tree)
	; (ad-nodes tree) -> (node ...)
	;each view of a tree as widgets gives it, no function here calls itself
	(defq out (list) todo (list tree))
	(while (defq node (pop todo))
		(push out node)
		(each (# (push todo %0)) (ifn (. node :find :children) (list))))
	out)

(defun ad-title (nodes)
	; (ad-title nodes) -> str
	(ifn (some (# (if (eql (str (. %0 :find :type)) ":Title") (. %0 :find :text))) nodes) "?"))

(defun ad-settle ()
	; (ad-settle) -> num
	;the windows, once no more come for half a second
	(defq was -1 now (ad-windows))
	(while (/= was now)
		(task-sleep 500000)
		(setq was now now (ad-windows)))
	now)

(defun ad-alive (mbox)
	; (ad-alive mbox) -> :t | :nil
	;is the task of the mailbox still there, as it is asked on the node of
	;the desktop, where it is
	(defq reply (mail-mbox))
	(open-task (str `(mail-send (hex-decode ,(hex-encode reply))
			(if (mail-validate (hex-decode ,(hex-encode mbox))) "t" "nil")))
		ad_node +kn_call_pin 0 (mail-mbox))
	(eql (mail-read reply) "t"))

(defun ad-close (mboxes)
	;close, as a user would, each that was opened
	(each (# (mail-send %0 (setf-> (str-alloc +ev_msg_action_size)
			(+ev_msg_type +ev_type_action)
			(+ev_msg_target_id +event_close)
			(+ev_msg_action_source_id 0)))) mboxes))

;a host with no display never has one: that ten seconds is how it is told
(while (and (empty? (setq ad_gui (mail-enquire "@Gui,"))) (< (- (pii-time) ad_t0) (task-timeout 10)))
	(task-sleep 50000))
(cond
	((empty? ad_gui)
		;a host with no display, an emulator say, can not make a desktop
		(test-skip "apps on a desktop" "no desktop could be started on this host"))
	(:t ;the desktop's own windows first, then all the apps at once
		(ad-settle)
		(defq ad_desk (ifn (gui-owners-rpc) (list)))
		(defq ad_node (slice (hex-decode (second (split (first ad_gui) ","))) +long_size -1)
			ad_mboxes (map (# (open-remote (cat "apps/" %0 "/app.lisp") ad_node +kn_call_pin)) ad_apps)
			ad_nowin (list) ad_undrawn (list) ad_nothing (list))
		(ad-settle)
		;an app that has no window is one whose task has none, the GUI says
		;who has each, and has gone. One whose window is of a task it started,
		;Files, is still there
		(defq ad_owners (ifn (gui-owners-rpc) (list)))
		(each (lambda (app mbox)
				(unless (or (find (hex-encode mbox) ad_owners) (ad-alive mbox)) (push ad_nowin app)))
			ad_apps ad_mboxes)
		;each window from the front: it can all be seen, so it has been drawn,
		;and closed the next is in front and is drawn where it was. Open all
		;at once they cover each other, and what can not be seen is not drawn
		(defq ad_seen 0 ad_keys (list :type :text :text_cache :paper :ink :font))
		(while (and (defq owners (gui-owners-rpc)) (nempty? owners)
				(nempty? (defq owner (first owners))) (not (find owner ad_desk)) (< ad_seen 100))
			(task-sleep 40000)
			(defq front (some (# (if (nql (ad-title (ad-nodes %0)) "?") %0))
					(ifn (. (ifn (ad-screen 6 (list :type :text)) (Emap)) :find :children) (list)))
				title (if front (ad-title (ad-nodes front)) "?"))
			(when (and (nql title "?") (str?? (defq text (gui-tree-rpc title :nil :nil 100 ad_keys)))
					(starts-with "(" text))
				(defq nodes (ad-nodes (tree-load (string-stream text)))
					texts (filter (# (eql (str (. %0 :find :type)) ":Text")) nodes))
				;words to draw, and none of them drawn. A Text has a :text_cache
				;once drawn, its words are its label's, found up the tree
				(and (nempty? texts) (notany (# (. %0 :find :text_cache)) texts)
					(push ad_undrawn title))
				;a plain property found as nothing
				(if (some (lambda (node) (some (# (and (. node :find %0) (eql (. node :find %0) :nil)))
						'(:paper :ink :font))) nodes)
					(push ad_nothing title)))
			(++ ad_seen)
			(ad-close (list (hex-decode owner)))
			(while (eql (first (ifn (gui-owners-rpc) (list ""))) owner)
				(task-sleep 20000)))
		(ad-close ad_mboxes)
		(assert-true "the windows of the apps were looked at, from the front" (> ad_seen 40))
		(assert-list-eq "every app opens its window" '() ad_nowin)
		(assert-list-eq "every window draws its words" '() ad_undrawn)
		(assert-list-eq "no view of a window has :paper :ink or :font as nothing" '() ad_nothing)
		(assert-true "there were apps to try" (> (length ad_apps) 40))))

;the desktop goes, and the network is as it was
(when ad_pid
	(when (defq ad_new (some (# (unless (find %0 ad_before) %0)) (lisp-nodes :t)))
		(open-task "(pii-exit)" ad_new +kn_call_pin 0 (mail-mbox)))
	(setq ad_t0 (pii-time))
	(while (and (pii-alive ad_pid) (< (- (pii-time) ad_t0) (task-timeout 5)))
		(task-sleep 10000))
	(if (pii-alive ad_pid) (pii-kill ad_pid))
	(while (/= (length (lisp-nodes :t)) (length ad_before))
		(task-sleep 10000)))

;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(long 'close))

(defmacro seq-find (item seq)
	`(cond 
		((str? ,seq) (find ,item ,seq))
		((lst? ,seq) (some (lambda (_e) (if (eql ,item _e) _)) ,seq))))

(defun-bind make-last (item seq)
	(if (defq index (seq-find item seq))
		(cond
			((defq test (= index (dec (length seq))))
				seq)
			((not test)		
				(cat (slice 0 index seq) (slice (inc index) -1 seq) (list item))))))

(defun-bind app-path (_)
	(cat "apps/" _ "/app.lisp"))

(defun-bind refresh-view (view screen)
	(bind '(w h) (view-get-size screen))
	(view-sub view)
	(gui-add-back (view-change view 0 0 w h)))


(defun-bind main ()
;handle auto launcher apps here from now on. Ensure wallpaper is last on list, if present.
(each (lambda (_)
	(open-child (app-path _) kn_call_open)) (make-last "wallpaper" (sort cmp *env_launcher_auto_apps*)))

(refresh-view (defq view (create-view)) (defq screen (penv (gui-add-back view))))
(while (cond
((and (= (get-long (defq msg (mail-read (task-mailbox))) ev_msg_type) ev_type_mouse)
		(= (get-int msg ev_msg_mouse_buttons) 0))
;make a list of screen width height and mouse x y and send it to launcher.
	(let ((params (cat (view-get-size screen) (list (get-int msg ev_msg_mouse_rx) (get-int msg ev_msg_mouse_ry)))))
		(mail-send params (open-child (app-path "launcher") kn_call_open))))
; ((and (< (get-long msg ev_msg_target_id) 0)
; 		(= (get-long msg ev_msg_type) ev_type_gui))
; 	(refresh-view view screen))
(t (view-event view msg)))))

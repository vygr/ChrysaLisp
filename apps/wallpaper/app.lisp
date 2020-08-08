;Copyright for images created by Gary Boyd for the ChrysaLisp OS is transferred
;to the project developer, Chris Hinsley. Attribution for creation of these
;works should be included in distribution of ChrysaLisp OS or derived works in
;which they appear. No warranty is given, and no liability for use or
;distribution is expressed or implied by the works' original copyright holder.

;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(defmacro seq-find (item seq)
	`(cond 
		((str? ,seq) (find ,item ,seq))
		((lst? ,seq) (some (lambda (_e) (if (eql ,item _e) _)) ,seq))))

(defun-bind app-path (_)
	(cat "apps/" _ "/app.lisp"))

(defun-bind refresh-wallpaper ()
	;pick nearest wallpaper to screen size
	(bind '(w h) (view-get-size screen))
	(defq index 0 err max_int flag 0)
	(each (lambda ((iw ih it))
		(defq iw (- w iw) ih (- h ih) new_err (+ (* iw iw) (* ih ih)))
		(when (< new_err err)
			(setq err new_err index _ flag (if (= it 32) 0 view_flag_opaque)))) images_info)
	(view-sub wallpaper)
	(canvas-swap (canvas-resize (setq wallpaper (create-canvas w h 1))
		(canvas-load (elem index *env_wallpaper_images*) load_flag_noswap)))
	(gui-add-back (view-change (view-set-flags wallpaper
		(+ (const (+ view_flag_at_back view_flag_dirty_all)) flag)
		(const (+ view_flag_at_back view_flag_dirty_all view_flag_opaque))) 0 0 w h)))

(defun-bind main ()
	(defq images_info (map canvas-info *env_wallpaper_images*) wallpaper (create-view)
			screen (penv (gui-add-back wallpaper)))
	(each (lambda (_)
		(open-child (app-path _) kn_call_open)) *env_launcher_auto_apps*)
	(refresh-wallpaper)
	(while t
		(when (and (< (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id) 0)
				(= (get-long msg ev_msg_type) ev_type_gui))
			;resized GUI
			(refresh-wallpaper))
		(when (and (= (get-long msg ev_msg_type) ev_type_mouse) (= (get-int msg ev_msg_mouse_buttons) 0))
			;make a list of screen width height and mouse x y and send it to launcher.
			(let ((params (cat (view-get-size screen) (list (get-int msg ev_msg_mouse_rx) (get-int msg ev_msg_mouse_ry)))))
				(mail-send params (open-child (app-path "launcher") kn_call_open)))))
	

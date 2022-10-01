;Copyright for images created by Gary Boyd for the ChrysaLisp OS is transferred
;to the project developer, Chris Hinsley. Attribution for creation of these
;works should be included in distribution of ChrysaLisp OS or derived works in
;which they appear. No warranty is given, and no liability for use or
;distribution is expressed or implied by the works' original copyright holder.

(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(defun app-path (_)
	(cat "apps/" _ "/app.lisp"))

(defun refresh-wallpaper ()
	;pick nearest wallpaper to screen size
	(bind '(w h) (. screen :get_size))
	(defq index 0 err +max_int flag 0)
	(each (lambda ((iw ih it))
		(defq iw (- w iw) ih (- h ih) new_err (+ (* iw iw) (* ih ih)))
		(when (< new_err err)
			(setq err new_err index _ flag (if (= it 32) 0 +view_flag_opaque)))) images_info)
	(. wallpaper :sub)
	(gui-add-back (.-> (setq wallpaper (Canvas w h 1))
		(:resize (Canvas-from-file (elem-get index *env_wallpaper_images*) +load_flag_noswap)) :swap
		(:set_flags (+ (const (+ +view_flag_at_back +view_flag_dirty_all)) flag)
			(const (+ +view_flag_at_back +view_flag_dirty_all +view_flag_opaque)))
		(:change 0 0 w h))))

(defun main ()
	(defq images_info (map pixmap-info *env_wallpaper_images*) wallpaper (View)
			screen (penv (gui-add-back wallpaper)) mouse_state :u)
	(each (lambda (_)
		(open-child (app-path _) +kn_call_open)) *env_launcher_auto_apps*)
	(refresh-wallpaper)
	(while :t
		(cond
			((and (< (getf (defq msg (mail-read (task-mailbox))) +ev_msg_target_id) 0)
					(= (getf msg +ev_msg_type) +ev_type_gui))
				;resized GUI
				(refresh-wallpaper))
			((= (getf msg +ev_msg_type) +ev_type_mouse)
				;mouse event
				(cond
					((/= (getf msg +ev_msg_mouse_buttons) 0)
						;mouse button is down
						(case mouse_state
							(:d ;was down last time
								)
							(:u ;was up last time
								(setq mouse_state :d))))
					(:t  ;mouse button is up
						(case mouse_state
							(:d ;was down last time
								;run launcher
								(open-child (app-path "launcher") +kn_call_open)
								(setq mouse_state :u))
							(:u ;was up last time
								))))))))

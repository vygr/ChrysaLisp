;Copyright for images created by Gary Boyd for the ChrysaLisp OS is transferred
;to the project developer, Chris Hinsley. Attribution for creation of these
;works should be included in distribution of ChrysaLisp OS or derived works in
;which they appear. No warranty is given, and no liability for use or
;distribution is expressed or implied by the works' original copyright holder.

(import "usr/env.inc")
(import "gui/lisp.inc")


(defun app-path (%0)
	(cat "apps/" %0 "/app.lisp"))

(defun refresh-wallpaper ()
	;pick nearest wallpaper to screen size, or with no images a plain one of
	;the user's :wallpaper_paper
	(bind '(w h) (. screen :get_size))
	(defq index 0 err +max_int flag (if (empty? images_info) +view_flag_opaque 0))
	(each (lambda ((iw ih it))
		(defq iw (- w iw) ih (- h ih) new_err (+ (* iw iw) (* ih ih)))
		(when (< new_err err)
			(setq err new_err index (!) flag (if (= it 32) 0 +view_flag_opaque)))) images_info)
	(. wallpaper :sub)
	(setq wallpaper (Canvas w h 1))
	(if (empty? images_info)
		(. wallpaper :fill (get :wallpaper_paper *env*))
		(. wallpaper :resize (canvas-load (elem-get (get :wallpaper_images *env*) index) +load_flag_noswap)))
	(gui-add-back-rpc (.-> wallpaper
		;the wallpaper is only ever shown, so its pixmap, the size of
		;the screen, is freed once it is a texture
		(:swap +swap_flag_free)
		(:set_flags (+ (const (+ +view_flag_at_back +view_flag_dirty_all)) flag)
			(const (+ +view_flag_at_back +view_flag_dirty_all +view_flag_opaque)))
		(:change 0 0 w h))))

(defun main ()
	(defq images_info (map (const canvas-info) (get :wallpaper_images *env*)) wallpaper (View)
			screen (penv (gui-add-back-rpc wallpaper)) mouse_state :u)
	(each (lambda (%0)
		(open-child (app-path %0) +kn_call_pin)) (get :launcher_auto_apps *env*))
	(refresh-wallpaper)
	(defq id :t)
	(while id
		(setq id (getf (defq msg (mail-read (task-mbox))) +ev_msg_target_id))
		(cond
			((= id +event_close)
				(setq id :nil))
			((= id +event_layout)
				;resized GUI
				(refresh-wallpaper))
			;the mouse, or a finger or pen: the backdrop is in no window, which
			;would make a pointer the mouse of a view that takes none, so here
			;a pointer down and up is a click as the mouse's is
			((defq buttons (cond
					((= (getf msg +ev_msg_type) +ev_type_mouse) (getf msg +ev_msg_mouse_buttons))
					((= (getf msg +ev_msg_type) +ev_type_pointer) (getf msg +ev_msg_pointer_contact))))
				(cond
					((/= buttons 0)
						;mouse button is down
						(case mouse_state
							(:d ;was down last time
								)
							(:u ;was up last time
								(setq mouse_state :d))))
					(:t ;mouse button is up
						(case mouse_state
							(:d ;was down last time
								;run launcher
								(open-child (app-path "system/launcher") +kn_call_pin)
								(setq mouse_state :u))
							(:u ;was up last time
								))))))))

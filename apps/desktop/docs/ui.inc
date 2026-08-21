;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/tools/edit/ui.inc")

;overload Edit app ui.inc
(redefun action-file-leaf-action ()
	;load up the file selected
	(setq *current_file* (. *file_selector* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id))))
	(. *file_selector* :select_node *current_file*)
	(populate-page *current_file*))

(redefun action-close ()
	(setq *running* :nil))

(redefun action-minimise ()
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(redefun action-maximise ()
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y w (* h 2)))
	(. *window* :change_dirty x y w h))

(redefun action-scale-up ()
	;scale font up
	(defq old_size (get :zoom *window*)
		new_size (min 1.5 (+ old_size 0.125)))
	(when (/= old_size new_size)
		(def *window* :zoom new_size)
		(populate-page *current_file*)))

(redefun action-scale-down ()
	;scale font down
	(defq old_size (get :zoom *window*)
		new_size (max 0.75 (- old_size 0.125)))
	(when (/= old_size new_size)
		(def *window* :zoom new_size)
		(populate-page *current_file*)))
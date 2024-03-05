;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(import "././edit/ui.inc")

;overload Edit app ui.inc
(redefun action-file-leaf-action ()
	;load up the file selected
	(setq *current_file* (. *file_tree* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id))))
	(visible-node *file_tree* *current_file*)
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
	(setq *page_scale* (min 1.5 (+ (defq old_size *page_scale*) 0.125)))
	(if (/= old_size *page_scale*) (populate-page *current_file*)))

(redefun action-scale-down ()
	;scale font down
	(setq *page_scale* (max 0.75 (- (defq old_size *page_scale*) 0.125)))
	(if (/= old_size *page_scale*) (populate-page *current_file*)))

;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/tools/edit/ui.inc")
(import "service/clipboard/app.inc")

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

(defun action-link ()
	;a link of the page was pressed. One to a document that is there is
	;gone to. One to a place on the web, a file that is not a document,
	;or a file that is not there, is put on the clipboard, there is
	;nothing here to show it with
	(when (and (defq view (. *window* :find_id (getf *msg* +ev_msg_action_source_id)))
			(defq target (def? :link view)))
		(cond
			((or (find ":" target) (starts-with "#" target))
				(clip-put-rpc target))
			((and (ends-with ".md" (defq file (md-link-file *current_file* target)))
					(/= (age file) 0))
				(setq *current_file* file)
				(if (starts-with "docs/" file) (. *file_selector* :select_node file))
				(populate-page file))
			((clip-put-rpc file)))))
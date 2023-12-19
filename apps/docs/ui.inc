;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(import "././edit/ui.inc")

;overload Edit app ui.inc
(defun action-file-leaf-action ()
	;load up the file selected
	(setq *current_file* (. *file_tree* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id))))
	(select-node *current_file*)
	(populate-page *current_file*))

(defun action-close ()
	(setq *running* :nil))

(defun action-minimise ()
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(defun action-maximise ()
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y w (* h 2)))
	(. *window* :change_dirty x y w h))

(defun action-scale-up ()
	;scale font up
	(setq *page_scale* (min 1.5 (+ (defq old_size *page_scale*) 0.125)))
	(if (/= old_size *page_scale*) (populate-page *current_file*)))

(defun action-scale-down ()
	;scale font down
	(setq *page_scale* (max 0.75 (- (defq old_size *page_scale*) 0.125)))
	(if (/= old_size *page_scale*) (populate-page *current_file*)))

(defun action-whole-words ()
	(setq *whole_words* (not *whole_words*))
	(toolbar-states *find_toolbar* (list *whole_words* *regexp* :nil :nil)))

(defun action-regexp ()
	(setq *regexp* (not *regexp*))
	(toolbar-states *find_toolbar* (list *whole_words* *regexp* :nil :nil)))

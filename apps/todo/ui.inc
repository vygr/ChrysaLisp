;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-minimise ()
	(layout-items *min_width* *min_height*)
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y w h))
	(. *window* :change_dirty x y w h))

(defun action-maximise ()
	(layout-items *max_width* *min_height*)
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y w h))
	(. *window* :change_dirty x y w h))

(defun action-close ()
	(setq *running* :nil))

(defun move-item (flow)
	(defq item (penv (penv (. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))
	(. flow :add_child item)
	(save-config)
	(populate-items)
	(layout-items *min_width* *min_height*))

(defun delete-item ()
	(defq item (penv (penv (. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))
	(. item :sub)
	(save-config)
	(populate-items)
	(layout-items *min_width* *min_height*))

(defun new-item ()
	(create-item *todo_flow*)
	(save-config)
	(. *stack_flow* :show_tab 0)
	(populate-items)
	(layout-items *min_width* *min_height*))

(defun action-todo-done () (move-item *done_flow*))
(defun action-todo-del () (move-item *deleted_flow*))
(defun action-done-redo () (move-item *todo_flow*))
(defun action-done-del () (move-item *deleted_flow*))
(defun action-del-redo () (move-item *todo_flow*))
(defun action-del-del () (delete-item))
(defun action-new-item () (new-item))

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
	(layout-items *max_width* *max_height*)
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y w h))
	(. *window* :change_dirty x y w h))

(defun action-close ()
	(setq *running* :nil))

(defun action-layout ()
	(bind '(sw sh) (. (first (. *todo_scroll* :children)) :get_size))
	(bind '(w h) (. *stack_flow* :get_size))
	(layout-items (- w sw) *min_height*))

(defun move-item (flow)
	(defq item (penv (penv (. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))
	(. flow :add_child item)
	(config-update)
	(populate-items)
	(action-layout))

(defun delete-item ()
	(defq item (penv (penv (. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))
	(. item :sub)
	(config-update)
	(populate-items)
	(action-layout))

(defun new-item ()
	(create-item *todo_flow*)
	(. *stack_flow* :show_tab 0)
	(config-update)
	(populate-items)
	(action-layout))

(defun action-todo-done () (move-item *done_flow*))
(defun action-todo-del () (move-item *deleted_flow*))
(defun action-done-redo () (move-item *todo_flow*))
(defun action-done-del () (move-item *deleted_flow*))
(defun action-del-redo () (move-item *todo_flow*))
(defun action-del-del () (delete-item))
(defun action-new-item () (new-item))

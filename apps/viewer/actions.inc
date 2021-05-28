;module
(env-push)

(defun action-left ()
	(. *current_buffer* :left)
	(refresh))

(defun action-right ()
	(. *current_buffer* :right)
	(refresh))

(defun action-up ()
	(. *current_buffer* :up)
	(refresh))

(defun action-down ()
	(. *current_buffer* :down)
	(refresh))

(defun action-minimise ()
	(vdu-resize +vdu_min_width +vdu_min_height))

(defun action-maximise ()
	(vdu-resize +vdu_max_width +vdu_max_height))

(defun action-close ()
	(setq *running* nil))

(defun action-layout ()
	;user window resize
	(apply window-resize (. *vdu* :max_size)))

(defun action-xscroll ()
	;user xscroll bar
	(setq *scroll_x* (get :value *xslider*))
	(load-display))

(defun action-yscroll ()
	;user yscroll bar
	(setq *scroll_y* (get :value *yslider*))
	(load-display))

(defun action-tree-action ()
	;any tree mutate action
	(.-> (penv (. *window* :find_id (getf *msg* +ev_msg_action_source_id)))
		:layout :dirty_all))

(defun action-file-leaf-action ()
	;load up the file selected
	(select-node (defq file (. *file_tree* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))
	(populate-vdu file))

(defq
event_map (xmap-kv
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise
	+event_layout action-layout
	+event_xscroll action-xscroll
	+event_yscroll action-yscroll
	+event_file_leaf_action action-file-leaf-action
	+event_tree_action action-tree-action)

key_map (xmap-kv
	0x40000050 action-left
	0x4000004f action-right
	0x40000052 action-up
	0x40000051 action-down)

key_map_shift (xmap-kv)

key_map_control (xmap-kv
	(ascii-code "-") action-minimise
	(ascii-code "=") action-maximise))

;module
(export (penv)
	event_map key_map key_map_shift key_map_control)
(env-pop)

(import "././login/env.inc")
(import "gui/lisp.inc")

;our UI widgets and events
(import "./widgets.inc")

;import actions, bindings and app ui classes
(import "./actions.inc")

(defun main ()
	;read parameters from parent
	(bind '(reply_mbox title) (mail-read (task-mbox)))
	(def *window_title* :text title)

	; Initialize state
	(defq *running* :t *selected_node* :nil)

	; Add some sample data to demonstrate the tree
	(.-> *tree_view*
		(:add_route "Root/.")
		(:add_route "Root/Folder1/.")
		(:add_route "Root/Folder1/Item1")
		(:add_route "Root/Folder1/Item2")
		(:add_route "Root/Folder2/.")
		(:add_route "Root/Folder2/SubFolder/.")
		(:add_route "Root/Folder2/SubFolder/Item3")
		(:add_route "Root/Item4")
		:expand)

	; Set minimum size for the tree view
	(def *tree_view* :min_width 400 :min_height 400)

	; Position and show window
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	; Event loop
	(while *running*
		(defq *msg* (mail-read (task-mbox)))
		(cond
			((defq id (getf *msg* +ev_msg_target_id) action (. *event_map* :find id))
				;call bound event action
				(action))
			(:t ;gui event
				(. *window* :event *msg*))))

	; Cleanup
	(gui-sub-rpc *window*))

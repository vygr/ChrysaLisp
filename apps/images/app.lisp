(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/files/files.inc")

;our UI widgets
(import "./widgets.inc")

(enums +select 0
	(enum main tip))

(defq +file_types ''(".cpm" ".tga" ".svg"))

(defun populate-file-trees ()
	;refresh file tree
	(bind '(w h) (. *file_tree* :pref_size))
	(def *file_tree* :min_width w)
	(def *file_tree_scroll* :min_width w)
	(. *file_tree* :change 0 0 w h)
	(.-> *file_tree_scroll* :layout :dirty_all))

(defun visible-node (tree file)
	;highlight and show the selected file
	(when (defq node (. tree :find_node file))
		(. tree :select file)
		(. (penv tree) :visible node)))

(defun select-node (file)
	;highlight and show the selected file in both tree views
	(visible-node *file_tree* file))

(defun win-refresh (file)
	(bind '(w h) (. (defq canvas (canvas-load file 0)) :pref_size))
	(def *image_scroll* :min_width w :min_height h)
	(def *window_title* :text (cat "Images -> " (slice file (rfind "/" file) -1)))
	(. *image_scroll* :add_child canvas)
	(. *window_title* :layout)
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(def *image_scroll* :min_width 128 :min_height 128)
	(select-node file)
	(. *window* :change_dirty x y w h))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t)
	(. *file_tree* :populate "." +file_types 2)
	(populate-file-trees)
	(bind '(x y w h) (apply view-locate (. (win-refresh "apps/images/data/tiger.svg") :get_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip event
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((defq id (getf *msg* +ev_msg_target_id) action (. *event_map* :find id))
				;call bound event action
				(dispatch-action action))
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key_down)
					(> (getf *msg* +ev_msg_key_scode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key)
					mod (getf *msg* +ev_msg_key_mod))
				(cond
					((bits? mod +ev_key_mod_control +ev_key_mod_alt +ev_key_mod_meta)
						;call bound control/command key action
						(when (defq action (. *key_map_control* :find key))
							(dispatch-action action)))
					((bits? mod +ev_key_mod_shift)
						;call bound shift key action, else insert
						(cond
							((defq action (. *key_map_shift* :find key))
								(dispatch-action action))
							((<= +char_space key +char_tilde)
								;insert char etc ...
								(char key))))
					((defq action (. *key_map* :find key))
						;call bound key action
						(dispatch-action action))
					((<= +char_space key +char_tilde)
						;insert char etc ...
						(char key))))
			(:t ;gui event
				(. *window* :event *msg*))))
	(gui-sub-rpc *window*))

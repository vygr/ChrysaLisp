;;;;;;;;;;;;;;
; files widget
;;;;;;;;;;;;;;

(import "././backdrop/lisp.inc")
(import "././title/lisp.inc")
(import "././scroll/lisp.inc")
(import "././flow/lisp.inc")
(import "././tree/lisp.inc")

(defclass Files (title event) (Flow)
	; (Files title event) -> files
	(defq my_id (. this :get_id))
	(ui-root root (Flow) (:flow_flags +flow_down_fill :color +argb_grey14)
		(ui-title-bar _ title (0xe940 0xe941) my_id)
		(ui-scroll file_tree_scroll +scroll_flag_vertical :nil
			(. (ui-tree file_tree event
				(:min_width 0 :color +argb_white
				:font *env_medium_terminal_font*)) :connect my_id)))
	(ui-root backdrop (Backdrop) (:color +argb_white))
	(. this :add_child root)
	(. this :add_child backdrop)
	(lower :file_tree_scroll :file_tree (:flow_flags +flow_stack_fill))

	(defmethod :layout_tree ()
		; (. files :layout_tree) -> files
		(raise :file_tree :file_tree_scroll)
		(bind '(w h) (. file_tree :pref_size))
		(def file_tree :min_width w)
		(def file_tree_scroll :min_width w)
		(. file_tree :change 0 0 w h :t)
		(.-> file_tree_scroll :layout :dirty_all)
		this)

	(defmethod :populate (&optional root exts n mode)
		; (. files :populate [root exts n mode]) -> files
		(raise :file_tree)
		(. file_tree :populate root exts n mode)
		(. this :layout_tree))

	(defmethod :empty ()
		; (. files :empty) -> files
		(raise :file_tree)
		(. file_tree :empty)
		this)

	(defmethod :highlight (route &optional state)
		; (. files :highlight route [state]) -> files
		(raise :file_tree)
		(. file_tree :highlight route state)
		this)

	(defmethod :add_route (route)
		; (. files :add_route route) -> files
		(raise :file_tree)
		(. file_tree :add_route route)
		this)

	(defmethod :get_route (node)
		; (. files :get_route node) -> route
		(raise :file_tree)
		(. file_tree :get_route node))

	(defmethod :select_node (file)
		; (. files :select_node file) -> files
		(raise :file_tree :file_tree_scroll)
		(when (defq node (. file_tree :find_node file))
			(. file_tree :select file)
			(. file_tree_scroll :visible node))
		this)

	(defmethod :action (event)
		; (. files :action event) -> files
		(raise :file_tree :file_tree_scroll (src_id (getf event +ev_msg_action_source_id)))
		(cond
			((= src_id (. file_tree :get_id))
				;any tree mutate action
				(.-> file_tree_scroll :layout :dirty_all))
			(:t	;expand/collapse
				(defq button (. this :find_id src_id))
				(if (eql button (first (. (penv button) :children)))
					(. file_tree :collapse)
					(. file_tree :expand))))
		this)
	)

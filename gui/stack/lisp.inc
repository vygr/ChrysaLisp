;;;;;;;;;;;;;;
; stack widget
;;;;;;;;;;;;;;

(import "././flow/lisp.inc")
(import "././button/lisp.inc")

(defclass Stack (tabs) (Flow)
	; (Stack tabs) -> stack
	(def (defq stack_flow (Flow))
		:flow_flags +flow_stack_fill)
	(def (defq tab_flow (Flow))
		:flow_flags +flow_right
		:color *env_toolbar_col*)
	(def this
		:flow_flags +flow_down_fill
		:stack_flow stack_flow
		:tab_flow tab_flow
		:stack_children (list))
	(.super this :add_child tab_flow)
	(.super this :add_child stack_flow)
	(setq tabs (map (# (if (num? %0) (num-to-utf8 %0) %0)) tabs))
	(each (# (def (defq tab (Button)) :text %0)
		(. tab_flow :add_child (. tab :connect (. this :get_id)))) tabs)

	(defgetmethod stack_flow)
	(defgetmethod stack_children)

	(defmethod :add_child (child)
		; (. stack :add_child child) -> stack
		(raise :stack_flow :stack_children)
		(. stack_flow :add_back child)
		(push stack_children child)
		this)

	(defmethod :show_tab (tab_index)
		; (. stack :show_tab tab_index) -> stack
		(raise :stack_children)
		(when (< tab_index (length stack_children))
			(. (elem-get stack_children tab_index) :to_front))
		(. this :emit))

	(defmethod :action (event)
		; (. stack :action event) -> stack
		(raise :stack_children :tab_flow)
		(defq tab (. tab_flow :find_id (getf event +ev_msg_action_source_id))
			tab_index (find tab (. tab_flow :children)))
		(. this :show_tab tab_index))
	)

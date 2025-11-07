;;;;;;;;;;;;;;
; stack widget
;;;;;;;;;;;;;;

(import "././flow/lisp.inc")
(import "././button/lisp.inc")

(defclass Stack (tabs) (Flow)
	; (Stack tabs) -> stack
	(def (defq stack_flow (Flow)) :flow_flags +flow_stack_fill)
	(defq radio_bar (Radiobar tabs))
	(def this
		:flow_flags +flow_down_fill
		:stack_flow stack_flow
		:radio_bar radio_bar
		:stack_children (list))
	(.super this :add_child radio_bar)
	(.super this :add_child stack_flow)
	(. radio_bar :connect (. this :get_id))

	(defgetmethod stack_flow)
	(defgetmethod stack_children)
	(defgetmethod radio_bar)

	(defmethod :add_child (child)
		; (. stack :add_child child) -> stack
		(raise :stack_flow :stack_children)
		(. stack_flow :add_back child)
		(push stack_children child)
		this)

	(defmethod :show_tab (tab_index)
		; (. stack :show_tab tab_index) -> stack
		(raise :stack_children :radio_bar)
		(when (< tab_index (length stack_children))
			(. (elem-get stack_children tab_index) :to_front)
			(. radio_bar :set_selected tab_index))
		(. this :emit))

	(defmethod :action (event)
		; (. stack :action event) -> stack
		(raise :radio_bar)
		(. this :show_tab (. radio_bar :get_selected)))
	)

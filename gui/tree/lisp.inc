;;;;;;;;;;;;;
; tree widget
;;;;;;;;;;;;;

(import "gui/button/lisp.inc")
(import "gui/flow/lisp.inc")
(import "gui/backdrop/lisp.inc")

(defclass Tree Flow (event)
	; (Tree event) -> tree
	(def this :flow_flags +flow_down_fill+ :action_event event)

	(defmethod :add_route (this route)
		; (. tree :add_route route) -> tree
		; routes are:
		; "a/b/c/file"
		; "a/b/."
		(defq route (reverse (split route "/")) root this)
		(while (> (length route) 1)
			(cond
				((defq node (some (# (if (eql (def? :text %0) (elem -2 route)) %0)) (. root :children)))
					;existing route
					(setq root (elem -2 (. (elem -2 (. node :children)) :children))))
				(t	;new route, so new node
					(def (defq node (Flow)) :flow_flags +flow_down_fill+ :text (elem -2 route))
					(def (defq tab_flow (Flow)) :flow_flags +flow_right_fill+)
					(def (defq tab (Backdrop)) :min_width 14 :style 1)
					(def (defq tab_child (Flow)) :flow_flags +flow_down_fill+)
					(.-> tab_flow (:add_child tab) (:add_child tab_child))
					(def (defq title_flow (Flow)) :flow_flags +flow_right_fill+)
					(def (defq bt (Button)) :text (num-to-utf8 0xe93b) :min_width 14
						:font *env_small_toolbar_font* :border 1)
					(. bt :connect (. this :get_id))
					(.-> title_flow (:add_child bt) (:add_child (Text)))
					(.-> node (:add_child title_flow) (:add_child tab_flow))
					(. root :add_child node)
					(setq root tab_child)))
			(pop route))
		(unless (eql (defq file (pop route)) ".")
			(def (defq node (Button)) :text file :border 0)
			(. node :connect (inc (get :action_event this)))
			(. root :add_child node))
		this)

	(defmethod :get_route (this node)
		; (. tree :get_route node) -> route
		(defq route (list))
		(until (eql node this)
			(push route (get :text node))
			(setq node (penv node))
			(unless (eql node this) (setq node (penv (penv node)))))
		(join (reverse route) "/"))

	(defmethod :action (this event)
		; (. tree :action event) -> tree
		(when (defq toggle (. this :find_id (get-long event ev_msg_action_source_id)))
			(defq node (elem -2 (. (penv (penv toggle)) :children))
				hiden (logxor (logand (. node :get_flags) +view_flag_hiden+) +view_flag_hiden+))
			(. node :set_flags hiden +view_flag_hiden+)
			(def toggle :text (num-to-utf8 (if (= hiden +view_flag_hiden+) 0xe93d 0xe93b)))
			(. this :emit))
		this)
	)

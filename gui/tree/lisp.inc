;;;;;;;;;;;;;
; tree widget
;;;;;;;;;;;;;

(import "gui/button/lisp.inc")
(import "gui/flow/lisp.inc")
(import "gui/backdrop/lisp.inc")

(defclass Tree ()
	; (Tree) -> tree
	; overide the default 'this' env with a tree component
	; inherit from Flow !
	(def (defq this (Flow)) :flow_flags +flow_down_fill+ :action_event 0)

	(defmethod :add_route (this route)
		; (. tree :add_route route) -> tree
		(defq route (reverse (split route "/")) root this)
		(while (> (length route) 1)
			(cond
				((defq node (some (# (if (eql (def? :text %0) (elem -2 route)) %0)) (. root :children)))
					;existing route
					(setq root (elem -2 (. (elem -2 (. node :children)) :children))))
				(t	;new route, so new node
					(def (defq node (Flow)) :flow_flags +flow_down_fill+ :text (elem -2 route))
					(def (defq tab_flow (Flow)) :flow_flags +flow_right_fill+)
					(def (defq tab (Backdrop)) :min_width 16 :style 1)
					(def (defq tab_child (Flow)) :flow_flags +flow_down_fill+)
					(. tab_flow :add_child tab)
					(. tab_flow :add_child tab_child)
					(def (defq title_flow (Flow)) :flow_flags +flow_right_fill+ :color +argb_grey14+)
					(def (defq bt (Button)) :text (num-to-utf8 0xe93b) :font *env_small_toolbar_font* :border 1)
					(def (defq lb (Label)) :flow_flags (logior +flow_flag_right+ +flow_flag_align_vcenter+) :border 0)
					(. title_flow :add_child bt)
					(. title_flow :add_child lb)
					(. node :add_child title_flow)
					(. node :add_child tab_flow)
					(. root :add_child node)
					(setq root tab_child)))
			(pop route))
		(unless (eql (defq file (pop route)) ".")
			(def (defq node (Button)) :text file :border 0)
			(component-connect node (get :action_event this))
			(. root :add_child node)))

	(defmethod :get_route (this node)
		; (. tree :get_route node) -> route
		(defq route (list))
		(until (eql node this)
			(push route (get :text node))
			(setq node (penv node))
			(unless (eql node this) (setq node (penv (penv node)))))
		(join (reverse route) "/"))
	)

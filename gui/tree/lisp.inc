;;;;;;;;;;;;;
; tree widget
;;;;;;;;;;;;;

(import "././node/lisp.inc")
(import "././flow/lisp.inc")
(import "././backdrop/lisp.inc")
(import "lib/files/files.inc")

(defclass Tree (event) (Flow)
	; (Tree event) -> tree
	(def this :flow_flags +flow_down :action_event event
		:border 0 :selected_node :nil)

	(defmethod :empty ()
		; (. tree :empty) -> tree
		;empty tree
		(each (# (. %0 :sub)) (. this :children))
		this)

	(defmethod :add_route (route)
		; (. tree :add_route route) -> tree
		; routes are:
		; "a/b/c/file"
		; "a/b/c/."
		(bind '(toggle_width _) (font-glyph-bounds (get :font this) ">"))
		(defq route (split route "/") root this)
		(each! (lambda (branch)
			(cond
				((defq node (some (# (if (eql (def? :text %0) branch) %0)) (. root :children)))
					;existing route
					(setq root (last (. (last (. node :children)) :children))))
				(:t ;new route, so new node
					(def (defq node (Flow)) :flow_flags +flow_down :text branch)
					(def (defq tab_flow (Flow)) :flow_flags +flow_right_fill)
					(def (defq tab (Backdrop)) :min_width toggle_width)
					(def (defq tab_child (Flow)) :flow_flags +flow_down)
					(def (defq title_flow (Flow)) :flow_flags +flow_right_fill)
					(def (defq toggle (Node)) :text "^" :min_width toggle_width)
					(. toggle :connect (. this :get_id))
					(.-> tab_flow (:add_child tab) (:add_child tab_child))
					(.-> title_flow (:add_child toggle) (:add_child
						(. (Node) :connect (get :action_event this))))
					(.-> node (:add_child title_flow) (:add_child tab_flow))
					(. root :add_child node)
					(setq root tab_child)))) (list route) 0 -2)
		(unless (or (eql (defq file (last route)) ".")
					(some (# (if (eql (def? :text %0) file) %0)) (. root :children)))
			(def (defq node (Node)) :text file)
			(. node :connect (inc (get :action_event this)))
			(. root :add_child node))
		this)

	(defmethod :find_node (route)
		; (. tree :find_node route) -> node | :nil
		; routes are:
		; "a/b/c/node"
		(defq route (split route "/") root this)
		(every (lambda (branch)
			(cond
				((defq node (some (# (if (eql (def? :text %0) branch) %0)) (. root :children)))
					;existing route
					(setq root (. node :children))
					(if (nempty? root) (setq root (last (. (last root) :children))))
					node)
				(:t ;new route, so failed
					:nil))) route))

	(defmethod :get_route (node)
		; (. tree :get_route node) -> route
		(defq route (list))
		(until (eql node this)
			(push route (get :text node))
			(setq node (penv node))
			(unless (eql node this) (setq node (penv (penv node)))))
		(join (reverse route) "/"))

	(defmethod :expand ()
		; (. tree :expand) -> tree
		(defq stack (list this))
		(while (defq node (pop stack))
			(each (# (cond
				((and (Node? %0) (find (get :text %0) "^>"))
					(defq flow (last (. (penv (penv %0)) :children)))
					(. flow :set_flags +view_flag_hidden +view_flag_hidden)
					(def %0 :text ">"))
				((push stack %0)))) (. node :children)))
		(bind '(x y) (. this :get_pos))
		(bind '(w h) (. this :pref_size))
		(.-> this (:change x y w h) :emit))

	(defmethod :collapse ()
		; (. tree :collapse) -> tree
		(defq stack (list this))
		(while (defq node (pop stack))
			(each (# (cond
				((and (Node? %0) (find (get :text %0) "^>"))
					(defq flow (last (. (penv (penv %0)) :children)))
					(. flow :set_flags 0 +view_flag_hidden)
					(def %0 :text "^"))
				((push stack %0)))) (. node :children)))
		(bind '(x y) (. this :get_pos))
		(bind '(w h) (. this :pref_size))
		(.-> this (:change x y w h) :emit))

	(defmethod :populate (&optional root exts n mode)
		; (. tree :populate [root exts n mode]) -> tree
		;load up a file tree
		(setd mode 3)
		(defq files (sort (files-all root exts n)))
		(if (/= (logand 1 mode) 0)
			(each (# (. this :add_route %0)) (files-dirs files)))
		(if (/= (logand 2 mode) 0)
			(each (# (. this :add_route %0)) files))
		this)

	(defmethod :select (route)
		; (. tree :select route) -> tree
		;select a tree route
		(raise :selected_node)
		(when selected_node
			(defq state (get :state selected_node))
			(def (. selected_node :dirty) :state
				(logand (lognot +text_state_selected) state)))
		(when (and route (setq selected_node (. this :find_node route)))
			(defq state (get :state selected_node))
			(def (. selected_node :dirty) :state
				(logior +text_state_selected state)))
		(lower :selected_node)
		this)

	(defmethod :highlight (route &optional state)
		; (. tree :highlight route [state]) -> tree
		;highlight/lolight a tree route
		(when (and route (defq node (. this :find_node route)))
			(defq node_state (get :state node))
			(if state
				(setq node_state (logand (lognot +text_state_highlight) node_state))
				(setq node_state (logior +text_state_highlight node_state)))
			(def (. node :dirty) :state node_state))
		this)

	(defmethod :action (event)
		; (. tree :action event) -> tree
		(when (defq toggle (. this :find_id (getf event +ev_msg_action_source_id)))
			(defq node (last (. (penv (penv toggle)) :children))
				hidden (logxor (logand (. node :get_flags) +view_flag_hidden) +view_flag_hidden))
			(. node :set_flags hidden +view_flag_hidden)
			(def toggle :text (if (= hidden +view_flag_hidden) ">" "^"))
			(bind '(x y) (. this :get_pos))
			(bind '(w h) (. this :pref_size))
			(.-> this (:change_dirty x y w h) :emit))
		this)
	)

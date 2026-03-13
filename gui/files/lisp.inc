;;;;;;;;;;;;;;
; files widget
;;;;;;;;;;;;;;

(import "././node/lisp.inc")
(import "././backdrop/lisp.inc")
(import "././title/lisp.inc")
(import "././button/lisp.inc")
(import "././scroll/lisp.inc")
(import "././flow/lisp.inc")
(import "lib/files/files.inc")

(defclass Files (title event) (Flow)
	; (Files title event) -> files
	(defq my_id (. this :get_id))
	(ui-root root (Flow) (:flow_flags +flow_down_fill :color +argb_grey14)
		(ui-title-bar _ title (0xe940 0xe941) my_id)
		(ui-scroll file_tree_scroll +scroll_flag_both :nil
			(ui-flow file_tree (:flow_flags +flow_down :min_width 0 :color +argb_white :font *env_medium_terminal_font*))))
	(ui-root backdrop (Backdrop) (:color +argb_white))
	(. this :add_child root)
	(. this :add_child backdrop)
	(lower :file_tree_scroll :file_tree (:flow_flags +flow_stack_fill :action_event event :selected_node :nil))

	(defmethod :layout_tree ()
		; (. files :layout_tree) -> files
		(raise :file_tree :file_tree_scroll)
		(bind '(w h) (. file_tree :pref_size))
		(def file_tree :min_width w)
		(def file_tree_scroll :min_width (/ (* w 75) 100))
		(. file_tree :change 0 0 w h :t)
		(.-> file_tree_scroll :layout :dirty_all)
		this)

	(defmethod :empty ()
		; (. files :empty) -> files
		(raise :file_tree)
		(each (# (. %0 :sub)) (. file_tree :children))
		this)

	(defmethod :add_route (route)
		; (. files :add_route route) -> files
		; routes are:
		; "a/b/c/file"
		; "a/b/c/."
		(raise :file_tree :action_event)
		(bind '(toggle_width &) (font-glyph-bounds (get :font file_tree) ">"))
		(defq route (split route "/") root file_tree)
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
						(. (Node) :connect action_event)))
					(.-> node (:add_child title_flow) (:add_child tab_flow))
					(. root :add_child node)
					(setq root tab_child)))) (list route) 0 -2)
		(unless (or (eql (defq file (last route)) ".")
					(some (# (if (eql (def? :text %0) file) %0)) (. root :children)))
			(def (defq node (Node)) :text file)
			(. node :connect (inc action_event))
			(. root :add_child node))
		this)

	(defmethod :find_node (route)
		; (. files :find_node route) -> node | :nil
		; routes are:
		; "a/b/c/node"
		(raise :file_tree)
		(defq route (split route "/") root file_tree)
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
		; (. files :get_route node) -> route
		(raise :file_tree)
		(defq route (list))
		(until (eql node file_tree)
			(push route (get :text node))
			(setq node (penv node))
			(unless (eql node file_tree) (setq node (penv (penv node)))))
		(join (reverse route) "/"))

	(defmethod :expand ()
		; (. files :expand) -> files
		(raise :file_tree)
		(defq stack (list file_tree))
		(while (defq node (pop stack))
			(each (# (cond
				((and (Node? %0) (find (get :text %0) "^>"))
					(defq flow (last (. (penv (penv %0)) :children)))
					(. flow :set_flags 0 +view_flag_hidden)
					(def %0 :text "^"))
				((push stack %0)))) (. node :children)))
		(. this :layout_tree)
		this)

	(defmethod :collapse ()
		; (. files :collapse) -> files
		(raise :file_tree)
		(defq stack (list file_tree))
		(while (defq node (pop stack))
			(each (# (cond
				((and (Node? %0) (find (get :text %0) "^>"))
					(defq flow (last (. (penv (penv %0)) :children)))
					(. flow :set_flags +view_flag_hidden +view_flag_hidden)
					(def %0 :text ">"))
				((push stack %0)))) (. node :children)))
		(. this :layout_tree)
		this)

	(defmethod :populate (&optional root exts n mode)
		; (. files :populate[root exts n mode]) -> files
		;load up a file tree
		(setd mode 3)
		(defq files (sort (files-all root exts n)))
		(if (/= (logand 1 mode) 0)
			(each (# (. this :add_route %0)) (files-dirs files)))
		(if (/= (logand 2 mode) 0)
			(each (# (. this :add_route %0)) files))
		(. this :layout_tree)
		this)

	(defmethod :highlight (route &optional state)
		; (. files :highlight route [state]) -> files
		;highlight/lolight a tree route
		(when (and route (defq node (. this :find_node route)))
			(defq node_state (get :state node))
			(if state
				(setq node_state (logand (lognot +text_state_highlight) node_state))
				(setq node_state (logior +text_state_highlight node_state)))
			(def (. node :dirty) :state node_state))
		this)

	(defmethod :select_node (route)
		; (. files :select_node route) -> files
		(raise :selected_node :file_tree_scroll)
		(when selected_node
			(defq state (get :state selected_node))
			(def (. selected_node :dirty) :state
				(logand (lognot +text_state_selected) state)))
		(when (and route (setq selected_node (. this :find_node route)))
			(defq state (get :state selected_node))
			(def (. selected_node :dirty) :state
				(logior +text_state_selected state))
			(. file_tree_scroll :visible selected_node +scroll_flag_vertical))
		(lower :selected_node)
		this)

	(defmethod :action (event)
		; (. files :action event) -> files
		(defq src_id (getf event +ev_msg_action_source_id)
			view (. this :find_id src_id))
		(cond
			((Node? view)
				; It's a tree toggle node
				(defq node (last (. (penv (penv view)) :children))
					hidden (logxor (logand (. node :get_flags) +view_flag_hidden) +view_flag_hidden))
				(. node :set_flags hidden +view_flag_hidden)
				(def view :text (if (= hidden +view_flag_hidden) ">" "^"))
				(. this :layout_tree))
			((Button? view)
				; It's a title bar button (collapse or expand)
				(if (eql view (first (. (penv view) :children)))
					(. this :expand)
					(. this :collapse))))
		this)
	)
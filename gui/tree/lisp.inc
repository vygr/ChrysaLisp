;;;;;;;;;;;;;
; tree widget
;;;;;;;;;;;;;

(import "././button/lisp.inc")
(import "././flow/lisp.inc")
(import "././backdrop/lisp.inc")
(import "lib/text/files.inc")

(defclass Tree (event) (Flow)
	; (Tree event) -> tree
	(def this :flow_flags +flow_down_fill :action_event event
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
		(each! 0 -2 (lambda (branch)
			(cond
				((defq node (some (# (if (eql (def? :text %0) branch) %0)) (. root :children)))
					;existing route
					(setq root (last (. (last (. node :children)) :children))))
				(:t ;new route, so new node
					(def (defq node (Flow)) :flow_flags +flow_down_fill :text branch)
					(def (defq tab_flow (Flow)) :flow_flags +flow_right_fill)
					(def (defq tab (Backdrop)) :min_width toggle_width)
					(def (defq tab_child (Flow)) :flow_flags +flow_down_fill)
					(def (defq title_flow (Flow)) :flow_flags +flow_right_fill)
					(def (defq toggle (Button)) :text "^" :min_width toggle_width :border 0)
					(. toggle :connect (. this :get_id))
					(.-> tab_flow (:add_child tab) (:add_child tab_child))
					(.-> title_flow (:add_child toggle) (:add_child
						(. (Button) :connect (get :action_event this))))
					(.-> node (:add_child title_flow) (:add_child tab_flow))
					(. root :add_child node)
					(setq root tab_child)))) (list route))
		(unless (or (eql (defq file (last route)) ".")
					(some (# (if (eql (def? :text %0) file) %0)) (. root :children)))
			(def (defq node (Button)) :text file)
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
					(setq root (last (. (last (. node :children)) :children)))
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
				((and (Button? %0) (find (get :text %0) "^>"))
					(defq flow (last (. (penv (penv %0)) :children)))
					(. flow :set_flags +view_flag_hiden +view_flag_hiden)
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
				((and (Button? %0) (find (get :text %0) "^>"))
					(defq flow (last (. (penv (penv %0)) :children)))
					(. flow :set_flags 0 +view_flag_hiden)
					(def %0 :text "^"))
				((push stack %0)))) (. node :children)))
		(bind '(x y) (. this :get_pos))
		(bind '(w h) (. this :pref_size))
		(.-> this (:change x y w h) :emit))

	(defmethod :populate (&optional root exts n)
		; (. tree :populate &optional root exts n) -> tree
		;load up a file tree
		(defq files (sort cmp (all-files root exts n)))
		(each (# (. this :add_route %0)) (all-dirs files))
		(each (# (. this :add_route %0)) files)
		this)

	(defmethod :select (route)
		; (. tree :select route) -> tree
		;highlight a tree route
		(raise :selected_node)
		(if selected_node (undef (. selected_node :dirty) :color))
		(and route (setq selected_node (. this :find_node route))
			(def (. selected_node :dirty) :color +argb_grey12))
		(lower :selected_node)
		this)

	(defmethod :get_relative (route)
		; (. tree :get_relative route) -> (x y w h)
		;relative position of route
		(defq x 0 y 0 w 0 h 0)
		(when (and route (defq node (. this :find_node route)))
			(bind '(x y w h) (. node :get_bounds))
			(until (eql (defq p (penv node)) this)
				(bind '(x1 y1) (. p :get_pos))
				(setq x (+ x x1) y (+ y y1) node p)))
		(list x y w h))

	(defmethod :action (event)
		; (. tree :action event) -> tree
		(when (defq toggle (. this :find_id (getf event +ev_msg_action_source_id)))
			(defq node (last (. (penv (penv toggle)) :children))
				hiden (logxor (logand (. node :get_flags) +view_flag_hiden) +view_flag_hiden))
			(. node :set_flags hiden +view_flag_hiden)
			(def toggle :text (if (= hiden +view_flag_hiden) ">" "^"))
			(bind '(x y) (. this :get_pos))
			(bind '(w h) (. this :pref_size))
			(.-> this (:change x y w h) :emit))
		this)
	)

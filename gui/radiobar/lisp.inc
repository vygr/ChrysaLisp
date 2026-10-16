;;;;;;;;;;;;;;;;
; radiobar widget
;;;;;;;;;;;;;;;;

(import "././flow/lisp.inc")
(import "././button/lisp.inc")

(defclass Radiobar (symbols &optional mode) (Flow)
	; (Radiobar symbols [mode]) -> radiobar
	(def this :flow_flags +flow_right :mode mode)
	(setq symbols (map (# (if (num? %0) (num-to-utf8 %0) %0)) symbols))
	(each (# (def (defq button (Button)) :text %0)
		(. this :add_child (. button :connect (. this :get_id)))) symbols)

	(defmethod :get_states ()
		; (. radiobar :get_states) -> states
		(map (# (if (def? :paper %0) :t)) (. this :children)))

	(defmethod :set_states (states)
		; (. radiobar :set_states states) -> this
		(defq radio_col (canvas-brighter (get :paper this)))
		(each (# (undef (. %0 :dirty) :paper)
				(if %1 (def %0 :paper radio_col)))
			(. this :children) states)
		this)

	(defmethod :set_selected (index)
		; (. radiobar :set_selected idx) -> this
		(defq radio_col (canvas-brighter (get :paper this)))
		(each (# (undef (. %0 :dirty) :paper)
				(if (= (!) index) (def %0 :paper radio_col)))
			(. this :children))
		this)

	(defmethod :get_selected ()
		; (. radiobar :get_selected) -> :nil | idx
		(some (# (if (def? :paper %0) (!))) (. this :children)))

	(defmethod :action (event)
		; (. radiobar :action event) -> radiobar
		(defq button (. this :find_id (getf event +ev_msg_action_source_id))
			children (. this :children) idx (find button children)
			radio_col (canvas-brighter (get :paper this)))
		(if (get :mode this)
			;toggle mode
			(if (def? :paper (. button :dirty))
				(undef button :paper)
				(def button :paper radio_col))
			;radio mode
			(each (# (undef (. %0 :dirty) :paper)
					(if (= (!) idx) (def %0 :paper radio_col)))
				children))
		(. this :emit))
	)

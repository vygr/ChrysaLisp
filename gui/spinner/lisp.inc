;;;;;;;;;;;;;;;;
; spinner widget
;;;;;;;;;;;;;;;;

(import "gui/button/lisp.inc")
(import "gui/flow/lisp.inc")
(import "gui/grid/lisp.inc")

(defclass Spinner () (Flow)
	; (Spinner) -> spinner
	(def this :flow_flags (+ +flow_flag_right +flow_flag_fillh)
		:value 0 :maximum 0 :minimum 0)
	(def (defq buton_grid (Grid))
		:grid_width 1 :grid_height 2 :border 1 :font *env_tiny_toolbar_font*)
	(def (defq label (Label))
		:text (str (get :value this)) :border *env_textfield_border*
		:color +argb_white :min_width 32)
	(def (defq up_button (Button)) :text (num-to-utf8 0xe946))
	(def (defq down_button (Button)) :text  (num-to-utf8 0xe93b))
	(.-> this (:add_child label) (:add_child buton_grid))
	(.-> buton_grid (:add_child up_button) (:add_child down_button))
	(. up_button :connect (. this :get_id))
	(. down_button :connect (. this :get_id))
	(def this :value_label label :up_id (. up_button :get_id) :down_id (. down_button :get_id))

	(defmethod :layout (this)
		; (. spinner :layout) -> spinner
		(set (get :value_label this) :text (str (get :value this)))
		(.super this :layout))

	(defmethod :action (this event)
		; (. spinner :action event) -> spinner
		(raise :value :value_label :maximum :minimum)
		(defq src_id (getf event +ev_msg_action_source_id))
		(cond
			((= src_id (get :up_id this))
				(setq value (inc value)))
			((= src_id (get :down_id this))
				(setq value (dec value))))
		(setq value (max (min maximum value) minimum))
		(lower :value)
		(set value_label :text (str value))
		(.-> value_label :layout :dirty)
		(. this :emit))
	)

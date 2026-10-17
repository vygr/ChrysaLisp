;;;;;;;;;;;;;;;;
; spinner widget
;;;;;;;;;;;;;;;;

(import "././button/lisp.inc")
(import "././flow/lisp.inc")
(import "././grid/lisp.inc")

(defclass Spinner () (Flow)
	; (Spinner) -> spinner
	(def (defq button_grid (Grid))
		:grid_width 1 :border 1)
	(view-variant button_grid :font :font_tiny_symbol)
	(def (defq label (Label))
		:text (str (get :value this))
		:paper +argb_white :min_width 32)
	(view-variant label :border :border_textfield)
	(def (defq up_button (Button)) :text (num-to-utf8 +sym_up))
	(def (defq down_button (Button)) :text (num-to-utf8 +sym_down))
	(.-> this (:add_child label) (:add_child button_grid))
	(.-> button_grid (:add_child up_button) (:add_child down_button))
	(. up_button :connect (. this :get_id))
	(. down_button :connect (. this :get_id))
	(def this :flow_flags +flow_right
		:value 0 :maximum 0 :minimum 0 :value_label label
		:up_id (. up_button :get_id) :down_id (. down_button :get_id))

	(defmethod :layout ()
		; (. spinner :layout) -> spinner
		(set (get :value_label this) :text (str (get :value this)))
		(.super this :layout))

	(defmethod :action (event)
		; (. spinner :action event) -> spinner
		(raise :value :value_label
			(src_id (getf event +ev_msg_action_source_id)))
		(cond
			((= src_id (get :up_id this)) (++ value))
			((= src_id (get :down_id this)) (-- value)))
		(setq value (max (min (get :maximum this) value) (get :minimum this)))
		(lower :value)
		(set value_label :text (str value))
		(.-> value_label (:constrain :r) :dirty)
		(. this :emit))
	)

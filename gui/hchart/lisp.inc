;;;;;;;;;;;;;;;
; hchart widget
;;;;;;;;;;;;;;;

(import "././label/lisp.inc")
(import "././flow/lisp.inc")
(import "././grid/lisp.inc")
(import "././progress/lisp.inc")

(defclass Hchart (name num_marks) (Flow)
	; (Hchart title num_marks) -> hchart
	(def (defq title (Label)) :text name :color +argb_white)
	(def (defq scale_grid (Grid)) :color +argb_white :grid_width num_marks :font *env_medium_terminal_font*)
	(def (defq bar_grid (Grid)) :grid_width 1)
	(times num_marks
		(def (defq _ (Label)) :text "|" :flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hright))
		(. scale_grid :add_child _))
	(.-> this (:add_child title) (:add_child scale_grid) (:add_child bar_grid))
	(def this :units 1 :scale_grid scale_grid :bar_grid bar_grid
		:flow_flags +flow_down_fill :maximum 1 :value 0 :border 0)

	(defgetmethod bar_grid)

	(defmethod :add_bar ()
		; (. hchart :add_bar) -> progress
		(. (get :bar_grid this) :add_child (defq bar (Progress)))
		bar)

	(defmethod :layout_bars ()
		; (. hchart :layout_bars) -> hchart
		(. (get :bar_grid this) :layout)
		this)

	(defmethod :update_scale ()
		; (. hchart :update_scale) -> hchart
		(raise :maximum :units)
		(defq scale (.-> (get :scale_grid this) :dirty_all :children))
		(each (lambda (mark)
			(defq val (/ (* maximum (inc (!))) (length scale)))
			(def mark :text (str (/ val units) "|"))
			(. mark :constrain :t)) scale)
		this)
	)

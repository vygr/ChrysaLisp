;;;;;;;;;;;;;;;
; hchart widget
;;;;;;;;;;;;;;;

(import "gui/button/lisp.inc")
(import "gui/flow/lisp.inc")
(import "gui/grid/lisp.inc")
(import "gui/progress/lisp.inc")

(defclass Hchart (name num_marks units) (Flow)
	; (Hchart title num_marks units) -> hchart
	(def (defq title (Label)) :text name :color +argb_white)
	(def (defq scale_grid (Grid)) :color +argb_white :grid_width num_marks :font *env_medium_terminal_font*)
	(def (defq bar_grid (Grid)) :grid_width 1)
	(times num_marks
		(def (defq _ (Label)) :text "|" :flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hright))
		(. scale_grid :add_child _))
	(.-> this (:add_child title) (:add_child scale_grid) (:add_child bar_grid))
	(def this :units units :scale_grid scale_grid :bar_grid bar_grid
		:flow_flags +flow_down_fill :maximum 1 :value 0 :border 0)

	(defmethod :add_bar (this)
		; (. hchart :add_bar) -> bar
		(. (get :bar_grid this) :add_child (defq bar (Progress)))
		bar)

	(defmethod :get_bar_grid (this)
		; (. hchart :get_bar_grid) -> bar_grid
		(get :bar_grid this))

	(defmethod :update_scale (this)
		; (. hchart :update_scale) -> this
		(raise :maximum :units)
		(defq scale (.-> (get :scale_grid this) :dirty_all :children))
		(each (lambda (mark)
			(defq val (/ (* maximum (inc _)) (length scale)))
			(def mark :text (str (/ val units) "|"))
			(. mark :layout)) scale)
		this)
	)

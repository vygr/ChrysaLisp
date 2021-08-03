(import "lib/debug/frames.inc")
;(import "lib/debug/profile.inc")

(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(import "./math.inc")

(enums +event 0
	(enum close max min)
	(enum reset))

(enums +select 0
	(enum main tip timer))

(defq timer_rate (/ 1000000 30) canvas_width 600 canvas_height 600 canvas_scale 1
	rotx (f2r 0.0) roty (f2r 0.0) rotz (f2r 0.0) verts '())

(ui-window *window* ()
	(ui-title-bar *title* "Cubes" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar main_toolbar ()
			(ui-buttons (0xe938) +event_reset))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-canvas main_widget canvas_width canvas_height canvas_scale))

(defun tooltips ()
	(def *window* :tip_mbox (elem +select_tip select))
	(each (# (def %0 :tip_text %1)) (. main_toolbar :children)
		'("reset")))

(defun circle (r)
	;cached circle generation, quantised to 1/4 pixel
	(defq r (* (floor (* r 4.0)) 0.25) i (% (logior r) 13)
		k (elem i '(()()()()()()()()()()()()())) p (elem i '(()()()()()()()()()()()()())))
	(cond ((defq i (some (lambda (i) (if (= i r) _)) k)) (elem i p))
		(t (push k r) (elem -2 (push p (list
			(path-gen-arc 0.0 0.0 0.0 +fp_2pi r 0.25 (path))))))))

(defun fpoly (canvas col x y _)
	;draw a polygon on a canvas
	(.-> canvas (:set_color col) (:fpoly x y +winding_odd_even _)))

(defun lighting (r g b z)
	;very basic attenuation
	(defq at (r2f (recip z)) r (* r at) g (* g at) b (* b at))
	(+ 0xff000000
		(<< (f2i (* r (const (i2f 0xff)))) 16)
		(<< (f2i (* g (const (i2f 0xff)))) 8)
			(f2i (* b (const (i2f 0xff))))))

(defun render-verts (canvas verts)
	;render circular verts
	(defq sx (const (* (i2r (* canvas_width canvas_scale)) +real_1/2))
		sy (const (* (i2r (* canvas_height canvas_scale)) +real_1/2)))
	(each (lambda ((x y z w))
		(task-sleep 0)
		(fpoly canvas (lighting 1.0 0.0 0.0 z)
			(r2f (+ sx (* x sx))) (r2f (+ sy (* y sy)))
			(circle (r2f (/ (const (i2r (* 25 canvas_scale))) z))))) verts))

(defun sort-verts (verts)
	(sort (lambda (v1 v2)
		(if (<= (elem +vertex_z v1) (elem +vertex_z v2)) 1 -1)) verts))

(defun render ()
	(defq matrix (matrix-unity)
		mrx (matrix-rotx rotx)
		mry (matrix-roty roty)
		mrz (matrix-rotz rotz)
		mrot (matrix-mul (matrix-mul mrx mry) mrz)
		mtrans (matrix-translate +real_0 +real_0 +real_3)
		mfrust (matrix-frustrum
			(const (f2r -1.1)) (const (f2r 1.1))
			(const (f2r -1.1)) (const (f2r 1.1))
			(f2r 1.0) (f2r 5.0))
		matrix (matrix-mul mfrust (matrix-mul mtrans (matrix-mul matrix mrot)))
		new_verts (vertex-mul matrix verts)
		new_verts (vertex-clip new_verts)
		new_verts (sort-verts new_verts))
	(. main_widget :fill +argb_black)
	(render-verts main_widget new_verts)
	(. main_widget :swap))

(defun reset ()
	(setq verts (vertex-cloud 1000))
	(render))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (print _)(print) t)))

(defun main ()
	(defq select (alloc-select +select_size) *running* t mouse_state :u)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(.-> main_widget (:fill +argb_black) :swap)
	(gui-add-front (. *window* :change x y w h))
	(tooltips)
	(. main_widget :set_canvas_flags +canvas_flag_antialias)
	(reset)
	(mail-timeout (elem +select_timer select) timer_rate 0)
	(while *running*
		(defq *msg* (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_tip)
				;tip event
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem +select_timer select) timer_rate 0)
				(setq
					rotx (% (+ rotx (const (f2r 0.001))) (const (f2r +fp_2pi)))
					roty (% (+ roty (const (f2r 0.02))) (const (f2r +fp_2pi)))
					rotz (% (+ rotz (const (f2r 0.002))) (const (f2r +fp_2pi))))
				(render))
			((defq id (getf *msg* +ev_msg_target_id) action (. event_map :find id))
				;call bound event action
				(dispatch-action action))
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key)
					(> (getf *msg* +ev_msg_key_keycode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key)
					mod (getf *msg* +ev_msg_key_mod))
				(cond
					((/= 0 (logand mod (const
							(+ +ev_key_mod_control +ev_key_mod_option +ev_key_mod_command))))
						;call bound control/command key action
						(when (defq action (. key_map_control :find key))
							(dispatch-action action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action, else insert
						(cond
							((defq action (. key_map_shift :find key))
								(dispatch-action action))
							((<= +char_space key +char_tilda)
								;insert char etc ...
								(char key))))
					((defq action (. key_map :find key))
						;call bound key action
						(dispatch-action action))
					((<= +char_space key +char_tilda)
						;insert char etc ...
						(char key))))
			((and (= id (. main_widget :get_id))
				(= (getf *msg* +ev_msg_type) +ev_type_mouse))
					;mouse event in main widget
					(defq rx (getf *msg* +ev_msg_mouse_rx)
						ry (getf *msg* +ev_msg_mouse_ry))
					(cond
						((/= (getf *msg* +ev_msg_mouse_buttons) 0)
							;mouse button is down
							(case mouse_state
								(:d ;was down last time
									)
								(:u ;was up last time
									(setq mouse_state :d)))
							;use rx, ry ...
							)
						(t  ;mouse button is up
							(case mouse_state
								(:d ;was down last time
									(setq mouse_state :u))
								(:u ;was up last time, so we are hovering
									)))))
			(t  ;gui event
				(. *window* :event *msg*))))
	(gui-sub *window*)
	(free-select select))

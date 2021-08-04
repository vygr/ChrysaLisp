;(import "lib/debug/frames.inc")
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

(defq anti_alias nil timer_rate (/ 1000000 30)
	canvas_width 600 canvas_height 600 canvas_scale (if anti_alias 1 2)
	rotx (f2r 0.0) roty (f2r 0.0) rotz (f2r 0.0) +focal_dist (f2r 4.0)
	+near +focal_dist +far (+ +near +real_4)
	verts '() +canvas_mode (if anti_alias +canvas_flag_antialias 0))

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
	(defq r (r2f r) r (* (floor (* r 4.0)) 0.25) i (% (logior r) 13)
		k (elem i '(()()()()()()()()()()()()())) p (elem i '(()()()()()()()()()()()()())))
	(cond ((defq i (some (lambda (i) (if (= i r) _)) k)) (elem i p))
		(t (push k r) (elem -2 (push p (list
			(path-gen-arc 0.0 0.0 0.0 +fp_2pi r 0.25 (path))))))))

(defun fpoly (canvas col x y _)
	;draw a polygon on a canvas
	(.-> canvas (:set_color col) (:fpoly (r2f x) (r2f y) +winding_odd_even _)))

(defun lighting (r g b s)
	;very basic attenuation
	(defq at (r2f s) r (* r at) g (* g at) b (* b at))
	(+ 0xff000000
		(<< (f2i (* r (const (i2f 0xff)))) 16)
		(<< (f2i (* g (const (i2f 0xff)))) 8)
			(f2i (* b (const (i2f 0xff))))))

(defun render-verts (canvas verts)
	;render circular verts
	(defq sw (const (* +real_1/2 (i2r (dec (* canvas_width canvas_scale)))))
		sh (const (* +real_1/2 (i2r (dec (* canvas_height canvas_scale))))))
	(each (lambda ((x y z w))
		(task-sleep 0)
		(defq w (recip w) x (* x w) y (* y w) z (* z w) s (recip (+ z +real_2)))
		(fpoly canvas (lighting 1.0 0.0 0.0 s)
			(* (+ x +real_1) sw)
			(* (+ y +real_1) sh)
			(circle (* (const (i2r (* 25 canvas_scale))) s)))) verts))

(defun print-verts (vs)
	(each (lambda ((x y z w))
		(print (r2f (/ x w)) " " (r2f (/ y w)) " " (r2f (/ z w)) " " (r2f w))) vs)
	(print))

(defun sort-verts (verts)
	(sort (lambda (v1 v2)
		(if (<= (elem +vertex_z v1) (elem +vertex_z v2)) 1 -1)) verts))

(defun vertex-cloud (num)
	;array of random verts
	(defq out (cap num (list)))
	(while (> (setq num (dec num)) -1)
		(push out (vertex-f
			(- (random 2.0) 1.0)
			(- (random 2.0) 1.0)
			(- (random 2.0) 1.0))))
	out)

(defun vertex-clip (vs)
	;clip verts
	(filter (lambda ((x y z w))
		(defq nw (- +real_0 w))
		(and (< +real_0 w) (<= +near w +far) (<= nw x w) (<= nw y w))) vs))

(defun render ()
	(defq mrx (matrix-rotx rotx) mry (matrix-roty roty) mrz (matrix-rotz rotz)
		mrot (matrix-mul (matrix-mul mrx mry) mrz)
		mtrans (matrix-translate +real_0 +real_0 (const (- +real_0 +focal_dist +real_2)))
		mfrust (matrix-frustum
			(const (f2r -1.0)) (const (f2r 1.0))
			(const (f2r 1.0)) (const (f2r -1.0))
			+near +far)
		verts (sort-verts (vertex-clip (vertex-mul
			(matrix-mul mfrust (matrix-mul mtrans mrot)) verts))))
	(. main_widget :fill +argb_black)
;   (print-verts verts)
	(render-verts main_widget verts)
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
	(.-> main_widget (:set_canvas_flags +canvas_mode) (:fill +argb_black) :swap)
	(gui-add-front (. *window* :change x y w h))
	(tooltips)
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
					rotx (% (+ rotx (const (f2r 0.01))) (const (f2r +fp_2pi)))
					roty (% (+ roty (const (f2r 0.03))) (const (f2r +fp_2pi)))
					rotz (% (+ rotz (const (f2r 0.02))) (const (f2r +fp_2pi))))
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

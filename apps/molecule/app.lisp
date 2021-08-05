;(import "lib/debug/frames.inc")
;(import "lib/debug/profile.inc")

(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/math/matrix.inc")

(enums +event 0
	(enum close max min)
	(enum prev next)
	(enum xrot yrot zrot))

(enums +select 0
	(enum main tip timer))

(enums +ball 0
	(enum vertex radius col))

(enums +col 0
	(enum red green blue))

(defq anti_alias nil timer_rate (/ 1000000 30)
	canvas_width 600 canvas_height 600 canvas_scale (if anti_alias 1 2)
	*rotx* (f2r 0.0) *roty* (f2r 0.0) *rotz* (f2r 0.0) +focal_dist (f2r 4.0)
	+near +focal_dist +far (+ +near +real_4) balls (list)
	+canvas_mode (if anti_alias +canvas_flag_antialias 0)
	palette (map (lambda (_) (fixeds
			(i2f (/ (logand (>> _ 16) 0xff) 0xff))
			(i2f (/ (logand (>> _ 8) 0xff) 0xff))
			(i2f (/ (logand _ 0xff) 0xff))))
		(list +argb_white +argb_cyan +argb_yellow +argb_magenta
			+argb_red +argb_green +argb_blue)))

(ui-window *window* ()
	(ui-title-bar *title* "Molecule" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar main_toolbar ()
			(ui-buttons (0xe91d 0xe91e) +event_prev))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-grid _ (:grid_width 1 :grid_height 3)
			(ui-label _ (:text "X rot:"))
			(ui-label _ (:text "Y rot:"))
			(ui-label _ (:text "Z rot:")))
		(ui-grid _ (:grid_width 1 :grid_height 3)
			(. (ui-slider xrot_slider (:value 0 :maximum 1000 :portion 10 :color +argb_green))
				:connect +event_xrot)
			(. (ui-slider yrot_slider (:value 0 :maximum 1000 :portion 10 :color +argb_green))
				:connect +event_yrot)
			(. (ui-slider zrot_slider (:value 0 :maximum 1000 :portion 10 :color +argb_green))
				:connect +event_zrot)))
	(ui-canvas main_widget canvas_width canvas_height canvas_scale))

(defun tooltips ()
	(def *window* :tip_mbox (elem +select_tip select))
	(each (# (def %0 :tip_text %1)) (. main_toolbar :children)
		'("prev" "next")))

(defun circle (r)
	;cached circle generation, quantised to 1/4 pixel
	(defq r (* (floor (* (r2f r) 4.0)) 0.25) i (% (logior r) 13)
		k (elem i '(()()()()()()()()()()()()())) p (elem i '(()()()()()()()()()()()()())))
	(cond ((defq i (some (lambda (i) (if (= i r) _)) k)) (elem i p))
		(t (push k r) (elem -2 (push p (list
			(path-gen-arc 0.0 0.0 0.0 +fp_2pi r 0.25 (path))))))))

(defun fpoly (canvas col x y _)
	;draw a polygon on a canvas
	(.-> canvas (:set_color col) (:fpoly (r2f x) (r2f y) +winding_odd_even _)))

(defun lighting (col s)
	;very basic attenuation
	(defq at (r2f s)
		r (* (elem +col_red col) at)
		g (* (elem +col_green col) at)
		b (* (elem +col_blue col) at))
	(+ 0xff000000
		(<< (f2i (* r (const (i2f 0xff)))) 16)
		(<< (f2i (* g (const (i2f 0xff)))) 8)
			(f2i (* b (const (i2f 0xff))))))

(defun render-balls (canvas balls)
	(defq sw (const (* +real_1/2 (i2r (dec (* canvas_width canvas_scale)))))
		sh (const (* +real_1/2 (i2r (dec (* canvas_height canvas_scale))))))
	(each (lambda (((x y z w) r c))
		(task-sleep 0)
		(defq w (recip w) x (* x w) y (* y w) z (* z w)
			s (recip (+ z +real_2))
			r (* r s) r4 (* r +real_1/4) r16 (* r +real_1/16)
			sx (* (+ x +real_1) sw) sy (* (+ y +real_1) sh))
		(fpoly canvas (lighting (vec-scale c 0.75 (const (fixeds 0.0 0.0 0.0))) s)
			sx sy (circle r))
		(fpoly canvas (lighting c s)
			(- sx r16) (- sy r16) (circle (- r r16)))
		(fpoly canvas (lighting (const (fixeds 1.0 1.0 1.0)) s)
			(- sx r4) (- sy r4) (circle r4))) balls))

(defun print-verts (balls)
	(each (lambda (((x y z w) _ _))
		(print (r2f (/ x w)) " " (r2f (/ y w)) " " (r2f (/ z w)) " " (r2f w))) balls)
	(print))

(defun sort-balls (balls)
	(sort (lambda ((v1 _ _) (v2 _ _))
		(if (<= (elem +vertex_z v1) (elem +vertex_z v2)) 1 -1)) balls))

(defun clip-balls (balls)
	(filter (lambda (((x y z w) _ _))
		(defq nw (- +real_0 w))
		(and (<= +near w +far) (<= nw x w) (<= nw y w))) balls))

(defun render ()
	(defq mrx (matrix-rotx *rotx*) mry (matrix-roty *roty*) mrz (matrix-rotz *rotz*)
		mrot (matrix-mul (matrix-mul mrx mry) mrz)
		mtrans (matrix-translate +real_0 +real_0 (const (- +real_0 +focal_dist +real_2)))
		mfrust (matrix-frustum +real_-1 +real_1 +real_1 +real_-1 +near +far)
		matrix (matrix-mul mfrust (matrix-mul mtrans mrot))
		balls (sort-balls (clip-balls (map (lambda ((v r c))
			(list (vertex-mul matrix v) r c)) balls))))
	(. main_widget :fill +argb_black)
	(render-balls main_widget balls)
;   (print-verts balls)
	(. main_widget :swap))

(defun ball-cloud (num)
	(clear balls)
	(while (> (setq num (dec num)) -1)
		(push balls (list
			(vertex-f
				(- (random 2.3) 1.15)
				(- (random 2.3) 1.15)
				(- (random 2.3) 1.15))
			(i2r (+ (* 30 canvas_scale) (random 10)))
			(elem (random (length palette)) palette)))))

(defun reset ()
	(ball-cloud 1000)
	(setq *dirty* t))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (print _)(print) t)))

(defun main ()
	(defq select (alloc-select +select_size) *running* t mouse_state :u *dirty* t)
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
				(when *dirty*
					(setq *dirty* nil)
					(render)))
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

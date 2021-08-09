;(import "lib/debug/frames.inc")
;(import "lib/debug/profile.inc")

(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/math/matrix.inc")

(enums +event 0
	(enum close max min)
	(enum prev next auto)
	(enum xrot yrot zrot)
	(enum layout)
	(enum plain grid axis))

(enums +select 0
	(enum main tip timer))

(enums +ball 0
	(enum vertex radius col))

(enums +col 0
	(enum red green blue))

(defun all-mol-files (p)
	(defq out (list))
	(each! 0 -1 (lambda (f m)
		(and (eql m "8") (ends-with ".sdf" f) (push out (cat p f))))
		(unzip (split (pii-dirlist p) ",") (list (list) (list))))
	(sort cmp out))

(defq anti_alias t timer_rate (/ 1000000 30) +min_size 450 +max_size 800
	canvas_size +min_size canvas_scale (if anti_alias 1 2)
	*rotx* +real_0 *roty* +real_0 *rotz* +real_0 +focal_dist +real_4
	+near +focal_dist +far (+ +near +real_4)
	+top (* +focal_dist +real_1/3) +bottom (* +focal_dist +real_-1/3)
	+left (* +focal_dist +real_1/3) +right (* +focal_dist +real_-1/3)
	+canvas_mode (if anti_alias +canvas_flag_antialias 0)
	*mol_index* 0 *auto_mode* nil *dirty* t
	balls (list) mol_files (all-mol-files "apps/molecule/")
	palette (map (lambda (_) (fixeds
			(i2f (/ (logand (>> _ 16) 0xff) 0xff))
			(i2f (/ (logand (>> _ 8) 0xff) 0xff))
			(i2f (/ (logand _ 0xff) 0xff))))
		(list +argb_black +argb_white +argb_red +argb_green
			+argb_cyan +argb_blue +argb_yellow +argb_magenta)))

(ui-window *window* ()
	(ui-title-bar *title* "Molecule" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar main_toolbar ()
			(ui-buttons (0xe91d 0xe91e 0xea43) +event_prev))
		(ui-tool-bar style_toolbar ()
			(ui-buttons (0xe976 0xe9a3 0xe9f0) +event_plain))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-grid _ (:grid_width 1 :grid_height 3 :font *env_body_font*)
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
	(ui-backdrop main_backdrop (:style :plain :color +argb_black :ink_color +argb_grey8
			:min_width +min_size :min_height +min_size)
		(ui-canvas main_widget canvas_size canvas_size canvas_scale)))

(defun tooltips ()
	(def *window* :tip_mbox (elem +select_tip select))
	(each (# (def %0 :tip_text %1)) (. main_toolbar :children)
		'("prev" "next" "auto"))
	(each (# (def %0 :tip_text %1)) (. style_toolbar :children)
		'("plain" "grid" "axis")))

(defun radio-select (toolbar idx)
	(each (lambda (button)
			(undef (. button :dirty) :color)
			(if (= _ idx) (def button :color *env_radio_col*)))
		(. toolbar :children)) idx)

(defun set-rot (slider angle)
	(set (. slider :dirty) :value
		(r2i (/ (* angle (const (i2r 1000))) +real_2pi))))

(defun get-rot (slider)
	(/ (* (i2r (get :value slider)) +real_2pi) (const (i2r 1000))))

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
	;very basic attenuation and diffuse
	(defq at (r2f s)
		r (* (elem +col_red col) at)
		g (* (elem +col_green col) at)
		b (* (elem +col_blue col) at))
	(+ 0xff000000
		(<< (f2i (min (* (+ r 0.15) 256.0) 255.0)) 16)
		(<< (f2i (min (* (+ g 0.15) 256.0) 255.0)) 8)
			(f2i (min (* (+ b 0.15) 256.0) 255.0))))

(defun render-balls (canvas balls)
	(defq sp (* +real_1/2 (i2r (dec (* canvas_size canvas_scale)))))
	(each (lambda (((x y z w) r c))
		(task-sleep 0)
		(defq w (recip w) x (* x w) y (* y w) z (* z w) s (recip (+ z +real_2))
			r (* r s sp) r4 (* r +real_1/4) r8 (* r +real_1/8) r16 (* r +real_1/16)
			sx (* (+ x +real_1) sp) sy (* (+ y +real_1) sp))
		(fpoly canvas (lighting (vec-scale c 0.5 (const (fixeds 0.0 0.0 0.0))) s)
			sx sy (circle r))
		(fpoly canvas (lighting c s) (- sx r16) (- sy r16) (circle (- r r16)))
		(fpoly canvas +argb_white (- sx r4) (- sy r4) (circle r8))) balls))

(defun print-verts (balls)
	(each (lambda (((x y z w) _ _))
		(print (r2f (/ x w)) " " (r2f (/ y w)) " " (r2f (/ z w)) " " (r2f w))) balls)
	(print))

(defun sort-balls (balls)
	(sort (lambda ((v1 _ _) (v2 _ _))
		(if (<= (elem +vec4_z v1) (elem +vec4_z v2)) 1 -1)) balls))

(defun clip-balls (balls)
	(filter (lambda (((x y z w) _ _))
		(defq nw (- +real_0 w))
;        (and (<= +near w +far) (<= nw x w) (<= nw y w))) balls))
		(and (<= +near w +far))) balls))

(defun render ()
	(defq mrx (mat4x4-rotx *rotx*) mry (mat4x4-roty *roty*) mrz (mat4x4-rotz *rotz*)
		mrot (mat4x4-mul (mat4x4-mul mrx mry) mrz)
		mtrans (mat4x4-translate +real_0 +real_0 (const (- +real_0 +focal_dist +real_2)))
		mfrust (mat4x4-frustum +left +right +top +bottom +near +far)
		matrix (mat4x4-mul mfrust (mat4x4-mul mtrans mrot))
		balls (sort-balls (clip-balls (map (lambda ((v r c))
			(list (vec4-mul matrix v) r c)) balls))))
	(. main_widget :fill 0)
	(render-balls main_widget balls)
;   (print-verts balls)
	(. main_widget :swap))

(defun ball-file (index)
	(when (defq stream (file-stream (elem index mol_files)))
		(set (.-> *title* :layout :dirty) :text
			(cat "Molecule -> " (elem -2 (split (elem index mol_files) "/"))))
		(clear balls)
		(times 3 (read-line stream))
		(defq num_atoms (str-to-num (elem 0 (split (read-line stream) " "))))
		(times num_atoms
			(defq line (split (read-line stream) " "))
			(bind '(x y z) (map
					(# (/ (i2r (str-to-num %0)) (const (i2r 65536))))
				(slice 0 3 line)))
			(bind '(radius col) (case (elem 3 line)
				("C" (list (const (i2r (* 70 canvas_scale))) (elem 0 palette)))
				("H" (list (const (i2r (* 25 canvas_scale))) (elem 1 palette)))
				("O" (list (const (i2r (* 60 canvas_scale))) (elem 2 palette)))
				("N" (list (const (i2r (* 65 canvas_scale))) (elem 3 palette)))
				("F" (list (const (i2r (* 50 canvas_scale))) (elem 4 palette)))
				("S" (list (const (i2r (* 88 canvas_scale))) (elem 6 palette)))
				("Si" (list (const (i2r (* 111 canvas_scale))) (elem 6 palette)))
				("P" (list (const (i2r (* 98 canvas_scale))) (elem 7 palette)))
				(t (list (const (i2r (* 100 canvas_scale))) (const (fixeds 1.0 1.0 0.0))))))
			(push balls (list (vec4-r x y z) radius col)))
		(bind '(center radius) (bounding-sphere balls (# (slice 0 3 (elem +ball_vertex %0)))))
		(defq scale_p (/ (const (f2r 2.0)) radius) scale_r (/ (const (f2r 0.025)) radius))
		(each (lambda (ball)
			(bind '(v r _) ball)
			(pop v) (push (vec-scale (vec-sub v center v) scale_p v) +real_1)
			(elem-set +ball_radius ball (* scale_r r))) balls)))

(defun reset ()
	(ball-file 0)
	(setq *dirty* t))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (print _)(print) t)))

(defun main ()
	(defq select (alloc-select +select_size) *running* t mouse_state :u)
	(bind '(x y w h) (apply view-locate (.-> *window* (:connect +event_layout) :pref_size)))
	(.-> main_widget (:set_canvas_flags +canvas_mode) (:fill +argb_black) :swap)
	(radio-select style_toolbar 0)
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
				(when *auto_mode*
					(setq *rotx* (% (+ *rotx* (f2r 0.01)) +real_2pi)
						*roty* (% (+ *roty* (f2r 0.02)) +real_2pi)
						*rotz* (% (+ *rotz* (f2r 0.03)) +real_2pi)
						*dirty* t)
					(set-rot xrot_slider *rotx*)
					(set-rot yrot_slider *roty*)
					(set-rot zrot_slider *rotz*))
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

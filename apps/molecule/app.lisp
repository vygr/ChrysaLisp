(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/math/matrix.inc")
(import "lib/files/files.inc")

(enums +event 0
	(enum close max min)
	(enum prev next auto)
	(enum xrot yrot zrot)
	(enum layout)
	(enum style))

(enums +select 0
	(enum main tip timer))

(enums +ball 0
	(enum vertex radius col))

(defq anti_alias :t timer_rate (/ 1000000 30) +min_size 450 +max_size 800
	canvas_size +min_size canvas_scale (if anti_alias 1 2)
	*rotx* +real_0 *roty* +real_0 *rotz* +real_0 +focal_dist +real_4
	+near +focal_dist +far (+ +near +real_4)
	+top (* +focal_dist +real_1/3) +bottom (* +focal_dist +real_-1/3)
	+left (* +focal_dist +real_-1/3) +right (* +focal_dist +real_1/3)
	+canvas_mode (if anti_alias +canvas_flag_antialias 0)
	*mol_index* 0 *auto_mode* :nil *dirty* :t
	balls (list) mol_files (sort cmp (files-all "apps/molecule/data/" '(".sdf")))
	+palette (push `(,quote) (map (lambda (_) (Vec3-f
			(n2f (/ (logand (>> _ 16) 0xff) 0xff))
			(n2f (/ (logand (>> _ 8) 0xff) 0xff))
			(n2f (/ (logand _ 0xff) 0xff))))
		(list +argb_black +argb_white +argb_red +argb_green
			+argb_cyan +argb_blue +argb_yellow +argb_magenta))))

(ui-window *window* ()
	(ui-title-bar *title* "" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (0xe91d 0xe91e 0xea43) +event_prev))
		(. (ui-radio-bar *style_toolbar* (0xe976 0xe9a3 0xe9f0)
			(:color *env_toolbar2_col*)) :connect +event_style)
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-grid _ (:grid_width 1 :font *env_body_font*)
			(ui-label _ (:text "X rot:"))
			(ui-label _ (:text "Y rot:"))
			(ui-label _ (:text "Z rot:")))
		(ui-grid _ (:grid_width 1)
			(. (ui-slider *xrot_slider* (:value 0 :maximum 1000 :portion 10 :color +argb_green))
				:connect +event_xrot)
			(. (ui-slider *yrot_slider* (:value 0 :maximum 1000 :portion 10 :color +argb_green))
				:connect +event_yrot)
			(. (ui-slider *zrot_slider* (:value 0 :maximum 1000 :portion 10 :color +argb_green))
				:connect +event_zrot)))
	(ui-backdrop *main_backdrop* (:style :grid :color +argb_black :ink_color +argb_grey8
			:min_width +min_size :min_height +min_size)
		(ui-canvas *main_widget* canvas_size canvas_size canvas_scale)))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *main_toolbar*
		'("prev" "next" "auto"))
	(ui-tool-tips *style_toolbar*
		'("plain" "grid" "axis")))

(defun set-rot (slider angle)
	(set (. slider :dirty) :value
		(n2i (/ (* angle (const (n2r 1000))) +real_2pi))))

(defun get-rot (slider)
	(/ (* (n2r (get :value slider)) +real_2pi) (const (n2r 1000))))

(defun circle (r)
	;cached circle generation, quantised to 1/4 pixel
	(defq r (* (floor (* (n2f r) 4.0)) 0.25))
	(memoize r (list (path-gen-arc 0.0 0.0 0.0 +fp_2pi r (path))) 13))

(defun fpoly (canvas col x y _)
	;draw a polygon on a canvas
	(.-> canvas (:set_color col) (:fpoly (n2f x) (n2f y) +winding_odd_even _)))

(defun lighting (col at)
	;very basic attenuation and diffuse
	(bind '(r g b) (vec-min (vec-add (vec-scale col (* (n2f at) 255.0) +fixeds_tmp3)
		(const (Vec3-f 32.0 32.0 32.0)) +fixeds_tmp3)
		(const (Vec3-f 255.0 255.0 255.0)) +fixeds_tmp3))
	(+ 0xff000000 (<< (n2i r) 16) (<< (n2i g) 8) (n2i b)))

(defun render-balls (canvas balls)
	(defq sp (* +real_1/2 (n2r (dec (* canvas_size canvas_scale)))))
	(each (lambda (((x y z w) r c))
		(defq w (recip w) x (* x w) y (* y w) z (* z w) at (recip (+ z +real_2))
			r (* r sp w) r4 (* r +real_1/4) r8 (* r +real_1/8) r16 (* r +real_1/16)
			sx (* (+ x +real_1) sp) sy (* (+ y +real_1) sp))
		(fpoly canvas (lighting c (* at +real_1/2)) sx sy (circle r))
		(fpoly canvas (lighting c at) (- sx r16) (- sy r16) (circle (- r r16)))
		(fpoly canvas (lighting (const (Vec3-f 1.5 1.5 1.5)) at) (- sx r4) (- sy r4) (circle r8))
		(task-slice)) balls))

(defun sort-balls (balls)
	(sort (lambda ((v1 _ _) (v2 _ _))
		(if (<= (elem-get v1 +vec4_w) (elem-get v2 +vec4_w)) 1 -1)) balls))

(defun clip-balls (balls)
	(filter-array (lambda (((_ _ _ w) _ _)) (<= +near w +far)) balls))

(defun render ()
	(defq mrx (Mat4x4-rotx *rotx*) mry (Mat4x4-roty *roty*) mrz (Mat4x4-rotz *rotz*)
		mrot (mat4x4-mul (mat4x4-mul mrx mry) mrz)
		mtrans (Mat4x4-translate +real_0 +real_0 (const (- +real_0 +focal_dist +real_2)))
		mfrust (Mat4x4-frustum +left +right +top +bottom +near +far)
		matrix (mat4x4-mul mfrust (mat4x4-mul mtrans mrot))
		balls (sort-balls (clip-balls (map (lambda ((v r c))
			(list (mat4x4-vec4-mul matrix v) r c)) balls))))
	(. *main_widget* :fill 0)
	(render-balls *main_widget* balls)
	(. *main_widget* :swap 0))

(defun ball-file (index)
	(when (defq stream (file-stream (defq file (elem-get mol_files index))))
		(def (.-> *title* :layout :dirty) :text
			(cat "Molecule -> " (slice file (inc (find-rev "/" file)) -1)))
		(clear balls)
		(times 3 (read-line stream))
		(defq num_atoms (str-as-num (first (split (read-line stream) +char_class_space))))
		(times num_atoms
			(defq line (split (read-line stream) +char_class_space))
			(bind '(x y z) (map
					(# (/ (n2r (str-as-num %0)) (const (n2r 65536))))
				(slice line 0 3)))
			(bind '(radius col) (case (elem-get line 3)
				("C" (list (const (n2r (* 70 canvas_scale))) (first +palette)))
				("H" (list (const (n2r (* 25 canvas_scale))) (second +palette)))
				("O" (list (const (n2r (* 60 canvas_scale))) (third +palette)))
				("N" (list (const (n2r (* 65 canvas_scale))) (elem-get +palette 3)))
				("F" (list (const (n2r (* 50 canvas_scale))) (elem-get +palette 4)))
				("S" (list (const (n2r (* 88 canvas_scale))) (elem-get +palette 6)))
				("Si" (list (const (n2r (* 111 canvas_scale))) (elem-get +palette 6)))
				("P" (list (const (n2r (* 98 canvas_scale))) (elem-get +palette 7)))
				(:t (list (const (n2r (* 100 canvas_scale))) (const (Vec3-f 1.0 1.0 0.0))))))
			(push balls (list (Vec3-r x y z) radius col)))
		(bind '(center radius) (bounding-sphere balls (# (elem-get %0 +ball_vertex))))
		(defq scale_p (/ (const (n2r 2.0)) radius) scale_r (/ (const (n2r 0.0625)) radius))
		(each (lambda (ball)
			(bind '(v r _) ball)
			(push (vec-scale (vec-sub v center v) scale_p v) +real_1)
			(elem-set ball +ball_radius (* scale_r r))) balls)))

(defun reset ()
	(ball-file 0)
	(setq *dirty* :t))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	(defq select (alloc-select +select_size) *running* :t)
	(bind '(x y w h) (apply view-locate (.-> *window* (:connect +event_layout) :pref_size)))
	(.-> *main_widget* (:set_canvas_flags +canvas_mode) (:fill +argb_black) (:swap 0))
	(. *style_toolbar* :set_selected 1)
	(gui-add-front (. *window* :change x y w h))
	(tooltips)
	(reset)
	(mail-timeout (elem-get select +select_timer) timer_rate 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip event
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem-get select +select_timer) timer_rate 0)
				(when *auto_mode*
					(setq *rotx* (% (+ *rotx* (n2r 0.01)) +real_2pi)
						*roty* (% (+ *roty* (n2r 0.02)) +real_2pi)
						*rotz* (% (+ *rotz* (n2r 0.03)) +real_2pi)
						*dirty* :t)
					(set-rot *xrot_slider* *rotx*)
					(set-rot *yrot_slider* *roty*)
					(set-rot *zrot_slider* *rotz*))
				(when *dirty*
					(setq *dirty* :nil)
					(render)))
			((defq id (getf *msg* +ev_msg_target_id) action (. *event_map* :find id))
				;call bound event action
				(dispatch-action action))
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key_down)
					(> (getf *msg* +ev_msg_key_scode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key)
					mod (getf *msg* +ev_msg_key_mod))
				(cond
					((/= 0 (logand mod (const
							(+ +ev_key_mod_control +ev_key_mod_alt +ev_key_mod_meta))))
						;call bound control/command key action
						(when (defq action (. *key_map_control* :find key))
							(dispatch-action action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action, else insert
						(cond
							((defq action (. *key_map_shift* :find key))
								(dispatch-action action))
							((<= +char_space key +char_tilde)
								;insert char etc ...
								(char key))))
					((defq action (. *key_map* :find key))
						;call bound key action
						(dispatch-action action))
					((<= +char_space key +char_tilde)
						;insert char etc ...
						(char key))))
			(:t ;gui event
				(. *window* :event *msg*))))
	(gui-sub *window*)
	(free-select select)
	(profile-report "Molecule"))

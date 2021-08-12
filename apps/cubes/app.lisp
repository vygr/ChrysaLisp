;(import "lib/debug/frames.inc")
;(import "lib/debug/profile.inc")

(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/math/mesh.inc")

(enums +event 0
	(enum close max min)
	(enum auto)
	(enum xrot yrot zrot)
	(enum layout)
	(enum plain grid axis))

(enums +select 0
	(enum main tip timer))

(defq timer_rate (/ 1000000 30) +min_size 450 +max_size 800
	canvas_size +min_size canvas_scale 2 +radius +real_1
	*rotx* +real_0 *roty* +real_0 *rotz* +real_0
	+focal_dist (* +radius +real_2) +canvas_mode 0
	+near +focal_dist +far (+ +near (* +radius +real_2))
	+top +radius +bottom (* +radius +real_-1)
	+left (* +radius +real_-1) +right +radius
	*mol_index* 0 *auto_mode* nil *dirty* t object nil
	palette (map (lambda (_) (fixeds
			(i2f (/ (logand (>> _ 16) 0xff) 0xff))
			(i2f (/ (logand (>> _ 8) 0xff) 0xff))
			(i2f (/ (logand _ 0xff) 0xff))))
		(list +argb_black +argb_white +argb_red +argb_green
			+argb_cyan +argb_blue +argb_yellow +argb_magenta)))

(ui-window *window* ()
	(ui-title-bar *title* "Cubes" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar main_toolbar ()
			(ui-buttons (0xea43) +event_auto))
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

(defun fpoly-zero (canvas col _)
	;draw a polygon on a canvas
	(.-> canvas (:set_color col) (:fpoly 0.0 0.0 +winding_odd_even _)))

(defun lighting (alpha col at)
	;very basic attenuation and diffuse
	(bind '(r g b) (vec-min (vec-add (vec-scale col (* (r2f at) 255.0) +fixeds_tmp3)
		(const (fixeds 32.0 32.0 32.0)) +fixeds_tmp3)
		(const (fixeds 255.0 255.0 255.0)) +fixeds_tmp3))
	(+ (<< (f2i (* alpha 255.0)) 24) (<< (f2i r) 16) (<< (f2i g) 8) (f2i b)))

(defun lighting-at3 (alpha col at)
	;very basic attenuation and diffuse
	(bind '(r g b) (vec-min (vec-add (vec-scale col (* (r2f at) (const (/ 255.0 3.0))) +fixeds_tmp3)
		(const (fixeds 32.0 32.0 32.0)) +fixeds_tmp3)
		(const (fixeds 255.0 255.0 255.0)) +fixeds_tmp3))
	(+ (<< (f2i (* alpha 255.0)) 24) (<< (f2i r) 16) (<< (f2i g) 8) (f2i b)))

(enums +object 0
	(enum mesh color))

(defun sort-verts (verts)
	(sort (lambda (v1 v2)
		(if (<= (elem +vec4_w v1) (elem +vec4_w v2)) 1 -1)) verts))

(defun sorted-tris (tris prog_verts)
	(sorted (lambda (t1 t2)
		(if (<= (elem +vec4_w (elem (elem +tri_i_v0 t1) prog_verts))
				(elem +vec4_w (elem (elem +tri_i_v0 t2) prog_verts)))
			1 -1)) tris))

(defun render-object-verts (canvas mat4x4_obj mat4x4_proj object)
	(defq sp (* +real_1/2 (i2r (dec (* canvas_size canvas_scale))))
		prog_verts (map (# (mat4x4-vec4-mul mat4x4_proj %0))
			(elem +mesh_verts (elem +object_mesh object))))
	(each (lambda ((x y z w))
			(defq w (recip w) x (* x w) y (* y w) z (* z w) at (recip (+ z +real_2))
				r (* (const (f2r 0.0125)) sp w) sx (* (+ x +real_1) sp) sy (* (+ y +real_1) sp))
			(fpoly canvas (lighting 1.0 (const (fixeds 1.0 1.0 1.0)) at) sx sy (circle r)))
		(sort-verts prog_verts)))

(defun render-object-tris (canvas mat4x4_obj mat4x4_proj object)
	(defq sp (* +real_1/2 (i2r (dec (* canvas_size canvas_scale))))
		obj_verts (map (# (mat4x4-vec4-mul mat4x4_obj %0))
			(elem +mesh_verts (elem +object_mesh object)))
		obj_norms (map (# (mat4x4-vec3-mul mat4x4_obj %0))
			(elem +mesh_norms (elem +object_mesh object)))
		prog_verts (map (# (mat4x4-vec4-mul mat4x4_proj %0))
			(elem +mesh_verts (elem +object_mesh object)))
		screen_verts (cap (length obj_verts) (list))
		ats (cap (length obj_verts) (list)))
	(each (lambda ((x y z w))
			(defq w (recip w) x (* x w) y (* y w) z (* z w) at (recip (+ z +real_2))
				sx (* (+ x +real_1) sp) sy (* (+ y +real_1) sp))
			(push screen_verts (path (r2f sx) (r2f sy)))
			(push ats at))
		prog_verts)
	(each (lambda ((i0 i1 i2 in))
			(defq v0 (elem i0 obj_verts) v1 (elem i1 obj_verts)
				v2 (elem i2 obj_verts) n (elem in obj_norms)
				at (+ (elem i0 ats) (elem i1 ats) (elem i2 ats)))
			(when (> (vec-dot n v0) +real_0)
				(fpoly-zero canvas (lighting-at3 0.9 (elem +object_color object) at)
					(list (cat (elem i0 screen_verts) (elem i1 screen_verts) (elem i2 screen_verts))))))
		(sorted-tris (elem +mesh_tris (elem +object_mesh object)) prog_verts)))

(defun render ()
	(defq mat4x4_rot (mat4x4-mul (mat4x4-mul
			(mat4x4-rotx *rotx*) (mat4x4-roty *roty*)) (mat4x4-rotz *rotz*))
		mat4x4_trans (mat4x4-translate +real_0 +real_0 (const (- +real_0 +focal_dist +radius)))
		mat4x4_scale (mat4x4-scale +real_1)
		mat4x4_frust (mat4x4-frustum +left +right +top +bottom +near +far)
		mat4x4_obj (mat4x4-mul mat4x4_trans (mat4x4-mul mat4x4_rot mat4x4_scale))
		mat4x4_proj (mat4x4-mul mat4x4_frust mat4x4_obj))
	(. main_widget :fill 0)
	(render-object-verts main_widget mat4x4_obj mat4x4_proj object)
	(render-object-tris main_widget mat4x4_obj mat4x4_proj object)
	(. main_widget :swap))

(defun reset ()
	(setq object (list (gen-torus (- +radius +real_1/3) +real_1/3 15) (fixeds 1.0 0.0 0.0))
		*dirty* t))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (print _)(print) t)))

(defun main ()
	(defq select (alloc-select +select_size) *running* t)
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
			(t  ;gui event
				(. *window* :event *msg*))))
	(gui-sub *window*)
	(free-select select)
	(if (get 'profile-report)
		(profile-report "Cubes")))

(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/math/matrix.inc")
(import "lib/files/files.inc")
(import "lib/gpu/vp.inc")

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
	*rotx* +real_0 *roty* +real_0 *rotz* +real_0 +focal_dist +real_4
	+near +focal_dist +far (+ +near +real_4)
	+top (* +focal_dist +real_1/3) +bottom (* +focal_dist +real_-1/3)
	+left (* +focal_dist +real_-1/3) +right (* +focal_dist +real_1/3)
	*verts* (reals) *radii* (reals) *colors* (list) *num_atoms* 0
	atom_draw_list (list) atom_cache (Fmap 31) canvas_size +min_size
	sdf_files (sort (files-all (cat *app_root* "data") '(".sdf")))
	*mol_index* 0 *auto_mode* :nil *dirty* :t
	+radius_quant (n2r 1.0)
	;an atom is a shader, a polished ball of the atom's color, as native
	;code. An image of it is made for each color and size that is drawn,
	;and kept
	atom_program (shader-load (cat *app_root* "atom.shader"))
	atom_native (shader-vp atom_program)
	;where the atoms are is a shader too, a vertex shader, as native code.
	;An atom goes in as its place and its radius, 5 numbers, and comes out
	;as 9
	place_program (shader-load (cat *app_root* "place.shader"))
	place_native (shader-vp-vertex place_program)
	*atoms* (reals) +placed_size 9
	+palette (push `(,quote) (map (lambda (%0) (Vec3-f
			(/ (n2f (logand (>> %0 16) 0xff)) 255.0)
			(/ (n2f (logand (>> %0 8) 0xff)) 255.0)
			(/ (n2f (logand %0 0xff)) 255.0)))
		;the first was black, and a black ball shows only its highlight, a
		;dark grey shows it is a ball
		(list 0xff505050 +argb_white +argb_red +argb_green
			+argb_cyan +argb_blue +argb_yellow +argb_magenta))))

(defclass Molecule-backdrop () (Backdrop)
	(def this :atom_draw_list (list))
	(defmethod :draw ()
		(.super this :draw)
		(raise :atom_draw_list)
		(each (lambda ((tid col x y tw th))
			(. this :ctx_blit tid col x y tw th)) atom_draw_list)
		this)
	)

(ui-window *window* ()
	(ui-title-bar *title* "" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_prev +sym_next +sym_auto) +event_prev))
		(. (ui-radio-bar *style_toolbar* (+sym_plain +sym_grid +sym_axis)
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
	(ui-element *main_widget* (Molecule-backdrop)
		(:style :grid :color +argb_black :ink_color +argb_grey8
			:min_width +min_size :min_height +min_size)))

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

(defun lighting (at)
	;how much of an atom's image is shown, less of it the further away it
	;is. A grey, the image has the color and the highlight in it
	(defq grey (min 255 (+ 96 (n2i (* (n2f at) 420.0)))))
	(+ 0xff000000 (<< grey 16) (<< grey 8) grey))

(defun get-atom-texture (radius kind)
	; (get-atom-texture radius kind) -> (tid tw th) | (:nil 0 0)
	;the image of an atom this big and of this color, which of the
	;palette. The shader draws it the first time it is asked for, straight
	;onto the pixels of a canvas. It goes in the shared pixmap cache of
	;the node, so every Molecule that is open has the one image of a
	;color and a size.
	(defq size (n2i (+ (* (quant radius +radius_quant) (n2r 2.0)) (n2r 0.5)))
		key (+ (* size 16) kind))
	(cond
		((<= size 0) (list :nil 0 0))
		(:t (unless (defq canvas (. atom_cache :find key))
				(defq name (cat "molecule/ball_" (str kind) "_" (str size)))
				(cond
					((defq pixmap (. *pixmap_cache* :find name))
						(setq canvas (Canvas-pixmap pixmap)))
					(:t (setq canvas (Canvas size size 1))
						(shader-vp-draw atom_native
							(shader-vp-frame atom_program atom_native (list
								(list 'resolution (list size size))
								(list 'color (map (const n2r) (elem-get +palette kind)))))
							(defq pixmap (getf canvas +canvas_pixmap 0)) 0 0 size size size :t)
						(. *pixmap_cache* :insert name pixmap)
						(. canvas :swap +swap_write)))
				(. atom_cache :insert key canvas))
			(texture-metrics (getf canvas +canvas_texture 0)))))

(defun render ()
	;the atoms are placed by the vertex shader, all of them in one call of
	;its native code. For each it gives where the atom is, 4 numbers, then
	;x, y and radius on the widget, then its depth and its light
	(bind '(w h) (. *main_widget* :get_size))
	(defq out (shader-vp-place place_native
			(shader-vp-frame place_program place_native (list
				(list 'spin (mat4x4-mul (mat4x4-mul (Mat4x4-rotx *rotx*) (Mat4x4-roty *roty*))
					(Mat4x4-rotz *rotz*)))
				(list 'move (const (Mat4x4-translate +real_0 +real_0 (- +real_0 +focal_dist +real_2))))
				(list 'lens (const (Mat4x4-frustum +left +right +top +bottom +near +far)))
				(list 'centre (list (>> w 1) (>> h 1)))
				(list 'half (* +real_1/2 (n2r (dec canvas_size))))))
			*atoms*)
		indices (if (> *num_atoms* 0)
					(filter (# (<= +near (elem-get out (+ (* %0 +placed_size) 3)) +far))
							(range 0 (dec *num_atoms*)))
					(list))
		indices (sort indices (# (if (<= (elem-get out (+ (* %0 +placed_size) 3))
										(elem-get out (+ (* %1 +placed_size) 3))) 1 -1)))
		new_draw_list (list))
	(each (lambda (i)
		(bind '(sx sy r z at) (slice out (+ (* i +placed_size) 4) (* (inc i) +placed_size)))
		(when (<= +real_-1 z +real_1)
			(bind '(tid tw th) (get-atom-texture r (elem-get *colors* i)))
			(when tid
				(defq col (lighting (* at +real_1/2))
					blit_x (n2i (- sx (n2r (/ tw 2))))
					blit_y (n2i (- sy (n2r (/ th 2)))))
				(push new_draw_list (list tid col blit_x blit_y tw th)))
			(task-slice))) indices)
	(set *main_widget* :atom_draw_list new_draw_list)
	(. *main_widget* :dirty))

(defun sdf-file (index)
	(when (defq stream (file-stream (defq file (elem-get sdf_files index))))
		(def (.-> *title* :layout :dirty) :text
			(cat "Molecule -> " (slice file (rfind "/" file) -1)))
		(clear *verts* *radii* *colors*)
		(times 3 (read-line stream))
		(setq *num_atoms* (str-as-num (slice (read-line stream) 0 3)))
		(times *num_atoms*
			(defq line (split (read-line stream) +char_class_space))
			(push *verts*
				(/ (n2r (str-as-num (elem-get line 0))) (const (n2r 65536)))
				(/ (n2r (str-as-num (elem-get line 1))) (const (n2r 65536)))
				(/ (n2r (str-as-num (elem-get line 2))) (const (n2r 65536))))
			(case (elem-get line 3)
				("C" (push *radii* (const (n2r 70))) (push *colors* 0))
				("H" (push *radii* (const (n2r 25))) (push *colors* 1))
				("O" (push *radii* (const (n2r 60))) (push *colors* 2))
				("N" (push *radii* (const (n2r 65))) (push *colors* 3))
				("F" (push *radii* (const (n2r 50))) (push *colors* 4))
				("S" (push *radii* (const (n2r 88))) (push *colors* 6))
				("Si" (push *radii* (const (n2r 111))) (push *colors* 6))
				("P" (push *radii* (const (n2r 98))) (push *colors* 7))
				(:t (push *radii* (const (n2r 100))) (push *colors* 6))))
		(bind '(center radius) (vector-bounds-sphere *verts* 3))
		(defq scale_p (/ (const (n2r 2.0)) radius) scale_r (/ (const (n2r 0.0625)) radius)
			new_verts (reals))
		(each (lambda (v)
			(push new_verts (vector-scale (vector-sub v center v) scale_p v) +real_1))
			(partition *verts* 3))
		(setq *verts* new_verts)
		(vector-scale *radii* scale_r *radii*)
		;what the vertex shader is given, each atom and then its radius
		(setq *atoms* (apply (const cat) (cat (list (reals))
			(map (# (cat %0 (reals %1))) (partition *verts* 4) *radii*))))))

(defun reset ()
	(setq *dirty* :t
		*mol_index* (ifn (some (# (if (ends-with "/maltose.sdf" %0) (!))) sdf_files) 0))
	(sdf-file *mol_index*))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t)
	(bind '(x y w h) (apply view-locate (.-> *window* (:connect +event_layout) :pref_size)))
	(. *style_toolbar* :set_selected 1)
	(gui-add-front-rpc (. *window* :change x y w h))
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
			((. *window* :dispatch *msg*))
			((. *window* :event *msg*))))
	(gui-sub-rpc *window*)
	(profile-report "Molecule"))

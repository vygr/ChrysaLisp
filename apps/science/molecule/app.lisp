(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/math/matrix.inc")
(import "lib/files/files.inc")
(import "./app.inc")

(enums +event 0
	(enum close max min)
	(enum prev next auto)
	(enum xrot yrot zrot)
	(enum layout)
	(enum style))

(enums +select 0
	(enum main tip timer task reply retry_timer idle_timer))

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
	+max_workers 8 +init_workers_% 10 +grow_workers_% 10
	+retry_timeout (task-timeout 5) +idle_timeout 5000000 +retry_timer_rate 1000000
	+atom_cache_dir (cat *app_root* "data/cache/") +radius_quant (n2r 1.0)
	+palette (push `(,quote) (map (lambda (%0) (Vec3-f
			(n2f (/ (logand (>> %0 16) 0xff) 0xff))
			(n2f (/ (logand (>> %0 8) 0xff) 0xff))
			(n2f (/ (logand %0 0xff) 0xff))))
		(list +argb_black +argb_white +argb_red +argb_green
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

(defun lighting (col at)
	;very basic attenuation and diffuse
	(bind '(r g b) (vector-min (vector-add (vector-scale col (* (n2f at) 380.0) +fixeds_tmp3)
		(const (Vec3-f 48.0 48.0 48.0)) +fixeds_tmp3)
		(const (Vec3-f 255.0 255.0 255.0)) +fixeds_tmp3))
	(+ 0xff000000 (<< (n2i r) 16) (<< (n2i g) 8) (n2i b)))

(defun get-atom-texture (radius)
	(defq key (n2i (+ (* (quant radius +radius_quant) (n2r 2.0)) (n2r 0.5)))
		canvas :nil file :nil)
	(if (> key 0)
		(progn
			(setq file (cat +atom_cache_dir "atom_" (str key) ".cpm"))
			(unless (setq canvas (. atom_cache :find key))
				(setq canvas (canvas-load file +load_flag_shared +pixmap_mode_greyscale))
				(if canvas (. atom_cache :insert key canvas)))))
	(if canvas
		(cat (texture-metrics (getf canvas +canvas_texture 0)) (list key file))
		(list :nil 0 0 key file)))

(defun render ()
	(defq mrx (Mat4x4-rotx *rotx*) mry (Mat4x4-roty *roty*) mrz (Mat4x4-rotz *rotz*)
		mrot (mat4x4-mul (mat4x4-mul mrx mry) mrz)
		mtrans (Mat4x4-translate +real_0 +real_0 (const (- +real_0 +focal_dist +real_2)))
		mfrust (Mat4x4-frustum +left +right +top +bottom +near +far)
		matrix (mat4x4-mul mfrust (mat4x4-mul mtrans mrot))
		tverts (mat4x4-vec4-mul matrix *verts*)
		indices (if (> *num_atoms* 0)
					(filter (# (<= +near (elem-get tverts (+ (* %0 4) 3)) +far))
							(range 0 (dec *num_atoms*)))
					(list))
		indices (sort indices (# (if (<= (elem-get tverts (+ (* %0 4) 3))
										(elem-get tverts (+ (* %1 4) 3))) 1 -1))))
	(bind '(w h) (. *main_widget* :get_size))
	(defq cx (n2r (>> w 1)) cy (n2r (>> h 1))
		sp (* +real_1/2 (n2r (dec canvas_size))) new_draw_list (list))
	(each (lambda (i)
		(defq vi (* i 4)
			mw (elem-get tverts (+ vi 3))
			rw (recip mw)
			z (* (elem-get tverts (+ vi 2)) rw))
		(when (<= +real_-1 z +real_1)
			(defq x (* (elem-get tverts vi) rw)
				y (* (elem-get tverts (+ vi 1)) rw)
				at (recip (+ z +real_2))
				r (* (elem-get *radii* i) sp rw)
				sx (+ cx (* x sp)) sy (+ cy (* y sp))
				c (elem-get *colors* i))
			(bind '(tid tw th key file) (get-atom-texture r))
			(if tid
				(progn
					(defq col (lighting c (* at +real_1/2))
						blit_x (n2i (- sx (n2r (/ tw 2))))
						blit_y (n2i (- sy (n2r (/ th 2)))))
					(push new_draw_list (list tid col blit_x blit_y tw th)))
				(when (and key file (not (find key atoms_asked)) (not (find key atoms_new)))
					(push atoms_new key)))
			(task-slice))) indices)
	(set *main_widget* :atom_draw_list new_draw_list)
	(. *main_widget* :dirty)
	(when (nempty? atoms_new)
		;the images not yet in the cache are made by a herd of children,
		;started when there is one to make
		(unless jobs
			(setq jobs (Jobs (cat *app_root* "child.lisp") (elem-get select +select_task)
				(elem-get select +select_reply) (list +max_workers
					(/ (* +max_workers +init_workers_%) 100)
					(/ (* +max_workers +grow_workers_%) 100))))
			(mail-timeout (elem-get select +select_retry_timer) +retry_timer_rate 0))
		(mail-timeout (elem-get select +select_idle_timer) +idle_timeout 0)
		(. jobs :add (map (# (setf-> (cat (str-alloc +atom_size) +atom_cache_dir "atom_" (str %0) ".cpm")
			(+atom_key %0))) atoms_new))
		(setq atoms_asked (cat atoms_asked atoms_new) atoms_new (list))))

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
				("C" (push *radii* (const (n2r 70))) (push *colors* (first +palette)))
				("H" (push *radii* (const (n2r 25))) (push *colors* (second +palette)))
				("O" (push *radii* (const (n2r 60))) (push *colors* (third +palette)))
				("N" (push *radii* (const (n2r 65))) (push *colors* (elem-get +palette 3)))
				("F" (push *radii* (const (n2r 50))) (push *colors* (elem-get +palette 4)))
				("S" (push *radii* (const (n2r 88))) (push *colors* (elem-get +palette 6)))
				("Si" (push *radii* (const (n2r 111))) (push *colors* (elem-get +palette 6)))
				("P" (push *radii* (const (n2r 98))) (push *colors* (elem-get +palette 7)))
				(:t (push *radii* (const (n2r 100))) (push *colors* (const (Vec3-f 1.0 1.0 0.0))))))
		(bind '(center radius) (vector-bounds-sphere *verts* 3))
		(defq scale_p (/ (const (n2r 2.0)) radius) scale_r (/ (const (n2r 0.0625)) radius)
			new_verts (reals))
		(each (lambda (v)
			(push new_verts (vector-scale (vector-sub v center v) scale_p v) +real_1))
			(partition *verts* 3))
		(setq *verts* new_verts)
		(vector-scale *radii* scale_r *radii*)))

(defun reset ()
	(setq *dirty* :t
		*mol_index* (ifn (some (# (if (ends-with "/maltose.sdf" %0) (!))) sdf_files) 0))
	(sdf-file *mol_index*))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t
		jobs :nil atoms_asked (list) atoms_new (list))
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
			((= idx +select_task)
				;child task launch response
				(if jobs (. jobs :launched *msg*)))
			((= idx +select_reply)
				;child response, an atom image is in the cache
				(when (and jobs (defq job (. jobs :job *msg*)))
					(defq atom_key (getf job +atom_key))
					(setq atoms_asked (filter (# (nql %0 atom_key)) atoms_asked) *dirty* :t)
					(. jobs :answered *msg*)))
			((= idx +select_retry_timer)
				;retry timer event
				(mail-timeout (elem-get select +select_retry_timer) +retry_timer_rate 0)
				(when jobs (. jobs :refresh +retry_timeout)))
			((= idx +select_idle_timer)
				;idle timer event
				(when (and jobs (= (. jobs :out) 0))
					(. jobs :close)
					(setq jobs :nil)
					(mail-timeout (elem-get select +select_retry_timer) 0 0)
					(mail-timeout (elem-get select +select_idle_timer) 0 0)
					; drop any stale network replies!
					(elem-set select +select_task (mail-mbox))
					(elem-set select +select_reply (mail-mbox))))
			((. *window* :dispatch *msg*))
			((. *window* :event *msg*))))
	(if jobs (. jobs :close))
	(gui-sub-rpc *window*)
	(profile-report "Molecule"))

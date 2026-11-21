(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/math/matrix.inc")
(import "lib/files/files.inc")
(import "lib/gpu/vp.inc")
(import "./atom.inc")

(enums +event +event_user
	(enum prev next auto)
	(enum xrot yrot zrot)
	(enum style))

(enums +select 0
	(enum main tip task reply timer))

(enums +ball 0
	(enum vertex radius col))

(defq anti_alias :t timer_rate (/ 1000000 30) +min_size 450 +max_size 800
	*rotx* +double_0 *roty* +double_0 *rotz* +double_0 +focal_dist +double_4
	+near +focal_dist +far (+ +near +double_4)
	+top (* +focal_dist +double_1/3) +bottom (* +focal_dist +double_-1/3)
	+left (* +focal_dist +double_-1/3) +right (* +focal_dist +double_1/3)
	*verts* (doubles) *radii* (doubles) *colors* (list) *num_atoms* 0
	atom_draw_list (list) atom_cache (Fmap 31) canvas_size +min_size
	;images asked for and not made yet, made by the farm of ./child.lisp on
	;the nodes of this machine, their pixels sent, the nearest made shown
	;till they come, and the farm while any are out
	atom_waiting (Fmap 31) jobs :nil +patience 10000000
	;the atoms in view by depth, near to far, a bucket for each 1024th of it,
	;kept from frame to frame
	depth_buckets (map! (lambda () (list)) '() 0 1024)
	+bucket_scale (/ (n2d 1023) (- +far +near))
	sdf_files (sort (files-all (cat *app_root* "data") '(".sdf")))
	*mol_index* 0 *auto_mode* :nil *dirty* :t
	;the toolbar as it was when it was last closed, (config-save)
	*config* :nil +config_version 1 +config_file (cat (get :home *env*) "molecule.tre")
	+radius_quant (n2d 1.0)
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
	*atoms* (doubles) +placed_size 9)

(defclass Molecule-backdrop () (Backdrop)
	(def this :atom_draw_list (list))
	(defmethod :draw ()
		(.super this :draw)
		(raise :atom_draw_list)
		(each (lambda ((tid col x y tw th))
				(. this :ctx_blit tid col x y tw th)) atom_draw_list)
		this))

(ui-window *window* ()
	(ui-title-bar *title* "" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_prev +sym_next +sym_auto) +event_prev))
		(. (ui-radio-bar *style_toolbar* (+sym_plain +sym_grid +sym_axis)
				(:paper :paper_toolbar2)) :connect +event_style)
		(ui-backdrop _ (:paper :paper_toolbar)))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-grid _ (:grid_width 1 :font :font_body)
			(ui-label _ (:text "X rot:"))
			(ui-label _ (:text "Y rot:"))
			(ui-label _ (:text "Z rot:")))
		(ui-grid _ (:grid_width 1)
			(. (ui-slider *xrot_slider* (:value 0 :maximum 1000 :portion 10 :paper +argb_green))
				:connect +event_xrot)
			(. (ui-slider *yrot_slider* (:value 0 :maximum 1000 :portion 10 :paper +argb_green))
				:connect +event_yrot)
			(. (ui-slider *zrot_slider* (:value 0 :maximum 1000 :portion 10 :paper +argb_green))
				:connect +event_zrot)))
	(ui-element *main_widget* (Molecule-backdrop)
		(:style :grid :paper +argb_black :ink +argb_grey8
			:min_width +min_size :min_height +min_size)))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *main_toolbar*
		'("prev" "next" "auto"))
	(ui-tool-tips *style_toolbar*
		'("plain" "grid" "axis")))

(defun set-rot (slider angle)
	(set (. slider :dirty) :value
			(n2i (/ (* angle (const (n2d 1000))) +double_2pi))))

(defun get-rot (slider)
	(/ (* (n2d (get :value slider)) +double_2pi) (const (n2d 1000))))

(defun lighting (z)
	; (lighting z) -> 0 to 5
	;how much light gets to an atom, less the further away it is, as one
	;of six steps. z is how deep it is as the lens has it, -1 at the front
	;and 1 at the back, which is not even, most of it is the far end, so
	;it is turned back into how far away. The light is drawn into the
	;atom's image, a texture with colors of its own is shown as it is and
	;can not be dimmed after, so there is an image for each step
	(defq away (/ (const (* +double_2 +far +near)) (- (const (+ +far +near)) (* z (const (- +far +near))))))
	(clamp (n2i (* (- +far away) (const (/ (n2d 5.99) (- +far +near))))) 0 5))

(defun atom-key (size kind level)
	(+ (* size 128) (* level 16) kind))

(defun atom-name (size kind level)
	; (atom-name size kind level) -> str
	;the name of an atom's image in the node's shared pixmap cache
	(cat "molecule/ball_" (str kind) "_" (str level) "_" (str size)))

(defun make-atom (size kind level)
	; (make-atom size kind level) -> canvas
	;the image of an atom this big, of this color, which of the palette,
	;and this well lit, (lighting). The shader draws it straight onto the
	;pixels of a canvas. It goes in the shared pixmap cache of the node, so
	;every Molecule that is open has the one image of a color, a size and a
	;light, and one another made is only wrapped
	(defq name (atom-name size kind level))
	(cond
		((defq pixmap (. *pixmap_cache* :find name))
			(defq canvas (Canvas-pixmap pixmap)))
		(:t (defq canvas (Canvas size size 1))
			(shader-vp-draw atom_native
				(shader-vp-frame atom_program atom_native (list
						(list 'resolution (list size size))
						(list 'color (map (const n2d) (elem-get +palette kind)))
						;from two thirds of the light at the back to all of it at the front
						(list 'light_level (/ (n2d (+ level 10)) (const (n2d 15))))))
				(defq pixmap (getf canvas +canvas_pixmap 0)) 0 0 size size size :t)
			(. *pixmap_cache* :insert name pixmap)
			(. canvas :swap +swap_write)))
	(. atom_cache :insert (atom-key size kind level) canvas)
	canvas)

(defun near-atom (size kind level)
	; (near-atom size kind level) -> :nil | canvas
	;the made image of this color nearest to this size, of this light if
	;there is one near, else of any
	(defq canvas :nil d 0)
	(while (and (not canvas) (<= (++ d) 8))
		(setq canvas (or (. atom_cache :find (atom-key (- size d) kind level))
				(. atom_cache :find (atom-key (+ size d) kind level)))))
	(setq d -1)
	(while (and (not canvas) (<= (++ d) 64))
		(some! (# (setq canvas (or (. atom_cache :find (atom-key (- size d) kind %0))
						(. atom_cache :find (atom-key (+ size d) kind %0)))))
			(list (map! (const !) '() 0 6))))
	canvas)

(defun get-atom-texture (radius kind level)
	; (get-atom-texture radius kind level) -> (tid tw th) | (:nil 0 0)
	;the image of an atom, made between frames, (make-waiting): till then
	;the nearest made of its color. Only the first of a color is made at
	;once, there is nothing near it
	(defq size (n2i (+ (* (quant radius +radius_quant) (n2d 2.0)) (n2d 0.5)))
		key (atom-key size kind level))
	(cond
		((<= size 0) (list :nil 0 0))
		(:t (unless (defq canvas (. atom_cache :find key))
				(unless (. atom_waiting :find key) (atom-ask size kind level))
				(setq canvas (ifn (near-atom size kind level) (make-atom size kind level))))
			(texture-sized (getf canvas +canvas_texture 0)))))

(defun atom-ask (size kind level)
	;an atom's image asked of the farm, made if there is none: one of the
	;node's cache, another Molecule's, at once
	(cond
		((. *pixmap_cache* :find (atom-name size kind level)) (make-atom size kind level))
		(:t (. atom_waiting :insert (atom-key size kind level) :t)
			(unless jobs (setq jobs (Jobs (cat *app_root* "child.lisp") (elem-get select +select_task)
				(elem-get select +select_reply) (list 8 1 1) :t)))
			(. jobs :add (list (setf-> (str-alloc +atom_job_size)
				(+atom_job_pixels size) (+atom_job_kind kind) (+atom_job_level level)))))))

(defun atom-made (msg)
	;the farm has drawn an atom's image: its pixels on a canvas, shown, and
	;kept in the node's cache. The farm let go of when none is out
	(when (and jobs (. jobs :answered msg))
		(bind '(size kind level) (getf-> msg +atom_reply_pixels +atom_reply_kind +atom_reply_level))
		(defq key (atom-key size kind level) canvas (Canvas size size 1))
		(. atom_waiting :erase key)
		(pixmap-read (getf canvas +canvas_pixmap 0) (string-stream (slice msg +atom_reply_size -1)) 32)
		(. canvas :swap +swap_write)
		(. *pixmap_cache* :insert (atom-name size kind level) (getf canvas +canvas_pixmap 0))
		(. atom_cache :insert key canvas)
		(setq *dirty* :t))
	(and jobs (= (. jobs :out) 0)
		(setq jobs (jobs-end jobs select +select_task +select_reply))))

(defun render ()
	;the atoms are placed by the vertex shader, all of them in one call of
	;its native code. For each it gives where the atom is, 4 numbers, then
	;x, y and radius on the widget, then its depth and its light
	(bind '(w h) (. *main_widget* :get_size))
	(defq out (shader-vp-place place_native
			(shader-vp-frame place_program place_native (list
					(list 'spin (mat4x4-mul (mat4x4-mul (mat4x4-rotx *rotx*) (mat4x4-roty *roty*))
							(mat4x4-rotz *rotz*)))
					(list 'move (const (mat4x4-translate +double_0 +double_0 (- +double_0 +focal_dist +double_2))))
					(list 'lens (const (mat4x4-frustum +left +right +top +bottom +near +far)))
					(list 'centre (list (>> w 1) (>> h 1)))
					(list 'half (* +double_1/2 (n2d (dec canvas_size))))))
			*atoms*)
		new_draw_list (list) bucket_at (length depth_buckets))
	;each atom in view to the bucket of its depth, one pass and no compares,
	;and drawn from the furthest bucket in: two in one bucket are as near as
	;makes no odds, which goes over which
	(each! (lambda () (defq w (elem-get out (+ (* (!) +placed_size) 3)))
			(if (<= +near w +far)
				(push (elem-get depth_buckets (n2i (* (- w +near) +bucket_scale))) (!)))) '() 0 *num_atoms*)
	(while (> bucket_at 0)
		(defq bucket (elem-get depth_buckets (setq bucket_at (dec bucket_at))))
		(each (lambda (i)
				(bind '(sx sy r z at) (slice out (+ (* i +placed_size) 4) (* (inc i) +placed_size)))
				(when (<= +double_-1 z +double_1)
					(bind '(tid tw th) (get-atom-texture r (elem-get *colors* i) (lighting z)))
					(when tid
						(defq col +argb_white
							blit_x (n2i (- sx (n2d (/ tw 2))))
							blit_y (n2i (- sy (n2d (/ th 2)))))
						(push new_draw_list (list tid col blit_x blit_y tw th)))
					(task-slice))) bucket)
		(clear bucket))
	(set *main_widget* :atom_draw_list new_draw_list)
	(. *main_widget* :dirty))

(defun sdf-file (index)
	(when (defq stream (file-stream (defq file (elem-get sdf_files index))))
		(. *title* :set_text (cat "Molecule -> " (path-name file)))
		(clear *verts* *radii* *colors*)
		(times 3 (read-line stream))
		(setq *num_atoms* (str-as-num (slice (read-line stream) 0 3)))
		(times *num_atoms*
			(defq line (split (read-line stream) +char_class_space))
			(push *verts*
				(/ (n2d (str-as-num (elem-get line 0))) (const (n2d 65536)))
				(/ (n2d (str-as-num (elem-get line 1))) (const (n2d 65536)))
				(/ (n2d (str-as-num (elem-get line 2))) (const (n2d 65536))))
			(case (elem-get line 3)
				("C" (push *radii* (const (n2d 70))) (push *colors* 0))
				("H" (push *radii* (const (n2d 25))) (push *colors* 1))
				("O" (push *radii* (const (n2d 60))) (push *colors* 2))
				("N" (push *radii* (const (n2d 65))) (push *colors* 3))
				("F" (push *radii* (const (n2d 50))) (push *colors* 4))
				("S" (push *radii* (const (n2d 88))) (push *colors* 6))
				("Si" (push *radii* (const (n2d 111))) (push *colors* 6))
				("P" (push *radii* (const (n2d 98))) (push *colors* 7))
				(:t (push *radii* (const (n2d 100))) (push *colors* 6))))
		(bind '(center radius) (vector-bounds-sphere *verts* 3))
		(defq scale_p (/ (const (n2d 2.0)) radius) scale_r (/ (const (n2d 0.0625)) radius)
			new_verts (doubles))
		(each (lambda (v)
				(push new_verts (vector-scale (vector-sub v center v) scale_p v) +double_1))
			(partition *verts* 3))
		(setq *verts* new_verts)
		(vector-scale *radii* scale_r *radii*)
		;what the vertex shader is given, each atom and then its radius
		(setq *atoms* (apply (const cat) (cat (list (doubles))
					(map (# (cat %0 (doubles %1))) (partition *verts* 4) *radii*))))))

(defun reset (name)
	;the molecule of that name, if it is still there, else maltose
	(setq *dirty* :t
		*mol_index* (ifn (some (# (if (ends-with (cat "/" name) %0) (!))) sdf_files)
			(ifn (some (# (if (ends-with "/maltose.sdf" %0) (!))) sdf_files) 0)))
	(sdf-file *mol_index*))

(defun config-load ()
	;the toolbar as it was when last closed, or as it is the first time
	(setq *config* (with-read-lock +config_file
			(tree-load (file-stream +config_file))))
	(if (or (not *config*) (nql (. *config* :find :version) +config_version))
		(setq *config* (scatter (Emap) :version +config_version
			:molecule "maltose.sdf" :auto :nil :style 1))))

(defun config-save ()
	;the toolbar: the molecule shown, turning or not, the backdrop
	(scatter *config* :molecule (path-name (elem-get sdf_files *mol_index*))
		:auto *auto_mode* :style (. *style_toolbar* :get_selected))
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))

;import actions and bindings
(import "./actions.inc")

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t)
	(. *window* :connect +event_layout)
	;atoms' images are shared by every Molecule of the node, from a cache:
	;those no Molecule is showing are let go of, and drawn again as wanted
	(canvas-flush)
	;the toolbar as it was
	(config-load)
	(bind '(name auto style) (gather *config* :molecule :auto :style))
	(. *style_toolbar* :set_selected style)
	(action-style)
	(. *window* :open)
	(tooltips)
	(reset name)
	(if auto (action-auto))
	(mail-timeout (elem-get select +select_timer) timer_rate 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip event
				(. *window* :tip *msg*))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem-get select +select_timer) timer_rate 0)
				(when *auto_mode*
					(setq *rotx* (% (+ *rotx* (n2d 0.01)) +double_2pi)
						*roty* (% (+ *roty* (n2d 0.02)) +double_2pi)
						*rotz* (% (+ *rotz* (n2d 0.03)) +double_2pi)
						*dirty* :t)
					(set-rot *xrot_slider* *rotx*)
					(set-rot *yrot_slider* *roty*)
					(set-rot *zrot_slider* *rotz*))
				(when *dirty*
					(setq *dirty* :nil)
					(render))
				;a child of the farm gone, or too long, started again
				(if jobs (. jobs :refresh +patience)))
			((= idx +select_task) (if jobs (. jobs :launched *msg*)))
			((= idx +select_reply) (atom-made *msg*))
			((. *window* :dispatch *msg*))
			((. *window* :event *msg*))))
	(config-save)
	(. *window* :close))

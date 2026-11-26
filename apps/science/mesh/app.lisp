(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/math/mesh.inc")
(import "lib/math/scene.inc")
(import "lib/gpu/render_farm.inc")
(import "./app.inc")

(enums +event +event_user
	(enum mode auto)
	(enum xrot yrot zrot)
	(enum style gpu))

;the timers are last. A frame can take longer than the frame timer, and
;what is first in the list is what is read first, so a timer that was
;ahead of the mail of what draws the faces would leave it unread
(enums +select 0
	(enum main task reply tip frame_timer retry_timer))

(defq anti_alias :nil frame_timer_rate (/ 1000000 30) retry_timer_rate 1000000
	retry_timeout 60000000 +min_size 450 +max_size 800
	canvas_size +min_size canvas_scale (if anti_alias 1 2)
	+canvas_mode (if anti_alias +canvas_flag_antialias 0)
	+stage_depth +double_4 +focal_dist +double_2
	*rotx* +double_0 *roty* +double_0 *rotz* +double_0
	+near +focal_dist +far (+ +near +stage_depth)
	+top (* +focal_dist +double_1/2) +bottom (* +focal_dist +double_-1/2)
	+left (* +focal_dist +double_-1/2) +right (* +focal_dist +double_1/2)
	*auto_mode* :nil *render_mode* :nil *use_gpu* :t
	;the size of picture that is wanted, the last a window being sized
	;said. And while a canvas of a new size has no whole frame yet, the
	;one that is still on show
	*want_size* canvas_size *shown* :nil
	;the toolbar as it was when it was last closed, (config-save)
	*config* :nil +config_version 1 +config_file (cat (get :home *env*) "mesh.tre"))

;the pixels of the canvas are in shared memory if the host has it, and
;the faces can then be drawn on them by a child on each node, a strip each
;the steps of a turn's slider, and the turn of the auto spin a tick
;about each way
(defq +rot_steps (n2d 1000) +spin_x (n2d 0.01) +spin_y (n2d 0.02) +spin_z (n2d 0.03))

(defun make-canvas (size)
	(ifn (canvas-shared size size canvas_scale) (Canvas size size canvas_scale)))

(ui-window *window* ()
	(ui-title-bar *title* "Mesh" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_mode +sym_auto) +event_mode))
		(. (ui-radio-bar *style_toolbar* (+sym_plain +sym_grid +sym_axis)
				(:paper :paper_toolbar2)) :connect +event_style)
		;what draws the faces, the nodes, a strip each, or the GPU
		(. (ui-radio-bar *gpu_toolbar* ("CPU" "GPU") (:font :font_body)) :connect +event_gpu)
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
	(ui-backdrop *main_backdrop* (:style :plain :paper +argb_black :ink +argb_grey8
			:min_width +min_size :min_height +min_size)
		(ui-element *main_widget* (make-canvas canvas_size))))

(defun tooltips (mbox)
	(def *window* :tip_mbox mbox)
	(ui-tool-tips *main_toolbar*
		'("mode" "auto"))
	(ui-tool-tips *style_toolbar*
		'("plain" "grid" "axis")))

(defun set-rot (slider angle)
	(set (. slider :dirty) :value
			(n2i (/ (* angle +rot_steps) +double_2pi))))

(defun get-rot (slider)
	(/ (* (n2d (get :value slider)) +double_2pi) +rot_steps))

(defun create-scene (job_que)
	; (create-scene job_que) -> scene_root
	;create mesh loader jobs
	(each (lambda ((name command))
			(push job_que (cat (str-alloc +mesh_name) (pad name 16) command)))
		`(
			("sphere.1" "(Mesh-iso (Iso-sphere 40 40 40) (n2d 0.25))")
			("cube.1" "(Mesh-iso (Iso-cube 10 10 10) (n2d 0.45))")
			("capsule" "(Mesh-iso (Iso-capsule 40 40 40) (n2d 0.25))")
			("torus.1" "(Mesh-torus +double_1 +double_1/3 40)")
			("sphere.2" "(Mesh-sphere +double_1/2 20)")
			("teapot.1" ,(cat "(Mesh-obj (file-stream {" *app_root* "data/teapot.obj}))"))))
	;create scene graph
	(defq scene (Scene "root")
		sphere_obj (Scene-object :nil (fixeds 1.0 1.0 1.0 1.0) "sphere.1")
		capsule1_obj (Scene-object :nil (fixeds 0.8 1.0 0.0 0.0) "capsule.1")
		capsule2_obj (Scene-object :nil (fixeds 0.8 0.0 1.0 1.0) "capsule.2")
		cube_obj (Scene-object :nil (fixeds 0.8 1.0 1.0 0.0) "cube.1")
		torus_obj (Scene-object :nil (fixeds 1.0 0.0 1.0 0.0) "torus.1")
		sphere2_obj (Scene-object :nil (fixeds 0.8 1.0 0.0 1.0) "sphere.2")
		teapot_obj (Scene-object :nil (fixeds 1.0 1.0 1.0 1.0) "teapot.1"))
	(. sphere_obj :set_translation (const (+ +double_-1/3 +double_-1/3)) (const (+ +double_-1/3 +double_-1/3)) (const (- +double_0 +focal_dist +double_1)))
	(. torus_obj :set_translation (const (- +double_1/2 +double_1/3)) (const (+ +double_1/2 +double_1/3)) (const (- +double_0 +focal_dist +double_2)))
	(. sphere2_obj :set_translation +double_0 +double_1/2 +double_0)
	(. cube_obj :set_translation +double_0 +double_-1/2 +double_0)
	(.-> capsule1_obj
		(:set_translation +double_0 +double_1/2 +double_0)
		(:set_rotation +double_0 +double_hpi +double_0))
	(. capsule2_obj :set_translation +double_0 +double_-1/2 +double_0)
	(.-> torus_obj (:add_node sphere2_obj) (:add_node cube_obj))
	(.-> sphere_obj (:add_node capsule1_obj) (:add_node capsule2_obj))
	(.-> teapot_obj
		(:set_translation (const (- +double_1/2 +double_-1/3)) (const (+ +double_-1/2 +double_-1/3)) (const (- +double_0 +focal_dist +double_1)))
		(:set_rotation +double_0 +double_hpi +double_0))
	;all of it shines, a highlight where a face is turned half way between
	;the light and the eye. What is round is lit smooth, a normal a
	;vertex. The cube is not, its faces are flat
	(each (# (def %0 :smooth :t))
		(list sphere_obj capsule1_obj capsule2_obj torus_obj sphere2_obj teapot_obj))
	(.-> scene (:set_shaders +scene_shiny_files)
		(:add_node sphere_obj) (:add_node torus_obj) (:add_node teapot_obj)))

;import actions and bindings
(import "./actions.inc")

(defun draw-faces ()
	;a frame of the faces, by the render: the GPU if it can and is wanted,
	;the nodes, a strip each, or here. A host with no GPU for it, and the
	;button goes back
	(unless (eql (. render :mode (if *use_gpu* :gpu :cpu)) :gpu)
		(when *use_gpu*
			(setq *use_gpu* :nil)
			(. *gpu_toolbar* :set_selected 0)))
	(. render :frame (. scene :draws +left +right +top +bottom +near +far (* canvas_size canvas_scale))))

(defun new-canvas ()
	;a canvas of the size that is wanted, if that is not the size there
	;is, drawn on from this frame. It is not put on show till a frame of it
	;is whole, (show-canvas): the one on show stays till then, so nothing
	;is seen that is not a picture, and nothing waits for a frame
	(unless (= *want_size* canvas_size)
		(unless *shown* (setq *shown* *main_widget*))
		(setq *main_widget* (make-canvas *want_size*) canvas_size *want_size*)
		(. *main_widget* :set_canvas_flags +canvas_mode)
		(. render :canvas *main_widget*)))

(defun show-canvas ()
	;a frame of the new canvas is whole: it goes on show, and the old one,
	;whose pixels may be shared, is let go of
	(.-> *shown* :sub :free)
	(. *main_backdrop* :add_child *main_widget*)
	(bind '(w h) (. *main_backdrop* :get_size))
	(. *main_widget* :change 0 0 w h)
	(. *window* :layout)
	(setq *shown* :nil))

(defun frame-rotate ()
	(. scene :set_rotation +double_0 +double_0 *rotz*)
	(each (# (. %0 :set_rotation *rotx* *roty* +double_0)) (. scene :children)))

(defun config-load ()
	;the toolbar as it was when last closed, or as it is the first time
	(setq *config* (with-read-lock +config_file
			(tree-load (file-stream +config_file))))
	(if (or (not *config*) (nql (. *config* :find :version) +config_version))
		(setq *config* (scatter (Emap) :version +config_version
			:mode :nil :auto :nil :style 0 :gpu :t))))

(defun config-save ()
	;the toolbar: the mode, turning or not, the backdrop, what draws the faces
	(scatter *config* :mode *render_mode* :auto *auto_mode*
		:style (. *style_toolbar* :get_selected) :gpu *use_gpu*)
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))

(defun main ()
	(. *window* :connect +event_layout)
	(.-> *main_widget* (:set_canvas_flags +canvas_mode) (:fill +argb_black) (:swap +swap_write))
	;the toolbar as it was
	(config-load)
	(bind '(mode auto style gpu) (gather *config* :mode :auto :style :gpu))
	(. *style_toolbar* :set_selected style)
	(action-style)
	(setq *use_gpu* gpu)
	(. *gpu_toolbar* :set_selected (if *use_gpu* 1 0))
	(. *window* :open)
	(defq select (task-mboxes +select_size) *running* :t *dirty* :t
		meshes (list) scene (create-scene meshes)
		;what draws the faces, lib/gpu/render_farm.inc. The scene has the
		;meshes, and draws a frame here when no farm can
		render (Render-tris *main_widget* (first +scene_shiny_files) (second +scene_shiny_files) :t)
		;the meshes are made by a herd of children on this machine's nodes
		jobs (Jobs (cat *app_root* "child.lisp") (elem-get select +select_task)
			(elem-get select +select_reply) '(4 2)))
	(. jobs :add meshes)
	(def render :clear 0
		:mesh_fn (lambda (id) (if (< -1 id (length (get :meshes scene))) (. scene :mesh id)))
		:here_fn (lambda (canvas draws) (. scene :draw canvas draws)))
	;the mode and turning buttons lit as they were
	(if mode (action-mode))
	(if auto (action-auto))
	(tooltips (elem-get select +select_tip))
	(mail-timeout (elem-get select +select_frame_timer) frame_timer_rate 0)
	(mail-timeout (elem-get select +select_retry_timer) retry_timer_rate 0)
	(while *running*
		;the render's mail is read with the app's, before the timers, and
		;dealt with
		(bind '(idx *msg*) (. render :read select +select_frame_timer))
		(cond
			((not idx))
			((= idx +select_tip)
				;tip event
				(. *window* :tip *msg*))
			((= idx +select_task)
				;child task launch response
				(. jobs :launched *msg*))
			((= idx +select_reply)
				;child mesh response
				(when (defq out (. jobs :answered *msg*))
					(defq mesh_name (trim (getf *msg* +mesh_reply_name))
						mesh (Mesh-data
							(getf *msg* +mesh_reply_num_verts)
							(getf *msg* +mesh_reply_num_norms)
							(getf *msg* +mesh_reply_num_tris)
							(slice *msg* +mesh_reply_data -1)))
					(each (# (. %0 :set_mesh mesh)) (. scene :find_nodes mesh_name))
					(setq *dirty* :t)
					(when (= out 0)
						;all the meshes are here, the children can go
						(mail-timeout (elem-get select +select_retry_timer) 0 0)
						(. jobs :close))))
			((= idx +select_retry_timer)
				;retry timer event
				(mail-timeout (elem-get select +select_retry_timer) retry_timer_rate 0)
				(. jobs :refresh retry_timeout))
			((= idx +select_frame_timer)
				;frame timer event
				(mail-timeout (elem-get select +select_frame_timer) frame_timer_rate 0)
				(when *auto_mode*
					(setq *rotx* (% (+ *rotx* +spin_x) +double_2pi)
						*roty* (% (+ *roty* +spin_y) +double_2pi)
						*rotz* (% (+ *rotz* +spin_z) +double_2pi)
						*dirty* :t)
					(set-rot *xrot_slider* *rotx*)
					(set-rot *yrot_slider* *roty*)
					(set-rot *zrot_slider* *rotz*))
				;the next frame, if the last is not still out with a farm
				(when (and *dirty* (not (. render :busy)))
					(setq *dirty* :nil)
					(new-canvas)
					(frame-rotate)
					(if *render_mode*
						(draw-faces)
						(. scene :render *main_widget* (* canvas_size canvas_scale)
							+left +right +top +bottom +near +far :nil)))
				;the patience of its farm, and one whose work has stopped, the
				;faces have stood still a while, is let go of
				(. render :tick))
			;must be gui event to main mailbox
			((. *window* :dispatch *msg*))
			((. *window* :event *msg*)))
		;a canvas of a new size goes on show once a frame of it is whole
		(and *shown* (not (. render :busy)) (show-canvas)))
	(config-save)
	(. jobs :close)
	(. render :close)
	(. *window* :close))

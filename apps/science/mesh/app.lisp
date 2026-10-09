(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/math/mesh.inc")
(import "lib/math/scene.inc")
(import "lib/gpu/tris.inc")
(import "lib/gpu/gui.inc")
(import "./app.inc")

(enums +event +event_user
	(enum mode auto)
	(enum xrot yrot zrot)
	(enum style gpu))

;the timers are last. A frame can take longer than the frame timer, and
;what is first in the list is what is read first, so a timer that was
;ahead of the farm would leave the farm unread
(enums +select 0
	(enum main task reply tip farm_task farm_reply farm_ask frame_timer retry_timer))

(defq anti_alias :nil frame_timer_rate (/ 1000000 30) retry_timer_rate 1000000
	retry_timeout (task-timeout 10) +min_size 450 +max_size 800
	canvas_size +min_size canvas_scale (if anti_alias 1 2)
	+canvas_mode (if anti_alias +canvas_flag_antialias 0)
	+stage_depth +real_4 +focal_dist +real_2
	*rotx* +real_0 *roty* +real_0 *rotz* +real_0
	+near +focal_dist +far (+ +near +stage_depth)
	+top (* +focal_dist +real_1/2) +bottom (* +focal_dist +real_-1/2)
	+left (* +focal_dist +real_-1/2) +right (* +focal_dist +real_1/2)
	*auto_mode* :nil *render_mode* :nil *use_gpu* :t)

;the pixels of the canvas are in shared memory if the host has it, and
;the faces are then drawn on them by a child on each node, a strip each
(defun make-canvas (size)
	(ifn (canvas-shared size size canvas_scale) (Canvas size size canvas_scale)))

(ui-window *window* ()
	(ui-title-bar *title* "Mesh" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_mode +sym_auto) +event_mode))
		(. (ui-radio-bar *style_toolbar* (+sym_plain +sym_grid +sym_axis)
			(:color *env_toolbar2_col*)) :connect +event_style)
		;what draws the faces, the nodes, a strip each, or the GPU of the GUI
		(. (ui-radio-bar *gpu_toolbar* ("CPU" "GPU") (:font *env_body_font*)) :connect +event_gpu)
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
	(ui-backdrop *main_backdrop* (:style :plain :color +argb_black :ink_color +argb_grey8
			:min_width +min_size :min_height +min_size)
		(ui-element *main_widget* (make-canvas canvas_size) (:color 0))))

(defun tooltips (mbox)
	(def *window* :tip_mbox mbox)
	(ui-tool-tips *main_toolbar*
		'("mode" "auto"))
	(ui-tool-tips *style_toolbar*
		'("plain" "grid" "axis")))

(defun set-rot (slider angle)
	(set (. slider :dirty) :value
		(n2i (/ (* angle (const (n2r 1000))) +real_2pi))))

(defun get-rot (slider)
	(/ (* (n2r (get :value slider)) +real_2pi) (const (n2r 1000))))

(defun create-scene (job_que)
	; (create-scene job_que) -> scene_root
	;create mesh loader jobs
	(each (lambda ((name command))
			(push job_que (cat (str-alloc +mesh_name) (pad name 16) command)))
		`(
		("sphere.1" "(Mesh-iso (Iso-sphere 40 40 40) (n2r 0.25))")
		("cube.1" "(Mesh-iso (Iso-cube 10 10 10) (n2r 0.45))")
		("capsule" "(Mesh-iso (Iso-capsule 40 40 40) (n2r 0.25))")
		("torus.1" "(Mesh-torus +real_1 +real_1/3 40)")
		("sphere.2" "(Mesh-sphere +real_1/2 20)")
		("teapot.1" ,(cat "(Mesh-obj (file-stream {" *app_root* "data/teapot.obj}))"))
		))
	;create scene graph
	(defq scene (Scene "root")
		sphere_obj (Scene-object :nil (fixeds 1.0 1.0 1.0 1.0) "sphere.1")
		capsule1_obj (Scene-object :nil (fixeds 0.8 1.0 0.0 0.0) "capsule.1")
		capsule2_obj (Scene-object :nil (fixeds 0.8 0.0 1.0 1.0) "capsule.2")
		cube_obj (Scene-object :nil (fixeds 0.8 1.0 1.0 0.0) "cube.1")
		torus_obj (Scene-object :nil (fixeds 1.0 0.0 1.0 0.0) "torus.1")
		sphere2_obj (Scene-object :nil (fixeds 0.8 1.0 0.0 1.0) "sphere.2")
		teapot_obj (Scene-object :nil (fixeds 1.0 1.0 1.0 1.0) "teapot.1")
		)
	(. sphere_obj :set_translation (const (+ +real_-1/3 +real_-1/3)) (const (+ +real_-1/3 +real_-1/3)) (const (- +real_0 +focal_dist +real_1)))
	(. torus_obj :set_translation (const (- +real_1/2 +real_1/3)) (const (+ +real_1/2 +real_1/3)) (const (- +real_0 +focal_dist +real_2)))
	(. sphere2_obj :set_translation +real_0 +real_1/2 +real_0)
	(. cube_obj :set_translation +real_0 +real_-1/2 +real_0)
	(.-> capsule1_obj
		(:set_translation +real_0 +real_1/2 +real_0)
		(:set_rotation +real_0 +real_hpi +real_0))
	(. capsule2_obj :set_translation +real_0 +real_-1/2 +real_0)
	(.-> torus_obj (:add_node sphere2_obj) (:add_node cube_obj))
	(.-> sphere_obj (:add_node capsule1_obj) (:add_node capsule2_obj))
	(.-> teapot_obj
		(:set_translation (const (- +real_1/2 +real_-1/3)) (const (+ +real_-1/2 +real_-1/3)) (const (- +real_0 +focal_dist +real_1)))
		(:set_rotation +real_0 +real_hpi +real_0))
	;all of it shines, a highlight where a face is turned half way between
	;the light and the eye. What is round is lit smooth, a normal a
	;vertex. The cube is not, its faces are flat
	(each (# (def %0 :smooth :t))
		(list sphere_obj capsule1_obj capsule2_obj torus_obj sphere2_obj teapot_obj))
	(.-> scene (:set_shaders +scene_shiny_files)
		(:add_node sphere_obj) (:add_node torus_obj) (:add_node teapot_obj)))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun strips (draws rows)
	;a job for each child of the farm, a strip of the frame, or with no
	;rows, which a child answers when it has the shaders and the meshes
	(defq count (. farm :size) size (* canvas_size canvas_scale))
	(setq farm_key (canvas-key *main_widget*))
	(. farm :add (map (# (shader-strip (first +scene_shiny_files) (second +scene_shiny_files)
			(elem-get select +select_farm_ask) farm_key size size
			(if rows (/ (* %0 size) count) 0) (if rows (/ (* (inc %0) size) count) 0) :t draws))
		(range 0 count))))

(defun warm-farm (draws)
	;a child on each node, started, or started again, one with nothing to
	;do for a while has gone. Till they have all said they are ready the
	;frames are drawn here
	;a child for each node but this one, and none of them on this one, a
	;strip is one long call and would hold up the app and the GUI
	(if farm (. farm :restart)
		(setq farm (Jobs +shader_tris_child (elem-get select +select_farm_task)
			(elem-get select +select_farm_reply)
			(list 64 (max 1 (dec (length (lisp-nodes :t)))) 0) :t)))
	(setq warming :t farming :nil last_farm 1)
	(strips draws :nil))

(defun draw-faces-gpu (draws)
	; (draw-faces-gpu draws) -> :t | :nil
	;a frame of the faces by the GPU of the GUI, if the host has one that
	;can and it is wanted, the g key says. :t if there is no more to do for
	;this frame, it was drawn, or the GPU is busy and it is to be tried
	;again. :nil if the frame is to be drawn by the nodes, or here
	(when (and *use_gpu* (not gpu_pair) (not gpu_failed))
		;the pair of shaders, the first time, the driver builds it in its
		;own time
		(unless (setq gpu_pair (shader-gui-pair (shader-load (first +scene_shiny_files))
				(shader-load (second +scene_shiny_files)) :t))
			(setq gpu_failed :t)))
	;a host that can not, and the button goes back
	(when (and *use_gpu* gpu_failed)
		(setq *use_gpu* :nil)
		(. *gpu_toolbar* :set_selected 0))
	(when (and *use_gpu* gpu_pair)
		(defq drawn (shader-gui-frame *main_widget*
			(map (lambda ((id vblock pblock &rest _))
				;a mesh goes to the GPU the first time it is drawn, and
				;is kept there
				(while (<= (length gpu_meshes) id) (push gpu_meshes :nil))
				(unless (elem-get gpu_meshes id)
					(elem-set gpu_meshes id (shader-gui-mesh (. scene :mesh id))))
				(list gpu_pair (ifn (elem-get gpu_meshes id) 0) vblock pblock)) draws)))
		(cond
			((eql drawn :error)
				(setq gpu_pair :nil gpu_failed :t *use_gpu* :nil)
				(. *gpu_toolbar* :set_selected 0)
				:nil)
			(drawn (setq gpu_drawn :t))
			;not drawn. Once the GPU has drawn a frame that is it being busy,
			;so this frame is tried again. Before that the driver is still
			;building the pair, and the frame is drawn the other way
			(gpu_drawn (setq *dirty* :t)))))

(defun draw-faces ()
	;a frame of the faces. By the GPU if it can and is wanted. If not, by
	;the farm if the pixels can be shared, there is more than this node,
	;and the children are ready. Here if not
	(defq draws (. scene :draws +left +right +top +bottom +near +far (* canvas_size canvas_scale))
		now (pii-time))
	(cond
		((draw-faces-gpu draws))
		((and (not no_farm) (not warming) (/= (canvas-key *main_widget*) 0)
				(> (length (lisp-nodes :t)) 1) farm (< (- now last_farm) +farm_stale))
			(setq farming :t last_farm now)
			(. *main_widget* :fill 0)
			(strips draws :t))
		(:t (if (and (not no_farm) (not warming) (/= (canvas-key *main_widget*) 0)
					(> (length (lisp-nodes :t)) 1))
				(warm-farm draws))
			(.-> scene (:draw *main_widget* draws))
			(. *main_widget* :swap +swap_write))))

(defun main ()
	(bind '(x y w h) (apply view-locate (.-> *window* (:connect +event_layout) :pref_size)))
	(.-> *main_widget* (:set_canvas_flags +canvas_mode) (:fill +argb_black) (:swap +swap_write))
	(. *style_toolbar* :set_selected 0)
	(. *gpu_toolbar* :set_selected (if *use_gpu* 1 0))
	(gui-add-front-rpc (. *window* :change x y w h))
	(defq select (task-mboxes +select_size) *running* :t *dirty* :t
		meshes (list) scene (create-scene meshes)
		;the farm that draws the faces, a frame is out with it, it has
		;been asked if it is ready, and it can not reach the pixels
		farm :nil farming :nil warming :nil no_farm :nil last_farm 0 ticks 0 farm_key 0
		;the pair of shaders on the GPU, it has drawn a frame, and it can not
		gpu_pair :nil gpu_drawn :nil gpu_failed :nil gpu_meshes (list)
		+farm_stale 3000000
		;the meshes are made by a herd of children on this machine's nodes
		jobs (Jobs (cat *app_root* "child.lisp") (elem-get select +select_task)
			(elem-get select +select_reply) '(4 2)))
	(. jobs :add meshes)
	(tooltips (elem-get select +select_tip))
	(mail-timeout (elem-get select +select_frame_timer) frame_timer_rate 0)
	(mail-timeout (elem-get select +select_retry_timer) retry_timer_rate 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip event
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
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
					(setq *rotx* (% (+ *rotx* (n2r 0.01)) +real_2pi)
						*roty* (% (+ *roty* (n2r 0.02)) +real_2pi)
						*rotz* (% (+ *rotz* (n2r 0.03)) +real_2pi)
						*dirty* :t)
					(set-rot *xrot_slider* *rotx*)
					(set-rot *yrot_slider* *roty*)
					(set-rot *zrot_slider* *rotz*))
				;the next frame, if the last is not still out with the farm
				(when (and *dirty* (not farming))
					(setq *dirty* :nil)
					(. scene :set_rotation +real_0 +real_0 *rotz*)
					(each (# (. %0 :set_rotation *rotx* *roty* +real_0)) (. scene :children))
					(if *render_mode*
						(draw-faces)
						(. scene :render *main_widget* (* canvas_size canvas_scale)
							+left +right +top +bottom +near +far :nil)))
				(if (and farm (= (setq ticks (% (inc ticks) 30)) 0))
					(. farm :refresh retry_timeout)))
			((= idx +select_farm_task)
				;a child of the farm has started
				(if farm (. farm :launched *msg*)))
			((= idx +select_farm_ask)
				;a child of the farm has not got a mesh
				(shader-mesh-send *msg* (. scene :mesh (getf *msg* +strip_ask_mesh))))
			((= idx +select_farm_reply)
				;a strip is drawn, or a child has said it is ready
				(when (and farm (defq out (. farm :answered *msg*)))
					;a child that could not reach the pixels. If they are
					;still the pixels of the canvas the farm is no use, and
					;the faces are drawn here from now on. If the canvas is
					;a new one, the window was sized, the farm is asked
					;again for the new one
					(when (= (getf *msg* +strip_reply_drawn) 0)
						(setq *dirty* :t last_farm 0)
						(if (= farm_key (canvas-key *main_widget*)) (setq no_farm :t)))
					(when (= out 0)
						(cond
							(warming (setq warming :nil)
								(if (> last_farm 0) (setq last_farm (pii-time))))
							(farming (setq farming :nil)
								(if (= farm_key (canvas-key *main_widget*))
									(. *main_widget* :swap +swap_write)))))))
			;must be gui event to main mailbox
			((defq id (getf *msg* +ev_msg_target_id) action (. *event_map* :find id))
				;call bound event action
				(dispatch-action action))
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key_down)
					(> (getf *msg* +ev_msg_key_scode) 0))
				;key event
				(bind '(key mod) (getf-> *msg* +ev_msg_key_key +ev_msg_key_mod))
				(cond
					((bits? mod +ev_key_mod_control +ev_key_mod_alt +ev_key_mod_meta)
						;call bound control/command key action
						(when (defq action (. *key_map_control* :find key))
							(dispatch-action action)))
					((bits? mod +ev_key_mod_shift)
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
			((. *window* :event *msg*))))
	(. jobs :close)
	(if farm (. farm :close))
	;what was kept on the GPU
	(each (# (if %0 (canvas-mesh-destroy %0))) gpu_meshes)
	(if gpu_pair (canvas-shader-destroy gpu_pair))
	(gui-sub-rpc *window*)
	(profile-report "Mesh"))

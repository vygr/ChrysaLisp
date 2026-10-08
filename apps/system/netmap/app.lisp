(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/task/global.inc")
(import "lib/net/links.inc")
(import "lib/math/mesh.inc")
(import "lib/math/scene.inc")
(import "lib/gpu/tris.inc")
(import "lib/gpu/gui.inc")
(import "./app.inc")
(import "./map.inc")

;The network as it is, in three dimensions. A ball for each node, the
;color of the machine it is on, bigger and whiter the more tasks it has. A
;bar for each link, hotter the more mail it carries, from blue through red
;to white. Each node is asked who its
;links are to, so it is the network that is there, not the one that was
;launched, and it is asked again as nodes come and go.
;
;It lays itself out as a bedspring. Every link is a spring, every node
;pushes every other away, and they are left to settle. A link that is busy
;pulls harder, so nodes that talk draw together. Dr Ian Thomas did a
;bedspring model of a network for Taos, this is in his honour.

(enums +event 0
	(enum close)
	(enum auto)
	(enum xrot yrot zrot)
	(enum layout))

(enums +select 0
	(enum main task reply frame_timer poll_timer))

(defq +size 640 +min_size 320 +scale 1 +frame_rate (/ 1000000 20) +poll_rate (/ 1000000 4)
	+retry_timeout (task-timeout 5)
	+focal_dist +real_2 +near +focal_dist +far (+ +near +real_4)
	+top (* +focal_dist +real_1/2) +bottom (* +focal_dist +real_-1/2)
	+left (* +focal_dist +real_-1/2) +right (* +focal_dist +real_1/2)
	;the shaders a ball is drawn with
	+shiny ''("lib/gpu/shaders/shiny_vertex.shader" "lib/gpu/shaders/shiny_lit.shader")
	+ball_size (n2r 0.09) +bar_size (n2r 0.016))

(ui-window *window* ()
	(ui-title-bar _ "Network Map" (0xea19) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (0xea43) +event_auto))
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
	(ui-flow _ (:flow_flags +flow_up_fill)
		(ui-label *status* (:text "..." :font *env_body_font*))
		(ui-backdrop _ (:style :plain :color +argb_black :min_width +size :min_height +size)
			(ui-element *canvas* (Canvas +size +size +scale) (:color 0)))))

(defun set-rot (slider angle)
	(set (. slider :dirty) :value
		(n2i (/ (* angle (const (n2r 1000))) +real_2pi))))

(defun get-rot (slider)
	(/ (* (n2r (get :value slider)) +real_2pi) (const (n2r 1000))))

(defun set-auto (on)
	;it turns by itself, or it is turned by the sliders. The button is
	;lit while it turns by itself
	(defq button (first (. *main_toolbar* :children)))
	(undef (. button :dirty) :color)
	(if (setq auto on)
		(def button :color (canvas-brighter (get :color *main_toolbar*)))))

(defun set-canvas (size)
	;the window is a new size, and so is the picture, a square that fits
	(unless (= size canvas_size)
		(defq parent (penv *canvas*))
		(. *canvas* :sub)
		(setq *canvas* (Canvas size size +scale) canvas_size size)
		(bind '(w h) (. parent :get_size))
		(. parent :add_child *canvas*)
		(. *canvas* :change 0 0 w h)
		(. *window* :layout)))

(defun share (obj proto)
	;an object that draws the mesh a first one of its kind was drawn with,
	;so the scene has the mesh once, and so has the GPU. And is lit as it
	;is, a ball shines
	(def obj :corners_of (get :corners_of proto) :corners_id (get :corners_id proto)
		:ball (get :ball proto))
	(if (def? :shaders proto) (def obj :shaders (get :shaders proto)))
	obj)

(defun make-bar ()
	;the bar of a new link, map.inc asks for one
	(share (Scene-object bar_mesh (fixeds 1.0 0.3 0.4 0.6)) bar_proto))

(defun create (key now)
	; (create key now) -> val
	;a node is seen, it gets a ball, and is put near the middle to be
	;pushed out to where it belongs
	(def (defq node (env 1)) :timestamp now :key key :tasks 0 :system "" :links (list)
		:pos (reals (rnd) (rnd) (rnd)) :vel (reals +real_0 +real_0 +real_0)
		:ball (share (Scene-object ball_mesh (fixeds 1.0 1.0 1.0 1.0)) ball_proto))
	(open-task (const (cat *app_root* "child.lisp")) key +kn_call_pin 0 (elem-get select +select_task))
	(setq changed :t)
	node)

(defun destroy (key node)
	; (destroy key val)
	;a node has gone
	(when (defq child (get :child node)) (mail-send child ""))
	(setq changed :t))

(defun node-heard (msg)
	;a node has said who it is and what its links are
	(when (defq node (. global_tasks :find (getf msg +reply_node)))
		(defq system (getf msg +reply_system) now (pii-time))
		(unless (find system machines) (push machines system))
		(def node :timestamp now :tasks (getf msg +reply_task_count) :system system
			:links (map (lambda (i)
					(defq link (slice msg (+ +reply_links (* i +link_size)) (+ +reply_links (* (inc i) +link_size))))
					(list (getf link +link_peer_node) (getf link +link_sent)))
				(range 0 (getf msg +reply_num_links))))
		(push poll_que (get :child node))))

(defun pose-scene ()
	;put each ball and each bar where the bedspring has it
	(defq objs (list))
	(. global_tasks :each (lambda (key node)
		(defq ball (get :ball node) pos (get :pos node)
			;as big on the screen however far out it is all drawn from
			load (/ (n2r (min 40 (get :tasks node))) (const (n2r 40)))
			size (/ (* +ball_size (+ +real_1 load)) zoom))
		(.-> ball (:set_translation (first pos) (second pos) (third pos)) (:set_scale size size size))
		;the color of its machine, and whiter the more it has to do
		(def ball :color (apply (const fixeds) (cat (list 1.0)
			(map (# (n2f (+ %0 (* (- +real_1 %0) load)))) (machine-color (get :system node))))))
		(push objs ball)))
	(. links :each (lambda (name link)
		(when (and (defq a (. global_tasks :find (get :a link)))
				(defq b (. global_tasks :find (get :b link))))
			;a bar is a cylinder that stands on y, one long. Its matrix is
			;made here, y laid along the link, and two more across it
			(defq pa (get :pos a) pb (get :pos b) d (nums-sub pb pa)
				mid (nums-scale (nums-add pa pb) +real_1/2)
				across (if (< (abs (first d)) (abs (second d))) (reals +real_1 +real_0 +real_0)
					(reals +real_0 +real_1 +real_0))
				u (apply (const reals) (vector-cross-3d d across))
				thick (/ +bar_size zoom)
				u (nums-scale u (/ thick (sqrt (+ (nums-dot u u) (const (n2r 0.000001))))))
				v (apply (const reals) (vector-cross-3d d u))
				v (nums-scale v (/ thick (sqrt (+ (nums-dot v v) (const (n2r 0.000001))))))
				;cold is a dim blue, half way is red, and hot is white
				heat (get :heat link) bar (get :bar link)
				red (min +real_1 (* heat +real_2)) cool (- +real_1 red)
				white (max +real_0 (- (* heat +real_2) +real_1)))
			;a machine to machine link is twice as thick
			(unless (eql (get :system a) (get :system b))
				(nums-scale u +real_2 u) (nums-scale v +real_2 v))
			(def bar :dirty :nil :matrix (reals
				(first u) (first d) (first v) (first mid)
				(second u) (second d) (second v) (second mid)
				(third u) (third d) (third v) (third mid)
				+real_0 +real_0 +real_0 +real_1)
				:color (fixeds 1.0
					(n2f (+ (* cool (const (n2r 0.25))) red))
					(n2f (+ (* cool (const (n2r 0.35))) (* red (const (n2r 0.2))) (* white (const (n2r 0.8)))))
					(n2f (+ (* cool (const (n2r 0.6))) (* red (const (n2r 0.1))) (* white (const (n2r 0.9)))))))
			(push objs bar))))
	(set world :children objs)
	(.-> world (:set_scale zoom zoom zoom) (:set_rotation rotx roty rotz)))

(defun draw-frame ()
	;a frame, by the GPU of the GUI if it has one that can, else here
	(defq draws (. scene :draws +left +right +top +bottom +near +far (* canvas_size +scale)))
	;the pair of shaders the bars are drawn with, and the pair the balls
	;are, on the GPU, the first time
	(when (and (not gpu_pair) (not gpu_failed))
		(unless (and (setq gpu_pair (shader-gui-pair (shader-load +scene_vertex_file)
					(shader-load +scene_pixel_file) :t))
				(setq gpu_shiny (shader-gui-pair (shader-load (first +shiny))
					(shader-load (second +shiny)) :t)))
			(setq gpu_pair :nil gpu_failed :t)))
	(defq drawn (if gpu_pair (shader-gui-frame *canvas*
		(map (lambda ((id vblock pblock &optional y y1 files))
			(while (<= (length gpu_meshes) id) (push gpu_meshes :nil))
			(unless (elem-get gpu_meshes id)
				(elem-set gpu_meshes id (shader-gui-mesh (. scene :mesh id))))
			(list (if files gpu_shiny gpu_pair) (ifn (elem-get gpu_meshes id) 0) vblock pblock)) draws))))
	(cond
		((eql drawn :error) (setq gpu_pair :nil gpu_failed :t))
		(drawn (setq gpu_drawn :t))
		;the GPU is busy, the next frame will do
		(gpu_drawn)
		(:t (. scene :draw *canvas* draws)
			(. *canvas* :swap +swap_write))))

(defun show-status ()
	(defq text (cat (str (. global_tasks :size)) " nodes, " (str (. links :size)) " links, "
		(str (length machines)) (if (= (length machines) 1) " machine" " machines")))
	(unless (eql (get :text *status*) text)
		(def *status* :text text)
		(.-> *status* :layout :dirty)))

(defun main ()
	(defq id :t select (task-mboxes +select_size) poll_que (list) changed :t canvas_size +size
		machines (list) links (Fmap 31) top_rate (n2r +quiet) zoom +real_1
		auto :nil rotx (const (n2r 0.4)) roty +real_0 rotz +real_0
		gpu_pair :nil gpu_shiny :nil gpu_drawn :nil gpu_failed :nil gpu_meshes (list)
		ball_mesh (Mesh-sphere +real_1 16) bar_mesh (Mesh-cylinder +real_1 +real_1 8)
		ball_proto (Scene-object ball_mesh (fixeds 1.0 1.0 1.0 1.0))
		bar_proto (Scene-object bar_mesh (fixeds 1.0 1.0 1.0 1.0))
		scene (Scene "root") world (Scene-node "world"))
	;the two meshes are made ready once, and every ball and bar shares them.
	;A ball is lit smooth, and shines
	(def ball_proto :smooth :t :shaders +shiny)
	(.-> world (:add_node ball_proto) (:add_node bar_proto))
	(.-> scene (:add_node world)
		(:set_translation +real_0 +real_0 (const (- +real_0 +focal_dist +real_2))))
	(. scene :draws +left +right +top +bottom +near +far (* canvas_size +scale))
	(defq global_tasks (Global create destroy))
	(set-auto :t)
	(bind '(x y w h) (apply view-locate (.-> *window* (:connect +event_layout) :pref_size)))
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(gui-add-front-rpc (. *window* :change x y w h))
	;it opens at its full size, and can then be made smaller
	(def (penv *canvas*) :min_width +min_size :min_height +min_size)
	(mail-timeout (elem-get select +select_frame_timer) +frame_rate 0)
	(mail-timeout (elem-get select +select_poll_timer) 1 0)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_layout)
						(bind '(w h) (. (penv *canvas*) :get_size))
						(set-canvas (max +min_size (min w h))))
					((= id +event_auto) (set-auto (not auto)))
					;a slider is moved, and it no longer turns by itself
					((= id +event_xrot) (set-auto :nil) (setq rotx (get-rot *xrot_slider*)))
					((= id +event_yrot) (set-auto :nil) (setq roty (get-rot *yrot_slider*)))
					((= id +event_zrot) (set-auto :nil) (setq rotz (get-rot *zrot_slider*)))
					((. *window* :event msg))))
			(+select_task
				;a child has started
				(defq child (getf msg +kn_msg_reply_id)
					node (. global_tasks :find (slice child +long_size -1)))
				(when node
					(def node :child child :timestamp (pii-time))
					(push poll_que child)))
			(+select_reply (node-heard msg))
			(+select_poll_timer
				(mail-timeout (elem-get select +select_poll_timer) +poll_rate 0)
				(. global_tasks :refresh +retry_timeout)
				(links-gather)
				(show-status)
				(each (# (mail-send %0 (elem-get select +select_reply))) poll_que)
				(clear poll_que))
			(:t ;frame timer
				(mail-timeout (elem-get select +select_frame_timer) +frame_rate 0)
				(when auto
					(setq rotx (% (+ rotx (const (n2r 0.003))) +real_2pi)
						roty (% (+ roty (const (n2r 0.01))) +real_2pi)
						rotz (% (+ rotz (const (n2r 0.002))) +real_2pi))
					(set-rot *xrot_slider* rotx)
					(set-rot *yrot_slider* roty)
					(set-rot *zrot_slider* rotz))
				(spring-step)
				(pose-scene)
				(draw-frame))))
	(. global_tasks :close)
	(each (# (if %0 (canvas-mesh-destroy %0))) gpu_meshes)
	(if gpu_pair (canvas-shader-destroy gpu_pair))
	(if gpu_shiny (canvas-shader-destroy gpu_shiny))
	(gui-sub-rpc *window*))

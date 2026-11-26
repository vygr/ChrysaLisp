(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/task/global.inc")
(import "service/source/app.inc")
(import "lib/net/links.inc")
(import "lib/math/mesh.inc")
(import "lib/math/scene.inc")
(import "lib/gpu/render_farm.inc")
(import "./app.inc")
(import "./map.inc")

;The network as it is, in three dimensions. A ball for each node, the
;color of the machine it is on, bigger the more tasks it has, and whiter the harder it works. A
;bar for each link, hotter the more mail it carries, from blue through red
;to white. Each node is asked who its
;links are to, so it is the network that is there, not the one that was
;launched, and it is asked again as nodes come and go.
;
;It lays itself out as a bedspring. Every link is a spring, every node
;pushes every other away, and they are left to settle. A link that is busy
;pulls harder, so nodes that talk draw together. Dr Ian Thomas did a
;bedspring model of a network for Taos, this is in his honour.

(enums +event +event_user
	(enum auto)
	(enum xrot yrot zrot))

(enums +select 0
	(enum main task reply tip frame_timer poll_timer))

;what is added to a length that is divided by, so that none is nothing.
;The number as it is written is a fixed, and one smaller than this is 0
(defq +tiny (n2d 0.0001))
(defq +size 640 +min_size 320 +scale 1 +frame_rate (/ 1000000 20) +poll_rate (/ 1000000 4)
	+retry_timeout 5000000
	+focal_dist +double_2 +near +focal_dist +far (+ +near +double_4)
	+top (* +focal_dist +double_1/2) +bottom (* +focal_dist +double_-1/2)
	+left (* +focal_dist +double_-1/2) +right (* +focal_dist +double_1/2)
	+ball_size (n2d 0.11) +bar_size (n2d 0.011))

(ui-window *window* ()
	(ui-title-bar _ "Network Map" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_auto) +event_auto))
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
	(ui-flow _ (:flow_flags +flow_up_fill)
		(ui-label *status* (:text "..." :font :font_body))
		(ui-backdrop *backdrop* (:style :plain :paper +argb_black :min_width +min_size :min_height +min_size
				:want_width +size :want_height +size)
			(ui-element *canvas* (Canvas +size +size +scale)))))

;the steps of a turn's slider; the most tasks a node is shown loaded
;by; a node's green and blue, cool and white; the turn it starts at, and
;the turn of the auto spin a tick about each way
(defq +rot_steps (n2d 1000) +load_most (n2d 40) +cool_green (n2d 0.35) +white_green (n2d 0.8)
	+cool_blue (n2d 0.6) +white_blue (n2d 0.9) +start_rotx (n2d 0.4) +spin_x (n2d 0.003)
	+spin_y (n2d 0.01) +spin_z (n2d 0.002))

(defun tooltips (mbox)
	(def *window* :tip_mbox mbox)
	(ui-tool-tips *main_toolbar*
		'("auto")))

(defun set-rot (slider angle)
	(set (. slider :dirty) :value
			(n2i (/ (* angle +rot_steps) +double_2pi))))

(defun get-rot (slider)
	(/ (* (n2d (get :value slider)) +double_2pi) +rot_steps))

(defun set-auto (on)
	;it turns by itself, or it is turned by the sliders. The button is
	;lit while it turns by itself
	(defq button (first (. *main_toolbar* :children)))
	(undef (. button :dirty) :paper)
	(if (setq auto on)
		(def button :paper (canvas-brighter (get :paper *main_toolbar*)))))

(defun set-canvas (size)
	;the window is a new size, and so is the picture that is wanted, a
	;square that fits. No canvas is made of it here: a window that is being
	;dragged says a new size many times a second, and the frame timer
	;makes a canvas of the last of them when the next frame is to be drawn,
	;(new-canvas). The picture on show still fills what it is in
	(bind '(w h) (. *backdrop* :get_size))
	(. (ifn shown *canvas*) :change 0 0 w h)
	(. *window* :layout)
	(setq want_size size))

(defun new-canvas ()
	;a canvas of the size that is wanted, if that is not the size there
	;is, drawn on from this frame. It is not put on show till a frame of it
	;is whole, (show-canvas): the one on show stays till then, so nothing
	;is seen that is not a picture, and nothing waits for a frame
	(unless (= want_size canvas_size)
		(unless shown (setq shown *canvas*))
		(setq *canvas* (Canvas want_size want_size +scale) canvas_size want_size)
		(. render :canvas *canvas*)))

(defun show-canvas ()
	;a frame of the new canvas is whole, it goes on show in place of the old
	(. shown :sub)
	(. *backdrop* :add_child *canvas*)
	(bind '(w h) (. *backdrop* :get_size))
	(. *canvas* :change 0 0 w h)
	(. *window* :layout)
	(setq shown :nil))

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
		:idle 0 :time 0 :busy +double_0
		:pos (doubles (rnd) (rnd) (rnd)) :vel (doubles +double_0 +double_0 +double_0)
		:ball (share (Scene-object ball_mesh (fixeds 1.0 1.0 1.0 1.0)) ball_proto))
	(open-task child_task key +kn_call_pin 0 (elem-get select +select_task))
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
		(machines-heard system)
		(node-busy node (getf msg +reply_idle) (getf msg +reply_time))
		(def node :timestamp now :tasks (getf msg +reply_task_count) :system system
			:links (map! (lambda () (defq link (slice msg (+ +reply_links (* (!) +link_size)) (+ +reply_links (* (inc (!)) +link_size))))
					(list (getf link +link_peer_node) (getf link +link_sent))) '() 0 (getf msg +reply_num_links)))
		(push poll_que (get :child node))))

(defun pose-scene ()
	;put each ball and each bar where the bedspring has it
	(defq objs (list))
	(. global_tasks :each (lambda (key node)
			(defq ball (get :ball node) pos (get :pos node)
				;as big on the screen however far out it is all drawn from
				load (/ (n2d (min 40 (get :tasks node))) +load_most)
				busy (get :busy node)
				size (/ (* +ball_size (+ +double_1 load)) zoom))
			(.-> ball (:set_translation (first pos) (second pos) (third pos)) (:set_scale size size size))
			;the color of its machine, and whiter the harder it works
			(def ball :color (apply (const fixeds) (cat (list 1.0)
						(map (# (n2q (+ %0 (* (- +double_1 %0) busy))))
							(if (defq at (find (get :system node) machines))
								(hue-color (elem-get machine_hues at))
								(machine-color (get :system node)))))))
			(push objs ball)))
	(. links :each (lambda (name link)
			;a link from a node to itself has no length, there is no bar to draw
			(when (and (defq a (. global_tasks :find (get :a link)))
					(defq b (. global_tasks :find (get :b link)))
					(not (eql a b)))
				;a bar is a cylinder that stands on y, one long. Its matrix is
				;made here, y laid along the link, and two more across it
				(defq pa (get :pos a) pb (get :pos b) d (nums-sub pb pa)
					mid (nums-scale (nums-add pa pb) +double_1/2)
					across (if (< (abs (first d)) (abs (second d))) (doubles +double_1 +double_0 +double_0)
						(doubles +double_0 +double_1 +double_0))
					u (apply (const doubles) (vector-cross-3d d across))
					thick (/ +bar_size zoom)
					u (nums-scale u (/ thick (sqrt (+ (nums-dot u u) +tiny))))
					;u, the link and v go round the way x, y and z do. The other
					;way and the bar is turned inside out, it is its far side
					;that is seen, lit as that side is
					v (apply (const doubles) (vector-cross-3d u d))
					v (nums-scale v (/ thick (sqrt (+ (nums-dot v v) +tiny))))
					;cold is a dim blue, half way is red, and hot is white
					heat (get :heat link) bar (get :bar link)
					red (min +double_1 (* heat +double_2)) cool (- +double_1 red)
					white (max +double_0 (- (* heat +double_2) +double_1)))
				;a machine to machine link is twice as thick
				(unless (eql (get :system a) (get :system b))
					(nums-scale u +double_2 u) (nums-scale v +double_2 v))
				(def bar :dirty :nil :matrix (doubles
						(first u) (first d) (first v) (first mid)
						(second u) (second d) (second v) (second mid)
						(third u) (third d) (third v) (third mid)
						+double_0 +double_0 +double_0 +double_1)
					:color (fixeds 1.0
						(n2q (+ (* cool +double_1/4) red))
						(n2q (+ (* cool +cool_green) (* red +double_1/5) (* white +white_green)))
						(n2q (+ (* cool +cool_blue) (* red +double_1/10) (* white +white_blue)))))
				(push objs bar))))
	(. world :set_children objs)
	(.-> world (:set_scale zoom zoom zoom) (:set_rotation rotx roty rotz)))

(defun draw-frame ()
	;a frame, by the render: the GPU if this host has one that can, else
	;here. Each thing is drawn with the pair of shaders of its kind, the
	;scene says which
	(. render :frame (. scene :draws +left +right +top +bottom +near +far (* canvas_size +scale))))

(defun show-status ()
	(defq text (cat (str (. global_tasks :size)) " nodes, " (str (. links :size)) " links, "
			(str (length machines)) (if (= (length machines) 1) " machine" " machines")))
	(. *status* :set_text text))

(defun main ()
	;each node's child is read from this system, by its @Source, the
	;natives it binds too where that node has not the same of them
	(defq child_task (source-task (cat *app_root* "child.lisp")))
	(defq id :t select (task-mboxes +select_size) poll_que (list) changed :t canvas_size +size
		machines (list) machine_hues (list) links (Fmap 31) top_rate (n2d +quiet) zoom +double_1
		auto :nil rotx +start_rotx roty +double_0 rotz +double_0
		;what draws the frames, lib/gpu/render_farm.inc. The size of
		;picture that is wanted, the last a window being sized said, and
		;while a canvas of a new size has no whole frame yet, the one that
		;is still on show
		render (Render-tris *canvas* +scene_vertex_file +scene_pixel_file :t)
		want_size +size shown :nil
		ball_mesh (Mesh-sphere +double_1 16) bar_mesh (Mesh-cylinder +double_1 +double_1 8)
		ball_proto (Scene-object ball_mesh (fixeds 1.0 1.0 1.0 1.0))
		bar_proto (Scene-object bar_mesh (fixeds 1.0 1.0 1.0 1.0))
		scene (Scene "root") world (Scene-node "world"))
	;the two meshes are made ready once, and every ball and bar shares them.
	;A ball is lit smooth, and shines. A bar does not: a highlight is white
	;whatever is under it, and a bar's color is how hot the link is
	(def ball_proto :smooth :t :shaders +scene_shiny_files)
	(def bar_proto :shaders +scene_matte_files)
	(.-> world (:add_node ball_proto) (:add_node bar_proto))
	(.-> scene (:add_node world)
		(:set_translation +double_0 +double_0 (const (- +double_0 +focal_dist +double_2))))
	(. scene :draws +left +right +top +bottom +near +far (* canvas_size +scale))
	;the scene has the meshes, and draws a frame here when the GPU can not
	(def render :clear 0
		:mesh_fn (lambda (id) (if (< -1 id (length (get :meshes scene))) (. scene :mesh id)))
		:here_fn (lambda (canvas draws) (. scene :draw canvas draws)))
	(defq global_tasks (Global create destroy))
	(set-auto :t)
	(tooltips (elem-get select +select_tip))
	(. *window* :connect +event_layout)
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(. *window* :open)
	(mail-timeout (elem-get select +select_frame_timer) +frame_rate 0)
	(mail-timeout (elem-get select +select_poll_timer) 1 0)
	(while id
		;the render's mail is read with the app's, before the timers, and
		;dealt with
		(bind '(idx msg) (. render :read select +select_frame_timer))
		(case (ifn idx -1)
			(-1)
			(+select_main
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_layout)
						(bind '(w h) (. *backdrop* :get_size))
						(set-canvas (max +min_size (min w h))))
					((= id +event_auto) (set-auto (not auto)))
					;a slider is moved, and it no longer turns by itself
					((= id +event_xrot) (set-auto :nil) (setq rotx (get-rot *xrot_slider*)))
					((= id +event_yrot) (set-auto :nil) (setq roty (get-rot *yrot_slider*)))
					((= id +event_zrot) (set-auto :nil) (setq rotz (get-rot *zrot_slider*)))
					((. *window* :event msg))))
			(+select_task
				;a child has started
				;a node that could not start one is left be from now on
				(defq child (getf msg +kn_msg_reply_id)
					node (. global_tasks :reply msg))
				(when node
					(def node :child child :timestamp (pii-time))
					(push poll_que child)))
			(+select_reply (node-heard msg))
			(+select_tip
				(. *window* :tip msg))
			(+select_poll_timer
				(mail-timeout (elem-get select +select_poll_timer) +poll_rate 0)
				(. global_tasks :refresh +retry_timeout)
				(links-gather)
				(machines-prune)
				(show-status)
				(each (# (mail-send %0 (elem-get select +select_reply))) poll_que)
				(clear poll_que))
			(:t ;frame timer
				(mail-timeout (elem-get select +select_frame_timer) +frame_rate 0)
				(when auto
					(setq rotx (% (+ rotx +spin_x) +double_2pi)
						roty (% (+ roty +spin_y) +double_2pi)
						rotz (% (+ rotz +spin_z) +double_2pi))
					(set-rot *xrot_slider* rotx)
					(set-rot *yrot_slider* roty)
					(set-rot *zrot_slider* rotz))
				(spring-step)
				;the next frame, if the last is not still out with the GPU
				(unless (. render :busy)
					(new-canvas)
					(pose-scene)
					(draw-frame))
				(. render :tick)))
		;a canvas of a new size goes on show once a frame of it is whole
		(and shown (not (. render :busy)) (show-canvas)))
	(. global_tasks :close)
	(. render :close)
	(. *window* :close))

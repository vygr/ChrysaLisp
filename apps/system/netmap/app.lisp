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
	(enum close))

(enums +select 0
	(enum main task reply frame_timer poll_timer))

(defq +size 640 +scale 1 +frame_rate (/ 1000000 20) +poll_rate (/ 1000000 4)
	+retry_timeout (task-timeout 5)
	+focal_dist +real_2 +near +focal_dist +far (+ +near +real_4)
	+top (* +focal_dist +real_1/2) +bottom (* +focal_dist +real_-1/2)
	+left (* +focal_dist +real_-1/2) +right (* +focal_dist +real_1/2)
	;the bedspring. How hard nodes push apart, how stiff a link is and how
	;long it would be, how much a busy link is more, the pull to the
	;middle, what is lost each step, and the step
	+k_push (n2r 0.02) +k_spring (n2r 4.0) +rest (n2r 0.5) +k_heat (n2r 1.0)
	+k_middle (n2r 0.3) +damp (n2r 0.85) +dt (n2r 0.05)
	;the bytes a link carries between polls that is no more than idle
	+quiet 4096
	;the shaders a ball is drawn with
	+shiny ''("lib/gpu/shaders/shiny_vertex.shader" "lib/gpu/shaders/shiny_lit.shader")
	;how much of the way a link's flow goes, each poll, to what it carried
	;in that poll. A fifth, at 4 polls a second, is a second or so
	+ease (n2r 0.2)
	+ball_size (n2r 0.09) +bar_size (n2r 0.016)
	;how far out the furthest node is drawn. The window is 2 from the
	;middle to an edge, where the middle of it all is
	+fit (n2r 1.5))

(ui-window *window* ()
	(ui-title-bar _ "Network Map" (0xea19) +event_close)
	(ui-flow _ (:flow_flags +flow_up_fill)
		(ui-label *status* (:text "..." :font *env_body_font*))
		(ui-backdrop _ (:style :plain :color +argb_black :min_width +size :min_height +size)
			(ui-element *canvas* (Canvas +size +size +scale) (:color 0)))))

(defun rnd ()
	;somewhere between -1/2 and 1/2
	(/ (n2r (- (random 1000) 500)) (const (n2r 1000))))

(defun machine-color (system)
	; (machine-color system) -> (r g b)
	;the color of a machine, from its id, so it is the same color on every
	;desktop and every time. The id picks where round the colors it is, and
	;it is kept bright, and not too deep, it is whitened by load
	(memoize system (progn
		(defq h (if (< (length system) +node_id_size) 200
				(% (logand (logxor (get-long system 0) (>>> (get-long system 0) 17)
					(get-long system 8) (>>> (get-long system 8) 29)) 0x7fffffff) 360))
			f (/ (n2r (% h 60)) (const (n2r 60)))
			lo (const (n2r 0.25)) up (+ lo (* f (const (n2r 0.75))))
			down (- +real_1 (* f (const (n2r 0.75)))))
		(case (/ h 60)
			(0 (list +real_1 up lo))
			(1 (list down +real_1 lo))
			(2 (list lo +real_1 up))
			(3 (list lo down +real_1))
			(4 (list up lo +real_1))
			(:t (list +real_1 lo down)))) 31))

(defun share (obj proto)
	;an object that draws the mesh a first one of its kind was drawn with,
	;so the scene has the mesh once, and so has the GPU. And is lit as it
	;is, a ball shines
	(def obj :corners_of (get :corners_of proto) :corners_id (get :corners_id proto)
		:ball (get :ball proto))
	(if (def? :shaders proto) (def obj :shaders (get :shaders proto)))
	obj)

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

(defun links-gather ()
	;the links there are, from what each end says, each once, with how
	;much mail both ends have put on it. How fast that is growing, taken
	;over a second or so, is its flow, and its flow set against the most
	;there has been of late is how hot it is. So heat comes up and goes
	;down smoothly, a flow that lasts is hotter than a burst, and the
	;bedspring is not tugged about
	(defq seen (Fmap 31) now (pii-time))
	(. global_tasks :each (lambda (key node)
		(each (lambda ((peer sent))
			(when (. global_tasks :find peer)
				(defq name (if (< (cmp key peer) 0) (cat key peer) (cat peer key)))
				(. seen :insert name (+ sent (ifn (. seen :find name) 0)))))
			(get :links node))))
	(defq old links)
	(setq links (Fmap 31))
	(. seen :each (lambda (name sent)
		(unless (defq link (. old :find name))
			(setq changed :t)
			(def (setq link (env 1)) :a (slice name 0 +node_id_size) :b (slice name +node_id_size -1)
				:sent sent :flow +real_0 :heat +real_0
				:bar (share (Scene-object bar_mesh (fixeds 1.0 0.3 0.4 0.6)) bar_proto)))
		;the most there has been is of what a poll carried, not of the
		;flow, or the busiest link would be white the moment it began
		(defq rate (n2r (max 0 (- sent (get :sent link)))) flow (get :flow link)
			flow (+ flow (* (- rate flow) +ease)))
		(setq top_rate (max top_rate rate))
		(def link :sent sent :flow flow :heat (/ flow top_rate))
		(. links :insert name link)))
	(if (/= (. links :size) (. old :size)) (setq changed :t))
	;the busiest fades, so a burst long gone does not leave all else cold
	;but not to nothing, or the pings of an idle network would be hot
	(setq top_rate (max (const (n2r +quiet)) (* top_rate (const (n2r 0.97))))))

(defun spring-step ()
	;a step of the bedspring
	(defq nodes (list) forces (list))
	(. global_tasks :each (lambda (key node) (push nodes node)
		(push forces (nums-scale (get :pos node) (neg +k_middle)))))
	;every node pushes every other away
	(each! (lambda (node force)
		(defq i (!) p (get :pos node))
		(each! (lambda (node1 force1)
			(defq d (nums-sub p (get :pos node1)) d2 (+ (nums-dot d d) (const (n2r 0.0001)))
				f (nums-scale d (/ +k_push (* d2 (sqrt d2)))))
			(nums-add force f force)
			(nums-sub force1 f force1))
			(list nodes forces) (inc i)))
		(list nodes forces))
	;every link pulls its two ends to the length it would be, a hot one
	;harder and to a shorter length
	(. links :each (lambda (name link)
		(when (and (defq a (. global_tasks :find (get :a link)))
				(defq b (. global_tasks :find (get :b link))))
			(defq d (nums-sub (get :pos b) (get :pos a)) len (sqrt (+ (nums-dot d d) (const (n2r 0.0001))))
				warm (+ +real_1 (* +k_heat (get :heat link)))
				f (nums-scale d (/ (* +k_spring warm (- len (/ +rest warm))) len))
				fa (elem-get forces (find a nodes)) fb (elem-get forces (find b nodes)))
			(nums-add fa f fa)
			(nums-sub fb f fb))))
	(defq reach (const (n2r 0.01)) middle (reals +real_0 +real_0 +real_0))
	(each (lambda (node force)
		(defq vel (get :vel node) pos (get :pos node))
		(nums-scale (nums-add vel (nums-scale force +dt) vel) +damp vel)
		(nums-add pos (nums-scale vel +dt) pos)
		(nums-add middle pos middle))
		nodes forces)
	;kept about its own middle, it does not drift off
	(when (nempty? nodes)
		(nums-scale middle (/ +real_1 (n2r (length nodes))) middle)
		(each (lambda (node)
			(defq pos (get :pos node))
			(nums-sub pos middle pos)
			(setq reach (max reach (nums-dot pos pos)))) nodes))
	;how big it all is, so that it can be drawn to fit, eased
	(setq zoom (+ zoom (* (- (/ +fit (sqrt reach)) zoom) (const (n2r 0.1))))))

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
	(.-> world (:set_scale zoom zoom zoom)
		(:set_rotation (const (n2r 0.4)) spin +real_0)))

(defun draw-frame ()
	;a frame, by the GPU of the GUI if it has one that can, else here
	(defq draws (. scene :draws +left +right +top +bottom +near +far (* +size +scale)))
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
	(defq id :t select (task-mboxes +select_size) poll_que (list) changed :t
		machines (list) links (Fmap 31) top_rate (n2r +quiet) zoom +real_1 spin +real_0
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
	(. scene :draws +left +right +top +bottom +near +far (* +size +scale))
	(defq global_tasks (Global create destroy))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(gui-add-front-rpc (. *window* :change x y w h))
	(mail-timeout (elem-get select +select_frame_timer) +frame_rate 0)
	(mail-timeout (elem-get select +select_poll_timer) 1 0)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
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
				(setq spin (% (+ spin (const (n2r 0.01))) +real_2pi))
				(spring-step)
				(pose-scene)
				(draw-frame))))
	(. global_tasks :close)
	(each (# (if %0 (canvas-mesh-destroy %0))) gpu_meshes)
	(if gpu_pair (canvas-shader-destroy gpu_pair))
	(if gpu_shiny (canvas-shader-destroy gpu_shiny))
	(gui-sub-rpc *window*))

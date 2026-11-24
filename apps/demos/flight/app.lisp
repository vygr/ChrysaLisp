(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/math/matrix.inc")
(import "lib/gpu/render_farm.inc")
(import "lib/gpu/vp.inc")

;A flight over a land that is made as it goes, drawn by a (Render-tris),
;lib/gpu/render_farm.inc, which has the GPU draw it, or with none the
;nodes of this machine, a strip of the frame each, or this task if there
;is only the one. A frame is two draws, each by its own pair of shaders:
;the sky, at the back, and the land, a grid that is moved under the eye
;and raised to the hills of a noise in its vertex shader, so the land
;rolls by and nothing of it is made again. The flight goes round a figure
;of eight, leaning into each turn, left and then right; or it finds its
;own way along the valleys and over the lakes, low, by the height of the
;land the shaders have, land.shader, which Lisp asks too, (shader-vp-func)

(enums +event +event_user
	(enum path mode))

;the timer is last, what is first in the list is what is read first, and
;a frame can take longer than a tick
(enums +select 0
	(enum main timer))

;the frame's two sizes, the min and max buttons
(defq +small ''(640 480) +large ''(1024 768))

(defq +timer_rate (/ 1000000 30)
	;the grid, cells each way and how big a cell is, the GPU's, and the
	;CPU's of cells twice as big over the same ground: a quarter of the
	;vertices, each of which is five samples of the land's height. The rest
	;of the frame is the same, the shaders and all they are given
	+cells 128 +cell (n2d 6.5) +cells_cpu 64 +cell_cpu (n2d 13)
	;the flight, a figure of eight: how far it reaches across and along,
	;how fast, how high, how it looks down, and how hard it leans into a
	;turn, as a plane does for a pull of so much
	+reach_x (n2d 640) +reach_z (n2d 320) +speed (n2d 24) +altitude (n2d 85) +pull (n2d 9)
	+pitch_eight (n2d 0.18)
	;the flight along the valleys, as a jet low down: how far ahead and how
	;far round it looks at the land, how hard it turns to the lower side,
	;how fast at most it turns, a second, and how slowly it comes to that,
	;how far over the land it keeps, how it looks down, how fast it may
	;climb and come down, a second, and how slowly it comes to that
	+ahead (n2d 120) +aside (n2d 0.45) +steer (n2d 0.004) +turn_most (n2d 0.25) +turn_ease (n2d 0.5)
	+clearance (n2d 24) +pitch_valley (n2d 0.1) +climb (n2d 16) +sink (n2d 8) +rise_ease (n2d 0.8)
	;how high the frame is at a distance of 1, its width of its shape, and
	;how near and far it sees
	+spread_y (n2d 0.5) +near (n2d 1) +far (n2d 520)
	+land_vertex (cat *app_root* "land_vertex.shader") +land_lit (cat *app_root* "land_lit.shader")
	sky_files (list (cat *app_root* "sky_vertex.shader") (cat *app_root* "sky_lit.shader"))
	;the meshes, by their numbers
	+mesh_land 0 +mesh_sky 1 +mesh_land_cpu 2
	;the sun, and the haze at the horizon and the blue above
	sun (doubles (n2d 0.55) (n2d 0.42) (n2d -0.72)) haze (doubles (n2d 0.74) (n2d 0.80) (n2d 0.86))
	above (doubles (n2d 0.28) (n2d 0.46) (n2d 0.78))
	select :nil id :t render :nil frames 0 since (pii-time) began (pii-time)
	;the size wanted, the size of the canvas drawn on, and the canvas still
	;on show till a frame of a new one is whole
	want +small size +small shown :nil
	;how far round the eight the flight is, and when it was last moved on
	round_at +double_0 moved (pii-time)
	;the flight, the eight or along the valleys: where it is and its way,
	;level, how high, how fast it turns, and the middle of the eight
	path :eight place (doubles +double_0 +double_0) way (doubles +double_0 +double_1)
	height +altitude rising +double_0 turning +double_0 middle (doubles +double_0 +double_0)
	;the height of the land at a place, of land.shader, the shaders' own
	land_height :nil
	;CPU or GPU, the flight and the size, as they were when last closed,
	;(config-save)
	*config* :nil +config_version 1 +config_file (cat (get :home *env*) "flight.tre"))

(defun make-canvas (size)
	; (make-canvas size) -> canvas
	;its pixels are in shared memory if the host has it, the nodes of this
	;machine then draw on them, a strip each
	(bind '(w h) size)
	(ifn (canvas-shared w h 1) (Canvas w h 1)))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Flight" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-radio-bar *path* ("Eight" "Valleys") (:font :font_body)) :connect +event_path)
		(. (ui-radio-bar *mode* ("CPU" "GPU") (:font :font_body :paper :paper_toolbar2)) :connect +event_mode)
		(ui-label *status* (:text "..." :font :font_body)))
	(ui-backdrop *backdrop* (:style :plain :paper +argb_black :min_width (first +small) :min_height (second +small))
		(ui-element *canvas* (make-canvas +small))))

(defun land-verts (cells cell)
	; (land-verts cells cell) -> doubles
	;the grid of cells each way, each cell big, its middle at 0 0, a vertex
	;a place across and along, two triangles a cell
	(defq out (list) half (* (n2d (/ cells 2)) cell))
	(each! (lambda () (defq z (- (* (n2d (!)) cell) half) z1 (+ z cell))
			(each! (lambda () (defq x (- (* (n2d (!)) cell) half) x1 (+ x cell))
					(push out x z x1 z x1 z1 x z x1 z1 x z1)) '() 0 cells)) '() 0 cells)
	(apply (const doubles) out))

(defun sky-verts ()
	; (sky-verts) -> doubles
	;two triangles over all of the frame, by its corners
	(doubles +double_-1 +double_-1 +double_1 +double_-1 +double_1 +double_1
		+double_-1 +double_-1 +double_1 +double_1 +double_-1 +double_1))

(defun mode-show ()
	;the button as the render has it: the GPU is not to be had on a host
	;with none
	(. *mode* :set_selected (if (eql (. render :mode) :gpu) 1 0)))

(defun argb (rgb)
	; (argb rgb) -> argb
	(+ 0xff000000 (reduce (lambda (a c) (+ (<< a 8) (n2i (* c (n2d 255))))) rgb 0)))

(defun blend (a b ka kb)
	; (blend a b ka kb) -> doubles
	;a times ka and b times kb, two ways in the world added
	(apply (const doubles) (map (lambda (x y) (+ (* x ka) (* y kb))) a b)))

(defun path-pace (at)
	; (path-pace at) -> double
	;how far the eight goes for a step of at there, so the flight is moved
	;on by its speed, as fast round the ends as through the middle
	(defq dx (* +reach_x (cos at)) dz (* (n2d 2) +reach_z (cos (* (n2d 2) at))))
	(sqrt (+ (* dx dx) (* dz dz))))

(defun eight-point (at)
	; (eight-point at) -> doubles
	;where on the eight, across and along, for its middle at 0 0
	(doubles (* +reach_x (sin at)) (* +reach_z (sin (* (n2d 2) at)))))

(defun ease-height (target dt)
	;the height on by how fast it rises, which is eased toward what would
	;bring it to the target, no faster up or down than it may: a climb and a
	;dive in a curve, not at a corner
	(defq want (max (neg +sink) (min +climb (* (- target height) (n2d 0.6)))))
	(setq rising (+ rising (* (- want rising) (min +double_1 (* dt +rise_ease))))
		height (+ height (* rising dt))))

(defun eight-pose (dt)
	; (eight-pose dt) -> (fx fz lean)
	;moved on round the eight by its speed, as fast round the ends as
	;through the middle: its way, level, and its lean into the turn, whose
	;tangent is speed squared times the bend over the pull
	(setq round_at (+ round_at (/ (* +speed dt) (path-pace round_at))))
	(defq at round_at two (* (n2d 2) at)
		dx (* +reach_x (cos at)) dz (* (n2d 2) +reach_z (cos two))
		ddx (neg (* +reach_x (sin at))) ddz (neg (* (n2d 4) +reach_z (sin two)))
		pace (sqrt (+ (* dx dx) (* dz dz))))
	(setq place (blend middle (eight-point at) +double_1 +double_1)
		way (doubles (/ dx pace) (/ dz pace)))
	(ease-height +altitude dt)
	(list (/ dx pace) (/ dz pace) (/ (* +speed +speed (/ (- (* dx ddz) (* dz ddx)) (* pace pace pace))) +pull)))

(defun land-at (x z)
	; (land-at x z) -> double
	;the land's height there, or the water's where it is under
	(max (n2d -4) (land_height (doubles x z))))

(defun valley-pose (dt)
	; (valley-pose dt) -> (fx fz lean)
	;on along its way, turning to the lower land ahead, left or right, by
	;how much lower, no faster than it may, and easing into a turn as a
	;plane does; kept over the highest land under it and ahead, so it
	;climbs before a ridge. Its lean is speed times how fast it turns
	(bind '(px pz) place)
	(bind '(fx fz) way)
	(defq c (cos +aside) s (sin +aside)
		;the ways ahead a little left, and a little right
		lx (+ (* fx c) (* fz s)) lz (- (* fz c) (* fx s))
		rx (- (* fx c) (* fz s)) rz (+ (* fz c) (* fx s))
		;the land a little left and a little right, at three distances, the
		;nearer the more
		left (+ (* +double_2 (land-at (+ px (* lx +ahead)) (+ pz (* lz +ahead))))
			(land-at (+ px (* lx +ahead +double_2)) (+ pz (* lz +ahead +double_2)))
			(* (n2d 0.5) (land-at (+ px (* lx +ahead (n2d 3))) (+ pz (* lz +ahead (n2d 3))))))
		right (+ (* +double_2 (land-at (+ px (* rx +ahead)) (+ pz (* rz +ahead))))
			(land-at (+ px (* rx +ahead +double_2)) (+ pz (* rz +ahead +double_2)))
			(* (n2d 0.5) (land-at (+ px (* rx +ahead (n2d 3))) (+ pz (* rz +ahead (n2d 3))))))
		;to the left is positive, toward the lower
		target (max (neg +turn_most) (min +turn_most (* +steer (- right left)))))
	(setq turning (+ turning (* (- target turning) (min +double_1 (* dt +turn_ease)))))
	(defq a (* turning dt) ca (cos a) sa (sin a)
		nx (+ (* fx ca) (* fz sa)) nz (- (* fz ca) (* fx sa)))
	(setq way (doubles nx nz)
		place (doubles (+ px (* nx +speed dt)) (+ pz (* nz +speed dt))))
	(bind '(px pz) place)
	(ease-height (+ +clearance (max (land-at px pz) (land-at (+ px (* nx (n2d 60))) (+ pz (* nz (n2d 60))))
			(land-at (+ px (* nx (n2d 120))) (+ pz (* nz (n2d 120))))
			(land-at (+ px (* nx (n2d 240))) (+ pz (* nz (n2d 240)))))) dt)
	(list nx nz (neg (/ (* +speed turning) +pull))))

(defun path-set (to)
	;the flight to the eight or along the valleys, from where it is: the
	;valleys go on from here, the eight is moved so that it is here
	(when (eql to :eight)
		(setq middle (blend place (eight-point round_at) +double_1 +double_-1)))
	(setq path to turning +double_0))

(defun draw-frame ()
	;where the flight is, the way it looks, and the frame: the sky, then the
	;land, a grid moved to under the eye by whole cells. Forward is along
	;its way, looking down a little, and it leans into a turn by its lean,
	;the tangent of the angle, made of its cosine and sine, no angle
	(defq now (pii-time) dt (/ (n2d (- now moved)) (n2d 1000000)))
	(setq moved now)
	(bind '(fx fz lean) (if (eql path :eight) (eight-pose dt) (valley-pose dt)))
	;the nose up a little as it climbs and down as it comes down
	(defq pitch (- (if (eql path :eight) +pitch_eight +pitch_valley) (* (n2d 0.5) (/ rising +speed)))
		pitch_cos (cos pitch) pitch_sin (sin pitch)
		lean_cos (recip (sqrt (+ +double_1 (* lean lean)))) lean_sin (* lean lean_cos)
		level (doubles (neg fz) +double_0 fx)
		forward (doubles (* fx pitch_cos) (neg pitch_sin) (* fz pitch_cos))
		upward (doubles (* fx pitch_sin) pitch_cos (* fz pitch_sin))
		right (blend level upward lean_cos (neg lean_sin)) up (blend upward level lean_cos lean_sin)
		eye (doubles (first place) height (second place))
		;how it looks, its rows the eye's ways, it looks along minus z
		look (cat right (doubles +double_0) up (doubles +double_0)
			(apply (const doubles) (map (const neg) forward)) (doubles +double_0 +double_0 +double_0 +double_0 +double_1))
		;the frame's width at a distance of 1, of the shape of the canvas
		spread_x (/ (* +spread_y (n2d (first size))) (n2d (second size)))
		lens (mat4x4-frustum (neg spread_x) spread_x +spread_y (neg +spread_y) +near +far)
		view (mat4x4-mul lens (mat4x4-mul look (mat4x4-translate (neg (first eye)) (neg (second eye)) (neg (third eye)))))
		seconds (/ (n2d (- moved began)) (n2d 1000000))
		;the GPU's land, or the CPU's of bigger cells, moved under the eye by
		;whole cells of it
		gpu (eql (. render :mode) :gpu) cell (if gpu +cell +cell_cpu)
		shift (doubles (* (n2d (n2i (/ (first eye) cell))) cell) (* (n2d (n2i (/ (third eye) cell))) cell)))
	(. render :frame (list
		(list +mesh_sky '()
			(list (list 'forward forward) (list 'right right) (list 'up up)
				(list 'spread (doubles spread_x +spread_y)) (list 'sun sun) (list 'haze haze) (list 'above above))
			'() sky_files)
		(list (if gpu +mesh_land +mesh_land_cpu) (list (list 'lens view) (list 'shift shift) (list 'eye eye))
			(list (list 'sun sun) (list 'haze haze) (list 'above above) (list 'eye eye)
				(list 'time seconds) (list 'rough +double_1) (list 'shadows 12)))))
	(++ frames))

(defun set-size (wanted)
	;the window the size of the frame wanted, now; the canvas of it is made
	;when the next frame is given, (new-canvas), and the one there is shown
	;the size till then
	(bind '(w h) wanted)
	(def *backdrop* :min_width w :min_height h :want_width w :want_height h)
	(. *window* :fit)
	(. (ifn shown *canvas*) :change 0 0 w h)
	(. *window* :layout)
	(setq want wanted))

(defun new-canvas ()
	;a canvas of the size that is wanted, if that is not the size there is,
	;drawn on from this frame. It is not put on show till a frame of it is
	;whole, (show-canvas): the one on show stays till then, so nothing is
	;seen that is not a picture
	(unless (eql want size)
		(unless shown (setq shown *canvas*))
		(setq *canvas* (make-canvas want) size want)
		(. render :canvas *canvas*)))

(defun show-canvas ()
	;a frame of the new canvas is whole: it goes on show, and the old one,
	;whose pixels may be shared, is let go of
	(.-> shown :sub :free)
	(. *backdrop* :add_child *canvas*)
	(bind '(w h) size)
	(. *canvas* :change 0 0 w h)
	(. *window* :layout)
	(setq shown :nil))

(defun config-default ()
	;the first time: on the GPU if there is one
	(scatter (Emap) :version +config_version :gpu :t :path :eight :large :nil))

(defun config-load ()
	;the defaults with what was kept laid over them,
	;docs/ai_digest/app_configuration.md
	(setq *config* (config-default))
	(and (defq old (with-read-lock +config_file
				(tree-load (file-stream +config_file))))
		(<= (ifn (. old :find :version) 0) +config_version)
		(. old :each (lambda (key val)
			(unless (eql key :version) (. *config* :insert key val))))))

(defun config-save ()
	(scatter *config* :gpu (= (. *mode* :get_selected) 1) :path path :large (eql want +large))
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))

(defun main ()
	(setq select (task-mboxes +select_size)
		render (Render-tris *canvas* +land_vertex +land_lit))
	(def render :clear (argb haze))
	(. render :mesh +mesh_land (land-verts +cells +cell))
	(. render :mesh +mesh_land_cpu (land-verts +cells_cpu +cell_cpu))
	(. render :mesh +mesh_sky (sky-verts) (first sky_files))
	(config-load)
	(setq land_height (shader-vp-func (shader-load (cat *app_root* "land.shader")) 'ground)
		place (eight-point round_at) way (doubles +double_1 +double_0))
	(path-set (. *config* :find :path))
	(. *path* :set_selected (if (eql path :eight) 0 1))
	;the size it was closed at
	(if (. *config* :find :large) (set-size +large))
	(.-> *canvas* (:fill (argb haze)) (:swap +swap_write))
	;it comes up on the GPU if there is one and it was last time
	(unless (. *config* :find :gpu) (. render :mode :cpu))
	(mode-show)
	(. *window* :open)
	(mail-timeout (elem-get select +select_timer) +timer_rate 0)
	(while id
		;the render's mail is read with the app's, before the timer, and
		;dealt with: a child has started, wants a mesh, or has drawn its
		;part of a frame
		(bind '(idx msg) (. render :read select +select_timer))
		(cond
			((not idx))
			((= idx +select_main)
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_max) (set-size +large))
					((= id +event_min) (set-size +small))
					((= id +event_path)
						(path-set (if (= (. *path* :get_selected) 0) :eight :valleys)))
					((= id +event_mode)
						(. render :mode (if (= (. *mode* :get_selected) 1) :gpu :cpu))
						(mode-show))
					((. *window* :event msg))))
			(:t ;the timer, the flight goes on and is drawn, if the last frame
				;is not still out with a farm
				(defq start (pii-time))
				;a new canvas goes on show once a frame of it is whole, before the
				;next frame is given: after it the render is busy again
				(and shown (not (. render :busy)) (show-canvas))
				(unless (. render :busy) (new-canvas) (draw-frame))
				;the next frame is a tick on, or four times as long as this
				;one took here if that is more: never more than a quarter of
				;this node, the apps beside it have the rest
				(defq took (- (pii-time) start))
				(mail-timeout (elem-get select +select_timer)
					(- (max +timer_rate (* took 4)) took) 0)
				(. render :tick)
				(when (>= (- (defq now (pii-time)) since) 1000000)
					(mode-show)
					(. *status* :set_text (cat (str (/ (* frames 1000000) (- now since))) " frames a second, "
							(. render :how)))
					(setq frames 0 since now)))))
	(config-save)
	(. render :close)
	(. *window* :close))

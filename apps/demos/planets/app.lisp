(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/math/matrix.inc")
(import "lib/gpu/vp.inc")
(import "lib/gpu/gui.inc")
(import "lib/gpu/tris.inc")

;A globe that turns, with a map on it: a mesh with a picture for a
;texture, drawn by the GPU of the GUI, or with none by native code, a
;strip of the frame by each node of this machine, or all of it here if
;there is only this one. The maps are NASA's, made small, see
;data/README.md

(enums +event +event_user
	(enum planet mode))

;the timer is last, what is first in the list is what is read first, and
;a frame can take longer than a tick
(enums +select 0
	(enum main farm_task farm_reply farm_ask timer))

(defq +size 512 +timer_rate (/ 1000000 30) +segs 64 +rings 32
	;each globe: its name, its map, how far it leans, and how fast it turns
	+planets ''(("Earth" "earth.cpm" 0.41 0.02) ("Moon" "moon.cpm" 0.03 0.012)
		("Mars" "mars.cpm" 0.44 0.02) ("Jupiter" "jupiter.cpm" 0.05 0.03)
		("Pluto" "pluto.cpm" 0.3 0.012))
	+near +real_2 +far (n2r 8) +away (n2r -5.2)
	vertex (shader-load (cat *app_root* "globe_vertex.shader"))
	pixel (shader-load (cat *app_root* "globe_lit.shader"))
	;the sun is up, to the left, and well round to our side of the globe
	sun (reals (n2r -0.4) (n2r 0.4) (n2r 0.8246))
	lens (Mat4x4-frustum (n2r -0.5) (n2r 0.5) (n2r 0.5) (n2r -0.5) +near +far)
	select :nil id :t which 0 turn +real_0
	;on the GPU, the pair of shaders, the ball, and has a frame been drawn
	gpu_pair :nil gpu_mesh :nil gpu_drawn :nil gpu_want :t
	;on this node, the same two shaders as native code
	pipeline :nil frames 0 since (pii-time)
	;how long the frames drawn with no GPU took, and how many of them
	cpu_time 0 cpu_frames 0
	;the children that draw a strip each, they have said they are ready,
	;a frame is out with them and when it went, and they can not be used
	farm :nil farm_ready :nil farm_asked :nil farming :nil farm_began 0 no_farm :nil ticks 0
	+retry_timeout (task-timeout 10)
	;a child with nothing to do for 5 seconds goes. When the farm was last
	;given a frame, and how long after that it is not to be counted on
	last_farm 0 +farm_stale 3000000)

;the maps, a canvas each, loaded when first wanted. Not in the (defq) of
;+planets: there the name is not yet a constant, and its value is the
;quoted list, of two things, not the list of globes
(defq maps (map (lambda (_) :nil) +planets))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Planets" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-radio-bar *planet* ("Earth" "Moon" "Mars" "Jupiter" "Pluto") (:font :font_body)) :connect +event_planet)
		(. (ui-radio-bar *mode* ("CPU" "GPU") (:font :font_body :paper :paper_toolbar2)) :connect +event_mode)
		(ui-backdrop _ (:paper :paper_toolbar)))
	(ui-label *status* (:text "..." :font :font_body))
	;space is black, the GPU draws nothing where the globe is not
	(ui-backdrop _ (:style :plain :paper +argb_black :min_width +size :min_height +size)
		;its pixels are in shared memory if the host has it, the nodes of
		;this machine then draw on them, a strip each
		(ui-element *canvas* (ifn (canvas-shared +size +size 1) (Canvas +size +size 1)) (:paper 0))))

(defun set-label (label text)
	;a label lays its text out once, so lay it out again for the new text
	(unless (eql (get :text label) text)
		(def label :text text)
		(.-> label :layout :dirty)))

(defun globe-verts (segs rings)
	; (globe-verts segs rings) -> reals
	;a ball of radius 1 as triangles, three vertices each, a vertex its
	;place, 3 numbers, and where on the map that is, 2. North is up, and
	;east goes round to the right as seen from outside, as a map has it.
	;A triangle goes round counter clockwise as seen from outside
	(defq out (list) rows (map (lambda (r)
		(defq theta (/ (* +real_pi (n2r r)) (n2r rings)) st (sin theta) ct (cos theta))
		(map (lambda (s)
			(defq phi (/ (* +real_2pi (n2r s)) (n2r segs)))
			(list (* st (sin phi)) ct (* st (cos phi))
				(/ (n2r s) (n2r segs)) (/ (n2r r) (n2r rings))))
			(range 0 (inc segs))))
		(range 0 (inc rings))))
	(each (lambda (r)
		(each (lambda (s)
			(defq a (elem-get (elem-get rows r) s) b (elem-get (elem-get rows (inc r)) s)
				c (elem-get (elem-get rows (inc r)) (inc s)) d (elem-get (elem-get rows r) (inc s)))
			;at a pole a quad is a triangle, the other has no area
			(if (< r (dec rings)) (each (lambda (v) (each (lambda (n) (push out n)) v)) (list a b c)))
			(if (> r 0) (each (lambda (v) (each (lambda (n) (push out n)) v)) (list a c d))))
			(range 0 segs)))
		(range 0 rings))
	(apply (const reals) out))

(defun globe-map (i)
	; (globe-map i) -> canvas
	;the map of a globe, loaded the first time it is wanted. Its pixels
	;are put in shared memory if the host has it, so the nodes that draw a
	;strip each all read the one copy of it
	(unless (elem-get maps i)
		(defq canvas (canvas-load (cat *app_root* "data/" (second (elem-get +planets i))) +load_flag_noswap)
			pixmap (getf canvas +canvas_pixmap 0)
			shared (canvas-shared (getf pixmap +pixmap_width 0) (getf pixmap +pixmap_height 0) 1))
		(when shared
			(defq stream (memory-stream))
			(pixmap-write pixmap stream 32)
			(stream-seek stream 0 0)
			(pixmap-read (getf shared +canvas_pixmap 0) stream 32)
			(setq canvas shared))
		(elem-set maps i (. canvas :swap +swap_write)))
	(elem-get maps i))

(defun globe-model ()
	;how the globe is turned and where it is: it turns about its own
	;axis, which leans, and it is away down the view
	(bind '(& & lean &rest _) (elem-get +planets which))
	(mat4x4-mul (Mat4x4-translate +real_0 +real_0 +away)
		(mat4x4-mul (Mat4x4-rotz (n2r lean)) (Mat4x4-roty turn))))

(defun draw-gpu (vvals)
	; (draw-gpu vvals) -> :t | :nil
	;a frame by the GPU. :nil if it is to be drawn here, there is no GPU
	;that can, or its driver has not yet built the pair
	(and gpu_want (not gpu_pair)
		(unless (setq gpu_pair (shader-gui-pair vertex pixel :t))
			(setq gpu_want :nil)
			(. *mode* :set_selected 0)))
	(when (and gpu_want gpu_pair (defq tid (shader-gui-texture (globe-map which))))
		(unless gpu_mesh (setq gpu_mesh (shader-gui-mesh (shader-verts-str verts))))
		(defq drawn (shader-gui-frame *canvas* (list (list gpu_pair (ifn gpu_mesh 0)
			(shader-pack vertex vvals) (shader-pack pixel (list (list 'sun sun))) (list tid)))))
		(cond
			((eql drawn :error)
				;a pair the driver could not build is still let go of
				(canvas-shader-destroy gpu_pair)
				(setq gpu_pair :nil gpu_want :nil)
				(. *mode* :set_selected 0)
				:nil)
			(drawn (setq gpu_drawn :t))
			;the GPU is busy with the last frame, or has not got the map
			;yet, which goes to it with the next frame of the GUI, so the
			;GUI is given a reason to draw one. This frame is let go
			(gpu_drawn (. *status* :dirty) :t))))

(defun strips (vvals rows)
	;a job for each child, a strip of the frame, or with no rows a job of
	;nothing, which has a child fetch the ball and the map and say it is
	;ready
	(defq count (. farm :size)
		draws (list (list 0 (shader-pack vertex vvals) (shader-pack pixel (list (list 'sun sun)))
			0 +size (list (ifn (shader-strip-texture (globe-map which)) "")))))
	(. farm :add (map (lambda (i)
			(shader-strip (cat *app_root* "globe_vertex.shader") (cat *app_root* "globe_lit.shader")
				(elem-get select +select_farm_ask) (canvas-key *canvas*) +size +size
				(if rows (/ (* i +size) count) 0) (if rows (/ (* (inc i) +size) count) 0) :t draws))
		(range 0 count))))

(defun draw-farm (vvals)
	; (draw-farm vvals) -> :t | :nil
	;a frame by the nodes of this machine, a strip each. :nil if it is to
	;be drawn here: the pixels are not shared, there is only this node,
	;or the children are not yet ready
	(when (and (not no_farm) (/= (canvas-key *canvas*) 0) (/= (canvas-key (globe-map which)) 0)
			(> (length (lisp-nodes :t)) 1))
		(cond
			((not farm)
				;a child for each node but this one, and none of them on
				;this one, a strip is one long call and would hold up the app
				(setq farm (Jobs +shader_tris_child (elem-get select +select_farm_task)
					(elem-get select +select_farm_reply)
					(list 64 (max 1 (dec (length (lisp-nodes :t)))) 0) :t)
					farm_asked :t)
				(strips vvals :nil))
			((or (not (or farm_ready farm_asked))
					(and farm_ready (> (- (pii-time) last_farm) +farm_stale)))
				;the GPU has had the frames for a while, and the children
				;may have gone. They are started again, and till they say
				;they are ready the frames are drawn here. A frame given to
				;children that had gone was never drawn, and the globe stood
				;still
				;or the mode was changed, and what it had been asked let go
				(setq farm_ready :nil farm_asked :t)
				(. farm :restart)
				(strips vvals :nil)))
		(when farm_ready
			(setq farming :t farm_began (pii-time) last_farm farm_began)
			(. *canvas* :fill +argb_black)
			(strips vvals :t)
			:t)))

(defun draw-cpu (vvals)
	;a frame by native code, here
	(unless pipeline (setq pipeline (shader-vp-pipeline vertex pixel)))
	(defq began (pii-time))
	(. *canvas* :fill +argb_black)
	(shader-vp-draw-tris pipeline verts tris (getf *canvas* +canvas_pixmap 0)
		(shader-vp-depth +size +size) vvals
		(list (list 'sun sun) (list 'surface (getf (globe-map which) +canvas_pixmap 0))) :t)
	(. *canvas* :swap +swap_write)
	(setq cpu_time (+ cpu_time (- (pii-time) began)) cpu_frames (inc cpu_frames)))

(defun draw-frame ()
	(defq vvals (list (list 'model (globe-model)) (list 'lens lens)))
	(cond
		((draw-gpu vvals))
		((draw-farm vvals))
		((draw-cpu vvals)))
	(++ frames))

(defun main ()
	(setq select (task-mboxes +select_size))
	(defq verts (globe-verts +segs +rings) tris (apply (const nums) (range 0 (/ (length verts) 5))))
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(. *planet* :set_selected 0)
	(. *mode* :set_selected 1)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(mail-timeout (elem-get select +select_timer) +timer_rate 0)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_planet)
						(if (defq chosen (. *planet* :get_selected)) (setq which chosen)))
					((= id +event_mode)
						(setq gpu_want (= (. *mode* :get_selected) 1) gpu_drawn :nil)
						;a frame that is out with the farm is not waited for. The
						;strips of it are told to a mailbox nobody reads from now,
						;the farm has a new one
						(when farm
							(elem-set select +select_farm_reply (mail-mbox))
							(. farm :reply_to (elem-get select +select_farm_reply))
							;and is asked afresh if it is wanted again
							(setq farming :nil farm_ready :nil farm_asked :nil)))
					((. *window* :event msg))))
			(+select_farm_task
				;a child of the farm has started
				(if farm (. farm :launched msg)))
			(+select_farm_ask
				;a child of the farm has not got the ball
				(shader-mesh-send msg (shader-verts-str verts)))
			(+select_farm_reply
				;a strip is drawn, or a child has said it is ready
				(when (and farm (defq out (. farm :answered msg)))
					;a child that could not reach the pixels, the frames are
					;drawn here from now on
					(if (= (getf msg +strip_reply_drawn) 0) (setq no_farm :t))
					(when (= out 0)
						(cond
							(farming (setq farming :nil
									cpu_time (+ cpu_time (- (pii-time) farm_began)) cpu_frames (inc cpu_frames))
								(. *canvas* :swap +swap_write))
							;they are ready, and as good as just used
							((setq farm_ready :t farm_asked :nil last_farm (pii-time)))))))
			(:t ;the timer, the globe turns and is drawn, if the last frame
				;is not still out with the farm
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(unless farming
					(setq turn (% (+ turn (n2r (elem-get (elem-get +planets which) 3))) +real_2pi))
					(draw-frame))
				(and farm (= (setq ticks (% (inc ticks) 30)) 0)
					(. farm :refresh +retry_timeout))
				(when (>= (- (defq now (pii-time)) since) 1000000)
					;the timer holds either to 30 a second, so for the frames
					;drawn here it says how long one took
					(defq tenths (if (> cpu_frames 0) (/ cpu_time cpu_frames 100) 0))
					(set-label *status* (cat (first (elem-get +planets which)) ", "
						(str (/ (* frames 1000000) (- now since))) " frames a second, "
						(if (> cpu_frames 0)
							(cat "native code, " (if (and farm farm_ready (not no_farm))
									(cat "a strip each by " (str (. farm :size)) " nodes, ") "on this node, ")
								(str (/ tenths 10)) "." (str (% tenths 10)) "ms a frame")
							"the GPU")))
					(setq frames 0 since now cpu_time 0 cpu_frames 0)))))
	(if farm (. farm :close))
	(if gpu_mesh (canvas-mesh-destroy gpu_mesh))
	(if gpu_pair (canvas-shader-destroy gpu_pair))
	(gui-sub-rpc *window*))

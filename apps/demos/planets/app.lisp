(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/math/matrix.inc")
(import "lib/gpu/render_farm.inc")

;A globe that turns, with a map on it: a mesh with a picture for a
;texture, drawn by a (Render-tris), lib/gpu/render_farm.inc, which has the
;GPU draw it, or with none the nodes of this machine, a strip of the frame
;each, or this task if there is only the one. The maps are NASA's, made
;small, see data/README.md

(enums +event +event_user
	(enum planet mode))

;the timer is last, what is first in the list is what is read first, and
;a frame can take longer than a tick
(enums +select 0
	(enum main timer))

(defq +size 512 +timer_rate (/ 1000000 30) +segs 64 +rings 32
	;each globe: its name, its map, how far it leans, and how fast it turns
	+planets ''(("Earth" "earth.cpm" 0.41 0.02) ("Moon" "moon.cpm" 0.03 0.012)
		("Mars" "mars.cpm" 0.44 0.02) ("Jupiter" "jupiter.cpm" 0.05 0.03)
		("Pluto" "pluto.cpm" 0.3 0.012))
	+near +double_2 +far (n2d 8) +away (n2d -5.2)
	;the sun is up, to the left, and well round to our side of the globe
	sun (doubles (n2d -0.4) (n2d 0.4) (n2d 0.8246))
	lens (mat4x4-frustum (n2d -0.5) (n2d 0.5) (n2d 0.5) (n2d -0.5) +near +far)
	select :nil id :t which 0 turn +double_0
	;what draws the frames, and how many it has in the second
	render :nil frames 0 since (pii-time))

;the maps, a canvas each, loaded when first wanted. Not in the (defq) of
;+planets: there the name is not yet a constant, and its value is the
;quoted list, of two things, not the list of globes
(defq maps (map (lambda (&ignore) :nil) +planets))

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
		(ui-element *canvas* (ifn (canvas-shared +size +size 1) (Canvas +size +size 1)))))

(defun globe-verts (segs rings)
	; (globe-verts segs rings) -> doubles
	;a ball of radius 1 as triangles, three vertices each, a vertex its
	;place, 3 numbers, and where on the map that is, 2. North is up, and
	;east goes round to the right as seen from outside, as a map has it.
	;A triangle goes round counter clockwise as seen from outside
	(defq out (list) rows (map! (lambda () (defq r (!)) (defq theta (/ (* +double_pi (n2d r)) (n2d rings)) st (sin theta) ct (cos theta))
				(map! (lambda () (defq phi (/ (* +double_2pi (n2d (!))) (n2d segs)))
						(list (* st (sin phi)) ct (* st (cos phi))
							(/ (n2d (!)) (n2d segs)) (/ (n2d r) (n2d rings)))) '() 0 (inc segs))) '() 0 (inc rings)))
	(each! (lambda () (defq r (!)) (each! (lambda () (defq s (!)) (defq a (elem-get (elem-get rows r) s) b (elem-get (elem-get rows (inc r)) s)
						c (elem-get (elem-get rows (inc r)) (inc s)) d (elem-get (elem-get rows r) (inc s)))
					;at a pole a quad is a triangle, the other has no area
					(if (< r (dec rings)) (each (lambda (v) (each (lambda (n) (push out n)) v)) (list a b c)))
					(if (> r 0) (each (lambda (v) (each (lambda (n) (push out n)) v)) (list a c d)))) '() 0 segs)) '() 0 rings)
	(apply (const doubles) out))

(defun globe-map (i)
	; (globe-map i) -> canvas
	;the map of a globe, loaded the first time it is wanted. Its pixels
	;are put in shared memory if the host has it, so the nodes that draw a
	;strip each all read the one copy of it
	(unless (elem-get maps i)
		(elem-set maps i (. (shader-strip-picture (canvas-load (cat *app_root* "data/"
			(second (elem-get +planets i))) +load_flag_noswap)) :swap +swap_write)))
	(elem-get maps i))

(defun globe-model ()
	;how the globe is turned and where it is: it turns about its own
	;axis, which leans, and it is away down the view
	(bind '(& & lean &ignore) (elem-get +planets which))
	(mat4x4-mul (mat4x4-translate +double_0 +double_0 +away)
		(mat4x4-mul (mat4x4-rotz (n2d lean)) (mat4x4-roty turn))))

(defun draw-frame ()
	;the globe as it is turned, the one mesh, with its map
	(. render :frame (list (list 0 (list (list 'model (globe-model)) (list 'lens lens))
			(list (list 'sun sun)) (list (globe-map which)))))
	(++ frames))

(defun mode-show ()
	;the button as the render has it: the GPU is not to be had on a host
	;with none
	(. *mode* :set_selected (if (eql (. render :mode) :gpu) 1 0)))

(defun main ()
	(setq select (task-mboxes +select_size)
		render (Render-tris *canvas* (cat *app_root* "globe_vertex.shader") (cat *app_root* "globe_lit.shader") :t))
	(. render :mesh 0 (globe-verts +segs +rings))
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(. *planet* :set_selected 0)
	(mode-show)
	(. *window* :open)
	(mail-timeout (elem-get select +select_timer) +timer_rate 0)
	(while id
		;the render's mail is read with the app's, before the timer, and
		;dealt with: a child has started, wants the ball, or has drawn its
		;part of a frame
		(bind '(idx msg) (. render :read select +select_timer))
		(cond
			((not idx))
			((= idx +select_main)
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_planet)
						(if (defq chosen (. *planet* :get_selected)) (setq which chosen)))
					((= id +event_mode)
						(. render :mode (if (= (. *mode* :get_selected) 1) :gpu :cpu))
						(mode-show))
					((. *window* :event msg))))
			(:t ;the timer, the globe turns and is drawn, if the last frame
				;is not still out with a farm
				(defq start (pii-time))
				(unless (. render :busy)
					(setq turn (% (+ turn (n2d (elem-get (elem-get +planets which) 3))) +double_2pi))
					(draw-frame))
				;the next frame is a tick on, or four times as long as this
				;one took here if that is more: on a slow machine drawing on
				;its own node the globe turns less often, and never has more
				;than a quarter of the node, the apps beside it have the rest
				(defq took (- (pii-time) start))
				(mail-timeout (elem-get select +select_timer)
					(- (max +timer_rate (* took 4)) took) 0)
				(. render :tick)
				(when (>= (- (defq now (pii-time)) since) 1000000)
					(mode-show)
					(. *status* :set_text (cat (first (elem-get +planets which)) ", "
							(str (/ (* frames 1000000) (- now since))) " frames a second, "
							(. render :how)))
					(setq frames 0 since now)))))
	(. render :close)
	(. *window* :close))

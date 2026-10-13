(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/math/matrix.inc")
(import "lib/gpu/vp.inc")
(import "lib/gpu/gui.inc")

;A globe that turns, with a map on it: a mesh with a picture for a
;texture, drawn by the GPU of the GUI, or by native code on this node
;where there is none. The maps are NASA's, made small, see data/README.md

(enums +event +event_user
	(enum planet mode))

(enums +select 0
	(enum main timer))

(defq +size 512 +timer_rate (/ 1000000 30) +segs 64 +rings 32
	;each globe: its name, its map, how far it leans, and how fast it turns
	+planets ''(("Earth" "earth.cpm" 0.41 0.02) ("Jupiter" "jupiter.cpm" 0.05 0.03))
	+near +real_2 +far (n2r 8) +away (n2r -5.2)
	vertex (shader-load (cat *app_root* "globe_vertex.shader"))
	pixel (shader-load (cat *app_root* "globe_lit.shader"))
	;the sun is up, to the left, and well round to our side of the globe
	sun (reals (n2r -0.4) (n2r 0.4) (n2r 0.8246))
	lens (Mat4x4-frustum (n2r -0.5) (n2r 0.5) (n2r 0.5) (n2r -0.5) +near +far)
	select :nil id :t which 0 turn +real_0
	;the maps, a canvas each, loaded when first wanted
	maps (map (lambda (_) :nil) +planets)
	;on the GPU, the pair of shaders, the ball, and has a frame been drawn
	gpu_pair :nil gpu_mesh :nil gpu_drawn :nil gpu_want :t
	;on this node, the same two shaders as native code
	pipeline :nil frames 0 since (pii-time))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Planets" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-radio-bar *planet* ("Earth" "Jupiter") (:font *env_body_font*)) :connect +event_planet)
		(. (ui-radio-bar *mode* ("CPU" "GPU") (:font *env_body_font* :color *env_toolbar2_col*)) :connect +event_mode)
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-label *status* (:text "..." :font *env_body_font*))
	;space is black, the GPU draws nothing where the globe is not
	(ui-backdrop _ (:style :plain :color +argb_black :min_width +size :min_height +size)
		(ui-element *canvas* (Canvas +size +size 1) (:color 0))))

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
	;the map of a globe, loaded the first time it is wanted
	(unless (elem-get maps i)
		(elem-set maps i (canvas-load (cat *app_root* "data/" (second (elem-get +planets i))) 0)))
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
				(setq gpu_pair :nil gpu_want :nil)
				(. *mode* :set_selected 0)
				:nil)
			(drawn (setq gpu_drawn :t))
			;the GPU is busy with the last frame, this one is let go
			(gpu_drawn :t))))

(defun draw-cpu (vvals)
	;a frame by native code, here
	(unless pipeline (setq pipeline (shader-vp-pipeline vertex pixel)))
	(. *canvas* :fill +argb_black)
	(shader-vp-draw-tris pipeline verts tris (getf *canvas* +canvas_pixmap 0)
		(shader-vp-depth +size +size) vvals
		(list (list 'sun sun) (list 'surface (getf (globe-map which) +canvas_pixmap 0))) :t)
	(. *canvas* :swap +swap_write))

(defun draw-frame ()
	(defq vvals (list (list 'model (globe-model)) (list 'lens lens)))
	(unless (draw-gpu vvals) (draw-cpu vvals))
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
						(setq which (. *planet* :get_selected)))
					((= id +event_mode)
						(setq gpu_want (= (. *mode* :get_selected) 1) gpu_drawn :nil))
					((. *window* :event msg))))
			(:t ;the timer, the globe turns and is drawn
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(setq turn (% (+ turn (n2r (elem-get (elem-get +planets which) 3))) +real_2pi))
				(draw-frame)
				(when (>= (- (defq now (pii-time)) since) 1000000)
					(set-label *status* (cat (first (elem-get +planets which)) ", "
						(str (/ (* frames 1000000) (- now since))) " frames a second, "
						(if (and gpu_want gpu_drawn) "the GPU" "native code on this node")))
					(setq frames 0 since now)))))
	(if gpu_mesh (canvas-mesh-destroy gpu_mesh))
	(if gpu_pair (canvas-shader-destroy gpu_pair))
	(gui-sub-rpc *window*))

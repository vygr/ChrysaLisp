(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/math/vector.inc")

(enums +select 0
	(enum main timer))

(enums +event 0
	(enum close))

(defq canvas_width 600 canvas_height 600 canvas_scale 1 id t
	f_canvas_width (i2f canvas_width) f_canvas_height (i2f canvas_height) f_canvas_scale (i2f canvas_scale)
	rate (/ 1000000 30)
	+eps 0.25 angle 0.0 font (create-font "fonts/OpenSans-Regular.ctf" 36)
	fp1 (font-glyph-paths font "    Glyphs!")
	fp2 (font-glyph-paths font "    Easy!")
	fp3 (font-glyph-paths font "    Simple!")
	fp4 (font-glyph-paths font "    Quality!"))

(ui-window *window* ()
	(ui-title-bar _ "Canvas" (0xea19) +event_close)
	(ui-canvas canvas canvas_width canvas_height canvas_scale))

(defun transform-copy (angle _)
	(defq sa (sin angle) ca (cos angle))
	(map (lambda (_)
		(path-transform
			(* f_canvas_scale ca) (* f_canvas_scale (* sa -1.0))
			(* f_canvas_scale sa) (* f_canvas_scale ca)
			(* f_canvas_width f_canvas_scale 0.5) (* f_canvas_height f_canvas_scale 0.5)
			_ (cat _))) _))

(defun transform (angle _)
	(defq sa (sin angle) ca (cos angle))
	(map (lambda (_)
		(path-transform
			(* f_canvas_scale ca) (* f_canvas_scale (* sa -1.0))
			(* f_canvas_scale sa) (* f_canvas_scale ca)
			(* f_canvas_width f_canvas_scale 0.5) (* f_canvas_height f_canvas_scale 0.5)
			_ _)) _))

(defun transform-norm (angle _)
	(defq sa (sin angle) ca (cos angle))
	(map (lambda (_)
		(path-transform
			(* f_canvas_width f_canvas_scale ca) (* f_canvas_height f_canvas_scale (* sa -1.0))
			(* f_canvas_width f_canvas_scale sa) (* f_canvas_height f_canvas_scale ca)
			(* f_canvas_width f_canvas_scale 0.5) (* f_canvas_height f_canvas_scale 0.5)
			_ _)) _))

(defun fpoly (col mode _)
	(.-> canvas (:set_color col) (:fpoly 0.0 0.0 mode _)))

(defun redraw ()
	(. canvas :fill 0)
	(fpoly +argb_red +winding_odd_even (transform-norm (* angle (i2f 2)) (list
		(path -0.5 -0.5 -0.25 0.5 0.0 -0.5 0.25 0.5 0.5 -0.5 -0.05 0.5))))
	(fpoly 0xff0ff0ff +winding_odd_even (transform (* angle -1.0)
		(path-stroke-polylines (list) (* f_canvas_width 0.05) +eps +join_bevel +cap_square +cap_square
			(list (path-gen-quadratic
				(* f_canvas_width -0.4) (* f_canvas_height 0.4)
				(* f_canvas_width -0.2) (* f_canvas_height -1.1)
				(* f_canvas_width 0.4) (* f_canvas_height 0.2)
				+eps (path))))))
	(fpoly 0xc000ff00 +winding_odd_even (transform angle
		(path-stroke-polylines (list) (* f_canvas_width 0x0.1) +eps +join_round +cap_round +cap_round
			(list (path (* f_canvas_width -0.4) (* f_canvas_height -0.4)
				(* f_canvas_width 0.3) (* f_canvas_height -0.3)
				(* f_canvas_width 0.4) (* f_canvas_height 0.4))))))
	(fpoly +argb_yellow +winding_odd_even (defq p (transform (* angle (i2f -2))
		(path-stroke-polygons (list) (* f_canvas_width 0.011) +eps +join_miter
			(path-stroke-polylines (list) (* f_canvas_width 0.033) +eps +join_bevel +cap_round +cap_arrow
				(list (path-gen-cubic
					(* f_canvas_width -0.45) (* f_canvas_height 0.3)
					(* f_canvas_width -0.3) (* f_canvas_height -0.3)
					(* f_canvas_width 0.45) (* f_canvas_height 0.6)
					(* f_canvas_width 0.4) (* f_canvas_height -0.4)
					+eps (path))))))))
	(fpoly 0x80000000 +winding_odd_even (slice 1 2 p))
	(fpoly 0xd0ff00ff +winding_odd_even (defq p (transform angle
		(path-stroke-polygons (list) (* f_canvas_width 0.02) +eps +join_miter
			(list (path-gen-arc
				(* f_canvas_width 0.2) (* f_canvas_height 0.3) 0.0 +fp_2pi
				(* f_canvas_width 0.125) +eps (path)))))))
	(fpoly 0x60000000 +winding_odd_even (slice 0 1 p))
	(fpoly 0xc00000ff +winding_odd_even (defq polygons (transform angle
		(path-stroke-polygons (list) (* f_canvas_width 0.025) +eps +join_miter
			(path-stroke-polylines (list) (* f_canvas_width 0.05) +eps +join_bevel +cap_square +cap_tri (list
				(path-gen-arc
					(* f_canvas_width -0.1) (* f_canvas_height -0.2) 0.9 1.5
					(* f_canvas_width 0.2) +eps (path))
				(path-gen-arc
					(* f_canvas_width -0.2) (* f_canvas_height -0.2) 4.0 2.0
					(* f_canvas_width 0o0.1) +eps (path))))))))
	(fpoly 0xa0ffffff +winding_odd_even (list (elem 1 polygons) (elem 3 polygons)))
	(fpoly 0xff000000 +winding_odd_even (transform-copy (/ angle 2.0) fp1))
	(fpoly 0xff000000 +winding_odd_even (transform-copy (+ (/ angle 2.0) +fp_pi) fp2))
	(fpoly 0xffffffff +winding_odd_even (transform-copy (+ (/ angle 2.0) +fp_hpi) fp3))
	(fpoly 0xffffffff +winding_odd_even (transform-copy (+ (/ angle 2.0) (* -1.0 +fp_hpi)) fp4))
	(. canvas :swap))

(defun main ()
	(defq select (alloc-select +select_size))
	(.-> canvas (:fill 0) (:set_canvas_flags +canvas_flag_antialias))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(mail-timeout (elem +select_timer select) rate 0)
	(while id
		(defq msg (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_main)
				;main mailbox
				(cond
					((= (getf msg +ev_msg_target_id) +event_close)
						(setq id nil))
					(t (. *window* :event msg))))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem +select_timer select) rate 0)
				(redraw)
				(setq angle (+ angle 0.0025)))))
	;close window
	(free-select select)
	(gui-sub *window*))

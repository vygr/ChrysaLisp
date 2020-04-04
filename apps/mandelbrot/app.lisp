;jit compile apps native functions if needed
(import 'cmd/asm.inc)
(make 'apps/mandelbrot/lisp.vp)

;imports
(import 'gui/lisp.inc)
(import 'apps/mandelbrot/mbmath.inc)

(structure 'event 0
	(byte 'win_close))

(defq canvas_width 800 canvas_height 800 canvas_scale 2 then nil area 0 select nil
	center_x (mbfp-from-fixed -0.5) center_y (mbfp-from-fixed 0.0) zoom (mbfp-from-fixed 1.0))

(ui-window window ()
	(ui-flow _ ('flow_flags flow_down_fill)
		(ui-flow _ ('flow_flags flow_left_fill 'font (create-font "fonts/Entypo.ctf" 22) 'color *env_title_col*)
			(ui-buttons (0xea19) (const event_win_close))
			(ui-title _ ('text "Mandelbrot" 'font (create-font "fonts/OpenSans-Regular.ctf" 18))))
		(ui-canvas canvas canvas_width canvas_height canvas_scale)))

(defun-bind reset ()
	(if select (mail-free-mbox (elem 1 select)))
	(setq then (time) select (array (task-mailbox) (mail-alloc-mbox))
		area (* canvas_width canvas_height canvas_scale canvas_scale))
	(mail-send (array (elem 1 select) 0 0 (* canvas_width canvas_scale) (* canvas_height canvas_scale)
		(* canvas_width canvas_scale) (* canvas_height canvas_scale) center_x center_y zoom (* (kernel-total) 4))
		(open-child "apps/mandelbrot/child.lisp" kn_call_child)))

(defun-bind tile (canvas data)
	;(tile canvas data) -> area
	(defq data (string-stream data) x (read-int data) y (read-int data)
		x1 (read-int data) y1 (read-int data) yp (dec y))
	(while (/= (setq yp (inc yp)) y1)
		(defq xp (dec x))
		(while (/= (setq xp (inc xp)) x1)
			(defq r (read-char data) r (if (= r 255) 0 r)
				g (<< (logand r 0x7f) 9) b (<< (logand r 0x3f) 2))
			(canvas-plot (canvas-set-color canvas (+ argb_black (<< r 16) g b)) xp yp))
		(task-sleep 0))
	(* (- x1 x) (- y1 y)))

;native versions
(ffi tile "apps/mandelbrot/tile" 0)

(defun-bind main ()
	;add window
	(canvas-swap (canvas-fill canvas argb_black))
	(gui-add (apply view-change (cat (list window 64 64) (view-pref-size window))))
	(reset)
	;main event loop
	(while (progn
		;next event
		(defq id (mail-select select) msg (mail-read (elem id select)))
		(cond
			((= id 0)
				;main mailbox
				(cond
					((= (setq id (get-long msg ev_msg_target_id)) event_win_close)
						;close button
						nil)
					((and (= id (component-get-id canvas))
							(= (get-long msg ev_msg_type) ev_type_mouse)
							(/= (get-int msg ev_msg_mouse_buttons) 0))
						;mouse click on the canvas view, zoom in/out, re-center
						(bind '(w h) (view-get-size canvas))
						(defq rx (- (get-int msg ev_msg_mouse_rx) (/ (- w canvas_width) 2))
							ry (- (get-int msg ev_msg_mouse_ry) (/ (- h canvas_height) 2)))
						(setq center_x (+ center_x (mbfp-offset rx canvas_width zoom))
							center_y (+ center_y (mbfp-offset ry canvas_height zoom))
							zoom (mbfp-mul zoom (if (= 0 (logand (get-int msg ev_msg_mouse_buttons) 2))
								(mbfp-from-fixed 0.5) (mbfp-from-fixed 2.0))))
						(reset))
					(t (view-event window msg))))
			(t	;child tile msg
				(setq area (- area (tile canvas msg)))
				(when (or (> (- (defq now (time)) then) 1000000) (= area 0))
					(canvas-swap canvas)
					(setq then now))
					t))))
	;close
	(view-hide window)
	(mail-free-mbox (elem 1 select)))

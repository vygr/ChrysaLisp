;jit compile apps native functions if needed
(import 'cmd/asm.inc)
(make 'apps/mandelbrot/lisp.vp)

;imports
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close))

(defq canvas_width 800 canvas_height 800 id t in nil then (time) total 0 select nil)

(ui-tree window (create-window window_flag_close) nil
	(ui-element canvas (create-canvas canvas_width canvas_height 1)))

(gui-add (apply view-change (cat (list window 64 64)
	(view-pref-size (window-set-title (window-connect-close window event_win_close) "Mandelbrot")))))

(defun-bind reset ()
	(if in (in-set-state in stream_mail_state_stopped))
	(canvas-swap (canvas-fill canvas argb_black))
	(setq in (in-stream) then (time) select (array (task-mailbox) (in-mbox in))
		total (* canvas_width canvas_height))
	(mail-send (array (elem 1 select) 0 0 canvas_width canvas_height canvas_width canvas_height)
		(open-child "apps/mandelbrot/child.lisp" kn_call_child)))

(reset)
(while id
	;next event
	(defq idx (mail-select select) msg (mail-read (elem idx select)))
	(cond
		((= idx 0)
			;main mailbox
			(cond
				((= (setq id (get-long msg ev_msg_target_id)) event_win_close)
					;close button
					(setq id nil))
				((and (= id (component-get-id canvas))
						(= (get-long msg ev_msg_type) ev_type_mouse)
						(= (get-int msg ev_msg_mouse_buttons) 0))
					;mouse click on the canvas
					(reset))
				(t (view-event window msg))))
		(t	;child msg
			(defq reply (string-stream msg)
				xp (read-char reply (const int_size)) y (read-char reply (const int_size))
				x1 (read-char reply (const int_size)) y1 (read-char reply (const int_size))
				total (- total (* (- x1 xp) (- y1 y))))
			(setq y (dec y))
			(while (/= (setq y (inc y)) y1)
				(defq x (dec xp))
				(while (/= (setq x (inc x)) x1)
					(defq c (read-char reply) c (if (= c 255) 0 c) r c g c b c)
					(canvas-plot (canvas-set-color canvas (+ argb_black (<< r 16) (<< g 8) b)) x y)))
			(when (or (= total 0) (> (- (defq now (time)) then) 1000000))
				(setq then now)
				(canvas-swap canvas)))))

;close
(in-set-state in stream_mail_state_stopped)
(view-hide window)

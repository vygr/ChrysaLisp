;jit compile apps native functions if needed
(import 'cmd/asm.inc)
(make 'apps/raymarch/lisp.vp)

;imports
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close))

(defq canvas_width 800 canvas_height 800 canvas_scale 1 id t in (in-stream) then (time)
	area (* canvas_width canvas_height canvas_scale canvas_scale)
	farm (open-farm "apps/raymarch/child.lisp" (min (* 2 (kernel-total)) (* canvas_height canvas_scale)) kn_call_child)
	select (array (task-mailbox) (in-mbox in))
	jobs (map (lambda (y)
		(array (elem 1 select) 0 y (* canvas_width canvas_scale) (inc y)
			(* canvas_width canvas_scale) (* canvas_height canvas_scale)))
		(range (dec (* canvas_height canvas_scale)) -1)))

(ui-tree window (create-window window_flag_close) nil
	(ui-element canvas (create-canvas canvas_width canvas_height canvas_scale)))

(canvas-swap (canvas-fill canvas argb_black))
(gui-add (apply view-change (cat (list window 64 64)
	(view-pref-size (window-set-title (window-connect-close window event_win_close) "Raymarch")))))

(defun-bind tile (canvas data)
	;(tile canvas data) -> area
	(defq data (string-stream data)
		x (read-char data (const int_size)) y (read-char data (const int_size))
		x1 (read-char data (const int_size)) y1 (read-char data (const int_size))
		yp (dec y))
	(while (/= (setq yp (inc yp)) y1)
		(defq xp (dec x))
		(while (/= (setq xp (inc xp)) x1)
			(canvas-plot (canvas-set-color canvas (read-char data (const int_size))) xp yp))
		(task-sleep 0))
	(* (- x1 x) (- y1 y)))

;native versions
(ffi tile "apps/raymarch/tile" 0)

;send first batch of jobs
(each (lambda (child) (mail-send (pop jobs) child)) farm)

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
				(t (view-event window msg))))
		(t	;child tile msg
			(if (defq child (get-long msg 0) next_job (pop jobs))
				;next job
				(mail-send next_job child))
			(setq area (- area (tile canvas (slice (const long_size) -1 msg))))
			(when (= area 0)
				;close farm and clear it
				(each (lambda (_) (mail-send "" _)) farm)
				(clear farm))
			(when (or (> (- (defq now (time)) then) 1000000) (= area 0))
				;swap canvas
				(setq then now)
				(canvas-swap canvas)))))

;close
(in-set-state in stream_mail_state_stopped)
(view-hide window)
(each (lambda (_) (mail-send "" _)) farm)

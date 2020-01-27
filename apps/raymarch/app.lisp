;jit compile apps native functions if needed
(import 'cmd/asm.inc)
(make 'apps/raymarch/lisp.vp)

;imports
(import 'gui/lisp.inc)
(import 'apps/raymarch/farm.inc)

(structure 'event 0
	(byte 'win_close))

(structure 'work 0
	(long 'width 'height 'y))

(defq canvas_width 800 canvas_height 800 canvas_scale 1 id t
	sw (* canvas_width canvas_scale) sh (* canvas_height canvas_scale) sy 0 sc 0
	farm (farm-open "apps/raymarch/child.lisp" (* (kernel-total) 2)))

(ui-tree window (create-window window_flag_close) nil
	(ui-element canvas (create-canvas canvas_width canvas_height canvas_scale)))

(canvas-swap (canvas-fill canvas argb_black))
(gui-add (apply view-change (cat (list window 310 64)
	(view-pref-size (window-set-title (window-connect-close window event_win_close) "Raymarch")))))

;send out first batch of work
(while (and (< sy sh) (< sy (farm-size farm)))
	(mail-send (array sw sh sy) (farm-child farm sy))
	(setq sc (inc sc) sy (inc sy)))

(defq then (time))
(while id
	;next event
	(defq in (farm-read farm))
	(cond
		((eql in t)
			;main mailbox
			(cond
				((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
					;close button
					(setq id nil))
				(t (view-event window msg))))
		(t	;child stream
			(setq sc (dec sc))
			(defq child (read-char in (const long_size)) x -1 y (read-char in (const int_size)))
			(while (< (setq x (inc x)) sw)
				(canvas-plot (canvas-set-color canvas (read-char in (const int_size))) x y))
			(when (> (- (defq now (time)) then) 1000000)
				(setq then now)
				(canvas-swap canvas))
			(cond
				((< sy sh)
					;can pass out more work
					(mail-send (array sw sh sy) child)
					(setq sc (inc sc) sy (inc sy)))
				((= sc 0)
					;close farm
					(canvas-swap canvas)
					(farm-close farm))))))

;close
(view-hide window)
(farm-close farm)

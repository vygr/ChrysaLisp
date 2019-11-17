;jit compile apps native functions if needed
(import 'cmd/asm.inc)
(make 'apps/raymarch/lisp.vp)

;imports
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close))

(structure 'work 0
	(long 'parent_id 'width 'height 'y))

(structure 'reply 0
	(long 'child_id)
	(int 'y)
	(offset 'data))

(defq canvas_width 800 canvas_height 800 canvas_scale 1 id t)

(ui-tree window (create-window window_flag_close) nil
	(ui-element canvas (create-canvas canvas_width canvas_height canvas_scale)))

(canvas-swap (canvas-fill (view-set-flags canvas view_flag_opaque view_flag_opaque) argb_black))
(gui-add (apply view-change (cat (list window 310 64)
	(view-pref-size (window-set-title (window-connect-close window event_win_close) "Raymarch")))))

;open farm and send out first batch of work
(defq sw (* canvas_width canvas_scale) sh (* canvas_height canvas_scale) sy 0 sc 0
	farm (open-farm "apps/raymarch/child.lisp" (* (kernel-total) 2) kn_call_child))
(while (and (< sy sh) (< sy (length farm)))
	(mail-send (array (task-mailbox) sw sh sy) (elem sy farm))
	(setq sc (inc sc) sy (inc sy)))

(defq then (time))
(while id
	(cond
		((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
			(setq id nil))
		((> id 0)
			(setq sc (dec sc))
			(defq x -1 y (get-int msg reply_y))
			(while (< (setq x (inc x)) sw)
				(canvas-plot (canvas-set-color canvas (get-int msg (+ reply_data (* x int_size)))) x y))
			(when (> (- (defq now (time)) then) 1000000)
				(setq then now)
				(canvas-swap canvas))
			(cond
				((< sy sh)
					;can pass out more work
					(mail-send (array (task-mailbox) sw sh sy) id)
					(setq sc (inc sc) sy (inc sy)))
				((= sc 0)
					(canvas-swap canvas)
					;send out multi-cast exit command
					(defq exit (char 0 long_size))
					(while (defq mbox (pop farm))
						(mail-send exit mbox)))))
		(t (view-event window msg))))

;wait for outstanding replies
(view-hide window)
(while (/= sc 0)
	(if (> (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id) 0)
		(setq sc (dec sc))))

;send out multi-cast exit command
(defq exit (char 0 long_size))
(while (defq mbox (pop farm))
	(mail-send exit mbox))

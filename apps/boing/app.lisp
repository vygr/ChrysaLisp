;import settings
(run 'apps/sys.inc)
(run 'apps/ui.inc)

(structure 'event 0
	(byte 'win_close))

(defq id t canvas_width 640 canvas_height 480 frames (map (lambda (_)
	(load-cpm (file-stream (cat "apps/boing/taoball_" (str _) ".cpm")))) (range 1 12)))

(ui-tree window (create-window window_flag_close) nil
;	(ui-element canvas (create-canvas canvas_width canvas_height 1.0))
	(ui-element _ (load-cpm (file-stream "apps/boing/taoball_1.cpm"))))

;(slot fill canvas 0xff000000)

(slot set_title window "Boing")
(slot connect_close window event_win_close)
(bind '(w h) (slot pref_size window))
(slot change window 512 256 w h)
(slot gui_add window)

(while id
	(cond
		((eq (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) event_win_close)
			(setq id nil))
		(t (slot event window msg))))

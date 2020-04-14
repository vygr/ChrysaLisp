;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'close)
	(byte 'prev 'next))

(defq images '("apps/images/wallpaper.cpm" "apps/images/chrysalisp.cpm"
	"apps/images/frill.cpm" "apps/images/magicbox.cpm" "apps/images/captive.cpm"
	"apps/images/balls.cpm" "apps/images/banstand.cpm" "apps/images/bucky.cpm"
	"apps/images/circus.cpm" "apps/images/cyl_test.cpm" "apps/images/logo.cpm"
	"apps/images/mice.cpm" "apps/images/molecule.cpm" "apps/images/nippon3.cpm"
	"apps/images/piramid.cpm" "apps/images/rings.cpm" "apps/images/sharpend.cpm"
	"apps/images/stairs.cpm" "apps/images/temple.cpm" "apps/images/vermin.cpm")
	index 0)

(ui-window window ()
	(ui-title-bar window_title "" (0xea19) (const event_close))
	(ui-tool-bar _ ()
		(ui-buttons (0xe91d 0xe91e) (const event_prev)))
	(ui-scroll image_scroll (logior scroll_flag_vertical scroll_flag_horizontal)))

(defun win-refresh (_)
	(bind '(w h) (view-pref-size (defq canvas (canvas-load (elem (setq index _) images) 0))))
	(def image_scroll 'min_width w 'min_height h)
	(def window_title 'text (elem _ images))
	(view-add-child image_scroll canvas)
	(view-layout window_title)
	(bind '(x y) (view-get-pos window))
	(bind '(w h) (view-pref-size window))
 	(def image_scroll 'min_width 32 'min_height 32)
	(view-change-dirty window x y w h))

(defun-bind main ()
	(gui-add (apply view-change (cat (list window 320 256) (view-get-size (win-refresh index)))))
	(while (cond
		((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_close)
			nil)
		((<= event_prev id event_next)
			(win-refresh (% (+ index (dec (* 2 (- id event_prev))) (length images)) (length images))))
		(t (view-event window msg))))
	(view-hide window))

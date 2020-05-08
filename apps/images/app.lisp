;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'close)
	(byte 'prev 'next))

(defun-bind all-images (p)
	(defq out (list))
	(each! 0 -1 (lambda (f m)
		(and (eql m "8") (or (ends-with ".cpm" f) (ends-with ".tga" f))
			(push out (cat p f))))
		(reduce (lambda (l e)
			(push (elem (% _ (length l)) l) e) l) (split (pii-dirlist p) ",") (list (list) (list))))
	(sort cmp out))

(defq images (all-images '"apps/images/") index 0)

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

;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'close)
	(byte 'prev 'next))

(defun-bind all-films (p)
	(defq out (list))
	(each! 0 -1 (lambda (f m) (and (eql m "8") (ends-with ".flm" f) (push out (cat p f))))
		(unzip (split (pii-dirlist p) ",") (list (list) (list))))
	(sort cmp out))

(defq films (all-films "apps/films/") index 0 canvas nil id t)

(ui-window window ()
	(ui-title-bar window_title "" (0xea19) (const event_close))
	(ui-tool-bar _ ()
		(ui-buttons (0xe91d 0xe91e) (const event_prev)))
	(ui-scroll image_scroll (logior scroll_flag_vertical scroll_flag_horizontal)))

(defun win-refresh (_)
	(bind '(w h) (view-pref-size (setq canvas (canvas-load (elem (setq index _) films) load_flag_film))))
	(def image_scroll :min_width w :min_height h)
	(def window_title :text (elem _ films))
	(view-add-child image_scroll canvas)
	(view-layout window_title)
	(bind '(x y w h) (apply view-fit (cat (view-get-pos window) (view-pref-size window))))
 	(def image_scroll :min_width 32 :min_height 32)
	(view-change-dirty window x y w h))

(defun-bind main ()
	(bind '(x y w h) (apply view-locate (view-get-size (win-refresh index))))
	(gui-add (view-change window x y w h))
	(while id
		(task-sleep 40000)
		(canvas-swap (canvas-next-frame canvas))
		(while (mail-poll (array (task-mailbox)))
			(cond
				((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_close)
					(setq id nil))
				((<= event_prev id event_next)
					(win-refresh (% (+ index (dec (* 2 (- id event_prev))) (length films)) (length films))))
				(t (view-event window msg)))))
	(view-hide window))

(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(enums +event 0
	(enum close)
	(enum prev next))

(defun all-images (p)
	(defq out (list))
	(each! 0 -1 (lambda (f m)
		(and (eql m "8") (or (ends-with ".cpm" f) (ends-with ".tga" f)) (push out (cat p f))))
		(unzip (split (pii-dirlist p) ",") (list (list) (list))))
	(sort cmp out))

(defq images (all-images "apps/images/") index (some (# (if (eql "apps/images/logo.cpm" %0) _)) images))

(ui-window mywindow ()
	(ui-title-bar window_title "" (0xea19) +event_close)
	(ui-tool-bar _ ()
		(ui-buttons (0xe91d 0xe91e) +event_prev))
	(ui-scroll image_scroll (logior +scroll_flag_vertical +scroll_flag_horizontal)))

(defun win-refresh (_)
	(bind '(w h) (. (defq canvas (Canvas-from-file (elem (setq index _) images) 0)) :pref_size))
	(def image_scroll :min_width w :min_height h)
	(def window_title :text (elem _ images))
	(. image_scroll :add_child canvas)
	(. window_title :layout)
	(bind '(x y w h) (apply view-fit (cat (. mywindow :get_pos) (. mywindow :pref_size))))
 	(def image_scroll :min_width 32 :min_height 32)
	(. mywindow :change_dirty x y w h))

(defun main ()
	(bind '(x y w h) (apply view-locate (. (win-refresh index) :get_size)))
	(gui-add (. mywindow :change x y w h))
	(while (cond
		((= (defq id (getf (defq msg (mail-read (task-mailbox))) +ev_msg_target_id)) +event_close)
			nil)
		((<= +event_prev id +event_next)
			(win-refresh (% (+ index (dec (* 2 (- id +event_prev))) (length images)) (length images))))
		(t (. mywindow :event msg))))
	(. mywindow :hide))

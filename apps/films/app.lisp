;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(structure '+event 0
	(byte 'close+)
	(byte 'prev+ 'next+))

(defun all-films (p)
	(defq out (list))
	(each! 0 -1 (lambda (f m) (and (eql m "8") (ends-with ".flm" f) (push out (cat p f))))
		(unzip (split (pii-dirlist p) ",") (list (list) (list))))
	(sort cmp out))

(defq films (all-films "apps/films/") index 0 canvas nil id t)

(ui-window mywindow ()
	(ui-title-bar window_title "" (0xea19) +event_close+)
	(ui-tool-bar _ ()
		(ui-buttons (0xe91d 0xe91e) +event_prev+))
	(ui-scroll image_scroll (logior +scroll_flag_vertical+ +scroll_flag_horizontal+)))

(defun win-refresh (_)
	(bind '(w h) (view-pref-size (setq canvas (canvas-load (elem (setq index _) films) +load_flag_film+))))
	(def image_scroll :min_width w :min_height h)
	(def window_title :text (elem _ films))
	(. image_scroll :add_child canvas)
	(. window_title :layout)
	(bind '(x y w h) (apply view-fit (cat (. mywindow :get_pos) (. mywindow :pref_size))))
 	(def image_scroll :min_width 32 :min_height 32)
	(. mywindow :change_dirty x y w h))

(defun main ()
	(bind '(x y w h) (apply view-locate (. (win-refresh index) :get_size)))
	(gui-add (view-change mywindow x y w h))
	(while id
		(task-sleep 40000)
		(canvas-swap (canvas-next-frame canvas))
		(while (mail-poll (array (task-mailbox)))
			(cond
				((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) +event_close+)
					(setq id nil))
				((<= +event_prev+ id +event_next+)
					(win-refresh (% (+ index (dec (* 2 (- id +event_prev+))) (length films)) (length films))))
				(t (. mywindow :event msg)))))
	(. mywindow :hide))

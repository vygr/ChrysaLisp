(import "usr/env.inc")
(import "gui/lisp.inc")
(import "gui/canvas/lisp.inc")

;The screen round the mouse, made bigger, in a window that stays where it
;is put. A few times a second the GUI is asked for what is round the
;mouse, (gui-grab-rpc), from its back buffer, and that is drawn bigger.
;What it shows is the last frame the GUI drew, so of the screen under its
;own window it shows itself, as a mirror would.

(enums +select 0
	(enum main tip timer))

(enums +event 0
	(enum close)
	(enum bigger smaller))

(defq +width 360 +height 240 +tick 100000 +zooms ''(2 3 4 6 8))

(ui-window *window* (:resizable :nil)
	(ui-title-bar *title* "Magnifier" (+sym_close) +event_close)
	(ui-tool-bar *tools* ()
		(ui-buttons (+sym_zoom_in +sym_zoom_out) +event_bigger))
	(ui-canvas *lens* +width +height 1))

(ui-tool-tips *tools* '("zoom in" "zoom out"))

(defun look ()
	;what is round the mouse, made the size of the lens
	(defq sw (/ +width *zoom*) sh (/ +height *zoom*)
		got (gui-grab-rpc (cat "mouse " (str sw) " " (str sh))))
	(when (list? got)
		(bind '(w h pixels) got)
		(unless (and *small* (= w *small_w*) (= h *small_h*))
			(setq *small* (Canvas w h 1) *small_w* w *small_h* h))
		(pixmap-read (getf *small* +canvas_pixmap 0) (string-stream pixels) 32)
		(.-> *lens* (:resize *small*) (:swap 0))))

(defun zoom (by)
	(defq at (find *zoom* +zooms))
	(setq *zoom* (elem-get +zooms (max 0 (min (dec (length +zooms)) (+ at by)))))
	(set *title* :text (cat "Magnifier " (str *zoom*) "x"))
	(.-> *title* :layout :dirty))

(defun main ()
	(defq id :t select (task-mboxes +select_size) *zoom* 3 *small* :nil *small_w* 0 *small_h* 0)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(zoom 0)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(mail-timeout (elem-get select +select_timer) +tick 0)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((and (= id +event_bigger) (= (getf msg +ev_msg_type) +ev_type_action)) (zoom 1))
					((and (= id +event_smaller) (= (getf msg +ev_msg_type) +ev_type_action)) (zoom -1))
					((. *window* :event msg))))
			(+select_tip
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))
			(+select_timer
				(look)
				(mail-timeout (elem-get select +select_timer) +tick 0))))
	(gui-sub-rpc *window*))

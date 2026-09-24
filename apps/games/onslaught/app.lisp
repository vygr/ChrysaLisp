;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Onslaught 2D Game Engine Framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *app_root* (path-to-file))

(import "usr/env.inc")
(import "lib/debug/frames.inc")
(import "gui/lisp.inc")

(enums +select 0
	(enum main timer))

; Engine core includes
(import "./enums.inc")
(import "./sprite.inc")
(import "./components.inc")
(import "./widgets.inc")
(import "./actions.inc")
(import "./title.inc")

(defq
	*running* :t)

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	(defq select (task-mboxes +select_size))
	(setq *running* :t)

	; Load panel canvas directly and position at bottom (0 168 320 72)
	(when (defq panel (canvas-load (cat *app_root* "image/panel.cpm") +load_flag_shared))
		(def panel :offset_x 0 :offset_y 0)
		(. panel :set_bounds 0 168 320 72)
		(. *layer_panel* :add_front panel))

	; Position and show window on desktop with all children already attached
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (.-> *window* (:change x y w h :t) :dirty_all))

	; Start the intro title sequence (flying letters, sword, blood)
	(title-sequence-start *playfield*)

	; Start 30 FPS game loop timer
	(mail-timeout (elem-get select +select_timer) +rate 0)

	; Main event loop
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				; Dispatch UI events (close, min, max, clicks, keys)
				(cond
					((. *window* :dispatch *msg*))
					((. *window* :event *msg*))))
			(+select_timer
				; Re-arm 30 FPS timer
				(mail-timeout (elem-get select +select_timer) +rate 0)
				; Advance title animation frame
				(title-sequence-update *playfield* +dt))))

	; Unregister window and exit cleanly
	(gui-sub-rpc *window*))

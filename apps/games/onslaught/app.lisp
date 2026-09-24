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
(import "./field.inc")

(defq
	*running* :t
	*game_state* :title)

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	(defq select (task-mboxes +select_size))
	(setq *running* :t *game_state* :title)

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
					((and (or (= (getf *msg* +ev_msg_type) +ev_type_key_down)
							  (= (getf *msg* +ev_msg_type) +ev_type_key_up))
						  (not (Textfield? (. *window* :find_id (getf *msg* +ev_msg_target_id)))))
						(defq
							type (getf *msg* +ev_msg_type)
							key (getf *msg* +ev_msg_key_key)
							kmask :nil)
						(cond
							((or (= key (ascii-code "q")) (= key (ascii-code "Q")))
								(setq kmask +fkey_up))
							((or (= key (ascii-code "a")) (= key (ascii-code "A")))
								(setq kmask +fkey_down))
							((or (= key (ascii-code "o")) (= key (ascii-code "O")))
								(setq kmask +fkey_left))
							((or (= key (ascii-code "p")) (= key (ascii-code "P")))
								(setq kmask +fkey_right))
							((= key (ascii-code " "))
								(setq kmask +fkey_keya)))
						(when kmask
							(if (= type +ev_type_key_down)
								(setq *game_controls* (logior *game_controls* kmask))
								(setq *game_controls* (logand *game_controls* (lognot kmask)))))
						:t)
					((. *window* :dispatch *msg*))
					((. *window* :event *msg*))))
			(+select_timer
				; Re-arm 30 FPS timer
				(mail-timeout (elem-get select +select_timer) +rate 0)
				(case *game_state*
					(:title
						(title-sequence-update *playfield* +dt)
						(when (eql *title_state* :done)
							(setq *game_state* :field)
							(field-sequence-start *playfield* 1)))
					(:field
						(field-sequence-update *playfield* +dt))))))

	; Unregister window and exit cleanly
	(gui-sub-rpc *window*))

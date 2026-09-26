;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught 2d game engine framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *app_root* (path-to-file))

(defq *running* :t *game_state* :title +zoom_min 1 +zoom_max 3 *zoom* 2
	*old_zoom* *zoom* +frame_rate 20 *running* :t *game_state* :title)

(import "lib/debug/frames.inc")

(import "service/audio/app.inc")
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./enums.inc")
(import "./map.inc")
(import "./sky.inc")
(import "./widgets.inc")
(import "./components.inc")
(import "./utils.inc")
(import "./assets.inc")
(import "./sprite.inc")
(import "./actions.inc")
(import "./title.inc")
(import "./field.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun window-resize ()
	; load assets
	(load-cpm-assets *zoom*)
	(clear-layer *layer_panel_detail*)
	(defq win_w (* *zoom* +screen_width)
		win_h (* *zoom* (- +screen_height +panel_height))
		pan_h (* *zoom* +panel_height))
	(set *world_scroll* :min_width win_w :min_height win_h)
	(set *panel_layers* :min_width win_w :min_height pan_h)
	(. *world_scroll* :set_bounds 0 0 win_w win_h)
	(. *world_layers* :set_bounds 0 0 win_w win_h)
	(. *layer_panel_detail* :add_front *img_panel*)
	(. *layer_land* :load_field_map)
	(when (/= *zoom* *old_zoom*)
		(rescale-active-sprites)
		(case *game_state*
			(:title
				(set-world-layers-size (* *zoom* +window_width) (* *zoom* +window_height))
				(set-world-layers-pos 0 0))
			(:field
				(set-world-layers-size (* *zoom* (* +map_width +tile_width)) (* *zoom* (* +map_height +tile_height)))
				(when *player_man*
					(bind '(mx my) (. *player_man* :sp_get_pos))
					(update-camera mx my))))
		(setq *old_zoom* *zoom*))
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y w h))
	(.-> *window* (:change_dirty x y w h :t)))

(enums +select 0
	(enum main timer trash))

(defun main ()
	(defq select (task-mboxes +select_size)
		game_service (mail-declare (elem-get select +select_trash) "@Onslaught" "Onslaught Game 1.0"))
	(def *window* :zoom *zoom*)
	(load-wav-assets)
	(window-resize)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	; start the intro title sequence (flying letters, sword, blood)
	(title-sequence-start)

	; start 30 fps game loop timer
	(mail-timeout (elem-get select +select_timer) (const (/ 1000000 +frame_rate)) 0)
	; main event loop
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				; dispatch ui events (close, min, max, clicks, keys)
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
				; re-arm 30 fps timer
				(mail-timeout (elem-get select +select_timer) (const (/ 1000000 +frame_rate)) 0)
				(case *game_state*
					(:title
						(title-sequence-update)
						(when (eql *title_state* :done)
							(setq *game_state* :field)
							(field-sequence-start)))
					(:field
						(field-sequence-update)))
				(. *world_scroll* :dirty_all)
				(. *panel_layers* :dirty_all))))
	; unregister window and exit cleanly
	(mail-forget game_service)
	(unload-wav-assets)
	(gui-sub-rpc *window*))
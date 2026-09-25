;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/onslaught/app.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught 2d game engine framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *app_root* (path-to-file))

(enums +select 0
	(enum main timer))

(defq *running* :t *game_state* :title +zoom_1x 1 +zoom_2x 2 +zoom_3x 3 +zoom_min 1 +zoom_max 3
	*zoom* +zoom_2x *old_zoom* *zoom*)

(import "service/audio/app.inc")
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./map.inc")
(import "./sky.inc")
(import "./widgets.inc")
(import "./components.inc")
(import "./utils.inc")
(import "./assets.inc")
(import "./enums.inc")
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
	(defq
		win_w (* *zoom* +game_width)
		win_h (* *zoom* (- +game_height +panel_height))
		pan_h (* *zoom* +panel_height))
	(set *world_scroll* :min_width win_w :min_height win_h)
	(set *panel_layers* :min_width win_w :min_height pan_h)
	(. *world_scroll* :set_bounds 0 0 win_w win_h)
	(. *world_layers* :set_bounds 0 0 win_w win_h)
	(. *layer_panel_detail* :add_front *img_panel*)
	(defq block_canvas (cond
			((= (get :map_idx *layer_land*) 2) *img_blocks2*)
			((= (get :map_idx *layer_land*) 3) *img_blocks3*)
			(:t *img_blocks1*)))
	(. *layer_land* :set_blocks_canvas block_canvas *zoom*)
	(when (/= *zoom* *old_zoom*)
		(rescale-active-sprites *old_zoom* *zoom*)
		(case *game_state*
			(:title
				(set-world-layers-size (* *zoom* +window_width) (* *zoom* +window_height))
				(set-world-layers-pos 0 0)
				(. *layer_sky* :dirty))
			(:field
				(defq
					world_w (* *zoom* (* +map_width +tile_width))
					world_h (* *zoom* (* +map_height +tile_height)))
				(set-world-layers-size world_w world_h)
				(if *player_man*
					(progn
						(bind '(mx my) (. *player_man* :get_pos))
						(update-camera mx my))
					(setq *cam_x* (/ (* *cam_x* *zoom*) *old_zoom*)
						  *cam_y* (/ (* *cam_y* *zoom*) *old_zoom*))
					(set-world-layers-pos (neg *cam_x*) (neg *cam_y*)))
				(. *layer_sky* :dirty)
				(. *layer_land* :dirty)))
		(setq *old_zoom* *zoom*))
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y w h))
	(.-> *window* (:change_dirty x y w h :t)))

(defun main ()
	(defq select (task-mboxes +select_size))
	(setq *running* :t *game_state* :title)

	(load-wav-assets)
	(window-resize)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	; start the intro title sequence (flying letters, sword, blood)
	(title-sequence-start)

	; start 30 fps game loop timer
	(mail-timeout (elem-get select +select_timer) +rate 0)
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
				(mail-timeout (elem-get select +select_timer) +rate 0)
				(case *game_state*
					(:title
						(title-sequence-update)
						(when (eql *title_state* :done)
							(setq *game_state* :field)
							(field-sequence-start)))
					(:field
						(field-sequence-update))))))

	; unregister window and exit cleanly
	(gui-sub-rpc *window*))
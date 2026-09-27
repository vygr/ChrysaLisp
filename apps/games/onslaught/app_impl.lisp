;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught 2d game engine framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./enums.inc")

(defq *app_root* (path-to-file) *game_state* +game_state_title +zoom_min 1 +zoom_max 3 *zoom* 2
	*old_zoom* 0 +frame_rate 20 *running* :t *old_game_state* -1)

(import "lib/debug/frames.inc")
(import "service/audio/app.inc")
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./assets.inc")

(import "./map.inc")
(import "./sky.inc")
(import "./widgets.inc")
(import "./actions.inc")

(import "./utils.inc")
(import "./sprite.inc")
(import "./fanatic.inc")
(import "./enemy.inc")

(import "./title.inc")
(import "./battle.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun window-resize ()
	; load assets
	(when (/= *zoom* *old_zoom*)
		(load-cpm-assets (setq *old_zoom* *zoom*))
		(clear-layer *layer_panel_detail*)
		(defq win_w (* *zoom* +screen_width)
			win_h (* *zoom* (- +screen_height +panel_height))
			pan_h (* *zoom* +panel_height))
		(set *world_scroll* :min_width win_w :min_height win_h)
		(set *panel_layers* :min_width win_w :min_height pan_h)
		(. *world_scroll* :set_bounds 0 0 win_w win_h)
		(. *world_layers* :set_bounds 0 0 win_w win_h)
		(. *layer_panel_detail* :add_front *img_panel*)
		(. *layer_land* :reset_field_map)
		(set-world-layers-pos 0 0)
		(set-world-layers-size (* *zoom* +window_width) (* *zoom* +window_height))
		(set-world-layers-size (* *zoom* (* +map_width +tile_width)) (* *zoom* (* +map_height +tile_height)))
		(rescale-active-sprites)
		(if *player_man* (update-camera *player_man*))
		(bind '(x y) (. *window* :get_pos))
		(bind '(w h) (. *window* :pref_size))
		(bind '(x y w h) (view-fit x y w h))
		(.-> *window* (:change_dirty x y w h :t))))

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
				(when (/= *game_state* *old_game_state*)
					(setq *old_game_state* *game_state*)
					(case *game_state*
						(+game_state_title
							(title-state-init))
						(+game_state_menu
							(menu-state-init))
						(+game_state_map
							(map-state-init))
						(+game_state_scores
							(scores-state-init))
						(+game_state_hiscore
							(hiscore-state-init))
						(+game_state_battle
							(battle-state-init))
						(+game_state_battle_won
							(battle-won-state-init))
						(+game_state_battle_lost
							(battle-lost-state-init))
						(+game_state_mind
							(mind-state-init))
						(+game_state_mind_won
							(mind-won-state-init))
						(+game_state_mind_lost
							(mind-lost-state-init))
						(+game_state_credits
							(credits-state-init))
						(+game_state_oracle
							(oracle-state-init))
						(+game_state_demo
							(demo-state-init))))
				(when (= *game_state* *old_game_state*)
					(case *game_state*
						(+game_state_title
							(title-state-update))
						(+game_state_menu
							(menu-state-update))
						(+game_state_map
							(map-state-update))
						(+game_state_scores
							(scores-state-update))
						(+game_state_hiscore
							(hiscore-state-update))
						(+game_state_battle
							(battle-state-update))
						(+game_state_battle_won
							(battle-won-state-update))
						(+game_state_battle_lost
							(battle-lost-state-update))
						(+game_state_mind
							(mind-state-update))
						(+game_state_mind_won
							(mind-won-state-update))
						(+game_state_mind_lost
							(mind-lost-state-update))
						(+game_state_credits
							(credits-state-update))
						(+game_state_oracle
							(oracle-state-update))
						(+game_state_demo
							(demo-state-update))))
				(. *world_scroll* :dirty_all)
				(. *panel_layers* :dirty_all))))
	; unregister window and exit cleanly
	(mail-forget game_service)
	(unload-wav-assets)
	(gui-sub-rpc *window*))
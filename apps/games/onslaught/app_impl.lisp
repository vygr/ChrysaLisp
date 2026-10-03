;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught 2d game engine framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./enums.inc")
(import "./app.inc")

(defq *app_root* (path-to-file) *game_state* +game_state_menu *game_state_next* +game_state_menu
	+zoom_min 1 +zoom_max 3 *zoom* 2 *old_zoom* 0 +frame_rate 20 *running* :t
	*old_game_state* -1 +rate (/ 1000000 +frame_rate) *game_flags* 0)

(import "lib/debug/frames.inc")
(import "lib/consts/scodes.inc")
(import "service/audio/app.inc")
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./assets.inc")

(import "./sprite.inc")
(import "./map.inc")
(import "./sky.inc")
(import "./widgets.inc")
(import "./actions.inc")

(import "./utils.inc")
(import "./collisions.inc")
(import "./addons.inc")
(import "./fanatic.inc")
(import "./enemy.inc")
(import "./footman.inc")
(import "./knight.inc")
(import "./spearman.inc")
(import "./wizard.inc")
(import "./horse.inc")
(import "./balista.inc")
(import "./cannon.inc")
(import "./oil.inc")
(import "./boarrider.inc")
(import "./carpet.inc")
(import "./tower.inc")
(import "./monk.inc")
(import "./beserk.inc")
(import "./skeleton.inc")
(import "./skeleton_monk.inc")
(import "./skeleton_horse.inc")
(import "./title.inc")
(import "./config.inc")
(import "./demo.inc")
(import "./battle.inc")
(import "./menu.inc")
(import "./mind.inc")
(import "./campaign.inc")
(import "./remote.inc")

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
		(. *world_scroll* :change 0 0 win_w win_h :t)
		(. *world_layers* :change 0 0 win_w win_h :t)
		(. *layer_panel_detail* :change 0 0 win_w pan_h :t)
		(. *layer_panel_detail* :add_front *img_panel*)
		(. *layer_panel_items* :change
			(* *zoom* +panel_items_x) (* *zoom* +panel_items_y)
			(* *zoom* +panel_items_w) (* *zoom* +panel_items_h) :t)
		(. *layer_panel_status* :change
			(* *zoom* +panel_status_x) (* *zoom* +panel_status_y)
			(* *zoom* +panel_status_w) (* *zoom* +panel_status_h) :t)
		(. *layer_panel_axes* :change
			(* *zoom* +panel_axes_x) (* *zoom* +panel_axes_y)
			(* *zoom* +panel_axes_w) (* *zoom* +panel_axes_h) :t)
		(def *panel_flag_l* :sp_canvas_l *img_frm_32c32*)
		(def *panel_flag_r* :sp_canvas_l *img_frm_32c32*)
		(def *panel_power* :sp_canvas_l *img_frm_32c32*)
		(def *panel_strength* :sp_canvas_l *img_frm_32c32*)
		(. *layer_land* :reset_field_map)
		(set-world-layers-pos 0 0)
		(set-world-layers-size (* *zoom* (* +map_width +tile_width)) (* *zoom* (* +map_height +tile_height)))
		(rescale-active-sprites)
		(update-panel-status)
		(menu-resize *zoom*)
		(campaign-resize *zoom*)
		(mind-resize *zoom*)
		(when *player_man* (update-camera *player_man*))
		(bind '(x y) (. *window* :get_pos))
		(bind '(w h) (. *window* :pref_size))
		(bind '(x y w h) (view-fit x y w h))
		(.-> *window* (:change_dirty x y w h :t))))

(enums +select 0
	(enum main timer service))

(defun main ()
	(defq select (task-mboxes +select_size)
		game_service (mail-declare (elem-get select +select_service) "@Onslaught" "Onslaught Game 1.0"))
	(def *window* :zoom *zoom*)
	(load-wav-assets)
	(config-load)
	(window-resize)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	; start game loop timer
	(mail-timeout (elem-get select +select_timer) +rate 0)
	; main event loop
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				; dispatch ui events (close, min, max, clicks, keys)
				(cond
					((and (or (= (defq ev_type (getf *msg* +ev_msg_type)) +ev_type_key_down)
							(= ev_type +ev_type_key_up))
						(not (Textfield? (. *window* :find_id (getf *msg* +ev_msg_target_id)))))
						(defq key (getf *msg* +ev_msg_key_key)
							scode (getf *msg* +ev_msg_key_scode) kmask :nil)
						(cond
							((or (= scode +sc_up) (= scode +sc_w) (= scode +sc_q)
								 (= key (ascii-code "q")) (= key (ascii-code "Q")) (= key (ascii-code "w")) (= key (ascii-code "W")) (= key 0x40000052))
								(setq kmask +fkey_up))
							((or (= scode +sc_down) (= scode +sc_s) (= scode +sc_a)
								 (= key (ascii-code "a")) (= key (ascii-code "A")) (= key (ascii-code "s")) (= key (ascii-code "S")) (= key 0x40000051))
								(setq kmask +fkey_down))
							((or (= scode +sc_left) (= scode +sc_o)
								 (= key (ascii-code "o")) (= key (ascii-code "O")) (= key 0x40000050))
								(setq kmask +fkey_left))
							((or (= scode +sc_right) (= scode +sc_p)
								 (= key (ascii-code "p")) (= key (ascii-code "P")) (= key 0x4000004f))
								(setq kmask +fkey_right))
							((or (= scode +sc_space) (= scode +sc_return) (= scode +sc_kp_enter)
								 (= key (ascii-code " ")) (= key +char_lf) (= key +char_cr) (= key 0x40000058))
								(setq kmask +fkey_keya))
							((or (= scode +sc_b) (= key (ascii-code "b")) (= key (ascii-code "B")))
								(setq kmask +fkey_keyb))
							((or (= scode +sc_leftbracket) (= scode +sc_z)
								 (= key (ascii-code "[")) (= key (ascii-code "z")) (= key (ascii-code "Z")))
								(setq kmask +fkey_keyc))
							((or (= scode +sc_rightbracket) (= scode +sc_x)
								 (= key (ascii-code "]")) (= key (ascii-code "x")) (= key (ascii-code "X")))
								(setq kmask +fkey_keyd)))
						(when kmask
							(if (= ev_type +ev_type_key_down)
								(setq *game_controls* (logior *game_controls* kmask)
									*user_controls* (logior *user_controls* kmask))
								(setq *game_controls* (logand *game_controls* (lognot kmask))
									*user_controls* (logand *user_controls* (lognot kmask))))
							:t))
					((. *window* :dispatch *msg*))
					((. *window* :event *msg*))))
			(+select_service
				; remote play requests
				(remote-request *msg*))
			(+select_timer
				; re-arm frame timer
				(mail-timeout (elem-get select +select_timer) +rate 0)
				(when (/= *game_state* *old_game_state*)
					(setq *old_game_state* *game_state*)
					(case *game_state*
						(+game_state_title
							(title-state-init *game_state_next*))
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
				(update-frame))))
	; unregister window and exit cleanly
	(record-battle-finish)
	(demo-restore-settings)
	(campaign-snapshot)
	(config-save)
	(mail-forget game_service)
	(unload-wav-assets)
	(gui-sub-rpc *window*))
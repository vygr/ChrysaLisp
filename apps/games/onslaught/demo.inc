;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught demo mode recording & playback
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	; input recording & playback state for demo mode
	*record_stream* :nil
	*record_keys* -1
	*record_count* 0
	*record_start_score* 0
	*playback_stream* :nil
	*playback_keys* 0
	*playback_count* 0
	*user_controls* 0
	*demo_skip_armed* :nil)

(defun record-battle-start ()
	; initialize demo recording stream with initial battle state header using write-xxx
	(setq *record_keys* -1
		  *record_count* 0
		  *record_start_score* *man_score*
		  *record_stream* (string-stream (cat "")))
	(write-long *record_stream* *game_seed*)
	(write-short *record_stream* *game_level*)
	(write-short *record_stream* *current_map_idx*)
	(write-short *record_stream* *enemy_army*)
	(write-short *record_stream* *enemy_banner*)
	(write-short *record_stream* *man_banner*)
	(write-short *record_stream* *enemy_wizshot*)
	(write-int *record_stream* *man_power*)
	(write-int *record_stream* *man_strength*)
	(write-short *record_stream* *item_selected*)
	(each (# (write-short *record_stream* %0)) *item_inventory*)
	(each (# (write-short *record_stream* %0)) *item_usage*)
	(write-short *record_stream* *menu_difficulty*)
	(write-short *record_stream* *menu_mode*)
	(write-int *record_stream* *man_score*)
	(write-short *record_stream* *mine_max*)
	(write-short *record_stream* *enemy_max*)
	(write-long *record_stream* (get :sky_top *layer_sky*))
	(write-long *record_stream* (get :sky_bottom *layer_sky*)))

(defun record-battle-tick (k)
	; record one tick of user controls using 2-byte rle (keys, count)
	(when *record_stream*
		(defq ctrl (logand k 0xff))
		(if (= *record_keys* -1)
			(setq *record_keys* ctrl *record_count* 0)
			(if (or (/= *record_keys* ctrl) (= *record_count* 255))
				(progn
					(write-byte *record_stream* *record_keys*)
					(write-byte *record_stream* *record_count*)
					(setq *record_keys* ctrl *record_count* 0))
				(++ *record_count*)))))

(defun record-battle-finish ()
	; finish recording and if battle score beats current best demo score, update and save to config
	(when *record_stream*
		(when (/= *record_keys* -1)
			(write-byte *record_stream* *record_keys*)
			(write-byte *record_stream* *record_count*))
		(defq demo_str (str *record_stream*)
			  battle_score (- *man_score* *record_start_score*))
		(setq *record_stream* :nil)
		(when (or (> battle_score *best_demo_score*)
				  (ifn *best_demo_data* :t (empty? *best_demo_data*)))
			(setq *best_demo_score* (max 0 battle_score)
				  *best_demo_data* demo_str)
			(config-save))))

(defun demo-play-keys ()
	(if (> *playback_count* 0)
		(progn
			(-- *playback_count*)
			*playback_keys*)
		(if (defq k (read-ubyte *playback_stream*))
			(if (defq c (read-ubyte *playback_stream*))
				(progn
					(setq *playback_keys* k *playback_count* c)
					*playback_keys*)
				-1)
			-1)))

(defun demo-state-init ()
	(if (or (ifn *best_demo_data* :t) (empty? *best_demo_data*))
		(progn
			(setq *playback_stream* :nil)
			(goto-title +game_state_menu))
		(progn
			(defq in_s (string-stream *best_demo_data*))
			(bind '(game_seed game_level map_idx enemy_army enemy_banner man_banner enemy_wizshot)
				(list (read-long in_s) (read-short in_s) (read-short in_s) (read-short in_s)
					  (read-short in_s) (read-short in_s) (read-short in_s)))
			(bind '(man_power man_strength item_selected)
				(list (read-int in_s) (read-int in_s) (read-short in_s)))
			(defq item_inv (list) i 0)
			(while (< i 8)
				(push item_inv (read-short in_s))
				(++ i))
			(defq item_use (list) i 0)
			(while (< i 8)
				(push item_use (read-short in_s))
				(++ i))
			(bind '(menu_diff menu_mode man_score mine_max enemy_max sky_top sky_bottom)
				(list (read-short in_s) (read-short in_s) (read-int in_s)
					  (read-short in_s) (read-short in_s) (read-long in_s) (read-long in_s)))
			(clear-sprite-layers)
			(set-world-layers-pos 0 0)
			(set-world-layers-size (* *zoom* (* +map_width +tile_width)) (* *zoom* (* +map_height +tile_height)))
			(. *layer_sky* :sky_set_cols sky_top sky_bottom)
			(setq *game_level* game_level
				  *current_map_idx* map_idx)
			(case game_level
				(+game_level_seige
					(. *layer_land* :load_seige_map))
				(+game_level_defend
					(. *layer_land* :load_defend_map))
				(:t
					(. *layer_land* :load_field_map map_idx)))
			(def (defq man (Fanatic)) :sp_dir 1 :man_xv 0 :man_yv 0 :man_walk_tick 0)
			(. man :sp_set_pos 8 208)
			(. *layer_player* :add_front man)
			(setq *player_man* man
				  *man_power* man_power
				  *man_strength* man_strength
				  *item_selected* item_selected
				  *item_inventory* item_inv
				  *item_usage* item_use
				  *menu_difficulty* menu_diff
				  *menu_mode* menu_mode
				  *game_mode* menu_mode
				  *man_score* man_score
				  *mine_max* mine_max
				  *enemy_max* enemy_max
				  *enemy_army* enemy_army
				  *enemy_banner* enemy_banner
				  *man_banner* man_banner
				  *enemy_wizshot* enemy_wizshot
				  *enemy_stack* (list)
				  *enemy_banner_captured* :nil
				  *enemy_count* 0
				  *enemy_spawn_delay* 1
				  *mine_count* 0
				  *game_controls* 0
				  *demo_skip_armed* :nil
				  *playback_stream* in_s
				  *playback_keys* 0
				  *playback_count* 0)
			(bind '(mx my ex ey) (case *game_level*
				(+game_level_seige '(16 192 2016 0))
				(+game_level_defend '(16 0 2016 192))
				(:t '(16 128 2016 128))))
			(defq man_ban (Banner +ftp_man_banner *man_banner*))
			(. man_ban :sp_set_pos mx my)
			(. *layer_bans* :add_front man_ban)
			(defq enemy_ban (Banner +ftp_enemy_banner *enemy_banner*))
			(. enemy_ban :sp_set_pos ex ey)
			(. *layer_bans* :add_front enemy_ban)
			(update-camera *player_man*)
			(game-seed game_seed))))

(defun demo-state-update ()
	; 1. Check if user pressed physical controls to abort demo and return to menu
	(if (/= *user_controls* 0)
		(setq *demo_skip_armed* :t)
		(when *demo_skip_armed*
			(setq *demo_skip_armed* :nil
				  *playback_stream* :nil
				  *game_controls* 0)
			(goto-title +game_state_menu)))
	; 2. Advance recorded playback
	(when *playback_stream*
		(defq keys (demo-play-keys))
		(cond
			((= keys -1)
				(setq *playback_stream* :nil
					  *game_controls* 0)
				(goto-title +game_state_credits))
			((empty? (. *layer_player* :children))
				(setq *playback_stream* :nil
					  *game_controls* 0)
				(goto-title +game_state_credits))
			((and *player_man* (get :sp_dead *player_man*))
				; death sequence in progress, wait for manbits to leave screen
				:nil)
			((or (>= (length *enemy_stack*) +max_enemy_stack) *enemy_banner_captured*)
				(setq *playback_stream* :nil
					  *game_controls* 0)
				(goto-title +game_state_credits))
			(:t
				(setq *game_controls* keys)
				(init-mines)
				(init-enemies)
				(update-camera *player_man*)))))

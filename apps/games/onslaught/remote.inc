;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught remote play service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +remote_state_names
	''(:title :menu :map :scores :hiscore :battle :battle_won :battle_lost
		:mind :mind_won :mind_lost :credits :oracle :demo))

(defun remote-sprites (layer)
	; (remote-sprites layer) -> ((class frame x y) ...)
	(reduce (lambda (out sp)
			(if (Sprite? sp)
				(push out (list (last (. sp :type_of)) (get :sp_frame sp)
					(get :sp_x sp) (get :sp_y sp)))
				out))
		(. layer :children) (list)))

(defun remote-man ()
	; (remote-man) -> (x y w h dir) | :nil
	(and (= *game_state* +game_state_battle) *player_man*
		(not (get :sp_dead *player_man*))
		(push (. *player_man* :sp_get_bounds) (get :sp_dir *player_man*))))

(defun remote-land ()
	; (remote-land) -> str, tile flags of the battle map, row by row
	(apply (const cat) (map (lambda (i)
			(char (. *layer_land* :get_tile_flags
				(* (% i +map_width) +tile_width) (* (/ i +map_width) +tile_height))))
		(range 0 (const (* +map_width +map_height))))))

(defun remote-state ()
	; (remote-state) -> (key value ...)
	(list :state (elem-get +remote_state_names *game_state*) :man (remote-man)
		:level *game_level* :frame *game_frame_count* :menu *menu_selected*
		;the letter under the sight when entering initials
		:letter (and (= *game_state* +game_state_hiscore) *hiscore_entry*
			(elem-get +hiscore_letters (% (+ *hiscore_text* +hiscore_pick)
				(length +hiscore_letters))))
		:score *man_score* :power *man_power* :strength *man_strength*
		:inventory *item_inventory* :usage *item_usage* :selected *item_selected*
		:location (list *location_x* *location_y*)
		:back (list *location_ox* *location_oy*) :territory *man_territory*
		:map (if (= *game_state* +game_state_map) *campaign_map* (list))
		:army (elem-get +army_names *enemy_army*) :stack (length *enemy_stack*)
		:player (remote-sprites *layer_player*)
		:enemies (remote-sprites *layer_enemies*)
		:missiles (remote-sprites *layer_missiles*)
		:items (remote-sprites *layer_items*)
		:bans (remote-sprites *layer_bans*)
		;peak task stack use on the game's node, and its memory use
		:max_stack (last (defq stats (kernel-stats))) :mem_used (second stats)))

(defun remote-request (msg)
	; act on a remote play request, anything malformed is just trash
	(when (>= (length msg) +ons_rpc_size)
		(defq reply_id (getf msg +ons_rpc_reply_id))
		(case (getf msg +ons_rpc_type)
			(+ons_type_keys
				;remote keys act just as the user's keys do
				(setq *game_controls* (logand (getf msg +ons_rpc_keys) 0xff)
					*user_controls* *game_controls*)
				(mail-send reply_id "ok"))
			(+ons_type_state
				(mail-send reply_id (str (remote-state))))
			(+ons_type_land
				(mail-send reply_id (remote-land)))
			(+ons_type_quit
				(setq *running* :nil)
				(mail-send reply_id "ok")))))

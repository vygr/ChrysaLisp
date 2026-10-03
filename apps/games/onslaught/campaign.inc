;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught campaign map, mind and oracle state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +campaign_width 16 +campaign_size 256 +campaign_event_rate 200
	+campaign_x 96 +campaign_y (- (/ +window_height 2) 64) +argb_dyellow 0xff7f7f00
	*campaign_map* (list) *campaign_view* :nil *campaign_sight* :nil
	*campaign_ban* :nil *campaign_lord* :nil
	*location_x* 0 *location_y* 0 *location_last_x* 0 *location_last_y* 0
	*location_ox* -1 *location_oy* 0 *event_count* 0
	*plague_create* 0 *crusade_create* 0 *rebellion_create* 0
	*plague_destroy* 0 *crusade_destroy* 0 *rebellion_destroy* 0
	*man_name* "" *man_homeland* "" *man_cult* 0 *man_experience* 0 *man_territory* 0
	*enemy_kingdom* "" *enemy_cult* 0 *enemy_lord* 0 *enemy_population* 0
	*enemy_popularity* 0 *enemy_info_col* +argb_green
	+part_names3 "GARRAGVARGERMONGORBALFAGGAFBANVERLATTOGROGTATMANRUD"
	+part_names2 "GOATITISETINMI"
	+status_names
	''("SCUM" "WARRIOR" "FANATIC" "CHAMPION" "HERO"
		"BARRON" "LORD" "LEGEND" "REAPER" "DEMIGOD")
	+cult_names
	''("EAGLE" "LIGHTNING" "ARDUCK" "SHESH" "BOAR" "STAG" "EYE" "MOON"
		"HOLY" "HAND" "ZORACK" "BROBEK" "MOUNTAIN" "FOREST" "RIMOG" "DEATH")
	+lord_names ''("COMMANDER" "CHIEFTAIN" "LORD" "DEMON")
	+popularity
	''(0 500 200 1000 300 1600 700 800 1800 400 900 1200 600 1400 100 2000)
	; location info indexed by campain tile type (colour line ...)
	+location_names
	`'((,+argb_white "THE" "ORACLE OF" "GARGORE")
		(,+argb_white "THIS IS A" "WATER" "TEMPLE")
		(,+argb_white "THIS IS A" "SWAMP" "TEMPLE")
		(,+argb_white "THIS IS A" "FOREST" "TEMPLE")
		(,+argb_white "THIS IS A" "MOUNTAIN" "TEMPLE")
		(,+argb_cyan "THIS IS A" "WATER" "LOCATION")
		(,+argb_yellow "THIS IS A" "SWAMP" "LOCATION")
		(,+argb_green "THIS IS A" "FOREST" "LOCATION")
		(,+argb_white "THIS IS A" "MOUNTAIN" "LOCATION")
		(,+argb_yellow "THIS IS A" "PLAYER" "LOCATION")
		(,+argb_yellow "THIS IS A" "PLAYER" "LOCATION")
		(,+argb_yellow "THIS IS A" "PLAYER" "LOCATION")
		(,+argb_yellow "THIS IS A" "PLAYER" "LOCATION")
		(,+argb_yellow "THIS IS A" "PLAYER" "LOCATION"))
	; oracle hints indexed by experience (x1 line1 x2 line2)
	+oracle_hints
	''((32 "TRY TO KEEP YOUR WEAPON TURNOVER" 80 "AS HIGH AS POSSIBLE !")
		(40 "TRY GOING OVERGROUND IF FACED" 64 "WITH A VERY STRONG ARMY !")
		(32 "ALWAYS HAVE AS MANY BLUE DEMONS" 56 "FLYING AROUND AS POSSIBLE !")
		(50 "THE SMALL CROSSBOW IS THE BEST" 40 "WEAPON WHEN FACED WITH MONKS !")
		(32 "WHITE SPELLS ARE VERY USEFULL IF" 48 "SAVED FOR USE ON THE TOWER !")
		(32 "YELLOW SPELLS ARE MORE EFFECTIVE" 80 "WHEN YOU ARE DUCKING !")
		(32 "MAXIMIZE YELLOW SPELL FIREBALLS" 72 "BY RUNNING AFTER THEM !")
		(48 "MINES CAN BE USED AS WEAPONS" 48 "IF YOU HAVE ENOUGH STRENGTH !")
		(40 "USE LEDGE TO LEFT OF TOWER TO" 40 "REDUCE FLAG LEVEL ON WAY UP !")
		(48 "GOT A HELL OF A NERVE ASKING" 64 "FOR HINTS AT THIS LEVEL !"))
	+oracle_no_hint ''(64 "YOU GET NO HINTS IN EASY" 88 "MODE OR TUTOR MODE"))

;;;;;;;;;;;;;;;;;;;;;;
; campaign map helpers
;;;;;;;;;;;;;;;;;;;;;;

(defun campaign-location ()
	; (campaign-location) -> index of the current location
	(+ *location_x* (* *location_y* +campaign_width)))

(defun campaign-replace (from to)
	; (campaign-replace from to)
	(each (# (if (= %0 from) (elem-set *campaign_map* (!) to))) *campaign_map*))

(defun campaign-count (tile)
	; (campaign-count tile) -> count
	(reduce (# (if (= %1 tile) (inc %0) %0)) *campaign_map* 0))

(defun campaign-set-location (x y)
	; (campaign-set-location x y)
	(setq *location_x* x *location_y* y)
	(when (= (elem-get *campaign_map* (campaign-location)) +frm_campain_player)
		;save last known player location
		(setq *location_last_x* x *location_last_y* y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; player and enemy info generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun game-pick-part ()
	; (game-pick-part) -> 3 or 2 letter name syllable
	(if (= (game-random 2) 0)
		(slice +part_names3 (defq i (* (game-random 17) 3)) (+ i 3))
		(slice +part_names2 (defq i (* (game-random 7) 2)) (+ i 2))))

(defun game-pick-name (parts)
	; (game-pick-name parts) -> name
	(defq name "")
	(times parts (setq name (cat name (game-pick-part))))
	name)

(defun game-generate-enemy-info ()
	; enemy info is a pure function of the current location,
	; so save the current seed and use the location as a temp seed
	(defq seed *game_seed* location (campaign-location)
		tile (elem-get *campaign_map* location))
	(game-seed location)
	(setq *enemy_kingdom* (game-pick-name (+ (game-random 2) 2))
		*enemy_cult* (game-random (length +cult_names))
		*enemy_banner* (+ +frm_16x16_banners *enemy_cult*)
		*enemy_lord* (game-random (length +lord_names))
		*mine_max* (inc (game-random 3)))
	;pick random enemy maximum and population
	(defq pop (game-random (if (= *menu_difficulty* 0) 15000 30000))
		strength (/ pop 10000))
	(setq *enemy_population* (+ pop 10000) *enemy_max* (+ strength 2)
		*enemy_info_col* (case strength
			(0 +argb_green)
			(1 +argb_yellow)
			(:t +argb_red))
		*enemy_wizshot* (game-random 2)
		;plague locations always field the plague army
		*enemy_army* (if (= tile +frm_campain_plague)
			(const (dec (length +army_tables)))
			(game-random (const (dec (length +army_tables)))))
		*game_level* (if (= tile +frm_campain_rebellion) +game_level_defend +game_level_field)
		*enemy_popularity* (elem-get +popularity (abs (- *man_banner* *enemy_banner*)))
		*game_seed* seed))

(defun game-generate-player-info ()
	; start a new campaign
	(setq *man_score* 0 *item_selected* 0
		*item_inventory* (list +frm_16x16_shield 0 0 0 0 0 0 0)
		*item_usage* (list 48 0 0 0 0 0 0 0)
		*campaign_map* (map (const code) (load (cat *app_root* "data/campainmap.dat")))
		*event_count* 0 *plague_create* 0 *crusade_create* 0 *rebellion_create* 0
		*plague_destroy* 0 *crusade_destroy* 0 *rebellion_destroy* 0)
	;claim a random enemy location as the homeland
	(defq location (game-random +campaign_size))
	(while (/= (elem-get *campaign_map* location) +frm_campain_enemy)
		(setq location (logand (inc location) 0xff)))
	(elem-set *campaign_map* location +frm_campain_player)
	(setq *location_x* (logand location 0xf) *location_y* (>> location 4)
		*location_last_x* *location_x* *location_last_y* *location_y*
		*location_ox* -1 *man_territory* 1
		*man_name* (game-pick-name 2)
		*man_homeland* (game-pick-name (+ (game-random 2) 2))
		*man_cult* (game-random (length +cult_names))
		*man_banner* (+ +frm_16x16_banners *man_cult*))
	(game-generate-enemy-info))

;;;;;;;;;;;;;;;;;;;;;
; campaign map events
;;;;;;;;;;;;;;;;;;;;;

(defun campaign-event-create (cnt last_location tile tst)
	; (campaign-event-create cnt last_location tile tst) -> cnt
	(when (< (-- cnt) 0)
		(setq cnt (- (game-random 10) *man_experience*))
		(defq location (game-random +campaign_size))
		(and (/= location last_location)
			(tst (elem-get *campaign_map* location))
			(elem-set *campaign_map* location tile)))
	cnt)

(defun campaign-event-destroy (cnt tile)
	; (campaign-event-destroy cnt tile) -> cnt
	(when (<= (-- cnt) 0)
		(campaign-replace tile +frm_campain_enemy)
		(setq cnt (+ (game-random 10) 10 *man_experience*)))
	cnt)

(defun campaign-event-grow (last_location)
	; plagues and crusades spread into weaker neighbours,
	; rebellions spread into player neighbours
	(defq growbuf (cat *campaign_map*))
	(each (lambda (tile)
		(when (<= +frm_campain_plague tile +frm_campain_rebellion)
			(defq x (logand (!) 0xf) y (>> (!) 4))
			(each (lambda ((dx dy))
				(defq nx (+ x dx) ny (+ y dy) location (+ nx (* ny +campaign_width)))
				(and (<= 0 nx 15) (<= 0 ny 15) (/= location last_location)
					(if (= tile +frm_campain_rebellion)
						(= (elem-get growbuf location) +frm_campain_player)
						(> (elem-get growbuf location) tile))
					(elem-set *campaign_map* location tile)))
				'((-1 0) (1 0) (0 -1) (0 1)))))
		growbuf))

(defun campaign-events ()
	; create, destroy and grow the plagues, crusades and rebellions
	(when (= (++ *event_count*) +campaign_event_rate)
		(defq last_location (+ *location_last_x* (* *location_last_y* +campaign_width)))
		(setq *event_count* 0
			*plague_create* (campaign-event-create *plague_create* last_location
				+frm_campain_plague (# (> %0 +frm_campain_plague)))
			*crusade_create* (campaign-event-create *crusade_create* last_location
				+frm_campain_crusade (# (= %0 +frm_campain_enemy)))
			*rebellion_create* (campaign-event-create *rebellion_create* last_location
				+frm_campain_rebellion (# (= %0 +frm_campain_player)))
			*plague_destroy* (campaign-event-destroy *plague_destroy* +frm_campain_plague)
			*crusade_destroy* (campaign-event-destroy *crusade_destroy* +frm_campain_crusade)
			*rebellion_destroy* (campaign-event-destroy *rebellion_destroy* +frm_campain_rebellion))
		(campaign-event-grow last_location)
		(setq *man_territory* (campaign-count +frm_campain_player))))

;;;;;;;;;;;;;;;;;;;;;;;
; campaign map controls
;;;;;;;;;;;;;;;;;;;;;;;

(defun campaign-attack (tile)
	; enter the current location
	(cond
		((= tile +frm_campain_player))
		((= tile +frm_campain_oracle)
			(goto-title +game_state_oracle))
		((>= tile +frm_campain_plague)
			(goto-title +game_state_battle))
		((<= +frm_campain_twater tile +frm_campain_tmountain)
			(goto-title +game_state_mind))))

(defun campaign-use-item ()
	; use the selected map talisman
	(when (<= +frm_16x16_diamond5 (defq item (elem-get *item_inventory* *item_selected*))
			+frm_16x16_diamond9)
		(defq location (game-random +campaign_size)
			tile (elem-get *campaign_map* location))
		(case item
			(+frm_16x16_diamond5
				;gamble
				(when (/= location (+ *location_last_x* (* *location_last_y* +campaign_width)))
					(cond
						((= tile +frm_campain_player)
							(elem-set *campaign_map* location (+ (game-random 4) +frm_campain_plague)))
						((>= tile +frm_campain_plague)
							(elem-set *campaign_map* location +frm_campain_player)))))
			(+frm_16x16_diamond6
				;autowin
				(if (>= tile +frm_campain_plague)
					(elem-set *campaign_map* location +frm_campain_player)))
			(+frm_16x16_diamond7
				(campaign-replace +frm_campain_plague +frm_campain_enemy))
			(+frm_16x16_diamond8
				(campaign-replace +frm_campain_crusade +frm_campain_enemy))
			(+frm_16x16_diamond9
				(campaign-replace +frm_campain_rebellion +frm_campain_player)))
		(elem-set *item_inventory* *item_selected* 0)
		(setq *man_territory* (campaign-count +frm_campain_player))))

(defun campaign-controls ()
	; move the location, attack, or use an item
	(if (= (defq controls *game_controls*) 0)
		(setq *game_flags* (logand *game_flags* (lognot +fflag_select)))
		(defq x *location_x* y *location_y* ox x oy y)
		(unless (bits? *game_flags* +fflag_select)
			(setq *game_flags* (logior *game_flags* +fflag_select))
			(cond
				((bits? controls +fkey_keya)
					(campaign-attack (elem-get *campaign_map* (campaign-location))))
				((bits? controls +fkey_keyb)
					(campaign-use-item))
				((bits? controls +fkey_up) (if (/= y 0) (-- y)))
				((bits? controls +fkey_down) (if (/= y 15) (++ y)))
				((bits? controls +fkey_left) (if (/= x 0) (-- x)))
				((bits? controls +fkey_right) (if (/= x 15) (++ x)))))
		;valid movements only
		(cond
			((/= *location_ox* -1)
				;has one legal move, back the way we came
				(when (and (= x *location_ox*) (= y *location_oy*))
					(setq *location_ox* -1)
					(campaign-set-location x y)))
			(:t ;free to move on from player, oracle and temple locations,
				;and from terrain if carrying the matching talisman
				(defq tile (elem-get *campaign_map* (+ x (* y +campaign_width))))
				(if (or (= tile +frm_campain_player) (< tile +frm_campain_water)
						(and (< tile +frm_campain_plague)
							(find (+ tile (const (- +frm_16x16_diamond +frm_campain_water)))
								*item_inventory*)))
					(setq ox -1))
				(setq *location_ox* ox *location_oy* oy)
				(campaign-set-location x y)))))

;;;;;;;;;;;;;;;;;;;;
; campaign map view
;;;;;;;;;;;;;;;;;;;;

(defclass CampaignView () (View)
	; (CampaignView) -> full-screen campaign map and info overlay view
	(def this :color 0 :map :nil :canvas :nil :texts '())

	(defmethod :draw ()
		(defq zoom (get :zoom this) tid (get :tid this))
		(when (and (defq tiles (get :map this)) (defq c (get :canvas this))
				(defq texture (getf c +canvas_texture 0)))
			;8x8 location tiles
			(bind '(mtid tw th) (texture-metrics texture))
			(defq ts (* zoom 8) mx (* zoom +campaign_x) my (* zoom +campaign_y))
			(each (lambda (tile)
				(defq raw_y (* tile ts))
				(. this :ctx_blit mtid +argb_white
					(+ mx (* (logand (!) 0xf) ts)) (+ my (* (>> (!) 4) ts)) ts ts
					(* (/ raw_y th) ts) (% raw_y th)))
				tiles)
			;map borders
			(each (# (defq y (+ +campaign_y (* %0 8)))
					(draw-ascii-string this "_" (const (- +campaign_x 8)) y +argb_magenta zoom tid)
					(draw-ascii-string this "_" (const (+ +campaign_x 128)) y +argb_magenta zoom tid))
				(range 0 16)))
		(each (lambda ((txt x y col))
			(draw-ascii-string this txt x y col zoom tid)) (get :texts this))
		this)
	)

(defun campaign-resize (zoom)
	(when (and *campaign_view* *img_ascii* (defq c (getf *img_ascii* +canvas_texture 0)))
		(bind '(tid tw th) (texture-metrics c))
		(def *campaign_view* :zoom zoom :tid tid :canvas *img_campain*)
		(. *campaign_view* :change 0 0 (* zoom +screen_width) (* zoom +screen_height) :t)))

(defun campaign-view-init ()
	; common full screen text overlay setup
	(clear-sprite-layers)
	(set-world-layers-pos 0 0)
	(set-world-layers-size (* *zoom* +screen_width) (* *zoom* +screen_height))
	(. *layer_land* :clear)
	(. *layer_sky* :sky_black)
	(unless *campaign_view* (setq *campaign_view* (CampaignView)))
	(. *layer_fx* :add_front *campaign_view*)
	(campaign-resize *zoom*))

(defun campaign-column (texts x col dcol rows)
	; (campaign-column texts x col dcol rows) -> texts
	(each (lambda ((heading value))
		(defq y (+ +campaign_y 32 (* (!) 16)))
		(push texts (list heading x y col) (list value x (+ y 8) dcol))) rows)
	texts)

(defun campaign-sync ()
	; update the info panels and sprites for the current location
	(game-generate-enemy-info)
	(defq tile (elem-get *campaign_map* (campaign-location))
		texts (campaign-column (list) 8 +argb_yellow +argb_dyellow (list
			(list "NAME" *man_name*) (list "HOMELAND" *man_homeland*)
			(list "CULT" (elem-get +cult_names *man_cult*))
			(list "STATUS" (elem-get +status_names *man_experience*))
			(list "TERRITORY" (str *man_territory*)) (list "GLORY" (str *man_score*)))))
	(. *campaign_sight* :sp_set_pos (+ (* *location_x* 8) (const (- +campaign_x 4)))
		(+ (* *location_y* 8) (const (- +campaign_y 4))))
	(if (and (/= tile +frm_campain_player) (>= tile +frm_campain_plague))
		(progn
			;enemy stats
			(campaign-column texts 232 *enemy_info_col*
				(logior +argb_black (logand (>> *enemy_info_col* 1) 0x7f7f7f)) (list
				(list "KINGDOM" *enemy_kingdom*)
				(list "WIZARDLORD" (elem-get +lord_names *enemy_lord*))
				(list "CULT" (elem-get +cult_names *enemy_cult*))
				(list "WARBAND" (elem-get +army_names *enemy_army*))
				(list "POPULARITY" (str *enemy_popularity*))
				(list "POPULATION" (str *enemy_population*))))
			(.-> *campaign_ban* (:sp_set_frame *enemy_banner*)
				(:sp_set_pos 240 (const (+ +campaign_y 8))))
			(.-> *campaign_lord* (:sp_set_frame (+ +frm_32c32_face (* *enemy_lord* 2)))
				(:sp_set_pos 272 +campaign_y)))
		;location stats
		(bind '(col &rest lines) (elem-get +location_names tile))
		(each (# (push texts (list %0 240 (+ +campaign_y 48 (* (!) 8)) col))) lines)
		(. *campaign_ban* :sp_set_pos -64 -64)
		(. *campaign_lord* :sp_set_pos -64 -64))
	(def *campaign_view* :map *campaign_map* :texts texts))

;;;;;;;;;;;;;;;;;;;;;
; map state lifecycle
;;;;;;;;;;;;;;;;;;;;;

(defun map-state-init ()
	(unless *campaign_active*
		(setq *campaign_active* :t)
		(game-generate-player-info))
	(campaign-view-init)
	;set man status and experience, wait for key release
	(setq *man_score* (min *man_score* 999999)
		*man_experience* (min 9 (/ *man_score* 65536))
		*game_flags* (logior *game_flags* +fflag_select)
		*campaign_sight* (Sprite *img_frm_16x16* :nil 16 16 :16x16)
		*campaign_ban* (Sprite *img_frm_16x16* :nil 16 16 :16x16)
		*campaign_lord* (Sprite *img_frm_32c32* :nil 32 32 :32c32))
	;player banner and stance
	(.-> (defq ban (Sprite *img_frm_16x16* :nil 16 16 :16x16))
		(:sp_set_frame *man_banner*) (:sp_set_pos 16 (const (+ +campaign_y 8))))
	(def (defq man (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic)) :sp_dir -1)
	(.-> man (:sp_set_frame +frm_fanatic_l_stance) (:sp_set_pos 48 +campaign_y))
	(. *campaign_sight* :sp_set_frame +frm_16x16_sight)
	(each (# (. *layer_fx* :add_front %0))
		(list ban man *campaign_ban* *campaign_lord* *campaign_sight*))
	(campaign-sync))

(defun map-state-update ()
	(campaign-events)
	(campaign-controls)
	(campaign-sync))

;;;;;;;;;;;;;;;;;;;;;;
; mind state lifecycle
;;;;;;;;;;;;;;;;;;;;;;

(defun mind-state-init ()
	(mind-combat-init (+ +frm_32c32_face (* *enemy_lord* 2))))

(defun mind-state-update ()
	(case (mind-combat-update (elem-get *campaign_map* (campaign-location)))
		(:won (setq *game_state* +game_state_mind_won))
		(:lost (setq *game_state* +game_state_mind_lost))))

(defun mind-won ()
	; (mind-won) -> next game state
	(defq location (campaign-location) state +game_state_map)
	(when (>= (elem-get *campaign_map* location) +frm_campain_plague)
		(cond
			((= *game_level* +game_level_seige)
				;won terrain location
				(elem-set *campaign_map* location +frm_campain_player)
				(setq *man_territory* (inc *man_territory*)
					*location_last_x* *location_x* *location_last_y* *location_y*
					*location_ox* -1 *man_score* (+ *man_score* *enemy_popularity*)))
			(:t ;won defending mind combat
				(setq *game_level* +game_level_defend state +game_state_battle))))
	state)

(defun mind-lost ()
	; (mind-lost) -> next game state
	(defq tile (elem-get *campaign_map* (campaign-location)) state +game_state_map)
	(cond
		((< tile +frm_campain_plague)
			;lost all talismans if temple mind combat
			(each (# (if (<= +frm_16x16_diamond %0 +frm_16x16_diamond9)
				(elem-set *item_inventory* (!) 0))) *item_inventory*))
		((= *game_level* +game_level_defend)
			;lost terrain location to the attacking type
			(elem-set *campaign_map*
				(+ *location_last_x* (* *location_last_y* +campaign_width)) tile)
			(if (= (setq *man_territory* (campaign-count +frm_campain_player)) 0)
				;game over
				(setq *campaign_active* :nil state +game_state_hiscore)
				;fall back to a random player location
				(defq location (game-random +campaign_size))
				(while (/= (elem-get *campaign_map* location) +frm_campain_player)
					(setq location (logand (inc location) 0xff)))
				(setq *location_x* (logand location 0xf) *location_y* (>> location 4)
					*location_last_x* *location_x* *location_last_y* *location_y*
					*location_ox* -1)))
		(:t ;lost seige mind combat
			(setq *game_level* +game_level_seige state +game_state_battle)))
	state)

(defun mind-won-state-init ()
	(setq *battle_state_count* 0))

(defun mind-won-state-update ()
	; delay for a while, then on via title with power restored
	(when (= (++ *battle_state_count*) 64)
		(goto-title (mind-won))
		(setq *man_power* *mind_power*)))

(defun mind-lost-state-init ()
	(setq *battle_state_count* 0))

(defun mind-lost-state-update ()
	(when (= (++ *battle_state_count*) 64)
		(goto-title (mind-lost))
		(setq *man_power* *mind_power*)))

;;;;;;;;;;;;;;;;;;;;;;;;
; oracle state lifecycle
;;;;;;;;;;;;;;;;;;;;;;;;

(defun oracle-state-init ()
	(campaign-view-init)
	(mind-map-init)
	(bind '(x1 line1 x2 line2)
		(if (and (/= *menu_difficulty* 0) (/= *menu_mode* +mode_tutor))
			(elem-get +oracle_hints *man_experience*)
			+oracle_no_hint))
	(def *campaign_view* :map :nil :texts (list
		(list line1 x1 (const (+ +campaign_y 56)) +argb_white)
		(list line2 x2 (const (+ +campaign_y 64)) +argb_white)))
	(setq *game_flags* (logior *game_flags* +fflag_select)))

(defun oracle-state-update ()
	;scroll the two planes against each other
	(mind-map-scroll 0 1 0 -1)
	(game-mr-smiths)
	(if (= *game_controls* 0)
		(setq *game_flags* (logand *game_flags* (lognot +fflag_select)))
		(unless (bits? *game_flags* +fflag_select)
			(setq *game_flags* (logior *game_flags* +fflag_select))
			(goto-title +game_state_map))))

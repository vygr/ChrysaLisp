;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; weapon and item addon sprites, movement tables & animations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	+mt_upper_mase_l
	''((32 0) (32 0) (8 0) (8 0) (-16 0) (-16 0))
	+mt_upper_mase_r
	''((-16 0) (-16 0) (8 0) (8 0) (32 0) (32 0))
	+mt_lower_mase_l
	''((32 -8) (32 -8) (8 0) (8 0) (-16 0) (-16 0))
	+mt_lower_mase_r
	''((-16 -8) (-16 -8) (8 0) (8 0) (32 0) (32 0))
	+mt_big_crossbow_l
	''((-16 16) (-16 16) (-16 16) (-16 16) (-16 0) (-16 0) (-16 0) (-16 0))
	+mt_big_crossbow_r
	''((32 16) (32 16) (32 16) (32 16) (32 0) (32 0) (32 0) (32 0))
	+mt_small_crossbow_l
	''((0 -16) (-16 0) (-16 0))
	+mt_small_crossbow_r
	''((16 -16) (32 0) (32 0))
	+mt_naptha_l
	''((32 0) (32 0) (32 0) (0 0) (0 0) (0 0) (-16 0) (-16 0) (-16 0))
	+mt_naptha_r
	''((-16 0) (-16 0) (-16 0) (0 0) (0 0) (0 0) (32 0) (32 0) (32 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; addon animation tables
; (terminate with -1 to trigger :sp_kill)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	+at_upper_mase
	''(1 2 3 -1)
	+at_lower_mase
	''(4 5 6 -1)
	+at_big_crossbow
	''(10 11 12 13 -1)
	+at_small_crossbow
	''(7 8 9 -1)
	+at_naptha
	''(14 14 15 -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; player man body animations during weapon use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	+at_man_upper_mase
	''(14 15 16 4)
	+at_man_lower_mase
	''(30 31 32 29)
	+at_man_big_crossbow
	''(20 21 22 23 0)
	+at_man_small_crossbow
	''(17 18 19 4)
	+at_man_naptha
	''(24 25 26 4))

(defq +black_spell_table
	; (item uses) pairs matching cpp: 40% yellow spell (43), 40% green spell (45), 20% super helper (42)
	''((43 1) (43 1) (45 1) (45 1) (42 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reusable 16x16 directional addon sprite class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Addon (man mt_table at_speed at_table) (Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr)
	; (Addon man mt_table at_speed at_table) -> addon sprite
	(def this :sp_dir (or (get :sp_dir man) 1))
	(def man :sp_addon this)
	; set initial position immediately to eliminate 1-frame spawn lag
	(bind '(mx my) (. man :sp_get_pos))
	(bind '(dx dy) (first mt_table))
	(. this :sp_set_pos (+ mx dx) (+ my dy))
	(defq at_comp (AT at_speed at_table :nil)
		mt_comp (MT 1 mt_table man))
	; start at index 1 since step 0 was applied at spawn
	(set at_comp :index 1 :count 0)
	(set mt_comp :index 1 :count 0)
	(.-> this
		(:sp_add_component mt_comp)
		(:sp_add_component at_comp)
		(:sp_set_frame (first at_table))))

(defclass EnemyAddon (enemy mt_table at_speed at_table &optional hitpnt) (Addon enemy mt_table at_speed at_table)
	; (EnemyAddon enemy mt_table at_speed at_table [hitpnt]) -> enemy weapon addon with player collision
	(def this :sp_hitpnt (or hitpnt 8))
	(. this :sp_add_component (CL *layer_player* +ftp_man *man_collision_table*)))

(defclass Mase (man mt_table at_speed at_table &optional hitpnt)
	(Addon man mt_table at_speed at_table)
	; (Mase man mt_table at_speed at_table [hitpnt]) -> player mase addon with collisions
	(def this :sp_hitpnt (or hitpnt 1))
	(.-> this
		(:sp_add_component (CL *layer_enemies* -1 *man_addon_collision_table*))
		(:sp_add_component (CL *layer_missiles* -1 *man_addon_collision_table*))))

(defun use-mase (man &optional hitpnt)
	; trigger mase attack animation and spawn tracking mase sprite
	(defq dir (or (get :sp_dir man) 1)
		duck (>= (get :sp_frame man) +frm_fanatic_l_duck)
		man_at (elem-get (get :sp_components man) +man_comp_at)
		body_anim (if duck +at_man_lower_mase +at_man_upper_mase)
		wep_anim (if duck +at_lower_mase +at_upper_mase)
		mt (cond
			(duck (if (= dir 1) +mt_lower_mase_r +mt_lower_mase_l))
			(:t (if (= dir 1) +mt_upper_mase_r +mt_upper_mase_l))))
	(audio-play-rpc *sfx_throw*)
	(def man :action_playing :t)
	(. man :sp_set_frame (first body_anim))
	(set man_at :speed 2 :table body_anim :count 0 :index 1 :loop :nil)
	(defq wep (Mase man mt 2 wep_anim hitpnt))
	(. man :add_before wep)
	wep)

(defun use-super-mase (man)
	; trigger magic / super mase with 1000 hit points
	(use-mase man 1000))

(defun dt-bomb-fx (this)
	; naptha bomb detonation: 4-explosion cluster, blast area, and explode sfx
	(bind '(x y) (. this :sp_get_pos))
	(defq cx (+ x 8) cy (+ y 8))
	(each (lambda ((ox oy))
		(defq exp (. (Explosion) :sp_set_pos (+ cx ox) (+ cy oy)))
		(. *layer_fx* :add_front exp))
		'((-32 -16) (-16 -32) (0 -16) (-16 0)))
	(audio-play-rpc *sfx_explode*)
	(blast-area (- x 24) (- y 24) 64 64))

;;;;;;;;;;;;;;;;;;;;
; player projectiles
;;;;;;;;;;;;;;;;;;;;

(defclass SmallArrow (x y dir) (Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr)
	; (SmallArrow x y dir) -> player light arrow projectile
	(def this :sp_dir dir :sp_hitpnt 1)
	(.-> this
		(:sp_add_component (MV (* 16 dir) 0 0 0 16 0))
		(:sp_add_component (CP offscreen-nodeath-update))
		(:sp_add_component (CL *layer_enemies* -1 *man_missile_collision_table*))
		(:sp_add_component (CL *layer_missiles* +ftp_enemy_fire *man_missile_collision_table*))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_16x16_l_arrow)))

(defclass BigArrow (x y dir) (Sprite *img_frm_32x16_l* *img_frm_32x16_r* 32 16 :32x16_lr)
	; (BigArrow x y dir) -> player heavy ballista bolt projectile
	(def this :sp_dir dir :sp_hitpnt 4)
	(.-> this
		(:sp_add_component (MV (* 10 dir) 0 0 0 10 0))
		(:sp_add_component (CP offscreen-nodeath-update))
		(:sp_add_component (CL *layer_enemies* -1 *man_missile_collision_table*))
		(:sp_add_component (CL *layer_missiles* +ftp_enemy_fire *man_missile_collision_table*))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_32x16_l_barrow)))

(defun cp-nbomb (this that)
	; bomb explosion upon reaching downward arc
	(when (defq mv (elem-get (get :sp_components this) 0))
		(when (>= (get :vy mv) 5)
			(bind '(x y) (. this :sp_get_pos))
			(. this :sp_kill)
			(spawn-bomb-fx x y)
			(audio-play-rpc *sfx_explode*)
			(blast-area (- x 24) (- y 24) 64 64))))

(defclass NBomb (x y dir) (Sprite *img_frm_16x16* :nil 16 16 :16x16 dt-bomb-fx)
	; (NBomb x y dir) -> thrown naptha grenade
	(.-> this
		(:sp_add_component (MV (* 10 dir) -4 0 1 10 8))
		(:sp_add_component (AT 1 '(24 25 26 27)))
		(:sp_add_component (CP cp-nbomb))
		(:sp_add_component (CP offscreen-nodeath-update))
		(:sp_set_pos x y)
		(:sp_set_frame 24)))

(defclass Frag (x y vx vy) (Sprite *img_frm_16x16* :nil 16 16 :16x16)
	; (Frag x y vx vy) -> elemental fire fragment dealing 1000 damage
	(def this :sp_hitpnt 1000)
	(.-> this
		(:sp_add_component (MV vx vy 0 0 8 8))
		(:sp_add_component (AT 1 (list +frm_16x16_fires (+ +frm_16x16_fires 1))))
		(:sp_add_component (CP offscreen-nodeath-update))
		(:sp_add_component (CL *layer_enemies* -1 *man_missile_collision_table*))
		(:sp_add_component (CL *layer_missiles* +ftp_enemy_fire *man_missile_collision_table*))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_16x16_fires)))

(defun dt-elemental-explode (this)
	; burst into 8 flaming fragments across all 8 directions
	(bind '(ex ey) (. this :sp_get_pos))
	(defq cx (+ ex 8) cy (+ ey 8))
	(each (lambda ((vx vy))
		(defq frag (. (Frag cx cy vx vy) :sp_set_pos cx cy))
		(. *layer_player* :add_front frag))
		'((0 -8) (6 -6) (8 0) (6 6) (0 8) (-6 6) (-8 0) (-6 -6)))
	(dt-exp-fx this))

(defclass Elemental (x y) (Sprite *img_frm_32c32* :nil 32 32 :32c32 dt-elemental-explode)
	; (Elemental x y) -> roaring demon head
	(.-> this
		(:sp_add_component (AT 3 (list +frm_32c32_elemental
			(+ +frm_32c32_elemental 1) (+ +frm_32c32_elemental 2)
			(+ +frm_32c32_elemental 3) (+ +frm_32c32_elemental 3) -1) :nil))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_32c32_elemental)))

(defun dt-super-helper-transform (this)
	; transform expired major demon into roaring elemental head
	(bind '(hx hy) (. this :sp_get_pos))
	(defq elem (Elemental (- hx 8) (- hy 8)))
	(. *layer_player* :add_front elem)
	(audio-play-rpc *sfx_roar*))

(defq
	+at_helper ''(28 29)
	+at_super_helper ''(30 31))

(defun cp-helper (this that)
	; homing helper demon tracking nearest enemy
	(defq life (dec (or (get :life this) 128)))
	(if (<= life 0)
		(. this :sp_kill)
		(def this :life life)
		(bind '(hx hy) (. this :sp_get_pos))
		(when (defq mv (elem-get (get :sp_components this) 0))
			(if (defq enemy (first (. *layer_enemies* :children)))
				(progn
					(bind '(ex ey ew eh) (. enemy :sp_get_bounds))
					(set mv :ax (if (>= (+ ex (/ ew 2)) hx) 1 -1)
							:ay (if (>= (+ ey (/ eh 2)) hy) 1 -1)))
				(set mv :ax 0 :ay 0 :vx 0 :vy 0)))))

(defclass Helper (x y) (Sprite *img_frm_16x16* :nil 16 16 :16x16 dt-fizz-fx)
	; (Helper x y) -> blue demon homing helper (frames 28, 29)
	(def this :sp_hitpnt 10 :life 128)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 8 8))
		(:sp_add_component (AT 6 +at_helper))
		(:sp_add_component (CP cp-helper))
		(:sp_add_component (CL *layer_enemies* -1 *man_missile_collision_table*))
		(:sp_add_component (CL *layer_missiles* +ftp_enemy_fire *man_missile_collision_table*))
		(:sp_set_pos x y)
		(:sp_set_frame 28)))

(defclass SuperHelper (x y) (Sprite *img_frm_16x16* :nil 16 16 :16x16 dt-super-helper-transform)
	; (SuperHelper x y) -> red demon homing helper (frames 30, 31)
	(def this :sp_hitpnt 20 :life 32)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 8 8))
		(:sp_add_component (AT 6 +at_super_helper))
		(:sp_add_component (CP cp-helper))
		(:sp_add_component (CL *layer_enemies* -1 *man_missile_collision_table*))
		(:sp_add_component (CL *layer_missiles* +ftp_enemy_fire *man_missile_collision_table*))
		(:sp_set_pos x y)
		(:sp_set_frame 30)))

;;;;;;;;;;;;;;;;;
; weapon triggers
;;;;;;;;;;;;;;;;;

(defun cp-small-crossbow (this that)
	(unless (get :arrow_fired this)
		(def this :arrow_fired :t)
		(bind '(ax ay) (. this :sp_get_pos))
		(defq dir (or (get :sp_dir this) 1)
			x (if (= dir 1) (+ ax 16) (- ax 16))
			arr (SmallArrow x ay dir))
		(. *layer_player* :add_front arr)
		(audio-play-rpc *sfx_twang2*)))

(defclass SmallCrossbow (man mt_table) (Addon man mt_table 1 +at_small_crossbow)
	; (SmallCrossbow man mt_table) -> player light crossbow addon
	(. this :sp_add_component (CP cp-small-crossbow)))

(defun use-small-crossbow (man)
	; trigger small crossbow shooting animation and spawn arrow
	(defq dir (or (get :sp_dir man) 1)
		man_at (elem-get (get :sp_components man) +man_comp_at)
		mt (if (= dir 1) +mt_small_crossbow_r +mt_small_crossbow_l))
	(def man :action_playing :t)
	(. man :sp_set_frame (first +at_man_small_crossbow))
	(set man_at :speed 1 :table +at_man_small_crossbow :count 0 :index 1 :loop :nil)
	(defq wep (SmallCrossbow man mt))
	(. man :add_before wep)
	wep)

(defun cp-big-crossbow (this that)
	(when (defq mt (elem-get (get :sp_components this) 0))
		(when (and (>= (get :index mt) 7) (not (get :arrow_fired this)))
			(def this :arrow_fired :t)
			(bind '(ax ay) (. this :sp_get_pos))
			(defq dir (or (get :sp_dir this) 1)
				x (if (= dir 1) (+ ax 8) (- ax 24))
				arr (BigArrow x ay dir))
			(. *layer_player* :add_front arr)
			(audio-play-rpc *sfx_twang1*))))

(defclass BigCrossbow (man mt_table) (Addon man mt_table 2 +at_big_crossbow)
	; (BigCrossbow man mt_table) -> player heavy crossbow addon
	(. this :sp_add_component (CP cp-big-crossbow)))

(defun use-big-crossbow (man)
	; trigger big crossbow shooting animation and spawn bolt
	(defq dir (or (get :sp_dir man) 1)
		man_at (elem-get (get :sp_components man) +man_comp_at)
		mt (if (= dir 1) +mt_big_crossbow_r +mt_big_crossbow_l))
	(def man :action_playing :t)
	(. man :sp_set_frame (first +at_man_big_crossbow))
	(set man_at :speed 2 :table +at_man_big_crossbow :count 0 :index 1 :loop :nil)
	(defq wep (BigCrossbow man mt))
	(. man :add_before wep)
	wep)

(defun cp-naptha (this that)
	(when (defq mt (elem-get (get :sp_components this) 0))
		(when (and (>= (get :index mt) 7) (not (get :bomb_thrown this)))
			(def this :bomb_thrown :t)
			(bind '(ax ay) (. this :sp_get_pos))
			(defq dir (or (get :sp_dir this) 1)
				x (if (= dir 1) (+ ax 4) (- ax 4))
				bomb (NBomb x ay dir))
			(. *layer_player* :add_front bomb))))

(defclass Naptha (man mt_table) (Addon man mt_table 3 +at_naptha)
	; (Naptha man mt_table) -> player naptha throwing addon
	(. this :sp_add_component (CP cp-naptha)))

(defun use-naptha (man)
	; trigger naptha throwing animation and lob bomb
	(defq dir (or (get :sp_dir man) 1)
		man_at (elem-get (get :sp_components man) +man_comp_at)
		mt (if (= dir 1) +mt_naptha_r +mt_naptha_l))
	(audio-play-rpc *sfx_throw*)
	(def man :action_playing :t)
	(. man :sp_set_frame (first +at_man_naptha))
	(set man_at :speed 3 :table +at_man_naptha :count 0 :index 1 :loop :nil)
	(defq wep (Naptha man mt))
	(. man :add_before wep)
	wep)

(defun use-helper (man)
	; spawn blue demon homing helper
	(bind '(mx my mw mh) (. man :sp_get_bounds))
	(defq wep (Helper (+ mx 8) (- (+ my (/ mh 2)) 8)))
	(. *layer_player* :add_front wep)
	(audio-play-rpc *sfx_spell*))

(defun use-super-helper (man)
	; spawn red demon homing helper
	(bind '(mx my mw mh) (. man :sp_get_bounds))
	(defq wep (SuperHelper (+ mx 8) (- (+ my (/ mh 2)) 8)))
	(. *layer_player* :add_front wep)
	(audio-play-rpc *sfx_spell*))

(defun use-yellow-spell (man)
	; yellow spell spawns roaring elemental head
	(bind '(mx my) (. man :sp_get_pos))
	(defq elem (Elemental mx (- my 32)))
	(. *layer_player* :add_front elem)
	(audio-play-rpc *sfx_roar*))

(defun use-black-spell (man)
	; black spell fills all 8 inventory slots with a random powerful spell/item
	(bind '(item uses) (elem-get +black_spell_table (random 5)))
	(defq idx 0)
	(while (< idx 8)
		(elem-set *item_inventory* idx item)
		(elem-set *item_usage* idx uses)
		(++ idx))
	(audio-play-rpc *sfx_spell*))

(defun use-green-spell (man)
	; green spell kills all active enemies on screen
	(audio-play-rpc *sfx_spell*)
	(each (lambda (enemy)
		(when (Sprite? enemy)
			(def enemy :sp_death dt-kill-zap)
			(. enemy :sp_kill)))
		(. *layer_enemies* :children)))

(defun use-white-spell (man)
	; white spell freezes non-stationary enemies in place
	(audio-play-rpc *sfx_spell*)
	(each (lambda (enemy)
		(when (Sprite? enemy)
			(defq type (or (get :sp_type enemy) 0))
			(unless (or (= type 0)
						(/= (logand type (logior +ftp_cannon +ftp_balista +ftp_oil +ftp_beserk)) 0))
				(each (lambda (comp)
					(when (and (env? comp) (def? :max_vx comp))
						(set comp :vx 0 :max_vx 0)))
					(get :sp_components enemy)))))
		(cat (. *layer_enemies* :children))))

(defun use-red-spell (man)
	; red spell restores 20% strength
	(setq *man_strength* (min +max_strength (+ (or *man_strength* 0) (/ +max_strength 5))))
	(audio-play-rpc *sfx_spell*))

(defun use-blue-spell (man)
	; blue spell restores 20% power
	(setq *man_power* (min +max_power (+ (or *man_power* 0) (/ +max_power 5))))
	(audio-play-rpc *sfx_spell*))

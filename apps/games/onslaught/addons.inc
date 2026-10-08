;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; weapon and item addon sprites, movement tables & animations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +mt_upper_mase_l ''((32 0) (32 0) (8 0) (8 0) (-16 0) (-16 0))
	+mt_upper_mase_r ''((-16 0) (-16 0) (8 0) (8 0) (32 0) (32 0))
	+mt_lower_mase_l ''((32 -8) (32 -8) (8 0) (8 0) (-16 0) (-16 0))
	+mt_lower_mase_r ''((-16 -8) (-16 -8) (8 0) (8 0) (32 0) (32 0))
	+mt_big_crossbow_l ''((-16 16) (-16 16) (-16 16) (-16 16) (-16 0) (-16 0) (-16 0) (-16 0))
	+mt_big_crossbow_r ''((32 16) (32 16) (32 16) (32 16) (32 0) (32 0) (32 0) (32 0))
	+mt_small_crossbow_l ''((0 -16) (-16 0) (-16 0))
	+mt_small_crossbow_r ''((16 -16) (32 0) (32 0))
	+mt_naptha_l ''((32 0) (32 0) (32 0) (0 0) (0 0) (0 0) (-16 0) (-16 0) (-16 0))
	+mt_naptha_r ''((-16 0) (-16 0) (-16 0) (0 0) (0 0) (0 0) (32 0) (32 0) (32 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; addon animation tables
; (terminate with -1 to trigger :sp_kill)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +at_upper_mase `'(,+frm_16x16_l_mase ,+frm_16x16_l_mase1 ,+frm_16x16_l_mase2 -1)
	+at_lower_mase `'(,+frm_16x16_l_lmase ,+frm_16x16_l_lmase1 ,+frm_16x16_l_lmase2 -1)
	+at_big_crossbow `'(,+frm_16x16_l_cbow ,+frm_16x16_l_cbow1 ,+frm_16x16_l_cbow2
		,+frm_16x16_l_cbow3 -1)
	+at_small_crossbow `'(,+frm_16x16_l_hbow ,+frm_16x16_l_hbow1 ,+frm_16x16_l_hbow2 -1)
	+at_naptha `'(,+frm_16x16_l_napth ,+frm_16x16_l_napth ,+frm_16x16_l_napth1 -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; player man body animations during weapon use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +at_man_upper_mase `'(,+frm_fanatic_l_masehit ,+frm_fanatic_l_masehit1
		,+frm_fanatic_l_masehit2 ,+frm_fanatic_l_walk4)
	+at_man_lower_mase `'(,+frm_fanatic_l_lmasehit ,+frm_fanatic_l_lmasehit1
		,+frm_fanatic_l_lmasehit2 ,+frm_fanatic_l_duck)
	+at_man_big_crossbow `'(,+frm_fanatic_l_crossbow ,+frm_fanatic_l_crossbow1
		,+frm_fanatic_l_crossbow2 ,+frm_fanatic_l_crossbow3 ,+frm_fanatic_l_walk)
	+at_man_small_crossbow `'(,+frm_fanatic_l_handbow ,+frm_fanatic_l_handbow1
		,+frm_fanatic_l_handbow2 ,+frm_fanatic_l_walk4)
	+at_man_naptha `'(,+frm_fanatic_l_naptha ,+frm_fanatic_l_naptha1
		,+frm_fanatic_l_naptha2 ,+frm_fanatic_l_walk4)
	; (item uses) pairs matching cpp: 40% yellow spell, 40% green spell, 20% super helper
	+black_spell_table `'((,+frm_16x16_spells 1) (,+frm_16x16_spells 1)
		(,+frm_16x16_spells2 1) (,+frm_16x16_spells2 1) (,+frm_16x16_shield6 3))
	+at_nbomb `'(,+frm_16x16_nbomb ,+frm_16x16_nbomb1 ,+frm_16x16_nbomb2 ,+frm_16x16_nbomb3)
	+at_helper `'(,+frm_16x16_demon ,+frm_16x16_demon1)
	+at_super_helper `'(,+frm_16x16_demon2 ,+frm_16x16_demon3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reusable 16x16 directional addon sprite class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dt-addon (this)
	; clear addon link on parent sprite when addon finishes or dies
	(when (defq parent (get :sp_parent_sprite this))
		(when (eql (get :sp_addon parent) this)
			(def parent :sp_addon :nil))))

(defclass Addon (man mt_table at_speed at_table) (Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr dt-addon)
	; (Addon man mt_table at_speed at_table) -> addon sprite
	(def this :sp_dir (get :sp_dir man) :sp_parent_sprite man)
	(def man :sp_addon this)
	; set initial position immediately to eliminate 1-frame spawn lag
	(bind '(mx my) (. man :sp_get_pos))
	(bind '(dx dy) (first mt_table))
	(. this :sp_set_pos (+ mx dx) (+ my dy))
	(defq at_comp (AT at_speed at_table :nil)
		mt_comp (MT 1 mt_table man))
	; start at index 1 since step 0 was applied at spawn
	(set at_comp 'index 1 'count 0)
	(set mt_comp 'index 1 'count 0)
	(.-> this
		(:sp_add_component mt_comp)
		(:sp_add_component at_comp)
		(:sp_set_frame (first at_table))))

(defclass EnemyAddon (enemy mt_table at_speed at_table &optional hitpnt) (Addon enemy mt_table at_speed at_table)
	; (EnemyAddon enemy mt_table at_speed at_table [hitpnt]) -> enemy weapon addon with player collision
	(def this :sp_hitpnt (or hitpnt 8) :has_hit :nil)
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
	(defq dir (get :sp_dir man)
		duck (>= (get :sp_frame man) +frm_fanatic_l_duck)
		man_at (elem-get (get :sp_components man) +man_comp_at)
		body_anim (if duck +at_man_lower_mase +at_man_upper_mase)
		wep_anim (if duck +at_lower_mase +at_upper_mase)
		mt (if duck
			(if (= dir 1) +mt_lower_mase_r +mt_lower_mase_l)
			(if (= dir 1) +mt_upper_mase_r +mt_upper_mase_l)))
	(play-sfx *sfx_throw* man)
	(def man :sp_flags (logior (get :sp_flags man) +fsp_action))
	(. man :sp_set_frame (first body_anim))
	(set man_at 'speed 2 'table body_anim 'count 0 'index 1 'loop :nil)
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
	(play-sfx *sfx_explode* this)
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
	; bomb explodes on its downward arc, the dt-bomb-fx death hook does the damage
	(if (>= (get 'vy (elem-get (get :sp_components this) +comp_move)) 5)
		(. this :sp_kill)))

(defclass NBomb (x y dir) (Sprite *img_frm_16x16* :nil 16 16 :16x16 dt-bomb-fx)
	; (NBomb x y dir) -> thrown naptha grenade
	(.-> this
		(:sp_add_component (MV (* 10 dir) -4 0 1 10 8))
		(:sp_add_component (AT 1 +at_nbomb))
		(:sp_add_component (CP cp-nbomb))
		(:sp_add_component (CP offscreen-nodeath-update))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_16x16_nbomb)))

(defq
	+at_frag `'(,+frm_16x16_fires ,+frm_16x16_fires1)
	+at_elemental `'(,+frm_32c32_elemental ,+frm_32c32_elemental1 ,+frm_32c32_elemental2
					 ,+frm_32c32_elemental3 ,+frm_32c32_elemental3 -1))

(defclass Frag (x y vx vy) (Sprite *img_frm_16x16* :nil 16 16 :16x16)
	; (Frag x y vx vy) -> elemental fire fragment dealing 1000 damage
	(def this :sp_hitpnt 1000)
	(.-> this
		(:sp_add_component (MV vx vy 0 0 8 8))
		(:sp_add_component (AT 1 +at_frag))
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
		(. *layer_player* :add_front (Frag cx cy vx vy)))
		'((0 -8) (6 -6) (8 0) (6 6) (0 8) (-6 6) (-8 0) (-6 -6)))
	(dt-exp-fx this))

(defclass Elemental (x y) (Sprite *img_frm_32c32* :nil 32 32 :32c32 dt-elemental-explode)
	; (Elemental x y) -> roaring demon head
	(.-> this
		(:sp_add_component (AT 3 +at_elemental :nil))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_32c32_elemental)))

(defun dt-super-helper-transform (this)
	; transform expired major demon into roaring elemental head
	(bind '(hx hy) (. this :sp_get_pos))
	(defq elem (Elemental (- hx 8) (- hy 8)))
	(. *layer_player* :add_front elem)
	(play-sfx *sfx_roar* this))

(defun cp-helper (this that)
	; homing helper demon tracking the first live enemy
	(defq life (dec (get :life this)))
	(if (<= life 0)
		(. this :sp_kill)
		(def this :life life)
		(bind '(hx hy) (. this :sp_get_pos))
		(defq mv (elem-get (get :sp_components this) +comp_move))
		(ifn (defq enemy (some (lambda (sp)
					(and (Sprite? sp) (not (get :sp_dead sp))
						(/= (get :sp_frame sp) -1) (/= (get :sp_type sp) 0) sp))
				(. *layer_enemies* :children)))
			(set mv 'ax 0 'ay 0 'vx 0 'vy 0)
			(bind '(ex ey ew eh) (. enemy :sp_get_bounds))
			(set mv 'ax (if (>= (- (+ ex (/ ew 2)) 8) hx) 1 -1)
				'ay (if (>= (- (+ ey (/ eh 2)) 8) hy) 1 -1))
			(spawn-fizz-fx hx hy))))

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
		(:sp_set_frame +frm_16x16_demon)))

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
		(:sp_set_frame +frm_16x16_demon2)))

;;;;;;;;;;;;;;;;;
; weapon triggers
;;;;;;;;;;;;;;;;;

(defun cp-small-crossbow (this that)
	(unless (get :arrow_fired this)
		(def this :arrow_fired :t)
		(bind '(ax ay) (. this :sp_get_pos))
		(defq dir (get :sp_dir this)
			x (if (= dir 1) (+ ax 16) (- ax 16))
			arr (SmallArrow x ay dir))
		(. *layer_player* :add_front arr)
		(play-sfx *sfx_twang2* this)))

(defclass SmallCrossbow (man mt_table) (Addon man mt_table 1 +at_small_crossbow)
	; (SmallCrossbow man mt_table) -> player light crossbow addon
	(def this :arrow_fired :nil)
	(. this :sp_add_component (CP cp-small-crossbow)))

(defun use-small-crossbow (man)
	; trigger small crossbow shooting animation and spawn arrow
	(defq dir (get :sp_dir man)
		man_at (elem-get (get :sp_components man) +man_comp_at)
		mt (if (= dir 1) +mt_small_crossbow_r +mt_small_crossbow_l))
	(def man :sp_flags (logior (get :sp_flags man) +fsp_action))
	(. man :sp_set_frame (first +at_man_small_crossbow))
	(set man_at 'speed 1 'table +at_man_small_crossbow 'count 0 'index 1 'loop :nil)
	(defq wep (SmallCrossbow man mt))
	(. man :add_before wep)
	wep)

(defun cp-big-crossbow (this that)
	(when (and (>= (get 'index (elem-get (get :sp_components this) +comp_move)) 7)
			(not (get :arrow_fired this)))
		(def this :arrow_fired :t)
		(bind '(ax ay) (. this :sp_get_pos))
		(defq dir (get :sp_dir this))
		(. *layer_player* :add_front (BigArrow (if (= dir 1) (+ ax 8) (- ax 24)) ay dir))
		(play-sfx *sfx_twang1* this)))

(defclass BigCrossbow (man mt_table) (Addon man mt_table 2 +at_big_crossbow)
	; (BigCrossbow man mt_table) -> player heavy crossbow addon
	(def this :arrow_fired :nil)
	(. this :sp_add_component (CP cp-big-crossbow)))

(defun use-big-crossbow (man)
	; trigger big crossbow shooting animation and spawn bolt
	(defq dir (get :sp_dir man)
		man_at (elem-get (get :sp_components man) +man_comp_at)
		mt (if (= dir 1) +mt_big_crossbow_r +mt_big_crossbow_l))
	(def man :sp_flags (logior (get :sp_flags man) +fsp_action))
	(. man :sp_set_frame (first +at_man_big_crossbow))
	(set man_at 'speed 2 'table +at_man_big_crossbow 'count 0 'index 1 'loop :nil)
	(defq wep (BigCrossbow man mt))
	(. man :add_before wep)
	wep)

(defun cp-naptha (this that)
	(when (and (>= (get 'index (elem-get (get :sp_components this) +comp_move)) 7)
			(not (get :bomb_thrown this)))
		(def this :bomb_thrown :t)
		(bind '(ax ay) (. this :sp_get_pos))
		(defq dir (get :sp_dir this))
		(. *layer_player* :add_front (NBomb (if (= dir 1) (+ ax 4) (- ax 4)) ay dir))))

(defclass Naptha (man mt_table) (Addon man mt_table 3 +at_naptha)
	; (Naptha man mt_table) -> player naptha throwing addon
	(def this :bomb_thrown :nil)
	(. this :sp_add_component (CP cp-naptha)))

(defun use-naptha (man)
	; trigger naptha throwing animation and lob bomb
	(defq dir (get :sp_dir man)
		man_at (elem-get (get :sp_components man) +man_comp_at)
		mt (if (= dir 1) +mt_naptha_r +mt_naptha_l))
	(play-sfx *sfx_throw* man)
	(def man :sp_flags (logior (get :sp_flags man) +fsp_action))
	(. man :sp_set_frame (first +at_man_naptha))
	(set man_at 'speed 3 'table +at_man_naptha 'count 0 'index 1 'loop :nil)
	(defq wep (Naptha man mt))
	(. man :add_before wep)
	wep)

(defun use-helper (man)
	; spawn blue demon homing helper
	(bind '(mx my mw mh) (. man :sp_get_bounds))
	(defq wep (Helper (+ mx 8) (- (+ my (/ mh 2)) 8)))
	(. *layer_player* :add_front wep)
	(play-sfx *sfx_spell* man))

(defun use-super-helper (man)
	; spawn red demon homing helper
	(bind '(mx my mw mh) (. man :sp_get_bounds))
	(defq wep (SuperHelper (+ mx 8) (- (+ my (/ mh 2)) 8)))
	(. *layer_player* :add_front wep)
	(play-sfx *sfx_spell* man))

(defun use-yellow-spell (man)
	; yellow spell spawns roaring elemental head
	(bind '(mx my) (. man :sp_get_pos))
	(defq elem (Elemental mx (- my 32)))
	(. *layer_player* :add_front elem)
	(play-sfx *sfx_roar* man))

(defun use-black-spell (man)
	; black spell fills all 8 inventory slots with a random powerful spell/item
	(bind '(item uses) (elem-get +black_spell_table (game-random 5)))
	(defq idx 0)
	(while (< idx 8)
		(elem-set *item_inventory* idx item)
		(elem-set *item_usage* idx uses)
		(++ idx))
	(play-sfx *sfx_spell* man))

(defun use-green-spell (man)
	; green spell kills all active enemies on screen
	(play-sfx *sfx_spell* man)
	(each (lambda (enemy)
		(when (and (Sprite? enemy)
				(/= (get :sp_frame enemy) -1)
				(not (get :sp_dead enemy))
				(/= (get :sp_type enemy) 0))
			(play-death-score enemy)
			(when (eql (get :sp_death enemy) dt-kill-enemy)
				(def enemy :sp_death dt-kill-zap))
			(. enemy :sp_kill)))
		(. *layer_enemies* :children)))

(defun use-white-spell (man)
	; white spell freezes non-stationary enemies in place
	(play-sfx *sfx_spell* man)
	(each (lambda (enemy)
		(when (Sprite? enemy)
			(defq sp_type (get :sp_type enemy))
			(unless (or (= sp_type 0)
					(bits? sp_type +ftp_cannon +ftp_balista +ftp_oil +ftp_beserk))
				(set (elem-get (get :sp_components enemy) +en_comp_mv) 'vx 0 'max_vx 0))))
		(. *layer_enemies* :children)))

(defun use-red-spell (man)
	; red spell restores 20% strength
	(setq *man_strength* (min +max_strength (+ *man_strength* (const (/ +max_strength 5)))))
	(play-sfx *sfx_spell* man))

(defun use-blue-spell (man)
	; blue spell restores 20% power
	(setq *man_power* (min +max_power (+ *man_power* (const (/ +max_power 5)))))
	(play-sfx *sfx_spell* man))

(defun use-talisman (man)
	; any talisman destroys all the missiles, scoring them, and all the mines
	(each (lambda (sp)
		(when (and (Sprite? sp) (not (get :sp_dead sp)))
			(. sp :sp_kill)
			(play-death-score sp)))
		(. *layer_missiles* :children))
	(each (lambda (sp)
		(if (and (Sprite? sp) (= (get :sp_type sp) +ftp_enemy_fire))
			(. sp :sp_kill)))
		(. *layer_bans* :children))
	(play-sfx *sfx_spell* man))

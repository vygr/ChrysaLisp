;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/onslaught/enemy.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; enemy classes, movement components, and animation tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	+mt_carpet
	''((0 -1) (0 -2) (0 -1) (0 0) (0 1) (0 2) (0 1) (0 0))
	+enemy_sizes
	''((64 64) (32 32) (32 32) (32 32) (32 32) (32 32) (32 32) (32 32)
	   (64 32) (64 32) (64 64) (32 32) (32 32) (32 32) (64 64) (32 32))
	+blat_offsets
	''((-32 -32) (0 -32) (0 0) (-32 0)
	   (-40 -16) (8 -16) (-16 -40) (-16 8)
	   (-24 -16) (-8 -16) (-16 -24) (-16 -8))
	+bone_vectors
	''((1 -8) (-1 -8) (2 -6) (-2 -6)))

(defun cp-enemy-fall (this that)
	; check map surface below front foot and apply gravity when walking off ledge
	(bind '(x y w h) (. this :sp_get_bounds))
	(defq dir (or (get :sp_dir this) 1)
		comps (get :sp_components this)
		mv (elem-get comps +en_comp_mv)
		at_comp (elem-get comps +en_comp_at)
		probe_x (if (= dir 1) (+ x w -1) x)
		probe_y (+ y h)
		flags (. *layer_land* :get_tile_flags probe_x probe_y)
		standing (bits? flags +fmap_stand))
	(ifn standing
		; in air / falling
		(progn
			(set mv :ay 1)
			(when (>= (get :vy mv) (get :max_vy mv))
				(defq vx (get :vx mv))
				(when (/= vx 0)
					(set mv :vx (- vx dir))))
			(when at_comp (set at_comp :speed 0))
			(when (defq fall_f (get :fall_frame this))
				(. this :sp_set_frame fall_f)))
		; on surface
		(when (/= (get :ay mv) 0)
			(set mv :vy 0 :ay 0 :vx (* (get :max_vx mv) dir))
			(. this :sp_set_pos x (- (logand probe_y (neg +tile_height)) h))
			(when at_comp
				(defq tbl (get :table at_comp))
				(set at_comp :speed 2 :count 0 :index (if (> (length tbl) 1) 1 0))
				(when tbl (. this :sp_set_frame (first tbl))))
			(when (/= 0 (logand (or (get :sp_type this) 0) (logior +ftp_horse +ftp_boarrider +ftp_skeleton_horse)))
				(audio-play-rpc *sfx_hooves*)))))

(defun cp-tower-patrol (this that)
	; tower patrols platform back and forth without falling off
	(bind '(x y w h) (. this :sp_get_bounds))
	(defq dir (or (get :sp_dir this) 1)
		mv (elem-get (get :sp_components this) +en_comp_mv)
		probe_x (if (= dir 1) (+ x w -1) x)
		probe_y (+ y h)
		flags (. *layer_land* :get_tile_flags probe_x probe_y))
	(unless (bits? flags +fmap_stand)
		(setq dir (neg dir))
		(def this :sp_dir dir)
		(set mv :vx (* (get :max_vx mv) dir)))
	this)

(defun cp-enemy-duck (this that)
	; adjust sprite height and y when ducking vs standing
	(when (defq duck_f (get :duck_frame this))
		(defq h (get :sp_h this)
			f (get :sp_frame this))
		(if (< f duck_f)
			(when (/= h 32)
				(. this :sp_set_bounds (get :sp_x this) (- (get :sp_y this) 16) (get :sp_w this) 32))
			(when (= h 32)
				(. this :sp_set_bounds (get :sp_x this) (+ (get :sp_y this) 16) (get :sp_w this) 16))))
	this)

(defun cp-enemy-offscreen (this that)
	; test if completely outside camera viewport
	(bind '(rx ry rw rh) (. *world_scroll* :get_relative this))
	(bind '(ww wh) (. *world_scroll* :get_size))
	(when (or (< rx (neg rw)) (>= rx ww) (< ry (neg rh)) (>= ry wh))
		; offscreen: if escaping off the left side, record to overrun stack
		(when (and (< rx (neg rw)) (< (or (get :sp_dir this) 1) 0))
			(when (< *enemy_stack_index* +max_enemy_stack)
				(push *enemy_stack* (get :bftp_type this))
				(++ *enemy_stack_index*)))
		(-- *enemy_count*)
		(def this :sp_death :nil)
		(. this :sp_kill))
	this)

(defun cp-footman (this that)
	(defq comps (get :sp_components this)
		mv (elem-get comps +en_comp_mv)
		at_comp (elem-get comps +en_comp_at)
		action (get :fsp_action this)
		collide (get :fsp_collide this))
	(ifn action
		(when collide
			(def this :fsp_action :t)
			(set mv :vx 0)
			(defq dir (or (get :sp_dir this) 1)
				upper (= (random 2) 0)
				body_anim (if upper '(32 33 34 24) '(38 39 40 37))
				wep_anim (if upper '(18 19 20 -1) '(18 16 17 -1))
				wep_mt (if upper
					(if (= dir 1) +mt_upper_mase_r +mt_upper_mase_l)
					(if (= dir 1) +mt_footman_lower_mase_r +mt_footman_lower_mase_l)))
			(. this :sp_set_frame (first body_anim))
			(cp-enemy-duck this :nil)
			(set at_comp :table body_anim :speed 2 :count 0 :index 1 :loop :nil)
			(defq wep (EnemyAddon this wep_mt 2 wep_anim 4))
			(. *layer_enemies* :add_back wep))
		(when (and at_comp (nil? (get :table at_comp)))
			; attack cycle finished: clear collision state and resume walk
			(def this :fsp_action :nil :fsp_collide :nil)
			(defq dir (or (get :sp_dir this) 1)
				walk_table '(25 26 27 28 29 30 31 24))
			(. this :sp_set_frame (first walk_table))
			(cp-enemy-duck this :nil)
			(set at_comp :table walk_table :speed 2 :count 0 :index 1 :loop :t)
			(set mv :vx (* (get :max_vx mv) dir)))))

(defun cp-knight (this that)
	(defq comps (get :sp_components this)
		mv (elem-get comps +en_comp_mv)
		at_comp (elem-get comps +en_comp_at)
		action (get :fsp_action this)
		collide (get :fsp_collide this))
	(ifn action
		(when collide
			(def this :fsp_action :t)
			(set mv :vx 0)
			(defq dir (or (get :sp_dir this) 1)
				man_duck (and *player_man* (>= (get :sp_frame *player_man*) +frm_fanatic_l_duck))
				upper (not man_duck)
				body_anim (if upper '(49 50 51 41) '(55 56 57 54))
				wep_anim (if upper '(24 25 26 -1) '(21 22 23 -1))
				wep_mt (if upper
					(if (= dir 1) +mt_upper_mase_r +mt_upper_mase_l)
					(if (= dir 1) +mt_knight_mase_r +mt_knight_mase_l)))
			(. this :sp_set_frame (first body_anim))
			(cp-enemy-duck this :nil)
			(set at_comp :table body_anim :speed 2 :count 0 :index 1 :loop :nil)
			(defq wep (EnemyAddon this wep_mt 2 wep_anim 8))
			(. *layer_enemies* :add_back wep))
		(when (and at_comp (nil? (get :table at_comp)))
			(def this :fsp_action :nil :fsp_collide :nil)
			(defq dir (or (get :sp_dir this) 1)
				walk_table '(42 43 44 45 46 47 48 41))
			(. this :sp_set_frame (first walk_table))
			(cp-enemy-duck this :nil)
			(set at_comp :table walk_table :speed 2 :count 0 :index 1 :loop :t)
			(set mv :vx (* (get :max_vx mv) dir)))))

(defun cp-skeleton (this that)
	(defq comps (get :sp_components this)
		mv (elem-get comps +en_comp_mv)
		at_comp (elem-get comps +en_comp_at)
		action (get :fsp_action this)
		collide (get :fsp_collide this))
	(ifn action
		(when collide
			(def this :fsp_action :t)
			(set mv :vx 0)
			(defq dir (or (get :sp_dir this) 1)
				upper (= (random 2) 0)
				skel +frm_fanatic_l_skeleton
				body_anim (if upper
					(list (+ skel 8) (+ skel 9) (+ skel 10) (+ skel 9))
					(list (+ skel 12) (+ skel 13) (+ skel 14) (+ skel 13)))
				wep_anim (if upper '(39 39 40 -1) '(41 41 42 -1))
				wep_mt (if (= dir 1) +mt_skeleton_mase_r +mt_skeleton_mase_l))
			(. this :sp_set_frame (first body_anim))
			(cp-enemy-duck this :nil)
			(set at_comp :table body_anim :speed 2 :count 0 :index 1 :loop :nil)
			(defq wep (EnemyAddon this wep_mt 2 wep_anim 8))
			(. *layer_enemies* :add_back wep))
		(when (and at_comp (nil? (get :table at_comp)))
			(def this :fsp_action :nil :fsp_collide :nil)
			(defq dir (or (get :sp_dir this) 1)
				skel +frm_fanatic_l_skeleton
				walk_table (list (+ skel 1) (+ skel 2) (+ skel 3) (+ skel 4)
					(+ skel 5) (+ skel 6) (+ skel 7) skel))
			(. this :sp_set_frame (first walk_table))
			(cp-enemy-duck this :nil)
			(set at_comp :table walk_table :speed 2 :count 0 :index 1 :loop :t)
			(set mv :vx (* (get :max_vx mv) dir)))))

(defun cp-beserk (this that)
	; trigger upper or lower mase swing addon on frame transition
	(defq f (get :sp_frame this)
		lower (>= f 15)
		current_swing (if lower :lower :upper)
		last_swing (get :beserk_swing this))
	(when (nql current_swing last_swing)
		(def this :beserk_swing current_swing)
		(defq dir (or (get :sp_dir this) 1)
			wep_anim (if lower '(30 31 32 -1) '(27 28 29 -1))
			wep_mt (if lower
				(if (= dir 1) +mt_beserk_lower_mase_r +mt_beserk_lower_mase_l)
				(if (= dir 1) +mt_beserk_upper_mase_r +mt_beserk_upper_mase_l))
			wep (EnemyAddon this wep_mt 3 wep_anim 8))
		(. *layer_enemies* :add_back wep)))

(defun cp-cannon (this that)
	; fire cannonball and spawn muzzle flame at bottom half on firing frame
	(defq f (get :sp_frame this))
	(if (= f 5)
		(unless (get :cannon_fired this)
			(def this :cannon_fired :t)
			(bind '(cx cy) (. this :sp_get_pos))
			(defq dir (or (get :sp_dir this) 1)
				flame (CannonFlame this)
				bx (if (= dir 1) (+ cx 32) (- cx 16))
				by (+ cy 16)
				ball (CannonBall bx by dir))
			(. *layer_enemies* :add_back flame)
			(. *layer_missiles* :add_front ball)
			(audio-play-rpc *sfx_explode*))
		(def this :cannon_fired :nil)))

(defun cp-balista (this that)
	; fire balista arrow at top half of sprite when bow releases from full draw
	(defq f (get :sp_frame this))
	(if (= f 2)
		(def this :balista_drawn :t)
		(when (and (= f 1) (get :balista_drawn this))
			(def this :balista_drawn :nil)
			(bind '(cx cy) (. this :sp_get_pos))
			(defq dir (or (get :sp_dir this) 1)
				ax (if (= dir 1) (+ cx 32) (- cx 32))
				ay cy
				arrow (BalistaArrow ax ay dir))
			(. *layer_missiles* :add_front arrow)
			(audio-play-rpc *sfx_twang1*))))

(defun blast-area (x y w h)
	; damage/destroy enemies, missiles, and other mines in blast box
	(while (defq hit (sprite-collide *layer_enemies* x y w h -1))
		(. hit :sp_kill))
	(while (defq hit (sprite-collide *layer_missiles* x y w h -1))
		(. hit :sp_kill))
	(while (defq hit (sprite-collide *layer_bans* x y w h +ftp_enemy_fire))
		(. hit :sp_kill)))

(defun dt-mine-explode (this)
	; mine detonation: cluster explosion, blast area, decrement count
	(-- *mine_count*)
	(bind '(mx my mw mh) (. this :sp_get_bounds))
	(defq cx (+ mx (/ mw 2))
		cy (+ my (/ mh 2)))
	(each (lambda ((ox oy))
		(defq exp (. (Explosion) :sp_set_pos (+ cx ox) (+ cy oy)))
		(. *layer_fx* :add_front exp))
		+blat_offsets)
	(audio-play-rpc *sfx_explode*)
	(blast-area (- mx 32) (- my 36) 80 80))

(defun cp-mine (this that)
	; offscreen mine cleanup without death explosion
	(bind '(rx ry rw rh) (. *world_scroll* :get_relative this))
	(bind '(ww wh) (. *world_scroll* :get_size))
	(when (or (< rx (neg rw)) (>= rx ww) (< ry (neg rh)) (>= ry wh))
		(-- *mine_count*)
		(def this :sp_death :nil)
		(. this :sp_kill)))

(defun dt-exp-fx (this)
	; death explosion fx centered on sprite
	(bind '(mx my mw mh) (. this :sp_get_bounds))
	(defq exp (. (Explosion) :sp_set_pos (- (+ mx (/ mw 2)) 16) (- (+ my (/ mh 2)) 16)))
	(. *layer_fx* :add_front exp)
	(audio-play-rpc *sfx_explode*))

(defun dt-blat-fx (this)
	; spawn 12-explosion cluster centered on sprite and play explosion sfx
	(bind '(mx my mw mh) (. this :sp_get_bounds))
	(defq cx (+ mx (/ mw 2))
		cy (+ my (/ mh 2)))
	(each (lambda ((ox oy))
		(defq exp (. (Explosion) :sp_set_pos (+ cx ox) (+ cy oy)))
		(. *layer_fx* :add_front exp))
		+blat_offsets)
	(audio-play-rpc *sfx_explode*))

(defun dt-monk-explode (this)
	; monk detonation: cluster explosion, 80x80 blast area, decrement enemy count
	(-- *enemy_count*)
	(bind '(mx my) (. this :sp_get_pos))
	(dt-blat-fx this)
	(blast-area (- mx 24) (- my 24) 80 80))

(defun spawn-flying-body (enemy)
	; spawn a dead body flying backwards and upwards with gravity
	(when (defq dead_f (get :dead_frame enemy))
		(bind '(x y w h) (. enemy :sp_get_bounds))
		(defq dir (or (get :sp_dir enemy) 1)
			bh (if (= h 16) 32 h)
			by (if (= h 16) (- y 16) y)
			cl (get :sp_canvas_l enemy)
			cr (get :sp_canvas_r enemy)
			tag (get :sp_canvas_tag enemy)
			body (Body cl cr w bh tag dead_f dir (neg (* dir 4)) -6))
		(. body :sp_set_pos x by)
		(. *layer_bodies* :add_front body)
		body))

(defun audio-play-death ()
	; randomly play death1 or death2 grunt sfx
	(audio-play-rpc (if (= (random 2) 0) *sfx_death1* *sfx_death2*)))

(defun dt-kill-enemy (this)
	; general enemy death: decrement count and spawn flying corpse with random death grunt
	(-- *enemy_count*)
	(spawn-flying-body this)
	(audio-play-death))

(defun dt-kill-horse (this)
	; horse death: rider knight dismounts, horse body flies away
	(bind '(x y) (. this :sp_get_pos))
	(defq dir (or (get :sp_dir this) 1)
		knight (Knight)
		mv (elem-get (get :sp_components knight) +en_comp_mv))
	(def knight :sp_dir dir)
	(when mv (set mv :vx (* (get :max_vx mv) dir)))
	(. knight :sp_set_pos (+ x 16) y)
	(. *layer_enemies* :add_front knight)
	(spawn-flying-body this)
	(audio-play-rpc *sfx_horse*))

(defun dt-kill-boarrider (this)
	; boarrider death: spearman dismounts, boar body flies away
	(bind '(x y) (. this :sp_get_pos))
	(defq dir (or (get :sp_dir this) 1)
		spearman (Spearman)
		mv (elem-get (get :sp_components spearman) +en_comp_mv))
	(def spearman :sp_dir dir)
	(when mv (set mv :vx (* (get :max_vx mv) dir)))
	(. spearman :sp_set_pos (+ x 16) y)
	(. *layer_enemies* :add_front spearman)
	(spawn-flying-body this)
	(audio-play-rpc *sfx_boar*))

(defun dt-kill-tower (this)
	; tower death: 4 spearmen emerge from the 4 corners, blat explosion
	(-- *enemy_count*)
	(bind '(x y) (. this :sp_get_pos))
	(each (lambda ((ox oy sdir))
		(defq sp (Spearman)
			mv (elem-get (get :sp_components sp) +en_comp_mv))
		(def sp :sp_dir sdir)
		(when mv (set mv :vx (* (get :max_vx mv) sdir)))
		(. sp :sp_set_pos (+ x ox) (+ y oy))
		(. *layer_enemies* :add_front sp)
		(++ *enemy_count*))
		'((0 0 -1) (32 0 1) (0 32 -1) (32 32 1)))
	(dt-blat-fx this))

(defun dt-kill-balista (this)
	; balista, cannon, and oil death: crew emerges as footman, machine body flies away
	(bind '(x y) (. this :sp_get_pos))
	(defq dir (or (get :sp_dir this) 1)
		footman (Footman)
		mv (elem-get (get :sp_components footman) +en_comp_mv))
	(def footman :sp_dir dir)
	(when mv (set mv :vx (* (get :max_vx mv) dir)))
	(. footman :sp_set_pos x y)
	(. *layer_enemies* :add_front footman)
	(spawn-flying-body this)
	(audio-play-rpc *sfx_clash2*))

(defun dt-kill-carpet (this)
	; flying carpet death: two wizards dismount at x and x+32
	(bind '(x y) (. this :sp_get_pos))
	(defq dir (or (get :sp_dir this) 1))
	(each (lambda (ox)
		(defq wiz (Wizard)
			mv (elem-get (get :sp_components wiz) +en_comp_mv))
		(def wiz :sp_dir dir)
		(when mv (set mv :vx (* (get :max_vx mv) dir)))
		(. wiz :sp_set_pos (+ x ox) y)
		(. *layer_enemies* :add_front wiz)
		(++ *enemy_count*))
		'(0 32))
	(dt-exp-fx this))

(defun dt-kill-zap (this)
	; zap disintegration death: scatter 8 fizz sparks across sprite extent and play spell sound
	(-- *enemy_count*)
	(bind '(x y w h) (. this :sp_get_bounds))
	(defq rw (max 1 (- w 16))
		rh (max 1 (- h 16)))
	(times 8
		(defq fx (. (Fizz) :sp_set_pos (+ x (random rw)) (+ y (random rh))))
		(. *layer_fx* :add_front fx))
	(audio-play-rpc *sfx_spell*))

(defun dt-kill-bones (this)
	; skeleton death: decrement count and scatter bouncing bone fragments from torso
	(-- *enemy_count*)
	(bind '(x y) (. this :sp_get_pos))
	(defq bx (+ x 8)
		by (+ y 12))
	(each (lambda ((vx vy))
		(defq bone (. (Bone bx by vx vy) :sp_set_pos bx by))
		(. *layer_fx* :add_front bone))
		+bone_vectors)
	(audio-play-death))

(defun dt-kill-skeleton-horse (this)
	; skeleton horse death: rider skeleton dismounts, horse bursts with bones from ribcage
	(bind '(x y w h) (. this :sp_get_bounds))
	(defq dir (or (get :sp_dir this) 1)
		skel (Skeleton)
		mv (elem-get (get :sp_components skel) +en_comp_mv))
	(def skel :sp_dir dir)
	(when mv (set mv :vx (* (get :max_vx mv) dir)))
	(. skel :sp_set_pos (+ x 16) y)
	(. *layer_enemies* :add_front skel)
	(defq bx (+ x 24)
		by (+ y 28))
	(each (lambda ((vx vy))
		(defq bone (. (Bone bx by vx vy) :sp_set_pos bx by))
		(. *layer_fx* :add_front bone))
		+bone_vectors)
	(audio-play-rpc *sfx_spell*))

(defun cp-bone (this that)
	; bone lifespan countdown and bounce against floor
	(defq life (- (or (get :life this) 20) 1))
	(if (<= life 0)
		(. this :sp_kill)
		(def this :life life)
		(bind '(x y w h) (. this :sp_get_bounds))
		(when (defq mv (elem-get (get :sp_components this) 0))
			(defq vy (get :vy mv))
			(when (> vy 0)
				(defq ground_y (logand (+ y h vy) (neg +tile_height)))
				(when (bits? (. *layer_land* :get_tile_flags (+ x 8) ground_y) +fmap_stand)
					(. this :sp_set_pos x (- ground_y h))
					(set mv :vy (neg vy))
					(spawn-fizz-fx (+ x 8) (+ y 8)))))))

;;;;;;;;;;;;;;;;;;;;;;
; enemy sprite classes
;;;;;;;;;;;;;;;;;;;;;;

(defclass Horse () (Sprite *img_frm_64x64_l* *img_frm_64x64_r* 64 64 :64x64_lr (const dt-kill-horse))
	(def this :sp_type +ftp_horse :bftp_type 0 :fall_frame 4 :dead_frame 5)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 8 8))
		(:sp_add_component (AT 2 '(0 1 2 3)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm_64x64_l_horse)))

(defclass Spearman () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr (const dt-kill-enemy))
	(def this :sp_type +ftp_spearman :bftp_type 1 :fall_frame 10 :duck_frame 11 :dead_frame 9)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 8))
		(:sp_add_component (AT 2 '(4 5 0 1 2 3)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-duck)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame 4)))

(defclass Wizard () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr (const dt-kill-enemy))
	(def this :sp_type +ftp_wizard :bftp_type 2 :fall_frame 22 :duck_frame 23 :dead_frame 21)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 8))
		(:sp_add_component (AT 2 '(16 17 12 13 14 15)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-duck)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame 16)))

(defclass Footman () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr (const dt-kill-enemy))
	(def this :sp_type +ftp_footman :bftp_type 3 :fall_frame 36 :duck_frame 37 :dead_frame 35)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 4 8))
		(:sp_add_component (AT 2 '(25 26 27 28 29 30 31 24)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-duck)))
		(:sp_add_component (CP (const cp-footman)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame 25)))

(defclass Knight () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr (const dt-kill-enemy))
	(def this :sp_type +ftp_knight :bftp_type 4 :fall_frame 53 :duck_frame 54 :dead_frame 52)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 4 8))
		(:sp_add_component (AT 2 '(42 43 44 45 46 47 48 41)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-duck)))
		(:sp_add_component (CP (const cp-knight)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame 42)))

(defclass BalistaArrow (x y dir) (Sprite *img_frm_32x16_l* *img_frm_32x16_r* 32 16 :32x16_lr)
	; (BalistaArrow x y dir) -> balista arrow missile sprite
	(def this :sp_type +ftp_enemy_fire :sp_dir dir :sp_hitpnt 8)
	(.-> this
		(:sp_add_component (MV (* 16 dir) 0 0 0 16 0))
		(:sp_add_component (CP (const offscreen-nodeath-update)))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_32x16_l_barrow)))

(defclass Balista () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr (const dt-kill-balista))
	; (Balista) -> balista siege engine enemy sprite
	(def this :sp_type +ftp_balista :bftp_type 5 :dead_frame 3)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 0 0))
		(:sp_add_component (AT 10 '(0 1 2 1)))
		(:sp_add_component (CP (const cp-balista)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm_32c32_l_balista)))

(defclass CannonFlame (cannon) (Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr)
	; (CannonFlame cannon) -> flame addon sprite at bottom half
	(bind '(cx cy) (. cannon :sp_get_pos))
	(defq dir (or (get :sp_dir cannon) 1)
		fx (if (= dir 1) (+ cx 32) (- cx 16))
		fy (+ cy 16))
	(def this :sp_dir dir)
	(.-> this
		(:sp_add_component (AT 4 '(33 33 34 -1) :nil))
		(:sp_add_component (CP (const offscreen-update)))
		(:sp_set_pos fx fy)
		(:sp_set_frame 33)))

(defclass Blood (x y dir) (Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr)
	; (Blood x y dir) -> directional blood spray sprite
	(def this :sp_dir dir)
	(.-> this
		(:sp_add_component (MV (* 4 dir) -4 0 1 100 100))
		(:sp_add_component (AT 2 '(35 36 37 38 -1) :nil))
		(:sp_add_component (CP (const offscreen-update)))
		(:sp_set_pos x y)
		(:sp_set_frame 35)))

(defclass OilDrop (x y) (Sprite *img_frm_16x32* :nil 16 32 :16x32 (const dt-exp-fx))
	; (OilDrop x y) -> falling boiling oil projectile with explosion on death
	(def this :sp_type +ftp_enemy_fire :sp_hitpnt 48)
	(.-> this
		(:sp_add_component (MV 0 0 0 1 0 10))
		(:sp_add_component (CP (const offscreen-nodeath-update)))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_16x32_oilshot)))

(defclass Spear (x y dir) (Sprite *img_frm_32x16_l* *img_frm_32x16_r* 32 16 :32x16_lr)
	; (Spear x y dir) -> thrown spear missile sprite
	(def this :sp_type +ftp_enemy_fire :sp_dir dir :sp_hitpnt 4)
	(.-> this
		(:sp_add_component (MV (* 12 dir) 0 0 0 12 0))
		(:sp_add_component (CP (const offscreen-nodeath-update)))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_32x16_l_spear)))

(defclass WizardShot (x y dir frame) (Sprite *img_frm_16x16* :nil 16 16 :16x16 (const dt-fizz-fx))
	; (WizardShot x y dir frame) -> wizard magic projectile with fizz sparks on death
	(def this :sp_type +ftp_enemy_fire :sp_dir dir :sp_hitpnt 16)
	(.-> this
		(:sp_add_component (MV (* 8 dir) 0 0 0 8 0))
		(:sp_add_component (AT 1 (list frame (+ frame 1))))
		(:sp_add_component (CP (const offscreen-nodeath-update)))
		(:sp_set_pos x y)
		(:sp_set_frame frame)))

(defclass CannonBall (x y dir) (Sprite *img_frm_16x16* :nil 16 16 :16x16 (const dt-exp-fx))
	; (CannonBall x y dir) -> cannonball missile sprite with explosion on death
	(def this :sp_type +ftp_enemy_fire :sp_dir dir :sp_hitpnt 24)
	(.-> this
		(:sp_add_component (MV (* 12 dir) 0 0 0 12 0))
		(:sp_add_component (CP (const offscreen-nodeath-update)))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_16x16_cball)))

(defclass Cannon () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr (const dt-kill-balista))
	; (Cannon) -> cannon artillery enemy sprite
	(def this :sp_type +ftp_cannon :bftp_type 6 :dead_frame 7)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 0 0))
		(:sp_add_component (AT 4 '(4 4 4 4 4 5 5 6)))
		(:sp_add_component (CP (const cp-cannon)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm_32c32_l_cannon)))

(defclass Oil () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr (const dt-kill-balista))
	; (Oil) -> boiling oil cauldron enemy sprite
	(def this :sp_type +ftp_oil :bftp_type 7 :dead_frame 11)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 0 0))
		(:sp_add_component (AT 4 '(8 9 8 9 10 9)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm_32c32_l_oil)))

(defclass BoarRider () (Sprite *img_frm_64x32_l* *img_frm_64x32_r* 64 32 :64x32_lr (const dt-kill-boarrider))
	(def this :sp_type +ftp_boarrider :bftp_type 8 :fall_frame 4 :dead_frame 5)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 10))
		(:sp_add_component (AT 2 '(0 1 2 3)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm64x32_l_boarrider)))

(defclass Carpet () (Sprite *img_frm_64x32_l* *img_frm_64x32_r* 64 32 :64x32_lr (const dt-kill-carpet))
	; (Carpet) -> flying carpet enemy sprite
	(def this :sp_type +ftp_carpet :bftp_type 9)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 0))
		(:sp_add_component (AT 2 '(6 7)))
		(:sp_add_component (MT 1 +mt_carpet))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm64x32_l_carpet)))

(defclass Tower () (Sprite *img_frm_64x64* :nil 64 64 :64x64 (const dt-kill-tower))
	(def this :sp_type +ftp_tower :bftp_type 10)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 0))
		(:sp_add_component (AT 2 '(0 1 2 3 0 1 2 0 1 2)))
		(:sp_add_component (CP (const cp-tower-patrol)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm64x64_tower)))

(defclass Monk () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr (const dt-monk-explode))
	; (Monk) -> monk suicide bomber enemy sprite
	(def this :sp_type +ftp_monk :bftp_type 11 :fall_frame 20)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 3 8))
		(:sp_add_component (AT 2 '(18 19 20)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm_32c32_l_monk)))

(defclass Beserk () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr (const dt-kill-zap))
	; (Beserk) -> stationary berserker enemy sprite
	(def this :sp_type +ftp_beserk :bftp_type 12 :fall_frame 14 :duck_frame 15)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 0 0))
		(:sp_add_component (AT 3 '(12 13 14 15 16 17)))
		(:sp_add_component (CP (const cp-enemy-duck)))
		(:sp_add_component (CP (const cp-beserk)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame 12)))

(defclass Body (canvas_l canvas_r w h tag frame dir vx vy) (Sprite canvas_l canvas_r w h tag)
	; (Body ...) -> dead flying body sprite
	(def this :sp_dir dir)
	(.-> this
		(:sp_add_component (MV vx vy 0 1 100 100))
		(:sp_add_component (CP (const offscreen-update)))
		(:sp_set_frame frame)))

(defclass SkeletonMonk () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic (const dt-monk-explode))
	; (SkeletonMonk) -> skeleton monk suicide bomber enemy sprite
	(def this :sp_type +ftp_skeleton_monk :bftp_type 13
		:fall_frame (+ +frm_fanatic_l_skeleton_monk 2))
	(.-> this
		(:sp_add_component (MV 0 0 0 0 3 8))
		(:sp_add_component (AT 2 (list +frm_fanatic_l_skeleton_monk
			(+ +frm_fanatic_l_skeleton_monk 1)
			(+ +frm_fanatic_l_skeleton_monk 2))))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm_fanatic_l_skeleton_monk)))

(defclass Bone (x y vx vy) (Sprite *img_frm_16x16* :nil 16 16 :16x16)
	; (Bone x y vx vy) -> bouncing skeletal bone fragment
	(def this :life 20)
	(.-> this
		(:sp_add_component (MV vx vy 0 1 10 10))
		(:sp_add_component (AT 1 (list +frm_16x16_bones (+ +frm_16x16_bones 1) (+ +frm_16x16_bones 2))))
		(:sp_add_component (CP (const cp-bone)))
		(:sp_add_component (CP (const offscreen-update)))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_16x16_bones)))

(defclass SkeletonHorse () (Sprite *img_frm_64x64_l* *img_frm_64x64_r* 64 64 :64x64_lr (const dt-kill-skeleton-horse))
	; (SkeletonHorse) -> undead skeleton horse enemy sprite
	(def this :sp_type +ftp_skeleton_horse :bftp_type 14 :fall_frame 10)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 8 8))
		(:sp_add_component (AT 2 '(6 7 8 9)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm_64x64_l_skeleton_horse)))

(defclass Skeleton () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic (const dt-kill-bones))
	; (Skeleton) -> skeleton foot soldier enemy sprite
	(def this :sp_type +ftp_skeleton :bftp_type 15
		:fall_frame (+ +frm_fanatic_l_skeleton 11)
		:duck_frame (+ +frm_fanatic_l_skeleton 12))
	(.-> this
		(:sp_add_component (MV 0 0 0 0 4 8))
		(:sp_add_component (AT 2 (list (+ +frm_fanatic_l_skeleton 1) (+ +frm_fanatic_l_skeleton 2)
			(+ +frm_fanatic_l_skeleton 3) (+ +frm_fanatic_l_skeleton 4)
			(+ +frm_fanatic_l_skeleton 5) (+ +frm_fanatic_l_skeleton 6)
			(+ +frm_fanatic_l_skeleton 7) +frm_fanatic_l_skeleton)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-duck)))
		(:sp_add_component (CP (const cp-skeleton)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm_fanatic_l_skeleton)))

(defclass Missile (frame &optional vx vy) (Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr)
	(setd vx 0 vy 0)
	(def this :sp_type +ftp_enemy_fire :sp_dir (if (>= vx 0) 1 -1))
	(.-> this
		(:sp_add_component (MV vx vy 0 0))
		(:sp_add_component (CP (const offscreen-update)))
		(:sp_set_frame frame)))

(defclass Mine () (Sprite *img_frm_16x16* :nil 16 8 :16x16 (const dt-mine-explode))
	; (Mine) -> ground mine sprite with 16-pixel texture cell height
	(def this :sp_type +ftp_enemy_fire :sp_hitpnt 150 :sp_frame_h 16)
	(.-> this
		(:sp_add_component (AT 2 (list +frm_16x16_mine (+ +frm_16x16_mine 1))))
		(:sp_add_component (CP (const cp-mine)))
		(:sp_set_frame +frm_16x16_mine)))

(defclass Item (frame) (Sprite *img_frm_16x16* :nil 16 16 :16x16)
	(def this :sp_type +ftp_item)
	(. this :sp_set_frame frame))

(defun create-enemy-type (type)
	; factory function mapping 0..15 to enemy class
	(defq sp (case type
		(0 (Horse))
		(1 (Spearman))
		(2 (Wizard))
		(3 (Footman))
		(4 (Knight))
		(5 (Balista))
		(6 (Cannon))
		(7 (Oil))
		(8 (BoarRider))
		(9 (Carpet))
		(10 (Tower))
		(11 (Monk))
		(12 (Beserk))
		(13 (SkeletonMonk))
		(14 (SkeletonHorse))
		(15 (Skeleton))
		(:t (Footman))))
	(def sp :bftp_type type)
	sp)
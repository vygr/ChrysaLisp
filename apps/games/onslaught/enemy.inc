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
	   (64 32) (64 32) (64 64) (32 32) (32 32) (32 32) (64 64) (32 32)))

(defun cp-enemy-fall (this that)
	; check map surface below feet and apply gravity when walking off ledge
	(bind '(x y w h) (. this :sp_get_bounds))
	(defq dir (or (get :sp_dir this) 1)
		comps (get :sp_components this)
		mv (elem-get comps +en_comp_mv)
		at_comp (elem-get comps +en_comp_at)
		probe_x1 (+ x 4)
		probe_x2 (+ x w -5)
		probe_y (+ y h)
		f1 (. *layer_land* :get_tile_flags probe_x1 probe_y)
		f2 (. *layer_land* :get_tile_flags probe_x2 probe_y)
		standing (or (bits? f1 +fmap_stand) (bits? f2 +fmap_stand)))
	(ifn standing
		; in air / falling
		(progn
			(set mv :ay 1)
			(when at_comp (set at_comp :speed 0))
			(when (defq fall_f (get :fall_frame this))
				(. this :sp_set_frame fall_f)))
		; on surface
		(when (/= (get :ay mv) 0)
			(set mv :vy 0 :ay 0 :vx (* (get :max_vx mv) dir))
			(. this :sp_set_pos x (- (logand probe_y (neg +tile_height)) h))
			(when at_comp (set at_comp :speed 2 :count 0)))))

(defun cp-tower-patrol (this that)
	; tower patrols platform back and forth without falling off
	(bind '(x y w h) (. this :sp_get_bounds))
	(defq dir (or (get :sp_dir this) 1)
		mv (elem-get (get :sp_components this) +en_comp_mv)
		probe_x (if (= dir 1) (+ x w) (- x 1))
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
					(if (= dir 1) +mt_footman_lower_mase_r +mt_footman_lower_mase_l))
				wep (Addon this wep_mt 2 wep_anim))
			(. *layer_enemies* :add_back wep)
			(set at_comp :table body_anim :speed 2 :count 0 :index 0 :loop :nil))
		(when (and at_comp (nil? (get :table at_comp)))
			; attack cycle finished: clear collision state and resume walk
			(def this :fsp_action :nil :fsp_collide :nil)
			(defq dir (or (get :sp_dir this) 1))
			(set at_comp :table '(25 26 27 28 29 30 31 24) :speed 2 :count 0 :index 0 :loop :t)
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
					(if (= dir 1) +mt_knight_mase_r +mt_knight_mase_l))
				wep (Addon this wep_mt 2 wep_anim))
			(. *layer_enemies* :add_back wep)
			(set at_comp :table body_anim :speed 2 :count 0 :index 0 :loop :nil))
		(when (and at_comp (nil? (get :table at_comp)))
			(def this :fsp_action :nil :fsp_collide :nil)
			(defq dir (or (get :sp_dir this) 1))
			(set at_comp :table '(42 43 44 45 46 47 48 41) :speed 2 :count 0 :index 0 :loop :t)
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
				body_anim (if upper
					(list (+ +frm_fanatic_l_skeleton 8) (+ +frm_fanatic_l_skeleton 9)
						(+ +frm_fanatic_l_skeleton 10) (+ +frm_fanatic_l_skeleton 9))
					(list (+ +frm_fanatic_l_skeleton 12) (+ +frm_fanatic_l_skeleton 13)
						(+ +frm_fanatic_l_skeleton 14) (+ +frm_fanatic_l_skeleton 13)))
				wep_anim (if upper '(39 39 40 -1) '(41 41 42 -1))
				wep_mt (if upper
					(if (= dir 1) +mt_upper_mase_r +mt_upper_mase_l)
					(if (= dir 1) +mt_skeleton_mase_r +mt_skeleton_mase_l))
				wep (Addon this wep_mt 2 wep_anim))
			(. *layer_enemies* :add_back wep)
			(set at_comp :table body_anim :speed 2 :count 0 :index 0 :loop :nil))
		(when (and at_comp (nil? (get :table at_comp)))
			(def this :fsp_action :nil :fsp_collide :nil)
			(defq dir (or (get :sp_dir this) 1))
			(set at_comp :table (list (+ +frm_fanatic_l_skeleton 1) (+ +frm_fanatic_l_skeleton 2)
					(+ +frm_fanatic_l_skeleton 3) (+ +frm_fanatic_l_skeleton 4)
					(+ +frm_fanatic_l_skeleton 5) (+ +frm_fanatic_l_skeleton 6)
					(+ +frm_fanatic_l_skeleton 7) +frm_fanatic_l_skeleton)
				:speed 2 :count 0 :index 0 :loop :t)
			(set mv :vx (* (get :max_vx mv) dir)))))

;;;;;;;;;;;;;;;;;;;;;;
; enemy sprite classes
;;;;;;;;;;;;;;;;;;;;;;

(defclass Horse () (Sprite *img_frm_64x64_l* *img_frm_64x64_r* 64 64 :64x64_lr)
	(def this :sp_type +ftp_horse :bftp_type 0 :fall_frame 4)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 8 8))
		(:sp_add_component (AT 2 '(0 1 2 3)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm_64x64_l_horse)))

(defclass Spearman () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	(def this :sp_type +ftp_spearman :bftp_type 1 :fall_frame 10 :duck_frame 11)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 8))
		(:sp_add_component (AT 2 '(4 5 0 1 2 3)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-duck)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame 4)))

(defclass Wizard () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	(def this :sp_type +ftp_wizard :bftp_type 2 :fall_frame 22 :duck_frame 23)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 8))
		(:sp_add_component (AT 2 '(16 17 12 13 14 15)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-duck)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame 16)))

(defclass Footman () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	(def this :sp_type +ftp_footman :bftp_type 3 :fall_frame 36 :duck_frame 37)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 4 8))
		(:sp_add_component (AT 2 '(25 26 27 28 29 30 31 24)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-duck)))
		(:sp_add_component (CP (const cp-footman)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame 25)))

(defclass Knight () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	(def this :sp_type +ftp_knight :bftp_type 4 :fall_frame 53 :duck_frame 54)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 4 8))
		(:sp_add_component (AT 2 '(42 43 44 45 46 47 48 41)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-duck)))
		(:sp_add_component (CP (const cp-knight)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame 42)))

(defclass Balista () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	(def this :sp_type +ftp_balista :bftp_type 5)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 0 0))
		(:sp_add_component (AT 10 '(0 1 2 1)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm_32c32_l_balista)))

(defclass Cannon () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	(def this :sp_type +ftp_cannon :bftp_type 6)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 0 0))
		(:sp_add_component (AT 4 '(4 4 4 4 4 5 5 6)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm_32c32_l_cannon)))

(defclass Oil () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	(def this :sp_type +ftp_oil :bftp_type 7)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 0 0))
		(:sp_add_component (AT 4 '(8 9 8 9 10 9)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm_32c32_l_oil)))

(defclass BoarRider () (Sprite *img_frm_64x32_l* *img_frm_64x32_r* 64 32 :64x32_lr)
	(def this :sp_type +ftp_boarrider :bftp_type 8 :fall_frame 4)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 10))
		(:sp_add_component (AT 2 '(0 1 2 3)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm64x32_l_boarrider)))

(defclass Carpet () (Sprite *img_frm_64x32_l* *img_frm_64x32_r* 64 32 :64x32_lr)
	(def this :sp_type +ftp_carpet :bftp_type 9)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 0))
		(:sp_add_component (AT 2 '(6 7)))
		(:sp_add_component (MT 1 +mt_carpet))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm64x32_l_carpet)))

(defclass Tower () (Sprite *img_frm_64x64* :nil 64 64 :64x64)
	(def this :sp_type +ftp_tower :bftp_type 10)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 0))
		(:sp_add_component (AT 2 '(0 1 2 3 0 1 2 0 1 2)))
		(:sp_add_component (CP (const cp-tower-patrol)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm64x64_tower)))

(defclass Monk () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	(def this :sp_type +ftp_monk :bftp_type 11 :fall_frame 20)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 3 8))
		(:sp_add_component (AT 2 '(18 19 20)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm_32c32_l_monk)))

(defclass Beserk () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	(def this :sp_type +ftp_beserk :bftp_type 12 :fall_frame 14 :duck_frame 15)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 0 0))
		(:sp_add_component (AT 3 '(12 13 14 15 16 17)))
		(:sp_add_component (CP (const cp-enemy-duck)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame 12)))

(defclass SkeletonMonk () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic)
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

(defclass SkeletonHorse () (Sprite *img_frm_64x64_l* *img_frm_64x64_r* 64 64 :64x64_lr)
	(def this :sp_type +ftp_skeleton_horse :bftp_type 14 :fall_frame 10)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 8 8))
		(:sp_add_component (AT 2 '(6 7 8 9)))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm_64x64_l_skeleton_horse)))

(defclass Skeleton () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic)
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
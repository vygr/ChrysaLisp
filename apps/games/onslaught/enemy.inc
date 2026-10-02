;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; enemy classes, movement components, and animation tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and movement tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enemy-size (type)
	; return (w h) based on type bitmask
	(cond
		((bits? type (logior +ftp_horse +ftp_tower +ftp_skeleton_horse)) '(64 64))
		((bits? type (logior +ftp_boarrider +ftp_carpet)) '(64 32))
		(:t '(32 32))))

(defq +blat_offsets ''((-32 -32) (0 -32) (0 0) (-32 0)
		(-40 -16) (8 -16) (-16 -40) (-16 8)
		(-24 -16) (-8 -16) (-16 -24) (-16 -8))
	+drop_table `'(,+frm_16x16_shield ,(+ +frm_16x16_shield 1) ,(+ +frm_16x16_shield 2)
		,(+ +frm_16x16_shield 3) ,(+ +frm_16x16_shield 4) ,(+ +frm_16x16_shield 5)
		,(+ +frm_16x16_shield 6) ,+frm_16x16_spells ,(+ +frm_16x16_spells 1)
		,(+ +frm_16x16_spells 2) ,(+ +frm_16x16_spells 3) ,(+ +frm_16x16_spells 4)
		,(+ +frm_16x16_spells 4) ,(+ +frm_16x16_spells 5) ,(+ +frm_16x16_spells 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; utilities & spawn helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun audio-play-death ()
	; randomly play death1 or death2 grunt sfx
	(audio-play-rpc (if (= (game-random 2) 0) *sfx_death1* *sfx_death2*)))

(defun blast-area (x y w h)
	; damage/destroy enemies, enemy missiles, and mines in blast box
	(while (defq hit (sprite-collide *layer_enemies* x y w h -1))
		(. hit :sp_kill))
	(while (defq hit (sprite-collide *layer_missiles* x y w h -1))
		(. hit :sp_kill))
	(while (defq hit (sprite-collide *layer_bans* x y w h +ftp_enemy_fire))
		(. hit :sp_kill)))

(defun item-info (frame)
	; return (uses timeout) matching cpp definitions
	(cond
		((= frame +frm_16x16_gold) '(1 64))
		((= frame +frm_16x16_shield) '(48 96))
		((= frame (+ +frm_16x16_shield 1)) '(8 64))
		((= frame (+ +frm_16x16_shield 2)) '(16 64))
		((= frame (+ +frm_16x16_shield 3)) '(6 48))
		((= frame (+ +frm_16x16_shield 4)) '(4 48))
		((= frame (+ +frm_16x16_shield 5)) '(8 16))
		((= frame (+ +frm_16x16_shield 6)) '(3 8))
		((= frame +frm_16x16_spells) '(1 16))
		((= frame (+ +frm_16x16_spells 1)) '(1 8))
		((= frame (+ +frm_16x16_spells 2)) '(1 16))
		((= frame (+ +frm_16x16_spells 3)) '(1 32))
		((= frame (+ +frm_16x16_spells 4)) '(1 32))
		((= frame (+ +frm_16x16_spells 5)) '(1 32))
		('(1 32))))

;;;;;;;;;;;;;;;;
; death handlers
;;;;;;;;;;;;;;;;

(defun dt-exp-fx (this)
	; death explosion fx centered on sprite
	(bind '(mx my mw mh) (. this :sp_get_bounds))
	(defq exp (. (Explosion) :sp_set_pos (- (+ mx (/ mw 2)) 16) (- (+ my (/ mh 2)) 16)))
	(. *layer_fx* :add_front exp)
	(audio-play-rpc *sfx_explode*))

(defun dt-fizz-fx (this)
	; spawn fizz spark fx on item death
	(bind '(x y w h) (. this :sp_get_bounds))
	(spawn-fizz-fx (+ x (/ w 2)) (+ y (/ h 2))))

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

(defun spawn-flying-body (enemy)
	; spawn a dead body flying backwards and upwards with gravity
	(when (defq dead_f (get :dead_frame enemy))
		(bind '(x y w h) (. enemy :sp_get_bounds))
		(defq dir (get :sp_dir enemy)
			bh (if (= h 16) 32 h)
			by (if (= h 16) (- y 16) y)
			cl (get :sp_canvas_l enemy)
			cr (get :sp_canvas_r enemy)
			tag (get :sp_canvas_tag enemy)
			body (Body cl cr w bh tag dead_f dir (neg (* dir 4)) -6))
		(. body :sp_set_pos x by)
		(. *layer_bodies* :add_front body)
		body))

(defun drop-item (enemy)
	; drop item upon enemy death
	(bind '(ex ey ew eh) (. enemy :sp_get_bounds))
	(when (<= 0 ex (- (* +map_width +tile_width) 32))
		(defq cnt (dec (or *item_drop_cnt* 0))
			frame :nil
			drop_x (+ ex (/ ew 2) -8)
			drop_y (+ ey eh -24)
			vy -9)
		(cond
			((<= cnt 0)
				; drop gold bag from top of visible screen
				(setq cnt (+ (game-random 12) 12)
					frame +frm_16x16_gold)
				(bind '(lx ly &ignore) (. *world_scroll* :get_relative *layer_land*))
				(bind '(ww &ignore) (. *world_scroll* :get_size))
				(setq drop_x (+ (/ (neg lx) *zoom*) (game-random (max 1 (- (/ ww *zoom*) 16))))
					drop_y (/ (neg ly) *zoom*)
					vy 0))
			(:t
				(setq frame (elem-get +drop_table (game-random (length +drop_table))))))
		(setq *item_drop_cnt* cnt)
		(bind '(uses timeout) (item-info frame))
		(defq item (Item frame uses timeout vy))
		(. item :sp_set_pos drop_x drop_y)
		(. *layer_items* :add_front item)
		item))

(defun dt-kill-zap (this)
	; zap disintegration death: drop item and scatter 8 sparks
	(-- *enemy_count*)
	(drop-item this)
	(bind '(x y w h) (. this :sp_get_bounds))
	(defq rw (max 1 (- w 16))
		rh (max 1 (- h 16)))
	(times 8
		(defq fx (. (Fizz) :sp_set_pos (+ x (game-random rw)) (+ y (game-random rh))))
		(. *layer_fx* :add_front fx))
	(audio-play-rpc *sfx_spell*))

(defun dt-kill-enemy (this)
	; general enemy death: drop item, spawn flying corpse, random death grunt
	(-- *enemy_count*)
	(drop-item this)
	(spawn-flying-body this)
	(audio-play-death))

;;;;;;;;;;;;;;;;;;;;
; component handlers
;;;;;;;;;;;;;;;;;;;;

(defun cp-enemy-fall (this that)
	; check map surface below front foot and apply gravity when walking off ledge
	(bind '(x y w h) (. this :sp_get_bounds))
	(defq dir (get :sp_dir this)
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
			(set mv 'ay 1)
			(when (>= (get 'vy mv) (get 'max_vy mv))
				(defq vx (get 'vx mv))
				(when (/= vx 0)
					(set mv 'vx (- vx dir))))
			(set at_comp 'speed 0)
			(when (defq fall_f (get :fall_frame this))
				(. this :sp_set_frame fall_f)))
		; on surface
		(defq snap_y (logand y (neg +tile_height)))
		(when (/= y snap_y)
			(setq y snap_y)
			(. this :sp_set_pos x y))
		(when (/= (get 'ay mv) 0)
			(set mv 'vy 0 'ay 0 'vx (* (get 'max_vx mv) dir))
			(setq y (- (logand probe_y (neg +tile_height)) h))
			(. this :sp_set_pos x y)
			(when at_comp
				(defq tbl (get 'table at_comp))
				(set at_comp 'speed 2 'count 0 'index (if (> (length tbl) 1) 1 0))
				(when tbl (. this :sp_set_frame (first tbl))))
			(when (bits? (get :sp_type this) +ftp_horse +ftp_boarrider +ftp_skeleton_horse)
				(audio-play-rpc *sfx_hooves*)))))

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
		(when (and (< rx 0) (< (get :sp_dir this) 0))
			(when (< (length *enemy_stack*) +max_enemy_stack)
				(push *enemy_stack* (get :sp_type this))))
		(-- *enemy_count*)
		(def this :sp_death :nil)
		(. this :sp_kill))
	this)

(defun cp-mine (this that)
	; offscreen mine cleanup without death explosion
	(bind '(rx ry rw rh) (. *world_scroll* :get_relative this))
	(bind '(ww wh) (. *world_scroll* :get_size))
	(when (or (< rx (neg rw)) (>= rx ww) (< ry (neg rh)) (>= ry wh))
		(-- *mine_count*)
		(def this :sp_death :nil)
		(. this :sp_kill)))

(defun cp-item (this that)
	; item physics: fall until hitting ground, then start timeout countdown
	(defq mv (elem-get (get :sp_components this) 0))
	(if (= (get 'ay mv) 0)
		(if (<= (defq timeout (dec (get :item_timeout this))) 0)
			(. this :sp_kill)
			(def this :item_timeout timeout))
		(bind '(x y w h) (. this :sp_get_bounds))
		(defq vy (get 'vy mv)
			grid_y (logand (+ y h) (neg +tile_height))
			prev_y (get :item_prev_y this))
		(def this :item_prev_y grid_y)
		(when (and (> vy 0) (nql grid_y prev_y))
			(defq flags (. *layer_land* :get_tile_flags (+ x (/ w 2)) grid_y))
			(when (and (bits? flags +fmap_stand) (not (bits? flags +fmap_climb)))
				(. this :sp_set_pos x (- grid_y h))
				(set mv 'vy 0 'ay 0)
				(dt-fizz-fx this))))
	this)

;;;;;;;;;;;;;;;;;;;;;;;;
; common sprite classes
;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Body (canvas_l canvas_r w h tag frame dir vx vy) (Sprite canvas_l canvas_r w h tag)
	; (Body ...) -> dead flying body sprite
	(def this :sp_dir dir)
	(.-> this
		(:sp_add_component (MV vx vy 0 1 100 100))
		(:sp_add_component (CP offscreen-update))
		(:sp_set_frame frame)))

(defq +at_blood ''(35 36 37 38 -1)
	+at_mine `'(,+frm_16x16_mine ,(+ +frm_16x16_mine 1)))

(defclass Blood (x y dir) (Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr)
	; (Blood x y dir) -> directional blood spray sprite
	(def this :sp_dir dir)
	(.-> this
		(:sp_add_component (MV (* 4 dir) -4 0 1 100 100))
		(:sp_add_component (AT 2 +at_blood :nil))
		(:sp_add_component (CP offscreen-update))
		(:sp_set_pos x y)
		(:sp_set_frame 35)))

(defclass Missile (frame &optional vx vy) (Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr)
	(setd vx 0 vy 0)
	(def this :sp_type +ftp_enemy_fire :sp_dir (if (>= vx 0) 1 -1))
	(.-> this
		(:sp_add_component (MV vx vy 0 0))
		(:sp_add_component (CP offscreen-update))
		(:sp_set_frame frame)))

(defclass Mine () (Sprite *img_frm_16x16* :nil 16 8 :16x16 dt-mine-explode)
	; (Mine) -> ground mine sprite with 16-pixel texture cell height
	(def this :sp_type +ftp_enemy_fire :sp_hitpnt 150 :sp_frame_h 16)
	(.-> this
		(:sp_add_component (AT 2 +at_mine))
		(:sp_add_component (CP cp-mine))
		(:sp_set_frame +frm_16x16_mine)))

(defclass Item (frame uses timeout &optional vy) (Sprite *img_frm_16x16* :nil 16 16 :16x16 dt-fizz-fx)
	; (Item frame uses timeout [vy]) -> collectible item on map
	(def this :sp_type +ftp_item :sp_hitpnt uses :item_timeout timeout :item_prev_y -1)
	(.-> this
		(:sp_add_component (MV 0 (or vy -9) 0 1 0 10))
		(:sp_add_component (CP cp-item))
		(:sp_add_component (CP offscreen-nodeath-update))
		(:sp_set_frame frame)))

(defun cp-banner (this that)
	; spawn fizz spark fx cloud around banner head matching C++ cp_banner
	(bind '(x y) (. this :sp_get_pos))
	(defq fx (. (Fizz) :sp_set_pos (+ x (- (game-random 32) 16)) (+ y (- (game-random 32) 16))))
	(. *layer_fx* :add_front fx))

(defclass Banner (type frame) (Sprite *img_frm_16x16* :nil 16 48 :16x16 dt-fizz-fx)
	; (Banner type frame) -> 16x48 banner sprite (flag top, pole middle & bottom)
	(def this :sp_type type :sp_frame frame :sp_frame_h 16)
	(.-> this
		(:sp_add_component (CP cp-banner))))

;;;;;;;;;;;;;;;;;;;;;;;
; enemy factory routine
;;;;;;;;;;;;;;;;;;;;;;;

(defun create-enemy-type (type)
	; factory function mapping enum.inc type bit to enemy class
	(case type
		(+ftp_horse (Horse))
		(+ftp_spearman (Spearman))
		(+ftp_wizard (Wizard))
		(+ftp_footman (Footman))
		(+ftp_knight (Knight))
		(+ftp_balista (Balista))
		(+ftp_cannon (Cannon))
		(+ftp_oil (Oil))
		(+ftp_boarrider (BoarRider))
		(+ftp_carpet (Carpet))
		(+ftp_tower (Tower))
		(+ftp_monk (Monk))
		(+ftp_beserk (Beserk))
		(+ftp_skeleton_monk (SkeletonMonk))
		(+ftp_skeleton_horse (SkeletonHorse))
		(+ftp_skeleton (Skeleton))
		(:t (Footman))))

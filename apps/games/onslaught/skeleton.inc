;;;;;;;;;;;;;;;;;;;;;;;
; skeleton enemy entity
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and animation tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +at_skeleton `'(,+frm_fanatic_l_skeleton1 ,+frm_fanatic_l_skeleton2
		,+frm_fanatic_l_skeleton3 ,+frm_fanatic_l_skeleton4
		,+frm_fanatic_l_skeleton5 ,+frm_fanatic_l_skeleton6
		,+frm_fanatic_l_skeleton7 ,+frm_fanatic_l_skeleton)
	+at_skeleton_upper `'(,+frm_fanatic_l_skeleton8 ,+frm_fanatic_l_skeleton9
		,+frm_fanatic_l_skeleton10 ,+frm_fanatic_l_skeleton9)
	+at_skeleton_lower `'(,+frm_fanatic_l_skeleton12 ,+frm_fanatic_l_skeleton13
		,+frm_fanatic_l_skeleton14 ,+frm_fanatic_l_skeleton13)
	+at_skeleton_upper_mase `'(,+frm_16x16_l_skeladd ,+frm_16x16_l_skeladd ,+frm_16x16_l_skeladd1 -1)
	+at_skeleton_lower_mase `'(,+frm_16x16_l_skeladd2 ,+frm_16x16_l_skeladd2 ,+frm_16x16_l_skeladd3 -1)
	+at_bone `'(,+frm_16x16_bones ,+frm_16x16_bones1 ,+frm_16x16_bones2)
	+bone_vectors ''((1 -8) (-1 -8) (2 -6) (-2 -6))
	+mt_skeleton_mase_l ''((32 0) (32 0) (0 0) (0 0) (-16 0) (-16 0))
	+mt_skeleton_mase_r ''((-16 0) (-16 0) (16 0) (16 0) (32 0) (32 0)))

;;;;;;;;;;;;;;;;;;;;
; component handlers
;;;;;;;;;;;;;;;;;;;;

(defun cp-bone (this that)
	; bone lifespan countdown and bounce against floor
	(defq life (dec (get :life this)))
	(if (<= life 0)
		(. this :sp_kill)
		(def this :life life)
		(bind '(x y w h) (. this :sp_get_bounds))
		(defq mv (elem-get (get :sp_components this) +comp_move) vy (get 'vy mv)
			ground_y (logand (+ y h vy) (neg +tile_height)))
		(when (and (> vy 0)
				(bits? (. *layer_land* :get_tile_flags (+ x 8) ground_y) +fmap_stand))
			(. this :sp_set_pos x (- ground_y h))
			(set mv 'vy (neg vy))
			(spawn-fizz-fx (+ x 8) (+ y 8)))))

(defun cp-skeleton (this that)
	(defq comps (get :sp_components this)
		mv (elem-get comps +en_comp_mv)
		at_comp (elem-get comps +en_comp_at)
		flags (get :sp_flags this))
	(ifn (bits? flags +fsp_action)
		(when (bits? flags +fsp_collide)
			(def this :sp_flags (logior flags +fsp_action))
			(set mv 'vx 0)
			(defq dir (get :sp_dir this)
				upper (= (game-random 2) 0)
				body_anim (if upper +at_skeleton_upper +at_skeleton_lower)
				wep_anim (if upper +at_skeleton_upper_mase +at_skeleton_lower_mase)
				wep_mt (if (= dir 1) +mt_skeleton_mase_r +mt_skeleton_mase_l))
			(. this :sp_set_frame (first body_anim))
			(cp-enemy-duck this :nil)
			(set at_comp 'table body_anim 'speed 2 'count 0 'index 1 'loop :nil)
			(defq wep (EnemyAddon this wep_mt 2 wep_anim 8))
			(. this :add_before wep))
		(unless (get 'table at_comp)
			(def this :sp_flags (logand (get :sp_flags this) (lognot (logior +fsp_action +fsp_collide))))
			(defq dir (get :sp_dir this))
			(. this :sp_set_frame (first +at_skeleton))
			(cp-enemy-duck this :nil)
			(set at_comp 'table +at_skeleton 'speed 2 'count 0 'index 1 'loop :t)
			(set mv 'vx (* (get 'max_vx mv) dir)))))

;;;;;;;;;;;;;;;;
; death handlers
;;;;;;;;;;;;;;;;

(defun dt-kill-bones (this)
	; skeleton death: drop item and scatter bouncing bone fragments
	(-- *enemy_count*)
	(drop-item this)
	(bind '(x y) (. this :sp_get_pos))
	(defq bx (+ x 8)
		by (+ y 12))
	(each (lambda ((vx vy))
		(. *layer_fx* :add_front (Bone bx by vx vy)))
		+bone_vectors)
	(audio-play-death))

;;;;;;;;;;;;;;;;;;;;;;;
; sprite classes
;;;;;;;;;;;;;;;;;;;;;;;

(defclass Bone (x y vx vy) (Sprite *img_frm_16x16* :nil 16 16 :16x16)
	; (Bone x y vx vy) -> bouncing skeletal bone fragment
	(def this :life 20)
	(.-> this
		(:sp_add_component (MV vx vy 0 1 10 10))
		(:sp_add_component (AT 1 +at_bone))
		(:sp_add_component (CP cp-bone))
		(:sp_add_component (CP offscreen-update))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_16x16_bones)))

(defclass Skeleton () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic dt-kill-bones)
	; (Skeleton) -> skeleton foot soldier enemy sprite
	(def this :sp_type +ftp_skeleton :sp_hitpnt 5
		:fall_frame +frm_fanatic_l_skeleton11 :duck_frame +frm_fanatic_l_skeleton12)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 4 8))
		(:sp_add_component (AT 2 +at_skeleton))
		(:sp_add_component (CP cp-enemy-fall))
		(:sp_add_component (CP cp-enemy-duck))
		(:sp_add_component (CP cp-skeleton))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm_fanatic_l_skeleton)))

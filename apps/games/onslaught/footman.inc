;;;;;;;;;;;;;;;;;;;;;;;;
; footman enemy entity
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and movement tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +at_footman `'(,+frm_32x32_l_footman1 ,+frm_32x32_l_footman2 ,+frm_32x32_l_footman3
		,+frm_32x32_l_footman4 ,+frm_32x32_l_footman5 ,+frm_32x32_l_footman6
		,+frm_32x32_l_footman7 ,+frm_32x32_l_footman)
	+at_footman_upper `'(,+frm_32x32_l_footman8 ,+frm_32x32_l_footman9 ,+frm_32x32_l_footman10
		,+frm_32x32_l_footman)
	+at_footman_lower `'(,+frm_32x32_l_footman14 ,+frm_32x32_l_footman15 ,+frm_32x32_l_footman16
		,+frm_32x32_l_footman13)
	+at_footman_upper_mase `'(,+frm_16x16_l_footadd2 ,+frm_16x16_l_footadd3 ,+frm_16x16_l_footadd4 -1)
	+at_footman_lower_mase `'(,+frm_16x16_l_footadd2 ,+frm_16x16_l_footadd ,+frm_16x16_l_footadd1 -1)
	+mt_footman_upper_mase_l ''((32 0) (32 0) (8 0) (8 0) (-16 0) (-16 0))
	+mt_footman_upper_mase_r ''((-16 0) (-16 0) (8 0) (8 0) (32 0) (32 0))
	+mt_footman_lower_mase_l ''((32 -3) (32 -3) (16 0) (16 0) (-16 0) (-16 0))
	+mt_footman_lower_mase_r ''((-16 -3) (-16 -3) (0 0) (0 0) (32 0) (32 0)))

;;;;;;;;;;;;;;;;;;;;
; component handlers
;;;;;;;;;;;;;;;;;;;;

(defun cp-footman (this that)
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
				body_anim (if upper +at_footman_upper +at_footman_lower)
				wep_anim (if upper +at_footman_upper_mase +at_footman_lower_mase)
				wep_mt (if upper
					(if (= dir 1) +mt_footman_upper_mase_r +mt_footman_upper_mase_l)
					(if (= dir 1) +mt_footman_lower_mase_r +mt_footman_lower_mase_l)))
			(. this :sp_set_frame (first body_anim))
			(cp-enemy-duck this :nil)
			(set at_comp 'table body_anim 'speed 2 'count 0 'index 1 'loop :nil)
			(defq wep (EnemyAddon this wep_mt 2 wep_anim 4))
			(. this :add_before wep))
		(when (and at_comp (nil? (get 'table at_comp)))
			; attack cycle finished: clear collision state and resume walk
			(def this :sp_flags (logand (get :sp_flags this) (lognot (logior +fsp_action +fsp_collide))))
			(defq dir (get :sp_dir this))
			(. this :sp_set_frame (first +at_footman))
			(cp-enemy-duck this :nil)
			(set at_comp 'table +at_footman 'speed 2 'count 0 'index 1 'loop :t)
			(set mv 'vx (* (get 'max_vx mv) dir)))))

;;;;;;;;;;;;;;;;;;;;;;;
; footman sprite class
;;;;;;;;;;;;;;;;;;;;;;;

(defclass Footman () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr dt-kill-enemy)
	(def this :sp_type +ftp_footman :sp_hitpnt 3
		:fall_frame +frm_32x32_l_footman12 :duck_frame +frm_32x32_l_footman13
		:dead_frame +frm_32x32_l_footman11)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 4 8))
		(:sp_add_component (AT 2 +at_footman))
		(:sp_add_component (CP cp-enemy-fall))
		(:sp_add_component (CP cp-enemy-duck))
		(:sp_add_component (CP cp-footman))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm_32x32_l_footman1)))

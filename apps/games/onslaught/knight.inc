;;;;;;;;;;;;;;;;;;;;;;;
; knight enemy entity
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and movement tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +at_knight `'(,+frm_32x32_l_knight1 ,+frm_32x32_l_knight2 ,+frm_32x32_l_knight3
		,+frm_32x32_l_knight4 ,+frm_32x32_l_knight5 ,+frm_32x32_l_knight6
		,+frm_32x32_l_knight7 ,+frm_32x32_l_knight)
	+at_knight_upper `'(,+frm_32x32_l_knight8 ,+frm_32x32_l_knight9 ,+frm_32x32_l_knight10
		,+frm_32x32_l_knight)
	+at_knight_lower `'(,+frm_32x32_l_knight14 ,+frm_32x32_l_knight15 ,+frm_32x32_l_knight16
		,+frm_32x32_l_knight13)
	+at_knight_upper_mase `'(,+frm_16x16_l_knightadd3 ,+frm_16x16_l_knightadd4 ,+frm_16x16_l_knightadd5 -1)
	+at_knight_lower_mase `'(,+frm_16x16_l_knightadd ,+frm_16x16_l_knightadd1 ,+frm_16x16_l_knightadd2 -1)
	+mt_knight_upper_mase_l ''((32 0) (32 0) (8 0) (8 0) (-16 0) (-16 0))
	+mt_knight_upper_mase_r ''((-16 0) (-16 0) (8 0) (8 0) (32 0) (32 0))
	+mt_knight_mase_l ''((32 0) (32 0) (16 0) (16 0) (-16 0) (-16 0))
	+mt_knight_mase_r ''((-16 0) (-16 0) (0 0) (0 0) (32 0) (32 0)))

;;;;;;;;;;;;;;;;;;;;
; component handlers
;;;;;;;;;;;;;;;;;;;;

(defun cp-knight (this that)
	(defq comps (get :sp_components this)
		mv (elem-get comps +en_comp_mv)
		at_comp (elem-get comps +en_comp_at)
		flags (get :sp_flags this))
	(ifn (bits? flags +fsp_action)
		(when (bits? flags +fsp_collide)
			(def this :sp_flags (logior flags +fsp_action))
			(set mv 'vx 0)
			(defq dir (get :sp_dir this)
				man_duck (and *player_man* (>= (get :sp_frame *player_man*) +frm_fanatic_l_duck))
				upper (not man_duck)
				body_anim (if upper +at_knight_upper +at_knight_lower)
				wep_anim (if upper +at_knight_upper_mase +at_knight_lower_mase)
				wep_mt (if upper
					(if (= dir 1) +mt_knight_upper_mase_r +mt_knight_upper_mase_l)
					(if (= dir 1) +mt_knight_mase_r +mt_knight_mase_l)))
			(. this :sp_set_frame (first body_anim))
			(cp-enemy-duck this :nil)
			(set at_comp 'table body_anim 'speed 2 'count 0 'index 1 'loop :nil)
			(defq wep (EnemyAddon this wep_mt 2 wep_anim 8))
			(. this :add_before wep))
		(unless (get 'table at_comp)
			(def this :sp_flags (logand (get :sp_flags this) (lognot (const (logior +fsp_action +fsp_collide)))))
			(defq dir (get :sp_dir this))
			(. this :sp_set_frame (first +at_knight))
			(cp-enemy-duck this :nil)
			(set at_comp 'table +at_knight 'speed 2 'count 0 'index 1 'loop :t)
			(set mv 'vx (* (get 'max_vx mv) dir)))))

;;;;;;;;;;;;;;;;;;;;;;
; knight sprite class
;;;;;;;;;;;;;;;;;;;;;;

(defclass Knight () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr dt-kill-enemy)
	(def this :sp_type +ftp_knight :sp_hitpnt 5
		:fall_frame +frm_32x32_l_knight12 :duck_frame +frm_32x32_l_knight13
		:dead_frame +frm_32x32_l_knight11)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 4 8))
		(:sp_add_component (AT 2 +at_knight))
		(:sp_add_component (CP cp-enemy-fall))
		(:sp_add_component (CP cp-enemy-duck))
		(:sp_add_component (CP cp-knight))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm_32x32_l_knight1)))

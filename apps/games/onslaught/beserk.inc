;;;;;;;;;;;;;;;;;;;;;
; beserk enemy entity
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and animation tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	+at_beserk
	`'(,+frm_32c32_l_beserk ,+frm_32c32_l_beserk1 ,+frm_32c32_l_beserk2
		,+frm_32c32_l_beserk3 ,+frm_32c32_l_beserk4 ,+frm_32c32_l_beserk5)
	+at_beserk_upper_mase
	`'(,+frm_16x16_l_bkaddon ,+frm_16x16_l_bkaddon1 ,+frm_16x16_l_bkaddon2 -1)
	+at_beserk_lower_mase
	`'(,+frm_16x16_l_bkaddon3 ,+frm_16x16_l_bkaddon4 ,+frm_16x16_l_bkaddon5 -1)
	+mt_beserk_upper_mase_l
	''((32 0) (32 0) (32 0) (8 0) (8 0) (8 0) (-16 0) (-16 0) (-16 0))
	+mt_beserk_upper_mase_r
	''((-16 0) (-16 0) (-16 0) (8 0) (8 0) (8 0) (32 0) (32 0) (32 0))
	+mt_beserk_lower_mase_l
	''((32 -8) (32 -8) (32 -8) (8 0) (8 0) (8 0) (-16 0) (-16 0) (-16 0))
	+mt_beserk_lower_mase_r
	''((-16 -8) (-16 -8) (-16 -8) (8 0) (8 0) (8 0) (32 0) (32 0) (32 0)))

;;;;;;;;;;;;;;;;;;;;
; component handlers
;;;;;;;;;;;;;;;;;;;;

(defun cp-beserk (this that)
	; trigger upper or lower mase swing addon on frame transition
	(defq f (get :sp_frame this)
		lower (>= f +frm_32c32_l_beserk3)
		current_swing (if lower :lower :upper)
		last_swing (get :beserk_swing this))
	(when (nql current_swing last_swing)
		(def this :beserk_swing current_swing)
		(defq dir (get :sp_dir this)
			wep_anim (if lower +at_beserk_lower_mase +at_beserk_upper_mase)
			wep_mt (if lower
				(if (= dir 1) +mt_beserk_lower_mase_r +mt_beserk_lower_mase_l)
				(if (= dir 1) +mt_beserk_upper_mase_r +mt_beserk_upper_mase_l))
			wep (EnemyAddon this wep_mt 3 wep_anim 8))
		(. this :add_before wep)))

;;;;;;;;;;;;;;;;;;;;;
; beserk sprite class
;;;;;;;;;;;;;;;;;;;;;

(defclass Beserk () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr dt-kill-zap)
	; (Beserk) -> stationary berserker enemy sprite
	(def this :sp_type +ftp_beserk :sp_hitpnt 20
		:fall_frame +frm_32c32_l_beserk2
		:duck_frame +frm_32c32_l_beserk3
		:beserk_swing :nil)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 0 0))
		(:sp_add_component (AT 3 +at_beserk))
		(:sp_add_component (CP cp-enemy-duck))
		(:sp_add_component (CP cp-beserk))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm_32c32_l_beserk)))

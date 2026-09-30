;;;;;;;;;;;;;;;;;;;;;;
; wizard enemy entity
;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and animation tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	+at_wizard
	`'(,+frm_32x32_l_wizard4 ,+frm_32x32_l_wizard5 ,+frm_32x32_l_wizard
		,+frm_32x32_l_wizard1 ,+frm_32x32_l_wizard2 ,+frm_32x32_l_wizard3)
	+at_wizard_throw
	`'(,+frm_32x32_l_wizard6 ,+frm_32x32_l_wizard7 ,+frm_32x32_l_wizard8
		,+frm_32x32_l_wizard7)
	+at_wizard_shot_strength
	`'(,+frm_16x16_wizshot ,(+ +frm_16x16_wizshot 1))
	+at_wizard_shot_power
	`'(,(+ +frm_16x16_wizshot 2) ,(+ +frm_16x16_wizshot 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; wizard and shot sprite classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass WizardShot (x y dir frame) (Sprite *img_frm_16x16* :nil 16 16 :16x16 dt-fizz-fx)
	; (WizardShot x y dir frame) -> wizard magic projectile with fizz sparks on death
	(def this :sp_type +ftp_enemy_fire :sp_dir dir :sp_hitpnt 16)
	(.-> this
		(:sp_add_component (MV (* 8 dir) 0 0 0 8 0))
		(:sp_add_component (AT 1 (list frame (+ frame 1))))
		(:sp_add_component (CP offscreen-nodeath-update))
		(:sp_set_pos x y)
		(:sp_set_frame frame)))

(defclass Wizard () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr dt-kill-enemy)
	(def this :sp_type +ftp_wizard :bftp_type 2 :sp_hitpnt 2
		:fall_frame +frm_32x32_l_wizard10
		:duck_frame +frm_32x32_l_wizard11
		:dead_frame +frm_32x32_l_wizard9)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 8))
		(:sp_add_component (AT 2 +at_wizard))
		(:sp_add_component (CP cp-enemy-fall))
		(:sp_add_component (CP cp-enemy-duck))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm_32x32_l_wizard4)))

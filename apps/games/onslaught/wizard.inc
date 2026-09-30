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

(defclass WizardShot (x y dir &optional at_table) (Sprite *img_frm_16x16* :nil 16 16 :16x16 dt-fizz-fx)
	; (WizardShot x y dir [at_table]) -> wizard magic projectile with fizz sparks on death
	(defq table (if at_table at_table (if (= *enemy_wizshot* 0) +at_wizard_shot_power +at_wizard_shot_strength))
		frame (first table))
	(def this :sp_type +ftp_enemy_fire :sp_dir dir :sp_hitpnt 16)
	(.-> this
		(:sp_add_component (MV (* 8 dir) 0 0 0 8 0))
		(:sp_add_component (AT 1 table))
		(:sp_add_component (CP offscreen-nodeath-update))
		(:sp_set_pos x y)
		(:sp_set_frame frame)))

;;;;;;;;;;;;;;;;;;;;
; component handlers
;;;;;;;;;;;;;;;;;;;;

(defun cp-wizard (this that)
	; periodic magic shot logic matching cpp cp_wizard
	(defq comps (get :sp_components this)
		mv (elem-get comps +en_comp_mv)
		at_comp (elem-get comps +en_comp_at))
	(when (and mv (= (get :ay mv) 0))
		(ifn (get :wizard_shooting this)
			(progn
				(defq cnt (dec (get :wizard_timer this)))
				(if (<= cnt 0)
					(progn
						(def this :wizard_shooting :t :wizard_shot :nil)
						(set at_comp :table +at_wizard_throw :speed 2 :count 0 :index 0)
						(. this :sp_set_frame (first +at_wizard_throw))
						(set mv :vx 0))
					(def this :wizard_timer cnt)))
			(defq idx (get :index at_comp))
			(when (and (not (get :wizard_shot this)) (= idx 2) (= (get :count at_comp) 0))
				(def this :wizard_shot :t)
				(bind '(x y) (. this :sp_get_pos))
				(defq dir (get :sp_dir this)
					sx (if (= dir 1) (+ x 16) x)
					shot (WizardShot sx y dir))
				(. *layer_missiles* :add_front shot))
			(when (and (get :wizard_shot this) (= idx 0) (= (get :count at_comp) 0))
				(def this :wizard_shooting :nil :wizard_shot :nil
					:wizard_timer (+ (game-random 16) 16))
				(set at_comp :table +at_wizard :speed 1 :count 0 :index 0)
				(. this :sp_set_frame (first +at_wizard))
				(set mv :vx (* (get :sp_dir this) (get :max_vx mv)))))))

;;;;;;;;;;;;;;;;;;;;;
; wizard sprite class
;;;;;;;;;;;;;;;;;;;;;

(defclass Wizard () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr dt-kill-enemy)
	(def this :sp_type +ftp_wizard :sp_hitpnt 2
		:fall_frame +frm_32x32_l_wizard10
		:duck_frame +frm_32x32_l_wizard11
		:dead_frame +frm_32x32_l_wizard9
		:wizard_timer (+ (game-random 16) 16)
		:wizard_shooting :nil
		:wizard_shot :nil)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 8))
		(:sp_add_component (AT 2 +at_wizard))
		(:sp_add_component (CP (const cp-enemy-fall)))
		(:sp_add_component (CP (const cp-enemy-duck)))
		(:sp_add_component (CP (const cp-wizard)))
		(:sp_add_component (CP (const cp-enemy-offscreen)))
		(:sp_set_frame +frm_32x32_l_wizard4)))

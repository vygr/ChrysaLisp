;;;;;;;;;;;;;;;;;;;;;;;;
; spearman enemy entity
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and animation tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	+at_spearman
	`'(,+frm_32x32_l_spearman4 ,+frm_32x32_l_spearman5 ,+frm_32x32_l_spearman
		,+frm_32x32_l_spearman1 ,+frm_32x32_l_spearman2 ,+frm_32x32_l_spearman3)
	+at_spearman_throw
	`'(,+frm_32x32_l_spearman6 ,+frm_32x32_l_spearman7 ,+frm_32x32_l_spearman8
		,+frm_32x32_l_spearman7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; spearman and missile sprite classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Spear (x y dir) (Sprite *img_frm_32x16_l* *img_frm_32x16_r* 32 16 :32x16_lr)
	; (Spear x y dir) -> thrown spear missile sprite
	(def this :sp_type +ftp_enemy_fire :sp_dir dir :sp_hitpnt 4)
	(.-> this
		(:sp_add_component (MV (* 12 dir) 0 0 0 12 0))
		(:sp_add_component (CP offscreen-nodeath-update))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_32x16_l_spear)))

;;;;;;;;;;;;;;;;;;;;
; component handlers
;;;;;;;;;;;;;;;;;;;;

(defun cp-spearman (this that)
	; periodic spear throwing logic matching cpp cp_spearman
	(defq comps (get :sp_components this)
		mv (elem-get comps +en_comp_mv)
		at_comp (elem-get comps +en_comp_at))
	(when (and mv (= (get :ay mv) 0))
		(ifn (get :spear_throwing this)
			(progn
				(defq cnt (dec (get :spear_timer this)))
				(if (<= cnt 0)
					(progn
						(def this :spear_throwing :t :spear_thrown :nil)
						(set at_comp :table +at_spearman_throw :speed 2 :count 0 :index 0)
						(. this :sp_set_frame (first +at_spearman_throw))
						(set mv :vx 0))
					(def this :spear_timer cnt)))
			(progn
				(defq idx (get :index at_comp))
				(when (and (not (get :spear_thrown this)) (= idx 2) (= (get :count at_comp) 0))
					(def this :spear_thrown :t)
					(bind '(x y) (. this :sp_get_pos))
					(defq dir (get :sp_dir this)
						spear (Spear (+ x (* dir 16)) y dir))
					(. *layer_missiles* :add_front spear))
				(when (and (get :spear_thrown this) (= idx 0) (= (get :count at_comp) 0))
					(def this :spear_throwing :nil :spear_thrown :nil)
					(def this :spear_timer (+ (game-random 8) 16))
					(defq dir (get :sp_dir this))
					(set at_comp :table +at_spearman :speed 2 :count 0 :index 0)
					(. this :sp_set_frame (first +at_spearman))
					(set mv :vx (* (get :max_vx mv) dir))))))
	this)

;;;;;;;;;;;;;;;;;;;;;;;
; spearman sprite class
;;;;;;;;;;;;;;;;;;;;;;;

(defclass Spearman () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr dt-kill-enemy)
	(def this :sp_type +ftp_spearman :sp_hitpnt 1
		:spear_timer (+ (game-random 8) 16) :spear_throwing :nil :spear_thrown :nil
		:fall_frame +frm_32x32_l_spearman10
		:duck_frame +frm_32x32_l_spearman11
		:dead_frame +frm_32x32_l_spearman9)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 8))
		(:sp_add_component (AT 2 +at_spearman))
		(:sp_add_component (CP cp-enemy-fall))
		(:sp_add_component (CP cp-enemy-duck))
		(:sp_add_component (CP cp-spearman))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm_32x32_l_spearman4)))

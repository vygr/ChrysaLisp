;;;;;;;;;;;;;;;;;;;;;;
; cannon enemy entity
;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and animation tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	+at_cannon
	`'(,+frm_32c32_l_cannon ,+frm_32c32_l_cannon ,+frm_32c32_l_cannon ,+frm_32c32_l_cannon
		,+frm_32c32_l_cannon ,+frm_32c32_l_cannon1 ,+frm_32c32_l_cannon1 ,+frm_32c32_l_cannon2)
	+at_cannon_flame
	`'(,+frm_16x16_l_flame ,+frm_16x16_l_flame ,+frm_16x16_l_flame1 -1))

;;;;;;;;;;;;;;;;;;;;
; component handlers
;;;;;;;;;;;;;;;;;;;;

(defun cp-cannon (this that)
	; fire cannonball and spawn muzzle flame at bottom half on firing frame
	(defq f (get :sp_frame this))
	(if (= f +frm_32c32_l_cannon1)
		(unless (get :cannon_fired this)
			(def this :cannon_fired :t)
			(bind '(cx cy) (. this :sp_get_pos))
			(defq dir (get :sp_dir this)
				flame (CannonFlame this)
				bx (if (= dir 1) (+ cx 32) (- cx 16))
				by (+ cy 16)
				ball (CannonBall bx by dir))
			(. this :add_before flame)
			(. *layer_missiles* :add_front ball)
			(audio-play-rpc *sfx_explode*))
		(def this :cannon_fired :nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cannon, ball, and flame addon sprite classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass CannonFlame (cannon) (Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr)
	; (CannonFlame cannon) -> flame addon sprite at bottom half
	(bind '(cx cy) (. cannon :sp_get_pos))
	(defq dir (get :sp_dir cannon)
		fx (if (= dir 1) (+ cx 32) (- cx 16))
		fy (+ cy 16))
	(def this :sp_type 0 :sp_dir dir)
	(.-> this
		(:sp_add_component (AT 4 +at_cannon_flame :nil))
		(:sp_add_component (CP offscreen-update))
		(:sp_set_pos fx fy)
		(:sp_set_frame +frm_16x16_l_flame)))

(defclass CannonBall (x y dir) (Sprite *img_frm_16x16* :nil 16 16 :16x16 dt-exp-fx)
	; (CannonBall x y dir) -> cannonball missile sprite with explosion on death
	(def this :sp_type +ftp_enemy_fire :sp_dir dir :sp_hitpnt 24)
	(.-> this
		(:sp_add_component (MV (* 12 dir) 0 0 0 12 0))
		(:sp_add_component (CP offscreen-nodeath-update))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_16x16_cball)))

(defclass Cannon () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr dt-kill-balista)
	; (Cannon) -> cannon artillery enemy sprite
	(def this :sp_type +ftp_cannon :sp_hitpnt 4
		:dead_frame +frm_32c32_l_cannon3
		:cannon_fired :nil)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 0 0))
		(:sp_add_component (AT 4 +at_cannon))
		(:sp_add_component (CP cp-cannon))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm_32c32_l_cannon)))

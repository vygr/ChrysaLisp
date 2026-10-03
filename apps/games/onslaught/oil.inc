;;;;;;;;;;;;;;;;;;;
; oil enemy entity
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and animation tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +at_oil `'(,+frm_32c32_l_oil ,+frm_32c32_l_oil1 ,+frm_32c32_l_oil
		,+frm_32c32_l_oil1 ,+frm_32c32_l_oil2 ,+frm_32c32_l_oil1))

;;;;;;;;;;;;;;;;;;;;
; component handlers
;;;;;;;;;;;;;;;;;;;;

(defun cp-oil (this that)
	; drop boiling oil on index 5 matching cpp cp_oil
	(defq comps (get :sp_components this)
		at_comp (elem-get comps +en_comp_at))
	(when (and (= (get 'index at_comp) 5) (= (get 'count at_comp) 0))
		(bind '(cx cy) (. this :sp_get_pos))
		(defq drop (OilDrop (+ cx 8) (+ cy 16)))
		(. *layer_missiles* :add_front drop)
		(audio-play-rpc *sfx_spell*))
	this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; oil and drop sprite classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass OilDrop (x y) (Sprite *img_frm_16x32* :nil 16 32 :16x32 dt-exp-fx)
	; (OilDrop x y) -> falling boiling oil projectile with explosion on death
	(def this :sp_type +ftp_enemy_fire :sp_hitpnt 48)
	(.-> this
		(:sp_add_component (MV 0 0 0 1 0 10))
		(:sp_add_component (CP offscreen-nodeath-update))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_16x32_oilshot)))

(defclass Oil () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr dt-kill-balista)
	; (Oil) -> boiling oil cauldron enemy sprite
	(def this :sp_type +ftp_oil :sp_hitpnt 4 :dead_frame +frm_32c32_l_oil3)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 0 0))
		(:sp_add_component (AT 4 +at_oil))
		(:sp_add_component (CP cp-oil))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm_32c32_l_oil)))

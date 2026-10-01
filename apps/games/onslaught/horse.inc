;;;;;;;;;;;;;;;;;;;;;
; horse enemy entity
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and animation tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +at_horse `'(,+frm_64x64_l_horse ,+frm_64x64_l_horse1 ,+frm_64x64_l_horse2 ,+frm_64x64_l_horse3))

;;;;;;;;;;;;;;;;
; death handlers
;;;;;;;;;;;;;;;;

(defun dt-kill-horse (this)
	; horse death: rider knight dismounts on ground level, horse body flies away
	(bind '(x y) (. this :sp_get_pos))
	(defq dir (get :sp_dir this)
		knight (Knight)
		mv (elem-get (get :sp_components knight) +en_comp_mv)
		ky (logand (+ y 32) (neg +tile_height)))
	(def knight :sp_dir dir)
	(when mv (set mv :vx (* (get :max_vx mv) dir)))
	(. knight :sp_set_pos (+ x 16) ky)
	(. *layer_enemies* :add_front knight)
	(spawn-flying-body this)
	(audio-play-rpc *sfx_horse*))

;;;;;;;;;;;;;;;;;;;
; horse sprite class
;;;;;;;;;;;;;;;;;;;

(defclass Horse () (Sprite *img_frm_64x64_l* *img_frm_64x64_r* 64 64 :64x64_lr dt-kill-horse)
	(def this :sp_type +ftp_horse :sp_hitpnt 6
		:fall_frame +frm_64x64_l_horse4 :dead_frame +frm_64x64_l_horse5)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 8 8))
		(:sp_add_component (AT 2 +at_horse))
		(:sp_add_component (CP cp-enemy-fall))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm_64x64_l_horse)))

;;;;;;;;;;;;;;;;;;;;;;;;;
; boarrider enemy entity
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and animation tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +at_boarrider `'(,+frm64x32_l_boarrider ,+frm64x32_l_boarrider1 ,+frm64x32_l_boarrider2 ,+frm64x32_l_boarrider3))

;;;;;;;;;;;;;;;;
; death handlers
;;;;;;;;;;;;;;;;

(defun dt-kill-boarrider (this)
	; boarrider death: spearman dismounts on ground level, boar body flies away
	(bind '(x y) (. this :sp_get_pos))
	(defq dir (get :sp_dir this)
		spearman (Spearman)
		mv (elem-get (get :sp_components spearman) +en_comp_mv)
		sy (logand y (neg +tile_height)))
	(def spearman :sp_dir dir)
	(when mv (set mv 'vx (* (get 'max_vx mv) dir)))
	(. spearman :sp_set_pos (+ x 16) sy)
	(. *layer_enemies* :add_front spearman)
	(spawn-flying-body this)
	(audio-play-rpc *sfx_boar*))

;;;;;;;;;;;;;;;;;;;;;;;;
; boarrider sprite class
;;;;;;;;;;;;;;;;;;;;;;;;

(defclass BoarRider () (Sprite *img_frm_64x32_l* *img_frm_64x32_r* 64 32 :64x32_lr dt-kill-boarrider)
	(def this :sp_type +ftp_boarrider :sp_hitpnt 2
		:fall_frame +frm64x32_l_boarrider4 :dead_frame +frm64x32_l_boarrider5)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 10))
		(:sp_add_component (AT 2 +at_boarrider))
		(:sp_add_component (CP cp-enemy-fall))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm64x32_l_boarrider)))

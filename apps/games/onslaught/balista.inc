;;;;;;;;;;;;;;;;;;;;;;;
; balista enemy entity
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and animation tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	+at_balista
	`'(,+frm_32c32_l_balista ,+frm_32c32_l_balista1 ,+frm_32c32_l_balista2 ,+frm_32c32_l_balista1))

;;;;;;;;;;;;;;;;
; death handlers
;;;;;;;;;;;;;;;;

(defun dt-kill-balista (this)
	; balista, cannon, and oil death: crew emerges as footman on ground level
	(bind '(x y) (. this :sp_get_pos))
	(defq dir (or (get :sp_dir this) 1)
		footman (Footman)
		mv (elem-get (get :sp_components footman) +en_comp_mv)
		fy (logand y (neg +tile_height)))
	(def footman :sp_dir dir)
	(when mv (set mv :vx (* (get :max_vx mv) dir)))
	(. footman :sp_set_pos x fy)
	(. *layer_enemies* :add_front footman)
	(spawn-flying-body this)
	(audio-play-rpc *sfx_clash2*))

;;;;;;;;;;;;;;;;;;;;
; component handlers
;;;;;;;;;;;;;;;;;;;;

(defun cp-balista (this that)
	; fire balista arrow when bow reaches full draw (matching cpp cp_balista: index 3, count 0)
	(defq comps (get :sp_components this)
		at_comp (elem-get comps +en_comp_at))
	(when (and at_comp (= (get :index at_comp) 3) (= (get :count at_comp) 0))
		(bind '(cx cy) (. this :sp_get_pos))
		(defq dir (or (get :sp_dir this) 1)
			ax (+ cx (* dir 8))
			ay cy
			arrow (BalistaArrow ax ay dir))
		(. *layer_missiles* :add_front arrow)
		(audio-play-rpc *sfx_twang1*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; balista and arrow sprite classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass BalistaArrow (x y dir) (Sprite *img_frm_32x16_l* *img_frm_32x16_r* 32 16 :32x16_lr)
	; (BalistaArrow x y dir) -> balista arrow missile sprite
	(def this :sp_type +ftp_enemy_fire :sp_dir dir :sp_hitpnt 8)
	(.-> this
		(:sp_add_component (MV (* 16 dir) 0 0 0 16 0))
		(:sp_add_component (CP offscreen-nodeath-update))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_32x16_l_barrow)))

(defclass Balista () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr dt-kill-balista)
	; (Balista) -> balista siege engine enemy sprite
	(def this :sp_type +ftp_balista :bftp_type 5 :sp_hitpnt 4
		:dead_frame +frm_32c32_l_balista3)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 0 0))
		(:sp_add_component (AT 10 +at_balista))
		(:sp_add_component (CP cp-balista))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm_32c32_l_balista)))

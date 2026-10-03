;;;;;;;;;;;;;;;;;;;;
; monk enemy entity
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and animation tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +at_monk `'(,+frm_32c32_l_monk ,+frm_32c32_l_monk1 ,+frm_32c32_l_monk2))

;;;;;;;;;;;;;;;;
; death handlers
;;;;;;;;;;;;;;;;

(defun dt-monk-explode (this)
	; monk detonation: cluster explosion, blast area, drop item
	(setq *game_flags* (logior *game_flags* +fflag_mine))
	(-- *enemy_count*)
	(bind '(mx my) (. this :sp_get_pos))
	(dt-blat-fx this)
	(blast-area (- mx 24) (- my 24) 80 80)
	(drop-item this))

;;;;;;;;;;;;;;;;;;;
; monk sprite class
;;;;;;;;;;;;;;;;;;;

(defclass Monk () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr dt-monk-explode)
	; (Monk) -> monk suicide bomber enemy sprite
	(def this :sp_type +ftp_monk :sp_hitpnt 1 :fall_frame +frm_32c32_l_monk2)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 3 8))
		(:sp_add_component (AT 2 +at_monk))
		(:sp_add_component (CP cp-enemy-fall))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm_32c32_l_monk)))

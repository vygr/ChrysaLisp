;;;;;;;;;;;;;;;;;;;;;;
; carpet enemy entity
;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and movement tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +at_carpet `'(,+frm64x32_l_carpet ,+frm64x32_l_carpet1)
	+mt_carpet ''((0 -1) (0 -2) (0 -1) (0 0) (0 1) (0 2) (0 1) (0 0)))

;;;;;;;;;;;;;;;;
; death handlers
;;;;;;;;;;;;;;;;

(defun dt-kill-carpet (this)
	; flying carpet death: two wizards dismount and fall to ground with gravity
	(-- *enemy_count*)
	(bind '(x y) (. this :sp_get_pos))
	(defq dir (get :sp_dir this)
		wy (logand y (const (neg +tile_height))))
	(each (lambda (ox)
		(defq wiz (Wizard)
			mv (elem-get (get :sp_components wiz) +en_comp_mv))
		(def wiz :sp_dir dir)
		(set mv 'vx (* (get 'max_vx mv) dir) 'ay 1)
		(. wiz :sp_set_pos (+ x ox) wy)
		(. *layer_enemies* :add_front wiz)
		(++ *enemy_count*))
		'(0 32))
	(dt-exp-fx this))

;;;;;;;;;;;;;;;;;;;;;
; carpet sprite class
;;;;;;;;;;;;;;;;;;;;;

(defclass Carpet () (Sprite *img_frm_64x32_l* *img_frm_64x32_r* 64 32 :64x32_lr dt-kill-carpet)
	; (Carpet) -> flying carpet enemy sprite
	(def this :sp_type +ftp_carpet :sp_hitpnt 4)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 0))
		(:sp_add_component (AT 2 +at_carpet))
		(:sp_add_component (MT 1 +mt_carpet))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm64x32_l_carpet)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; skeleton horse enemy entity
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and animation tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	+at_skeleton_horse
	`'(,+frm_64x64_l_skeleton_horse ,+frm_64x64_l_skeleton_horse1
		,+frm_64x64_l_skeleton_horse2 ,+frm_64x64_l_skeleton_horse3))

;;;;;;;;;;;;;;;;
; death handlers
;;;;;;;;;;;;;;;;

(defun dt-kill-skeleton-horse (this)
	; skeleton horse death: rider skeleton dismounts on ground level, horse bursts with bones
	(bind '(x y w h) (. this :sp_get_bounds))
	(defq dir (or (get :sp_dir this) 1)
		skel (Skeleton)
		mv (elem-get (get :sp_components skel) +en_comp_mv)
		sy (logand (+ y 32) (neg +tile_height)))
	(def skel :sp_dir dir)
	(when mv (set mv :vx (* (get :max_vx mv) dir)))
	(. skel :sp_set_pos (+ x 16) sy)
	(. *layer_enemies* :add_front skel)
	(defq bx (+ x 24) by (+ y 28))
	(each (lambda ((vx vy))
		(defq bone (. (Bone bx by vx vy) :sp_set_pos bx by))
		(. *layer_fx* :add_front bone))
		+bone_vectors)
	(audio-play-rpc *sfx_spell*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; skeleton horse sprite class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass SkeletonHorse () (Sprite *img_frm_64x64_l* *img_frm_64x64_r* 64 64 :64x64_lr dt-kill-skeleton-horse)
	; (SkeletonHorse) -> undead skeleton horse enemy sprite
	(def this :sp_type +ftp_skeleton_horse :bftp_type 14 :sp_hitpnt 6 :fall_frame 10)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 8 8))
		(:sp_add_component (AT 2 +at_skeleton_horse))
		(:sp_add_component (CP cp-enemy-fall))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm_64x64_l_skeleton_horse)))

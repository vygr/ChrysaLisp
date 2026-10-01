;;;;;;;;;;;;;;;;;;;;;;;;;;
; skeleton monk enemy entity
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and animation tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +at_skeleton_monk `'(,+frm_fanatic_l_skeleton_monk
		,+frm_fanatic_l_skeleton_monk1 ,+frm_fanatic_l_skeleton_monk2)
	+at_skeleton_item `'(,+frm_16x16_bones3 ,+frm_16x16_bones4))

;;;;;;;;;;;;;;;;
; death handlers
;;;;;;;;;;;;;;;;

(defun dt-skeleton-item (this)
	; skull bomb lands and spawns a skeleton enemy
	(bind '(x y) (. this :sp_get_pos))
	(defq dir (get :sp_dir this)
		skel (Skeleton)
		mv (elem-get (get :sp_components skel) +en_comp_mv))
	(def skel :sp_dir dir)
	(when mv (set mv :vx (* (get :max_vx mv) dir)))
	(. skel :sp_set_pos (- x 8) (- y 16))
	(. *layer_enemies* :add_back skel)
	(++ *enemy_count*)
	(dt-fizz-fx skel))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; skull bomb sprite class
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass SkullBomb (x y dir) (Sprite *img_frm_16x16* :nil 16 16 :16x16 dt-skeleton-item)
	; (SkullBomb x y dir) -> skull projectile that turns into a Skeleton when landing
	(def this :sp_dir dir :item_timeout 24 :item_prev_y -1)
	(.-> this
		(:sp_add_component (MV 0 -9 0 1 0 10))
		(:sp_add_component (AT 1 +at_skeleton_item))
		(:sp_add_component (CP cp-item))
		(:sp_add_component (CP offscreen-nodeath-update))
		(:sp_set_pos x y)
		(:sp_set_frame +frm_16x16_bones3)))

;;;;;;;;;;;;;;;;;;;;
; component handlers
;;;;;;;;;;;;;;;;;;;;

(defun cp-skeleton-monk (this that)
	; skeleton monk periodically fires skull bomb
	(defq cnt (dec (get :skull_timer this)))
	(when (<= cnt 0)
		(setq cnt (+ (game-random 20) 20))
		(bind '(x y) (. this :sp_get_pos))
		(defq dir (get :sp_dir this)
			bomb (SkullBomb (+ x 8) (- y 8) dir))
		(. *layer_items* :add_back bomb)
		(dt-fizz-fx bomb))
	(def this :skull_timer cnt)
	this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; skeleton monk sprite class
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass SkeletonMonk () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic dt-kill-zap)
	; (SkeletonMonk) -> skeleton monk enemy sprite (zaps on death, shoots skull bombs)
	(def this :sp_type +ftp_skeleton_monk :sp_hitpnt 8
		:skull_timer (+ (game-random 20) 20)
		:fall_frame +frm_fanatic_l_skeleton_monk2)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 3 8))
		(:sp_add_component (AT 2 +at_skeleton_monk))
		(:sp_add_component (CP cp-enemy-fall))
		(:sp_add_component (CP cp-skeleton-monk))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm_fanatic_l_skeleton_monk)))

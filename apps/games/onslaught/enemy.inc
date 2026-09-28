;;;;;;;;;;;;;;;;;;;;;;
; enemy sprite classes
;;;;;;;;;;;;;;;;;;;;;;

(defclass Horse () (Sprite *img_frm_64x64_l* *img_frm_64x64_r* 64 64 :64x64_lr)
	; friendly cavalry horse (64x64)
	(def this :sp_type +ftp_horse)
	(.-> this
		(:sp_add_component (AT 4 (list +frm_64x64_l_horse (+ +frm_64x64_l_horse 1) (+ +frm_64x64_l_horse 2)
			(+ +frm_64x64_l_horse 3) (+ +frm_64x64_l_horse 4) (+ +frm_64x64_l_horse 5))))
		(:sp_add_component (MV 0 0 0 0 8 8))
		(:sp_set_frame +frm_64x64_l_horse)))

(defclass SkeletonHorse () (Sprite *img_frm_64x64_l* *img_frm_64x64_r* 64 64 :64x64_lr)
	; undead skeleton horse (64x64)
	(def this :sp_type +ftp_skeleton_horse)
	(.-> this
		(:sp_add_component (AT 4 (list +frm_64x64_l_skeleton_horse (+ +frm_64x64_l_skeleton_horse 1) (+ +frm_64x64_l_skeleton_horse 2)
			(+ +frm_64x64_l_skeleton_horse 3) (+ +frm_64x64_l_skeleton_horse 4) (+ +frm_64x64_l_skeleton_horse 5))))
		(:sp_add_component (MV 0 0 0 0 8 8))
		(:sp_set_frame +frm_64x64_l_skeleton_horse)))

(defclass Spearman () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; spearman infantry (32x32)
	(def this :sp_type +ftp_spearman)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 8))
		(:sp_set_frame +frm_32x32_l_spearman)))

(defclass Wizard () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; wizard caster (32x32)
	(def this :sp_type +ftp_wizard)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 8))
		(:sp_set_frame +frm_32x32_l_wizard)))

(defclass Footman () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; footman soldier (32x32)
	(def this :sp_type +ftp_footman)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 4 8))
		(:sp_set_frame +frm_32x32_l_footman)))

(defclass Knight () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; armored knight (32x32)
	(def this :sp_type +ftp_knight)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 4 8))
		(:sp_set_frame +frm_32x32_l_knight)))

(defclass BoarRider () (Sprite *img_frm_64x32_l* *img_frm_64x32_r* 64 32 :64x32_lr)
	; boar rider (64x32)
	(def this :sp_type +ftp_boarrider)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 10))
		(:sp_set_frame +frm_64x32_l_boarrider)))

(defclass Carpet () (Sprite *img_frm_64x32_l* *img_frm_64x32_r* 64 32 :64x32_lr)
	; flying magic carpet (64x32)
	(def this :sp_type +ftp_carpet)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 0))
		(:sp_add_component (MT 1 '(0 -1 0 -2 0 -1 0 0 0 1 0 2 0 1 0 0)))
		(:sp_set_frame +frm_64x32_l_carpet)))

(defclass Tower () (Sprite *img_frm_64x64* :nil 64 64 :64x64)
	; defense tower building (64x64)
	(def this :sp_type +ftp_tower)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 0))
		(:sp_set_frame +frm_64x64_tower)))

(defclass Balista () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; siege balista (32x32)
	(def this :sp_type +ftp_balista)
	(.-> this
		(:sp_add_component (AT 10 '(0 1 2 1)))
		(:sp_set_frame +frm_32c32_l_balista)))

(defclass Cannon () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; siege cannon (32x32)
	(def this :sp_type +ftp_cannon)
	(.-> this
		(:sp_add_component (AT 4 '(4 4 4 4 4 5 5 6)))
		(:sp_set_frame +frm_32c32_l_cannon)))

(defclass Oil () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; boiling oil cauldron (32x32)
	(def this :sp_type +ftp_oil)
	(.-> this
		(:sp_add_component (AT 4 '(8 9 8 9 10 9)))
		(:sp_set_frame +frm_32c32_l_oil)))

(defclass Beserk () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; berserker warrior (32x32)
	(def this :sp_type +ftp_beserk)
	(.-> this
		(:sp_add_component (AT 3 '(12 13 14 15 16 17)))
		(:sp_set_frame +frm_32c32_l_beserk)))

(defclass Monk () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; holy monk (32x32)
	(def this :sp_type +ftp_monk)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 3 8))
		(:sp_add_component (AT 1 '(18 19 20)))
		(:sp_set_frame +frm_32c32_l_monk)))

(defclass Skeleton () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic)
	; undead skeleton warrior (32x32)
	(def this :sp_type +ftp_skeleton)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 4 8))
		(:sp_add_component (AT 1 '(29 30 31 32 33 34 35 28)))
		(:sp_set_frame +frm_fanatic_l_skeleton)))

(defclass SkeletonMonk () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic)
	; undead skeleton monk (32x32)
	(def this :sp_type +ftp_skeleton_monk)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 3 8))
		(:sp_add_component (AT 1 '(43 44 45)))
		(:sp_set_frame +frm_fanatic_skeleton_monk)))

(defclass Missile (frame &optional vx vy) (Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr)
	; (Missile frame [vx vy]) -> sprite
	(setd vx 0 vy 0)
	(def this :sp_type +ftp_enemy_fire :sp_dir (if (>= vx 0) 1 -1))
	(.-> this
		(:sp_add_component (MV vx vy 0 0))
		(:sp_add_component (CP (const offscreen-update)))
		(:sp_set_frame frame)))

(defclass Item (frame) (Sprite *img_frm_16x16* :nil 16 16 :16x16)
	; collectible item / pickup (16x16)
	(def this :sp_type +ftp_item)
	(. this :sp_set_frame frame));;;;;;;;;;;;;;;;;;;;;;
; enemy sprite classes
;;;;;;;;;;;;;;;;;;;;;;

(defclass Horse () (Sprite *img_frm_64x64_l* *img_frm_64x64_r* 64 64 :64x64_lr)
	; friendly cavalry horse (64x64)
	(def this :sp_type +ftp_horse)
	(.-> this
		(:sp_add_component (AT 4 (list +frm_64x64_l_horse (+ +frm_64x64_l_horse 1) (+ +frm_64x64_l_horse 2)
			(+ +frm_64x64_l_horse 3) (+ +frm_64x64_l_horse 4) (+ +frm_64x64_l_horse 5))))
		(:sp_add_component (MV 0 0 0 0 8 8))
		(:sp_set_frame +frm_64x64_l_horse)))

(defclass SkeletonHorse () (Sprite *img_frm_64x64_l* *img_frm_64x64_r* 64 64 :64x64_lr)
	; undead skeleton horse (64x64)
	(def this :sp_type +ftp_skeleton_horse)
	(.-> this
		(:sp_add_component (AT 4 (list +frm_64x64_l_skeleton_horse (+ +frm_64x64_l_skeleton_horse 1) (+ +frm_64x64_l_skeleton_horse 2)
			(+ +frm_64x64_l_skeleton_horse 3) (+ +frm_64x64_l_skeleton_horse 4) (+ +frm_64x64_l_skeleton_horse 5))))
		(:sp_add_component (MV 0 0 0 0 8 8))
		(:sp_set_frame +frm_64x64_l_skeleton_horse)))

(defclass Spearman () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; spearman infantry (32x32)
	(def this :sp_type +ftp_spearman)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 8))
		(:sp_set_frame +frm_32x32_l_spearman)))

(defclass Wizard () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; wizard caster (32x32)
	(def this :sp_type +ftp_wizard)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 8))
		(:sp_set_frame +frm_32x32_l_wizard)))

(defclass Footman () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; footman soldier (32x32)
	(def this :sp_type +ftp_footman)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 4 8))
		(:sp_set_frame +frm_32x32_l_footman)))

(defclass Knight () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; armored knight (32x32)
	(def this :sp_type +ftp_knight)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 4 8))
		(:sp_set_frame +frm_32x32_l_knight)))

(defclass BoarRider () (Sprite *img_frm_64x32_l* *img_frm_64x32_r* 64 32 :64x32_lr)
	; boar rider (64x32)
	(def this :sp_type +ftp_boarrider)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 10))
		(:sp_set_frame +frm_64x32_l_boarrider)))

(defclass Carpet () (Sprite *img_frm_64x32_l* *img_frm_64x32_r* 64 32 :64x32_lr)
	; flying magic carpet (64x32)
	(def this :sp_type +ftp_carpet)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 0))
		(:sp_add_component (MT 1 '(0 -1 0 -2 0 -1 0 0 0 1 0 2 0 1 0 0)))
		(:sp_set_frame +frm_64x32_l_carpet)))

(defclass Tower () (Sprite *img_frm_64x64* :nil 64 64 :64x64)
	; defense tower building (64x64)
	(def this :sp_type +ftp_tower)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 0))
		(:sp_set_frame +frm_64x64_tower)))

(defclass Balista () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; siege balista (32x32)
	(def this :sp_type +ftp_balista)
	(.-> this
		(:sp_add_component (AT 10 '(0 1 2 1)))
		(:sp_set_frame +frm_32c32_l_balista)))

(defclass Cannon () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; siege cannon (32x32)
	(def this :sp_type +ftp_cannon)
	(.-> this
		(:sp_add_component (AT 4 '(4 4 4 4 4 5 5 6)))
		(:sp_set_frame +frm_32c32_l_cannon)))

(defclass Oil () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; boiling oil cauldron (32x32)
	(def this :sp_type +ftp_oil)
	(.-> this
		(:sp_add_component (AT 4 '(8 9 8 9 10 9)))
		(:sp_set_frame +frm_32c32_l_oil)))

(defclass Beserk () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; berserker warrior (32x32)
	(def this :sp_type +ftp_beserk)
	(.-> this
		(:sp_add_component (AT 3 '(12 13 14 15 16 17)))
		(:sp_set_frame +frm_32c32_l_beserk)))

(defclass Monk () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; holy monk (32x32)
	(def this :sp_type +ftp_monk)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 3 8))
		(:sp_add_component (AT 1 '(18 19 20)))
		(:sp_set_frame +frm_32c32_l_monk)))

(defclass Skeleton () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic)
	; undead skeleton warrior (32x32)
	(def this :sp_type +ftp_skeleton)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 4 8))
		(:sp_add_component (AT 1 '(29 30 31 32 33 34 35 28)))
		(:sp_set_frame +frm_fanatic_l_skeleton)))

(defclass SkeletonMonk () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic)
	; undead skeleton monk (32x32)
	(def this :sp_type +ftp_skeleton_monk)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 3 8))
		(:sp_add_component (AT 1 '(43 44 45)))
		(:sp_set_frame +frm_fanatic_skeleton_monk)))

(defclass Missile (frame &optional vx vy) (Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr)
	; (Missile frame [vx vy]) -> sprite
	(setd vx 0 vy 0)
	(def this :sp_type +ftp_enemy_fire :sp_dir (if (>= vx 0) 1 -1))
	(.-> this
		(:sp_add_component (MV vx vy 0 0))
		(:sp_add_component (CP (const offscreen-update)))
		(:sp_set_frame frame)))

(defclass Item (frame) (Sprite *img_frm_16x16* :nil 16 16 :16x16)
	; collectible item / pickup (16x16)
	(def this :sp_type +ftp_item)
	(. this :sp_set_frame frame))
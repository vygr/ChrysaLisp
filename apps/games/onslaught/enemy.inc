;;;;;;;;;;;;;;;;;;;;;;
; enemy sprite classes
;;;;;;;;;;;;;;;;;;;;;;

(defclass Horse () (Sprite *img_frm_64x64_l* *img_frm_64x64_r* 64 64 :64x64_lr)
	; friendly cavalry horse (64x64)
	(.-> this
		(:add_component (const at-update) (const at-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_64x64_l_horse))
	(set this
		:at_speed 4
		:at_table (list +frm_64x64_l_horse (+ +frm_64x64_l_horse 1) (+ +frm_64x64_l_horse 2)
					(+ +frm_64x64_l_horse 3) (+ +frm_64x64_l_horse 4) (+ +frm_64x64_l_horse 5))))

(defclass SkeletonHorse () (Sprite *img_frm_64x64_l* *img_frm_64x64_r* 64 64 :64x64_lr)
	; undead skeleton horse (64x64)
	(.-> this
		(:add_component (const at-update) (const at-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_64x64_l_skeleton_horse))
	(set this
		:at_speed 4
		:at_table (list +frm_64x64_l_skeleton_horse (+ +frm_64x64_l_skeleton_horse 1) (+ +frm_64x64_l_skeleton_horse 2)
					(+ +frm_64x64_l_skeleton_horse 3) (+ +frm_64x64_l_skeleton_horse 4) (+ +frm_64x64_l_skeleton_horse 5))))

(defclass Spearman () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; spearman infantry (32x32)
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_spearman)))

(defclass Wizard () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; wizard caster (32x32)
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_wizard)))

(defclass Footman () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; footman soldier (32x32)
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_footman)))

(defclass Knight () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; armored knight (32x32)
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_knight)))

(defclass BoarRider () (Sprite *img_frm_64x32_l* *img_frm_64x32_r* 64 32 :64x32_lr)
	; boar rider (64x32)
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_64x32_l_boarrider)))

(defclass Carpet () (Sprite *img_frm_64x32_l* *img_frm_64x32_r* 64 32 :64x32_lr)
	; flying magic carpet (64x32)
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_64x32_l_carpet)))

(defclass Tower () (Sprite *img_frm_64x64* :nil 64 64 :64x64)
	; defense tower building (64x64)
	(. this :set_frame +frm_64x64_tower))

(defclass Balista () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; siege balista (32x32)
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_balista)))

(defclass Cannon () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; siege cannon (32x32)
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_cannon)))

(defclass Oil () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; boiling oil cauldron (32x32)
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_oil)))

(defclass Beserk () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; berserker warrior (32x32)
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_beserk)))

(defclass Monk () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; holy monk (32x32)
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_monk)))

(defclass Skeleton () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic)
	; undead skeleton warrior (32x32)
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_fanatic_l_skeleton)))

(defclass SkeletonMonk () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic)
	; undead skeleton monk (32x32)
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_fanatic_skeleton_monk)))

(defclass Missile (frame &optional vx vy) (Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr)
	; (MissileSprite frame [vx vy]) -> sprite
	(setd vx 0 vy 0)
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:add_component (const offscreen-update))
		(:set_frame frame))
	(set this :mv_vx vx :mv_vy vy :sp_dir (if (>= vx 0) 1 -1)))

(defclass Item (frame) (Sprite *img_frm_16x16* :nil 16 16 :16x16)
	; collectible item / pickup (16x16)
	(. this :set_frame frame))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/onslaught/sprite.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught sprite class & standard component assembly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/lisp.inc")
(import "./enums.inc")
(import "./assets.inc")
(import "./components.inc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; base sprite class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Sprite (canvas_l canvas_r width height tag) (View)
	; (Sprite canvas_l canvas_r width height) -> sprite
	(def this
		:color 0
		:canvas_l canvas_l
		:canvas_r canvas_r
		:canvas_tag tag
		:base_w width
		:base_h height
		:frame 0
		:dir -1
		:components (list)
		:death :nil)
	(. this :set_size (* *zoom* width) (* *zoom* height))

	(defgetmethod :canvas_l)
	(defgetmethod :canvas_r)
	(defgetmethod :canvas_tag)
	(defgetmethod :base_w)
	(defgetmethod :base_h)

	(defmethod :get_canvas ()
		; (. sprite :get_canvas) -> canvas
		(raise :canvas_l :canvas_r)
		(if (and canvas_r (>= (or (get :dir this) -1) 0)) canvas_r canvas_l))

	(defmethod :kill ()
		; (. sprite :kill) -> sprite
		(unless (= (get :frame this) -1)
			(def this :frame -1)
			(when (defq death (get :death this))
				(def this :death :nil)
				(death this))
			(. this :sub))
		this)

	(defmethod :set_frame (f)
		; (. sprite :set_frame f) -> sprite
		(when (/= f (get :frame this))
			(def this :frame f)
			(when (= f -1)
				(. this :kill)))
		this)

	(defmethod :add_component (update &optional init)
		; (. sprite :add_component update [init]) -> sprite
		(when init (init this))
		(push (get :components this) update)
		this)

	(defmethod :update ()
		; (. sprite :update) -> sprite
		(if (= (get :frame this) -1)
			(. this :kill)
			(progn
				(each (lambda (fn)
					(unless (= (get :frame this) -1)
						(fn this)))
					(get :components this))
				(when (= (get :frame this) -1)
					(. this :kill))))
		this)

	(defmethod :draw ()
		; (. sprite :draw) -> sprite
		(defq f (get :frame this))
		(when (/= f -1)
			(when (defq c (. this :get_canvas))
				(when (defq texture (if (View? c) (getf c +canvas_texture 0) c))
					(bind '(tid tw th) (texture-metrics texture))
					(bind '(w h) (. this :get_size))
					(defq
						raw_y (* f h)
						sy (% raw_y th)
						col (/ raw_y th)
						sx (* col w)
						dir (or (get :dir this) 1))
					(when (and (>= dir 0) (get :canvas_r this))
						(setq sx (- tw (+ sx w))))
					(. this :ctx_blit tid +argb_white 0 0 w h sx sy))))
		this)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; title sequence sprite subtypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass TitleLetter (idx) (Sprite *img_frm_32c32* :nil 32 32 :32c32)
	; (TitleLetter idx) -> sprite
	(def this :letter_idx idx)
	(.-> this
		(:add_component (const ml-update) (const ml-init))
		(:set_frame (+ +frm_32c32_letter_o idx)))
	(set this :ml_speed (* *zoom* 16)))

(defclass TitleSword () (Sprite *img_frm_16x64* :nil 16 64 :16x64)
	; (TitleSword) -> sprite
	(.-> this
		(:add_component (const ml-update) (const ml-init))
		(:set_frame +frm_16x64_sword))
	(set this :ml_speed (* *zoom* 14)))

(defclass TitleBloodDrip () (Sprite *img_frm_16x16* :nil 16 16 :16x16)
	; (TitleBloodDrip) -> sprite
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:add_component (const offscreen-update) (const offscreen-init))
		(:set_frame +frm_16x16_blooddrip))
	(set this
		:mv_ay *zoom* :mv_max_vy (* *zoom* 10)
		:offscreen_max_y (* *zoom* 168)))

(defclass ExplosionSprite () (Sprite *img_frm_32c32* :nil 32 32 :32c32)
	; big explosion fx (32x32)
	(.-> this
		(:add_component (const at-update) (const at-init))
		(:set_frame +frm_32c32_bigexp))
	(set this
		:at_speed 2
		:at_table (list +frm_32c32_bigexp (+ +frm_32c32_bigexp 1) (+ +frm_32c32_bigexp 2) (+ +frm_32c32_bigexp 3) (+ +frm_32c32_bigexp 4) -1)))

(defclass FizzSprite () (Sprite *img_frm_16x16* :nil 16 16 :16x16)
	; fizz spark fx (16x16)
	(.-> this
		(:add_component (const at-update) (const at-init))
		(:set_frame +frm_16x16_fizz))
	(set this
		:at_speed 2
		:at_table (list +frm_16x16_fizz (+ +frm_16x16_fizz 1) (+ +frm_16x16_fizz 2) (+ +frm_16x16_fizz 1) +frm_16x16_fizz -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; game entity sprite subtypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ManSprite () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic)
	; (ManSprite) -> sprite
	(def this :xv 0 :yv 0 :dir 1 :walk_tick 0)
	(.-> this
		(:set_frame +frm_fanatic_l_stance)))

(defclass HorseSprite () (Sprite *img_frm_64x64_l* *img_frm_64x64_r* 64 64 :64x64_lr)
	; friendly cavalry horse (64x64)
	(.-> this
		(:add_component (const at-update) (const at-init))
		(:add_component (const bounds-bounce-update) (const bounds-bounce-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_64x64_l_horse))
	(set this
		:at_speed 4
		:at_table (list +frm_64x64_l_horse (+ +frm_64x64_l_horse 1) (+ +frm_64x64_l_horse 2)
					(+ +frm_64x64_l_horse 3) (+ +frm_64x64_l_horse 4) (+ +frm_64x64_l_horse 5))))

(defclass SkeletonHorseSprite () (Sprite *img_frm_64x64_l* *img_frm_64x64_r* 64 64 :64x64_lr)
	; undead skeleton horse (64x64)
	(.-> this
		(:add_component (const at-update) (const at-init))
		(:add_component (const bounds-bounce-update) (const bounds-bounce-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_64x64_l_skeleton_horse))
	(set this
		:at_speed 4
		:at_table (list +frm_64x64_l_skeleton_horse (+ +frm_64x64_l_skeleton_horse 1) (+ +frm_64x64_l_skeleton_horse 2)
					(+ +frm_64x64_l_skeleton_horse 3) (+ +frm_64x64_l_skeleton_horse 4) (+ +frm_64x64_l_skeleton_horse 5))))

(defclass SpearmanSprite () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; spearman infantry (32x32)
	(.-> this
		(:add_component (const bounds-bounce-update) (const bounds-bounce-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_spearman)))

(defclass WizardSprite () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; wizard caster (32x32)
	(.-> this
		(:add_component (const bounds-bounce-update) (const bounds-bounce-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_wizard)))

(defclass FootmanSprite () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; footman soldier (32x32)
	(.-> this
		(:add_component (const bounds-bounce-update) (const bounds-bounce-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_footman)))

(defclass KnightSprite () (Sprite *img_frm_32x32_l* *img_frm_32x32_r* 32 32 :32x32_lr)
	; armored knight (32x32)
	(.-> this
		(:add_component (const bounds-bounce-update) (const bounds-bounce-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_knight)))

(defclass BoarRiderSprite () (Sprite *img_frm_64x32_l* *img_frm_64x32_r* 64 32 :64x32_lr)
	; boar rider (64x32)
	(.-> this
		(:add_component (const bounds-bounce-update) (const bounds-bounce-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_64x32_l_boarrider)))

(defclass CarpetSprite () (Sprite *img_frm_64x32_l* *img_frm_64x32_r* 64 32 :64x32_lr)
	; flying magic carpet (64x32)
	(.-> this
		(:add_component (const bounds-bounce-update) (const bounds-bounce-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_64x32_l_carpet)))

(defclass TowerSprite () (Sprite *img_frm_64x64* :nil 64 64 :64x64)
	; defense tower building (64x64)
	(. this :set_frame +frm_64x64_tower))

(defclass BalistaSprite () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; siege balista (32x32)
	(.-> this
		(:add_component (const bounds-bounce-update) (const bounds-bounce-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_balista)))

(defclass CannonSprite () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; siege cannon (32x32)
	(.-> this
		(:add_component (const bounds-bounce-update) (const bounds-bounce-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_cannon)))

(defclass OilSprite () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; boiling oil cauldron (32x32)
	(.-> this
		(:add_component (const bounds-bounce-update) (const bounds-bounce-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_oil)))

(defclass BeserkSprite () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; berserker warrior (32x32)
	(.-> this
		(:add_component (const bounds-bounce-update) (const bounds-bounce-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_beserk)))

(defclass MonkSprite () (Sprite *img_frm_32c32_l* *img_frm_32c32_r* 32 32 :32c32_lr)
	; holy monk (32x32)
	(.-> this
		(:add_component (const bounds-bounce-update) (const bounds-bounce-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_32x32_l_monk)))

(defclass SkeletonSprite () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic)
	; undead skeleton warrior (32x32)
	(.-> this
		(:add_component (const bounds-bounce-update) (const bounds-bounce-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_fanatic_l_skeleton)))

(defclass SkeletonMonkSprite () (Sprite *img_fanatic_l* *img_fanatic_r* 32 32 :fanatic)
	; undead skeleton monk (32x32)
	(.-> this
		(:add_component (const bounds-bounce-update) (const bounds-bounce-init))
		(:add_component (const mv-update) (const mv-init))
		(:set_frame +frm_fanatic_skeleton_monk)))

(defclass MissileSprite (frame &optional vx vy) (Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr)
	; (MissileSprite frame [vx vy]) -> sprite
	(setd vx 0 vy 0)
	(.-> this
		(:add_component (const mv-update) (const mv-init))
		(:add_component (const offscreen-update) (const offscreen-init))
		(:set_frame frame))
	(set this :mv_vx (* *zoom* vx) :mv_vy (* *zoom* vy) :dir (if (>= vx 0) 1 -1)))

(defclass ItemSprite (frame) (Sprite *img_frm_16x16* :nil 16 16 :16x16)
	; collectible item / pickup (16x16)
	(. this :set_frame frame))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Onslaught Sprite Class & Standard Component Assembly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/lisp.inc")
(import "./enums.inc")
(import "./components.inc")

(unless (def? '*app_root*)
	(defq *app_root* (path-to-file)))

(defq +canvas_flag_shared +load_flag_shared)

(defun get-cached-canvas (rel_path)
	; (get-cached-canvas rel_path) -> canvas
	(canvas-load (cat *app_root* rel_path) +load_flag_shared))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Base Sprite Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Sprite (canvas width height) (View)
	; (Sprite canvas width height) -> sprite
	(def this
		:canvas canvas :frame 0 :components (list) :death :nil)
	(. this :set_size width height)

	(defmethod :constraint ()
		; (. sprite :constraint) -> (width height)
		(. this :get_size))

	(defmethod :kill ()
		; (. sprite :kill) -> sprite
		; Triggers death hook and removes sprite from parent layer
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

	(defmethod :add_component (comp)
		; (. sprite :add_component comp) -> sprite
		(comp this :init)
		(push (get :components this) comp)
		this)

	(defmethod :update (&optional dt)
		; (. sprite :update [dt]) -> sprite
		(if (= (get :frame this) -1)
			(. this :kill)
			(progn
				(each (lambda (comp)
					(unless (= (get :frame this) -1)
						(comp this :update)))
					(get :components this))
				(when (= (get :frame this) -1)
					(. this :kill))))
		this)

	(defmethod :draw ()
		; (. sprite :draw) -> sprite
		(when (/= (get :frame this) -1)
			(when (defq c (get :canvas this))
				(when (defq texture (if (View? c) (getf c +canvas_texture 0) c))
					(bind '(tid tw th) (texture-metrics texture))
					(bind '(w h) (. this :get_size))
					(defq maxv (max 1 (/ th h))
						f (get :frame this)
						col_idx (/ f maxv)
						row_idx (% f maxv)
						sx (* col_idx w)
						sy (* row_idx h))
					; Blit with negative frame offset; View extent automatically clips
					(. this :ctx_blit tid (or (def? :color this) +argb_white) (neg sx) (neg sy) tw th))))
		this)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Title Sequence Sprite Subtypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass TitleLetter (idx) (Sprite (get-cached-canvas "image/frm_32c32.cpm") 32 32)
	; (TitleLetter idx) -> sprite
	; In C++: sp_letter uses ML (speed 16) and CP (cp_letter)
	(def this :letter_idx idx)
	(.-> this
		(:add_component (const comp-ml))
		(:add_component (const comp-code))
		(:set_frame (+ +frm_letters idx)))
	(set this :ml_speed 16))

(defclass TitleSword () (Sprite (get-cached-canvas "image/frm_16x64.cpm") 16 64)
	; (TitleSword) -> sprite
	; In C++: sp_sword uses ML (speed 14) and CP (cp_sword)
	(.-> this
		(:add_component (const comp-ml))
		(:add_component (const comp-code))
		(:set_frame +frm_sword))
	(set this :ml_speed 14))

(defclass TitleBloodDrip () (Sprite (get-cached-canvas "image/frm_16x16.cpm") 16 16)
	; (TitleBloodDrip) -> sprite
	; In C++: sp_drip uses MV (ay=1, max_vy=10), cp_offscreen (max_y=168), and dt_drip death hook
	(.-> this
		(:add_component (const comp-mv))
		(:add_component (const comp-offscreen))
		(:set_frame +frm_blooddrip))
	(set this
		:ay 1 :max_vy 10
		:offscreen_max_y 168))

(defclass ExplosionSprite () (Sprite (get-cached-canvas "image/frm_32c32.cpm") 32 32)
	; Big explosion FX (32x32) using standard Animation Table (AT)
	(.-> this
		(:add_component (const comp-at))
		(:set_frame +frm_bigexp))
	(set this
		:at_speed 2
		:at_table (list +frm_bigexp (+ +frm_bigexp 1) (+ +frm_bigexp 2) (+ +frm_bigexp 3) (+ +frm_bigexp 4) -1)))

(defclass FizzSprite () (Sprite (get-cached-canvas "image/frm_16x16.cpm") 16 16)
	; Fizz spark FX (16x16) using standard Animation Table (AT)
	(.-> this
		(:add_component (const comp-at))
		(:set_frame +frm_fizz))
	(set this
		:at_speed 2
		:at_table (list +frm_fizz (+ +frm_fizz 1) (+ +frm_fizz 2) (+ +frm_fizz 1) +frm_fizz -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Entity Sprite Subtypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ManSprite () (Sprite (get-cached-canvas "image/fanatic_l.cpm") 32 32)
	; Player / fanatic man (32x32)
	(.-> this
		(:add_component (const comp-at))
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_walk))
	(set this
		:at_speed 3
		:at_table (list +frm_walk (+ +frm_walk 1) (+ +frm_walk 2) (+ +frm_walk 3)
					(+ +frm_walk 4) (+ +frm_walk 5) (+ +frm_walk 6) (+ +frm_walk 7))))

(defclass HorseSprite () (Sprite (get-cached-canvas "image/frm_64x64_l.cpm") 64 64)
	; Friendly cavalry horse (64x64)
	(.-> this
		(:add_component (const comp-at))
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_horse))
	(set this
		:at_speed 4
		:at_table (list +frm_horse (+ +frm_horse 1) (+ +frm_horse 2)
					(+ +frm_horse 3) (+ +frm_horse 4) (+ +frm_horse 5))))

(defclass SkeletonHorseSprite () (Sprite (get-cached-canvas "image/frm_64x64_l.cpm") 64 64)
	; Undead skeleton horse (64x64)
	(.-> this
		(:add_component (const comp-at))
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_skeleton_horse))
	(set this
		:at_speed 4
		:at_table (list +frm_skeleton_horse (+ +frm_skeleton_horse 1) (+ +frm_skeleton_horse 2)
					(+ +frm_skeleton_horse 3) (+ +frm_skeleton_horse 4) (+ +frm_skeleton_horse 5))))

(defclass SpearmanSprite () (Sprite (get-cached-canvas "image/frm_32x32_l.cpm") 32 32)
	; Spearman infantry (32x32)
	(.-> this
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_spearman)))

(defclass WizardSprite () (Sprite (get-cached-canvas "image/frm_32x32_l.cpm") 32 32)
	; Wizard caster (32x32)
	(.-> this
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_wizard)))

(defclass FootmanSprite () (Sprite (get-cached-canvas "image/frm_32x32_l.cpm") 32 32)
	; Footman soldier (32x32)
	(.-> this
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_footman)))

(defclass KnightSprite () (Sprite (get-cached-canvas "image/frm_32x32_l.cpm") 32 32)
	; Armored knight (32x32)
	(.-> this
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_knight)))

(defclass BoarRiderSprite () (Sprite (get-cached-canvas "image/frm_64x32_l.cpm") 64 32)
	; Boar rider (64x32)
	(.-> this
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_boarrider)))

(defclass CarpetSprite () (Sprite (get-cached-canvas "image/frm_64x32_l.cpm") 64 32)
	; Flying magic carpet (64x32)
	(.-> this
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_carpet)))

(defclass TowerSprite () (Sprite (get-cached-canvas "image/frm_64x64.cpm") 64 64)
	; Defense tower building (64x64)
	(. this :set_frame +frm_tower))

(defclass BalistaSprite () (Sprite (get-cached-canvas "image/frm_32c32_l.cpm") 32 32)
	; Siege balista (32x32)
	(.-> this
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_balista)))

(defclass CannonSprite () (Sprite (get-cached-canvas "image/frm_32c32_l.cpm") 32 32)
	; Siege cannon (32x32)
	(.-> this
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_cannon)))

(defclass OilSprite () (Sprite (get-cached-canvas "image/frm_32c32_l.cpm") 32 32)
	; Boiling oil cauldron (32x32)
	(.-> this
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_oil)))

(defclass BeserkSprite () (Sprite (get-cached-canvas "image/frm_32c32_l.cpm") 32 32)
	; Berserker warrior (32x32)
	(.-> this
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_beserk)))

(defclass MonkSprite () (Sprite (get-cached-canvas "image/frm_32c32_l.cpm") 32 32)
	; Holy monk (32x32)
	(.-> this
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_monk)))

(defclass SkeletonSprite () (Sprite (get-cached-canvas "image/fanatic_l.cpm") 32 32)
	; Undead skeleton warrior (32x32)
	(.-> this
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_skeleton)))

(defclass SkeletonMonkSprite () (Sprite (get-cached-canvas "image/fanatic_l.cpm") 32 32)
	; Undead skeleton monk (32x32)
	(.-> this
		(:add_component (const comp-bounds-bounce))
		(:add_component (const comp-mv))
		(:set_frame +frm_skeleton_monk)))

(defclass MissileSprite (frame &optional vx vy) (Sprite (get-cached-canvas "image/frm_16x16_l.cpm") 16 16)
	; Projectile/missile (16x16)
	(setd vx 0 vy 0)
	(.-> this
		(:add_component (const comp-mv))
		(:add_component (const comp-offscreen))
		(:set_frame frame))
	(set this :vx vx :vy vy))

(defclass ItemSprite (frame) (Sprite (get-cached-canvas "image/frm_16x16.cpm") 16 16)
	; Collectible item / pickup (16x16)
	(. this :set_frame frame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Onslaught Sprite Class & Component Entity System
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

(defclass Sprite (canvas width height &optional x y) (View)
	; (Sprite canvas width height [x y]) -> sprite
	(setd x 0 y 0)
	(def this
		:canvas canvas :frame 0 :components (list))
	(. this :set_bounds x y width height)

	(defmethod :constraint ()
		; (. sprite :constraint) -> (width height)
		(. this :get_size))

	(defmethod :set_frame (f)
		; (. sprite :set_frame f) -> sprite
		(when (/= f (get :frame this))
			(def this :frame f)
			(. this :dirty))
		this)

	(defmethod :move_to (new_x new_y)
		; (. sprite :move_to new_x new_y) -> sprite
		(bind '(ox oy w h) (. this :get_bounds))
		(when (defq parent (penv this))
			(. parent :add_dirty ox oy w h))
		(. this :set_bounds new_x new_y w h)
		(. this :dirty)
		this)

	(defmethod :add_component (comp_fn &rest init_args)
		; (. sprite :add_component comp_fn [init_args...]) -> sprite
		(apply comp_fn (cat (list this :init) init_args))
		(push (get :components this) comp_fn)
		this)

	(defmethod :update (dt)
		; (. sprite :update dt) -> sprite
		(each (# (%0 this :update dt)) (get :components this))
		this)

	(defmethod :draw ()
		; (. sprite :draw) -> sprite
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
				(. this :ctx_blit tid (or (def? :color this) +argb_white) (neg sx) (neg sy) tw th)))
		this)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Title Sequence Sprite Subtypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass TitleLetter (idx &optional x y) (Sprite (get-cached-canvas "image/frm_32c32.cpm") 32 32 x y)
	; (TitleLetter idx [x y]) -> sprite
	(setd x 0 y 0)
	(def this :letter_idx idx)
	(. this :set_frame (+ +frm_letters idx))
	(. this :add_component comp-fly-to)

	(defmethod :fly_to (tx ty speed on_arrive &optional on_step)
		; (. letter :fly_to tx ty speed on_arrive [on_step]) -> letter
		(def this :target_x tx :target_y ty :speed speed :on_arrive on_arrive :on_step on_step :arrived :nil)
		this)
	)

(defclass TitleSword (&optional x y) (Sprite (get-cached-canvas "image/frm_16x64.cpm") 16 64 x y)
	; (TitleSword [x y]) -> sprite
	(setd x 152 y -64)
	(. this :set_frame +frm_sword)
	(. this :add_component comp-drop)

	(defmethod :drop_to (target_y drop_speed on_arrive)
		; (. sword :drop_to target_y drop_speed on_arrive) -> sword
		(def this :target_y target_y :drop_speed drop_speed :on_arrive on_arrive :arrived :nil)
		this)
	)

(defclass TitleBloodDrip (&optional x y) (Sprite (get-cached-canvas "image/frm_16x16.cpm") 16 16 x y)
	; (TitleBloodDrip [x y]) -> sprite
	(setd x 152 y 84)
	(. this :set_frame +frm_blooddrip)
	(. this :add_component comp-blood-drip)

	(defmethod :set_drip (start_y max_y drip_speed on_drip &optional on_finish drips_left)
		; (. drip :set_drip start_y max_y drip_speed on_drip [on_finish] [drips_left]) -> drip
		(setd drips_left 1)
		(def this
			:drip_start_y start_y
			:drip_max_y max_y
			:drip_speed drip_speed
			:on_drip on_drip
			:on_finish on_finish
			:drips_left drips_left
			:drip_done :nil)
		this)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Entity Sprite Subtypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ManSprite (&optional x y) (Sprite (get-cached-canvas "image/fanatic_l.cpm") 32 32 x y)
	; Player / fanatic man (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_walk)
	(. this :add_component comp-anim :anim_start +frm_walk :anim_count 8 :anim_speed 3)
	(. this :add_component comp-motion))

(defclass HorseSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_64x64_l.cpm") 64 64 x y)
	; Friendly cavalry horse (64x64)
	(setd x 0 y 0)
	(. this :set_frame +frm_horse)
	(. this :add_component comp-anim :anim_start +frm_horse :anim_count 6 :anim_speed 4)
	(. this :add_component comp-motion))

(defclass SkeletonHorseSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_64x64_l.cpm") 64 64 x y)
	; Undead skeleton horse (64x64)
	(setd x 0 y 0)
	(. this :set_frame +frm_skeleton_horse)
	(. this :add_component comp-anim :anim_start +frm_skeleton_horse :anim_count 6 :anim_speed 4)
	(. this :add_component comp-motion))

(defclass SpearmanSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32x32_l.cpm") 32 32 x y)
	; Spearman infantry (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_spearman)
	(. this :add_component comp-anim :anim_start +frm_spearman :anim_count 12 :anim_speed 3)
	(. this :add_component comp-motion))

(defclass WizardSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32x32_l.cpm") 32 32 x y)
	; Wizard caster (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_wizard)
	(. this :add_component comp-anim :anim_start +frm_wizard :anim_count 12 :anim_speed 3)
	(. this :add_component comp-motion))

(defclass FootmanSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32x32_l.cpm") 32 32 x y)
	; Footman soldier (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_footman)
	(. this :add_component comp-anim :anim_start +frm_footman :anim_count 16 :anim_speed 3)
	(. this :add_component comp-motion))

(defclass KnightSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32x32_l.cpm") 32 32 x y)
	; Armored knight (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_knight)
	(. this :add_component comp-anim :anim_start +frm_knight :anim_count 16 :anim_speed 3)
	(. this :add_component comp-motion))

(defclass BoarRiderSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_64x32_l.cpm") 64 32 x y)
	; Boar rider (64x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_boarrider)
	(. this :add_component comp-anim :anim_start +frm_boarrider :anim_count 6 :anim_speed 4)
	(. this :add_component comp-motion))

(defclass CarpetSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_64x32_l.cpm") 64 32 x y)
	; Flying magic carpet (64x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_carpet)
	(. this :add_component comp-anim :anim_start +frm_carpet :anim_count 6 :anim_speed 4)
	(. this :add_component comp-motion))

(defclass TowerSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_64x64.cpm") 64 64 x y)
	; Defense tower building (64x64)
	(setd x 0 y 0)
	(. this :set_frame +frm_tower))

(defclass BalistaSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32c32_l.cpm") 32 32 x y)
	; Siege balista (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_balista)
	(. this :add_component comp-anim :anim_start +frm_balista :anim_count 4 :anim_speed 4)
	(. this :add_component comp-motion))

(defclass CannonSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32c32_l.cpm") 32 32 x y)
	; Siege cannon (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_cannon)
	(. this :add_component comp-anim :anim_start +frm_cannon :anim_count 4 :anim_speed 4)
	(. this :add_component comp-motion))

(defclass OilSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32c32_l.cpm") 32 32 x y)
	; Boiling oil cauldron (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_oil)
	(. this :add_component comp-anim :anim_start +frm_oil :anim_count 4 :anim_speed 4)
	(. this :add_component comp-motion))

(defclass BeserkSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32c32_l.cpm") 32 32 x y)
	; Berserker warrior (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_beserk)
	(. this :add_component comp-anim :anim_start +frm_beserk :anim_count 6 :anim_speed 3)
	(. this :add_component comp-motion))

(defclass MonkSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32c32_l.cpm") 32 32 x y)
	; Holy monk (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_monk)
	(. this :add_component comp-anim :anim_start +frm_monk :anim_count 6 :anim_speed 3)
	(. this :add_component comp-motion))

(defclass SkeletonSprite (&optional x y) (Sprite (get-cached-canvas "image/fanatic_l.cpm") 32 32 x y)
	; Undead skeleton warrior (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_skeleton)
	(. this :add_component comp-anim :anim_start +frm_skeleton :anim_count 14 :anim_speed 3)
	(. this :add_component comp-motion))

(defclass SkeletonMonkSprite (&optional x y) (Sprite (get-cached-canvas "image/fanatic_l.cpm") 32 32 x y)
	; Undead skeleton monk (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_skeleton_monk)
	(. this :add_component comp-motion))

(defclass MissileSprite (frame &optional x y vx vy) (Sprite (get-cached-canvas "image/frm_16x16_l.cpm") 16 16 x y)
	; Projectile/missile (16x16)
	(setd x 0 y 0 vx 0 vy 0)
	(. this :set_frame frame)
	(. this :add_component comp-motion :vx vx :vy vy))

(defclass ItemSprite (frame &optional x y) (Sprite (get-cached-canvas "image/frm_16x16.cpm") 16 16 x y)
	; Collectible item / pickup (16x16)
	(setd x 0 y 0)
	(. this :set_frame frame))

(defclass ExplosionSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32c32.cpm") 32 32 x y)
	; Big explosion FX (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_bigexp)
	(. this :add_component comp-anim :anim_start +frm_bigexp :anim_count 5 :anim_speed 2 :loop :nil))

(defclass FizzSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_16x16.cpm") 16 16 x y)
	; Fizz spark FX (16x16)
	(setd x 0 y 0)
	(. this :set_frame +frm_fizz)
	(. this :add_component comp-anim :anim_start +frm_fizz :anim_count 3 :anim_speed 2 :loop :nil))

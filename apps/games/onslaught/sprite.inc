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

(defclass Sprite (canvas width height &optional x y) (View)
	; (Sprite canvas width height [x y]) -> sprite
	(setd x 0 y 0)
	(def this
		:canvas canvas :frame 0 :components (list) :death :nil)
	(. this :set_bounds x y width height)

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
			(bind '(x y w h) (. this :get_bounds))
			(when (defq parent (penv this))
				(. parent :add_dirty x y w h))
			(. this :sub))
		this)

	(defmethod :set_frame (f)
		; (. sprite :set_frame f) -> sprite
		(when (/= f (get :frame this))
			(def this :frame f)
			(if (= f -1)
				(. this :kill)
				(. this :dirty)))
		this)

	(defmethod :move_to (new_x new_y)
		; (. sprite :move_to new_x new_y) -> sprite
		(bind '(ox oy w h) (. this :get_bounds))
		(when (defq parent (penv this))
			(. parent :add_dirty ox oy w h))
		(. this :set_bounds new_x new_y w h)
		(. this :dirty)
		this)

	(defmethod :add_component (comp_fn)
		; (. sprite :add_component comp_fn) -> sprite
		(push (get :components this) comp_fn)
		this)

	(defmethod :update (dt)
		; (. sprite :update dt) -> sprite
		(if (= (get :frame this) -1)
			(. this :kill)
			(progn
				(each (lambda (comp)
					(unless (= (get :frame this) -1)
						(comp this dt)))
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

(defclass TitleLetter (idx &optional x y) (Sprite (get-cached-canvas "image/frm_32c32.cpm") 32 32 x y)
	; (TitleLetter idx [x y]) -> sprite
	(setd x 0 y 0)
	(def this :letter_idx idx)
	(. this :set_frame (+ +frm_letters idx))
	(. this :add_component comp-ml)

	(defmethod :fly_to (tx ty speed done_code &optional step_code)
		; (. letter :fly_to tx ty speed done_code [step_code]) -> letter
		(def this :target_x tx :target_y ty :ml_speed speed :ml_done_code done_code :ml_step_code step_code)
		this)
	)

(defclass TitleSword (&optional x y) (Sprite (get-cached-canvas "image/frm_16x64.cpm") 16 64 x y)
	; (TitleSword [x y]) -> sprite
	(setd x 152 y -64)
	(. this :set_frame +frm_sword)
	(. this :add_component comp-ml)

	(defmethod :drop_to (target_y drop_speed done_code)
		; (. sword :drop_to target_y drop_speed done_code) -> sword
		(bind '(cx cy) (. this :get_pos))
		(def this :target_x cx :target_y target_y :ml_speed drop_speed :ml_done_code done_code)
		this)
	)

(defclass TitleBloodDrip (&optional x y) (Sprite (get-cached-canvas "image/frm_16x16.cpm") 16 16 x y)
	; (TitleBloodDrip [x y]) -> sprite
	; In C++: sp_drip uses MV (ay=1, max_vy=10), cp_offscreen, and dt_drip death hook
	(setd x 152 y 84)
	(. this :set_frame +frm_blooddrip)
	(. this :add_component comp-mv)
	(. this :add_component comp-offscreen)

	(defmethod :set_drip (start_y max_y death_hook)
		; (. drip :set_drip start_y max_y death_hook) -> drip
		(bind '(cx cy) (. this :get_pos))
		(. this :move_to cx start_y)
		(def this
			:vx 0 :vy 0 :ax 0 :ay 1 :max_vy 10
			:offscreen_max_y max_y
			:death death_hook)
		this)
	)

(defclass ExplosionSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32c32.cpm") 32 32 x y)
	; Big explosion FX (32x32) using standard Animation Table (AT)
	(setd x 0 y 0)
	(def this
		:at_speed 2
		:at_table (list +frm_bigexp (+ +frm_bigexp 1) (+ +frm_bigexp 2) (+ +frm_bigexp 3) (+ +frm_bigexp 4) -1))
	(. this :set_frame +frm_bigexp)
	(. this :add_component comp-at))

(defclass FizzSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_16x16.cpm") 16 16 x y)
	; Fizz spark FX (16x16) using standard Animation Table (AT)
	(setd x 0 y 0)
	(def this
		:at_speed 2
		:at_table (list +frm_fizz (+ +frm_fizz 1) (+ +frm_fizz 2) (+ +frm_fizz 1) +frm_fizz -1))
	(. this :set_frame +frm_fizz)
	(. this :add_component comp-at))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Entity Sprite Subtypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ManSprite (&optional x y) (Sprite (get-cached-canvas "image/fanatic_l.cpm") 32 32 x y)
	; Player / fanatic man (32x32)
	(setd x 0 y 0)
	(def this
		:at_speed 3
		:at_table (list +frm_walk (+ +frm_walk 1) (+ +frm_walk 2) (+ +frm_walk 3)
					(+ +frm_walk 4) (+ +frm_walk 5) (+ +frm_walk 6) (+ +frm_walk 7)))
	(. this :set_frame +frm_walk)
	(. this :add_component comp-at)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass HorseSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_64x64_l.cpm") 64 64 x y)
	; Friendly cavalry horse (64x64)
	(setd x 0 y 0)
	(def this
		:at_speed 4
		:at_table (list +frm_horse (+ +frm_horse 1) (+ +frm_horse 2)
					(+ +frm_horse 3) (+ +frm_horse 4) (+ +frm_horse 5)))
	(. this :set_frame +frm_horse)
	(. this :add_component comp-at)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass SkeletonHorseSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_64x64_l.cpm") 64 64 x y)
	; Undead skeleton horse (64x64)
	(setd x 0 y 0)
	(def this
		:at_speed 4
		:at_table (list +frm_skeleton_horse (+ +frm_skeleton_horse 1) (+ +frm_skeleton_horse 2)
					(+ +frm_skeleton_horse 3) (+ +frm_skeleton_horse 4) (+ +frm_skeleton_horse 5)))
	(. this :set_frame +frm_skeleton_horse)
	(. this :add_component comp-at)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass SpearmanSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32x32_l.cpm") 32 32 x y)
	; Spearman infantry (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_spearman)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass WizardSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32x32_l.cpm") 32 32 x y)
	; Wizard caster (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_wizard)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass FootmanSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32x32_l.cpm") 32 32 x y)
	; Footman soldier (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_footman)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass KnightSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32x32_l.cpm") 32 32 x y)
	; Armored knight (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_knight)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass BoarRiderSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_64x32_l.cpm") 64 32 x y)
	; Boar rider (64x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_boarrider)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass CarpetSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_64x32_l.cpm") 64 32 x y)
	; Flying magic carpet (64x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_carpet)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass TowerSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_64x64.cpm") 64 64 x y)
	; Defense tower building (64x64)
	(setd x 0 y 0)
	(. this :set_frame +frm_tower))

(defclass BalistaSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32c32_l.cpm") 32 32 x y)
	; Siege balista (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_balista)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass CannonSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32c32_l.cpm") 32 32 x y)
	; Siege cannon (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_cannon)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass OilSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32c32_l.cpm") 32 32 x y)
	; Boiling oil cauldron (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_oil)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass BeserkSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32c32_l.cpm") 32 32 x y)
	; Berserker warrior (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_beserk)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass MonkSprite (&optional x y) (Sprite (get-cached-canvas "image/frm_32c32_l.cpm") 32 32 x y)
	; Holy monk (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_monk)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass SkeletonSprite (&optional x y) (Sprite (get-cached-canvas "image/fanatic_l.cpm") 32 32 x y)
	; Undead skeleton warrior (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_skeleton)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass SkeletonMonkSprite (&optional x y) (Sprite (get-cached-canvas "image/fanatic_l.cpm") 32 32 x y)
	; Undead skeleton monk (32x32)
	(setd x 0 y 0)
	(. this :set_frame +frm_skeleton_monk)
	(. this :add_component comp-bounds-bounce)
	(. this :add_component comp-mv))

(defclass MissileSprite (frame &optional x y vx vy) (Sprite (get-cached-canvas "image/frm_16x16_l.cpm") 16 16 x y)
	; Projectile/missile (16x16)
	(setd x 0 y 0 vx 0 vy 0)
	(def this :vx vx :vy vy)
	(. this :set_frame frame)
	(. this :add_component comp-mv)
	(. this :add_component comp-offscreen))

(defclass ItemSprite (frame &optional x y) (Sprite (get-cached-canvas "image/frm_16x16.cpm") 16 16 x y)
	; Collectible item / pickup (16x16)
	(setd x 0 y 0)
	(. this :set_frame frame))

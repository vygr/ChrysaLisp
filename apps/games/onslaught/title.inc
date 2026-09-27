;;;;;;;;;;;;;;;;;;;;;;;
; onslaught title state
;;;;;;;;;;;;;;;;;;;;;;;

(defun spawn-fizz-fx (x y)
	; spawn fizz spark fx centered at (x, y)
	(defq fizz (. (Fizz) :sp_set_pos (- x 8) (- y 8)))
	(. *layer_fx* :add_front fizz)
	fizz)

(defun spawn-bomb-fx (x y)
	; spawn 4 explosion sprites in a diamond around the letter center
	(defq cx (+ x 16) cy (+ y 16))
	(each (lambda ((ox oy))
		(defq exp (. (Explosion) :sp_set_pos (+ cx ox) (+ cy oy)))
		(. *layer_missiles* :add_front exp))
		'((-32 -16) (-16 -32) (0 -16) (-16 0))))

(defun spawn-title-letter (idx)
	(bind '(tx ty) (elem-get '((0 68) (36 68) (72 68) (108 68) (144 68) (180 68) (216 68) (252 68) (288 68)) idx))
	(bind '(sx sy) (get-xy-outer 32 32))
	(defq sp (TitleLetter idx))
	(init-ml sp sx sy tx ty 16)
	(. *layer_bodies* :add_front sp)
	sp)

(defun spawn-title-drip (x y)
	(defq sp (. (TitleBloodDrip) :sp_set_pos x y))
	(. *layer_player* :add_front sp)
	sp)

(defun spawn-title-sword ()
	; drops through center of 'A' (x=144..176) to y=52
	(defq sp (TitleSword))
	(. *layer_enemies* :add_front sp)
	(. sp :sp_set_pos 152 -64)
	sp)

(defun dt-drip (this)
	; death hook for drip
	(audio-play-rpc *sfx_drip*)
	(setq *game_state* +game_state_battle))

(defun cp-sword (this)
	; callback when sword reaches target
	(when (and (>= (get :sp_y this) 52) (/= (get :mv_ay this) 0))
		(set this :mv_ay 0 :mv_vy 0)
		(bind '(sx &) (. this :sp_get_pos))
		(. this :sp_set_pos sx 52)
		(defq fx_x (+ sx 8) fx_y (+ 52 32))
		(audio-play-rpc *sfx_clash2*)
		(spawn-fizz-fx fx_x fx_y)
		(spawn-title-drip sx fx_y)))

(defun cp-letter (this)
	; callback while letter flies and when it arrives
	(when (/= (or (get :ml_speed this) 0) 0)
		(if (>= (or (get :ml_count this) -1) 0)
			(progn
				(bind '(lx ly) (. this :sp_get_pos))
				(spawn-fizz-fx (+ lx 16) (+ ly 16)))
			(progn
				(set this :ml_speed 0)
				(audio-play-rpc *sfx_explode*)
				(bind '(lx ly) (. this :sp_get_pos))
				(spawn-bomb-fx lx ly)
				(setq *title_letter_idx* (+ *title_letter_idx* 1))
				(if (< *title_letter_idx* 9)
					(spawn-title-letter *title_letter_idx*)
					(spawn-title-sword))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; title sequence sprite subtypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass TitleLetter (idx) (Sprite *img_frm_32c32* :nil 32 32 :32c32)
	; (TitleLetter idx) -> sprite
	(def this :letter_idx idx)
	(.-> this
		(:sp_add_component (const ml-update) (const ml-init))
		(:sp_add_component (const cp-letter))
		(:sp_set_frame (+ +frm_32c32_letter_o idx)))
	(set this :ml_speed 16))

(defclass TitleSword () (Sprite *img_frm_16x64* :nil 16 64 :16x64)
	; (TitleSword) -> sprite
	(.-> this
		(:sp_add_component (const mv-update) (const mv-init))
		(:sp_add_component (const cp-sword))
		(:sp_set_frame +frm_16x64_sword))
	(set this :mv_ay 2))

(defclass TitleBloodDrip () (Sprite *img_frm_16x16* :nil 16 16 :16x16 (const dt-drip))
	; (TitleBloodDrip) -> sprite
	(.-> this
		(:sp_add_component (const mv-update) (const mv-init))
		(:sp_add_component (const offscreen-update))
		(:sp_set_frame +frm_16x16_blooddrip))
	(set this :mv_ay 1 :mv_max_vy 10))

(defclass Explosion () (Sprite *img_frm_32c32* :nil 32 32 :32c32)
	; big explosion fx (32x32)
	(.-> this
		(:sp_add_component (const at-update) (const at-init))
		(:sp_set_frame +frm_32c32_bigexp))
	(set this
		:at_speed 2
		:at_table (list +frm_32c32_bigexp (+ +frm_32c32_bigexp 1) (+ +frm_32c32_bigexp 2) (+ +frm_32c32_bigexp 3) (+ +frm_32c32_bigexp 4) -1)))

(defclass Fizz () (Sprite *img_frm_16x16* :nil 16 16 :16x16)
	; fizz spark fx (16x16)
	(.-> this
		(:sp_add_component (const at-update) (const at-init))
		(:sp_set_frame +frm_16x16_fizz))
	(set this
		:at_speed 2
		:at_table (list +frm_16x16_fizz (+ +frm_16x16_fizz 1) (+ +frm_16x16_fizz 2) (+ +frm_16x16_fizz 1) +frm_16x16_fizz -1)))

;state control

(defq *title_letter_idx* 0)

(defun title-state-init (&optional layer)
	(setq *title_letter_idx* 0)
	(clear-sprite-layers)
	(set-world-layers-pos 0 0)
	(set-world-layers-size (* *zoom* +screen_width) (* *zoom* +screen_height))
	(. *layer_land* :clear)
	(. *layer_sky* :sky_black)
	(spawn-title-letter *title_letter_idx*))

(defun title-state-update ()
	(update-sprite-layers))

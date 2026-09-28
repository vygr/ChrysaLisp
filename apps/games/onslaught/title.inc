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
	(init-ml sp (get :ml_comp sp) sx sy tx ty 16)
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

(defun cp-sword (this that)
	; this = sprite, that = component instance
	(when (defq mv (get :mv_comp this))
		(when (and (>= (get :sp_y this) 52) (/= (get :ay mv) 0))
			(set mv :ay 0 :vy 0)
			(bind '(sx &) (. this :sp_get_pos))
			(. this :sp_set_pos sx 52)
			(defq fx_x (+ sx 8) fx_y (+ 52 32))
			(audio-play-rpc *sfx_clash2*)
			(spawn-fizz-fx fx_x fx_y)
			(spawn-title-drip sx fx_y))))

(defun cp-letter (this that)
	; this = sprite, that = component instance
	(when (defq ml (get :ml_comp this))
		(when (/= (or (get :speed ml) 0) 0)
			(if (>= (or (get :count ml) -1) 0)
				(progn
					(bind '(lx ly) (. this :sp_get_pos))
					(spawn-fizz-fx (+ lx 16) (+ ly 16)))
				(set ml :speed 0)
				(audio-play-rpc *sfx_explode*)
				(bind '(lx ly) (. this :sp_get_pos))
				(spawn-bomb-fx lx ly)
				(setq *title_letter_idx* (+ *title_letter_idx* 1))
				(if (< *title_letter_idx* 9)
					(spawn-title-letter *title_letter_idx*)
					(spawn-title-sword))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; title sequence sprite subtypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass TitleLetter (idx) (Sprite *img_frm_32c32* :nil 32 32 :32c32)
	; (TitleLetter idx) -> sprite
	(def this :letter_idx idx :ml_comp (ML 16))
	(.-> this
		(:sp_add_component (get :ml_comp this))
		(:sp_add_component (CP (const cp-letter)))
		(:sp_set_frame (+ +frm_32c32_letter_o idx))))

(defclass TitleSword () (Sprite *img_frm_16x64* :nil 16 64 :16x64)
	; (TitleSword) -> sprite
	(def this :mv_comp (MV 0 0 0 2))
	(.-> this
		(:sp_add_component (get :mv_comp this))
		(:sp_add_component (CP (const cp-sword)))
		(:sp_set_frame +frm_16x64_sword)))

(defclass TitleBloodDrip () (Sprite *img_frm_16x16* :nil 16 16 :16x16 (const dt-drip))
	; (TitleBloodDrip) -> sprite
	(.-> this
		(:sp_add_component (MV 0 0 0 1 100 10))
		(:sp_add_component (CP (const offscreen-update)))
		(:sp_set_frame +frm_16x16_blooddrip)))

(defclass Explosion () (Sprite *img_frm_32c32* :nil 32 32 :32c32)
	; big explosion fx (32x32)
	(.-> this
		(:sp_add_component (AT 2 (list +frm_32c32_bigexp (+ +frm_32c32_bigexp 1) (+ +frm_32c32_bigexp 2) (+ +frm_32c32_bigexp 3) (+ +frm_32c32_bigexp 4) -1)))
		(:sp_set_frame +frm_32c32_bigexp)))

(defclass Fizz () (Sprite *img_frm_16x16* :nil 16 16 :16x16)
	; fizz spark fx (16x16)
	(.-> this
		(:sp_add_component (AT 2 (list +frm_16x16_fizz (+ +frm_16x16_fizz 1) (+ +frm_16x16_fizz 2) (+ +frm_16x16_fizz 1) +frm_16x16_fizz -1)))
		(:sp_set_frame +frm_16x16_fizz)))

; state control

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
	; check fire button to skip title sequence directly to battle
	(if (bits? *game_controls* +fkey_keya)
		(setq *test_addon_pressed* :t
			  *game_state* +game_state_battle)
		(update-sprite-layers)))
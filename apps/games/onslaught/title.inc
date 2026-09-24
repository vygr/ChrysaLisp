;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Onslaught Title Sequence
; Flying-in letters, dropping sword, dripping blood
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "service/audio/app.inc")
(import "./enums.inc")
(import "./components.inc")
(import "./sprite.inc")
(import "./widgets.inc")

(unless (def? '*app_root*)
	(defq *app_root* (path-to-file)))

(defq
	*title_letter_idx* 0
	*title_delay* 0
	*title_state* :letters ; :letters, :delay, :sword, :drip, :done
	*snd_explode* :nil
	*snd_clash* :nil
	*snd_drip* :nil)

(defun init-title-audio ()
	(catch (setq *snd_explode* (audio-add-rpc (cat *app_root* "sfx/explode.wav"))) :nil)
	(catch (setq *snd_clash* (audio-add-rpc (cat *app_root* "sfx/clash1.wav"))) :nil)
	(catch (setq *snd_drip* (audio-add-rpc (cat *app_root* "sfx/drip.wav"))) :nil))

(defun play-title-sfx (handle)
	(when handle
		(catch (audio-play-rpc handle) :nil)))

; Vertically centered in the 168-pixel play area (0 to 168):
; (168 - 32) / 2 = 68 pixels from top, exactly 68 pixels above panel!
(defq +title_letter_destinations
	''((0 68)
	  (36 68)
	  (72 68)
	  (108 68)
	  (144 68)
	  (180 68)
	  (216 68)
	  (252 68)
	  (288 68)))

(defun get-xy-outer (w h)
	; Pick a random start point just outside the playable area (WINDOW_WIDTH x WINDOW_HEIGHT)
	(case (random 4)
		(0 (list (- (random (+ +window_width (* 2 w))) w) (neg h)))
		(1 (list (- (random (+ +window_width (* 2 w))) w) +window_height))
		(2 (list (neg w) (- (random (+ +window_height (* 2 h))) h)))
		(3 (list +window_width (- (random (+ +window_height (* 2 h))) h)))))

(defq +bomb_offsets
	''((-32 -16)
	  (-16 -32)
	  (0 -16)
	  (-16 0)))

(defun spawn-bomb-fx (x y)
	; Spawn 4 explosion sprites in a diamond around the letter center (from C++ dt_bomb_fx)
	(defq cx (+ x 16) cy (+ y 16))
	(each (lambda ((ox oy))
		(defq exp (. (ExplosionSprite) :set_pos (+ cx ox) (+ cy oy)))
		(. *layer_fx* :add_front exp))
		+bomb_offsets))

(defun spawn-fizz-fx (x y)
	; Spawn fizz spark FX centered at (x, y) (from C++ dt_fizz_fx)
	(defq fizz (. (FizzSprite) :set_pos (- x 8) (- y 8)))
	(. *layer_fx* :add_front fizz)
	fizz)

(defun on-title-drip-done (&ignore)
	(setq *title_state* :done))

(defun dt-drip (this)
	; Corresponds to C++ dt_drip (onslaught.cpp:10178)
	(play-title-sfx *snd_drip*)
	(on-title-drip-done))

(defun spawn-title-drip (x y)
	; In C++: sp_drip uses MV (ay=1, max_vy=10), cp_offscreen, and dt_drip death hook
	; When it hits status panel (y=168), cp_offscreen kills it, triggering dt-drip
	(defq sp (. (TitleBloodDrip) :set_pos x y))
	(set sp :death (const dt-drip))
	(. *layer_fx* :add_back sp)
	sp)

(defun cp-sword (this)
	; Corresponds to C++ cp_sword (onslaught.cpp:9431)
	(when (/= (or (get :ml_speed this) 0) 0)
		(when (< (or (get :ml_count this) 0) 0)
			(set this :ml_speed 0)
			(bind '(sx sy) (. this :get_pos))
			(defq fx_x (+ sx 8) fx_y (+ sy 32))
			(spawn-fizz-fx fx_x fx_y)
			(play-title-sfx *snd_clash*)
			(setq *title_state* :drip)
			(spawn-title-drip sx fx_y))))

(defun spawn-title-sword ()
	; Drops through center of 'A' (x=144..176) to y=52 (hilt at 52, tip at 116) behind the letters
	(defq sp (TitleSword))
	(set sp :cp_code (const cp-sword))
	(ml-init sp 152 -64 152 52 14)
	(. *layer_sprites* :add_back sp)
	sp)

(defun cp-letter (this)
	; Corresponds to C++ cp_letter (onslaught.cpp:9413)
	(when (/= (or (get :ml_speed this) 0) 0)
		(if (>= (or (get :ml_count this) -1) 0)
			(progn
				(bind '(lx ly) (. this :get_pos))
				(spawn-fizz-fx (+ lx 16) (+ ly 16)))
			(progn
				(set this :ml_speed 0)
				(play-title-sfx *snd_explode*)
				(bind '(lx ly) (. this :get_pos))
				(spawn-bomb-fx lx ly)
				(setq *title_letter_idx* (+ *title_letter_idx* 1))
				(if (< *title_letter_idx* 9)
					(spawn-title-letter *title_letter_idx*)
					(progn
						(setq *title_state* :delay)
						(setq *title_delay* 16)))))))

(defun spawn-title-letter (idx)
	(bind '(tx ty) (elem-get +title_letter_destinations idx))
	(bind '(sx sy) (get-xy-outer 32 32))
	(defq sp (TitleLetter idx))
	(set sp :cp_code (const cp-letter))
	(ml-init sp sx sy tx ty 16)
	(. *layer_fx* :add_front sp)
	sp)

(defun title-sequence-start (&optional layer)
	(setq *title_letter_idx* 0
		*title_delay* 0
		*title_state* :letters)
	(clear-layer *layer_sprites*)
	(clear-layer *layer_fx*)
	(defq
		world_w (* +map_width +tile_width)
		world_h (* +map_height +tile_height))
	(. *layer_sprites* :set_size world_w world_h)
	(. *layer_fx* :set_size world_w world_h)
	(. *layer_land* :set_size world_w world_h)
	(. *layer_sky* :set_size world_w world_h)
	(init-title-audio)
	(spawn-title-letter 0))

(defun title-sequence-update (layer dt)
	(each (# (when (Sprite? %0) (. %0 :update dt))) (cat (. *layer_sprites* :children)))
	(each (# (when (Sprite? %0) (. %0 :update dt))) (cat (. *layer_fx* :children)))
	(when (eql *title_state* :delay)
		(setq *title_delay* (- *title_delay* 1))
		(when (<= *title_delay* 0)
			(setq *title_state* :sword)
			(spawn-title-sword)))
	(. layer :dirty_all))

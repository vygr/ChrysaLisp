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
	; Pick a random start point just outside the screen
	(case (random 4)
		(0 (list (- (random (+ +game_width (* 2 w))) w) (neg h)))
		(1 (list (- (random (+ +game_width (* 2 w))) w) +game_height))
		(2 (list (neg w) (- (random (+ +game_height (* 2 h))) h)))
		(3 (list +game_width (- (random (+ +game_height (* 2 h))) h)))))

(defq +bomb_offsets
	''((-32 -16)
	  (-16 -32)
	  (0 -16)
	  (-16 0)))

(defun spawn-bomb-fx (x y)
	; Spawn 4 explosion sprites in a diamond around the letter center (from C++ dt_bomb_fx)
	(defq cx (+ x 16) cy (+ y 16))
	(each (lambda ((ox oy))
		(defq exp (ExplosionSprite (+ cx ox) (+ cy oy)))
		(. *layer_fx* :add_front exp)
		(. exp :dirty))
		+bomb_offsets))

(defun spawn-fizz-fx (x y)
	; Spawn fizz spark FX centered at (x, y) (from C++ dt_fizz_fx)
	(defq fizz (FizzSprite (- x 8) (- y 8)))
	(. *layer_fx* :add_front fizz)
	(. fizz :dirty)
	fizz)

(defun on-title-drip-done (&ignore)
	(setq *title_state* :done))

(defun spawn-title-drip (x y)
	; Drips from center of letter 'A' where sword impacts (y=84) down to top of status panel (y=168)
	; Added with :add_back to *layer_fx* so it sits between the sword (behind in *layer_sprites*)
	; and the letters (in front at the head of *layer_fx*)
	(defq sp (TitleBloodDrip x y))
	(. sp :set_drip y 168
		(lambda (&ignore) (play-title-sfx *snd_drip*))
		(const on-title-drip-done))
	(. *layer_fx* :add_back sp)
	(. sp :dirty)
	sp)

(defun on-title-sword-arrived (sp)
	(play-title-sfx *snd_clash*)
	(bind '(sx sy) (. sp :get_pos))
	; Fizz FX where the sword hits and sticks into 'A' (from C++ cp_sword dt_fizz_fx)
	(defq fx_x (+ sx 8) fx_y (+ sy 32))
	(spawn-fizz-fx fx_x fx_y)
	(setq *title_state* :drip)
	(spawn-title-drip sx fx_y))

(defun spawn-title-sword ()
	; Drops through center of 'A' (x=144..176) to y=52 (hilt at 52, tip at 116) behind the letters
	(defq sp (TitleSword 152 -64))
	(. sp :drop_to 52 14 (const on-title-sword-arrived))
	(. *layer_sprites* :add_back sp)
	(. sp :dirty)
	sp)

(defun on-title-letter-arrived (sp)
	(play-title-sfx *snd_explode*)
	(bind '(lx ly) (. sp :get_pos))
	(spawn-bomb-fx lx ly)
	(setq *title_letter_idx* (+ *title_letter_idx* 1))
	(if (< *title_letter_idx* 9)
		(spawn-title-letter *title_letter_idx*)
		(progn
			(setq *title_state* :delay)
			(setq *title_delay* 16))))

(defun spawn-title-letter (idx)
	(bind '(tx ty) (elem-get +title_letter_destinations idx))
	(bind '(sx sy) (get-xy-outer 32 32))
	(defq sp (TitleLetter idx sx sy))
	(. sp :fly_to tx ty 16
		(const on-title-letter-arrived)
		(# (bind '(lx ly) (. %0 :get_pos))
			(spawn-fizz-fx (+ lx 16) (+ ly 16))))
	(. *layer_fx* :add_front sp)
	(. sp :dirty)
	sp)

(defun title-sequence-start (&optional layer)
	(setq *title_letter_idx* 0
		*title_delay* 0
		*title_state* :letters)
	(init-title-audio)
	(spawn-title-letter 0))

(defun title-sequence-update (layer dt)
	(each (# (when (Sprite? %0) (. %0 :update dt))) (cat (. *layer_sprites* :children)))
	(each (# (when (Sprite? %0) (. %0 :update dt))) (cat (. *layer_fx* :children)))
	(when (eql *title_state* :delay)
		(setq *title_delay* (- *title_delay* 1))
		(when (<= *title_delay* 0)
			(setq *title_state* :sword)
			(spawn-title-sword))))

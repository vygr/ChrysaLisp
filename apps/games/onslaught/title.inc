;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/onslaught/title.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught title sequence
; flying-in letters, dropping sword, dripping blood
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

; destinations in base coordinates
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
	; pick a random start point just outside the playable area
	(defq
		win_w (* *zoom* +window_width)
		win_h (* *zoom* +window_height))
	(case (random 4)
		(0 (list (- (random (+ win_w (* 2 w))) w) (neg h)))
		(1 (list (- (random (+ win_w (* 2 w))) w) win_h))
		(2 (list (neg w) (- (random (+ win_h (* 2 h))) h)))
		(3 (list win_w (- (random (+ win_h (* 2 h))) h)))))

(defq +bomb_offsets
	''((-32 -16)
	  (-16 -32)
	  (0 -16)
	  (-16 0)))

(defun spawn-bomb-fx (x y)
	; spawn 4 explosion sprites in a diamond around the letter center
	(defq cx (+ x (* *zoom* 16)) cy (+ y (* *zoom* 16)))
	(each (lambda ((ox oy))
		(defq exp (. (ExplosionSprite) :set_pos (+ cx (* *zoom* ox)) (+ cy (* *zoom* oy))))
		(. *layer_fx* :add_front exp))
		+bomb_offsets))

(defun spawn-fizz-fx (x y)
	; spawn fizz spark fx centered at (x, y)
	(defq fizz (. (FizzSprite) :set_pos (- x (* *zoom* 8)) (- y (* *zoom* 8))))
	(. *layer_fx* :add_front fizz)
	fizz)

(defun on-title-drip-done (&ignore)
	(setq *title_state* :done))

(defun dt-drip (this)
	; death hook for drip
	(play-title-sfx *snd_drip*)
	(on-title-drip-done))

(defun spawn-title-drip (x y)
	(defq sp (. (TitleBloodDrip) :set_pos x y))
	(set sp :death (const dt-drip))
	(. *layer_fx* :add_back sp)
	sp)

(defun cp-sword (this)
	; callback when sword reaches target
	(when (/= (or (get :ml_speed this) 0) 0)
		(when (< (or (get :ml_count this) 0) 0)
			(set this :ml_speed 0)
			(bind '(sx sy) (. this :get_pos))
			(defq fx_x (+ sx (* *zoom* 8)) fx_y (+ sy (* *zoom* 32)))
			(spawn-fizz-fx fx_x fx_y)
			(play-title-sfx *snd_clash*)
			(setq *title_state* :drip)
			(spawn-title-drip sx fx_y))))

(defun spawn-title-sword ()
	; drops through center of 'A' (x=144..176) to y=52
	(defq sp (TitleSword))
	(. sp :add_component (const cp-sword))
	(init-ml sp (* *zoom* 152) (* *zoom* -64) (* *zoom* 152) (* *zoom* 52) (* *zoom* 14))
	(. *layer_sprites* :add_back sp)
	sp)

(defun cp-letter (this)
	; callback while letter flies and when it arrives
	(when (/= (or (get :ml_speed this) 0) 0)
		(if (>= (or (get :ml_count this) -1) 0)
			(progn
				(bind '(lx ly) (. this :get_pos))
				(spawn-fizz-fx (+ lx (* *zoom* 16)) (+ ly (* *zoom* 16))))
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
	(setq tx (* *zoom* tx) ty (* *zoom* ty))
	(bind '(sx sy) (get-xy-outer (* *zoom* 32) (* *zoom* 32)))
	(defq sp (TitleLetter idx))
	(. sp :add_component (const cp-letter))
	(init-ml sp sx sy tx ty (* *zoom* 16))
	(. *layer_fx* :add_front sp)
	sp)

(defun title-sequence-start (&optional layer)
	(setq *title_letter_idx* 0
		*title_delay* 0
		*title_state* :letters)
	(reset-layers-title)
	(init-title-audio)
	(spawn-title-letter 0))

(defun title-sequence-update ()
	(update-sprite-layers)
	(when (eql *title_state* :delay)
		(setq *title_delay* (- *title_delay* 1))
		(when (<= *title_delay* 0)
			(setq *title_state* :sword)
			(spawn-title-sword)))
	(. *layer_sky* :dirty)
	(. *world_layers* :dirty_all))
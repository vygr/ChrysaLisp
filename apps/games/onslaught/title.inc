;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught title sequence
; flying-in letters, dropping sword, dripping blood
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	*title_letter_idx* 0
	*title_delay* 0
	*title_state* :letters)

; destinations in base coordinates
(defq +title_letter_destinations
	''((0 68) (36 68) (72 68) (108 68) (144 68) (180 68) (216 68) (252 68) (288 68)))

(defun get-xy-outer (w h)
	; pick a random start point just outside the playable area
	(case (random 4)
		(0 (list (- (random (+ +window_width (* 2 w))) w) (neg h)))
		(1 (list (- (random (+ +window_width (* 2 w))) w) +window_height))
		(2 (list (neg w) (- (random (+ +window_height (* 2 h))) h)))
		(3 (list +window_width (- (random (+ +window_height (* 2 h))) h)))))

(defq +bomb_offsets ''((-32 -16) (-16 -32) (0 -16) (-16 0)))

(defun spawn-bomb-fx (x y)
	; spawn 4 explosion sprites in a diamond around the letter center
	(defq cx (+ x 16) cy (+ y 16))
	(each (lambda ((ox oy))
		(defq exp (. (ExplosionSprite) :sp_set_pos (+ cx ox) (+ cy oy)))
		(. *layer_fx* :add_front exp))
		+bomb_offsets))

(defun spawn-fizz-fx (x y)
	; spawn fizz spark fx centered at (x, y)
	(defq fizz (. (FizzSprite) :sp_set_pos (- x 8) (- y 8)))
	(. *layer_fx* :add_front fizz)
	fizz)

(defun on-title-drip-done (&ignore)
	(setq *title_state* :done))

(defun dt-drip (this)
	; death hook for drip
	(audio-play-rpc *sfx_drip*)
	(on-title-drip-done))

(defun spawn-title-drip (x y)
	(defq sp (. (TitleBloodDrip) :sp_set_pos x y))
	(set sp :sp_death (const dt-drip))
	(. *layer_fx* :add_back sp)
	sp)

(defun cp-sword (this)
	; callback when sword reaches target
	(when (/= (or (get :ml_speed this) 0) 0)
		(when (< (or (get :ml_count this) 0) 0)
			(set this :ml_speed 0)
			(bind '(sx sy) (. this :sp_get_pos))
			(defq fx_x (+ sx 8) fx_y (+ sy 32))
			(spawn-fizz-fx fx_x fx_y)
			(audio-play-rpc *sfx_clash2*)
			(setq *title_state* :drip)
			(spawn-title-drip sx fx_y))))

(defun spawn-title-sword ()
	; drops through center of 'A' (x=144..176) to y=52
	(defq sp (TitleSword))
	(. sp :add_component (const cp-sword))
	(init-ml sp 152 -64 152 52 14)
	(. *layer_fx* :add_back sp)
	sp)

(defun title-sequence-start (&optional layer)
	(setq *title_letter_idx* 0 *title_delay* 0 *title_state* :letters)
	(reset-layers-title)
	(spawn-title-letter 0))

(defun title-sequence-update ()
	(update-sprite-layers)
	(when (eql *title_state* :delay)
		(setq *title_delay* (- *title_delay* 1))
		(when (<= *title_delay* 0)
			(setq *title_state* :sword)
			(spawn-title-sword))))

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
					(progn
						(setq *title_state* :delay)
						(setq *title_delay* 16)))))))

(defun spawn-title-letter (idx)
	(bind '(tx ty) (elem-get +title_letter_destinations idx))
	(bind '(sx sy) (get-xy-outer 32 32))
	(defq sp (TitleLetter idx))
	(. sp :add_component (const cp-letter))
	(init-ml sp sx sy tx ty 16)
	(. *layer_fx* :add_front sp)
	sp)

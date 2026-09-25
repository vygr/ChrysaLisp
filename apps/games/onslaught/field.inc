;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/onslaught/field.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught field battle sequence
; handles field map loop, QAOP controls, player man physics & camera tracking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./enums.inc")
(import "./sprite.inc")
(import "./widgets.inc")
(import "./map.inc")
(import "./sky.inc")

(defq
	*player_man* :nil
	*game_controls* 0
	*cam_x* 0
	*cam_y* 0
	*current_map_idx* 1)

(defun update-camera (man_x man_y)
	; center camera on player man
	(defq
		win_w (* *zoom* +window_width)
		win_h (* *zoom* +window_height)
		cam_x (- man_x (* *zoom* 144))
		cam_y (- (+ man_y (* *zoom* 16)) (* *zoom* 84))
		max_cam_x (- (* +map_width (* +tile_width *zoom*)) win_w)
		max_cam_y (- (* +map_height (* +tile_height *zoom*)) win_h))
	(setq *cam_x* (max 0 (min max_cam_x cam_x))
		  *cam_y* (max 0 (min max_cam_y cam_y)))
	; scroll all world layers by negative camera offset
	(set-world-layers-pos (neg *cam_x*) (neg *cam_y*)))

(defun player-man-update (man)
	; physics & QAOP controls for player man
	(bind '(x y) (. man :get_pos))
	(defq
		xv (or (get :xv man) 0)
		yv (or (get :yv man) 0)
		h (* *zoom* 32)
		walk_spd (* *zoom* 4)
		tile_h (* *zoom* +tile_height)
		y1 (+ h yv y)
		flags1 (. *layer_land* :get_tile_flags (+ x (* *zoom* 8)) y1)
		flags2 (. *layer_land* :get_tile_flags (+ x (* *zoom* 23)) y1))
	; falling check
	(if (and (not (bits? flags1 +fmap_climb +fmap_stand))
			 (not (bits? flags2 +fmap_climb +fmap_stand)))
		(progn
			; falling in air
			(. man :set_frame +frm_fall)
			(setq yv (min (* *zoom* 10) (+ yv *zoom*))
				  x (+ x xv)
				  y (+ y yv))
			(set man :xv xv :yv yv))
		(progn
			; on floor or ladder
			(when (/= yv 0)
				(if (= yv (* *zoom* 10))
					(progn
						(. man :set_frame +frm_duck)
						(setq xv 0))
					(. man :set_frame +frm_walk))
				(setq y (- (logand y1 (neg tile_h)) h)
					  yv 0))
			; process QAOP movement controls
			(cond
				((bits? *game_controls* +fkey_up)
					; climb up ladder
					(defq
						cy1 (+ y h -1)
						cflags1 (. *layer_land* :get_tile_flags (+ x (* *zoom* 4)) cy1)
						cflags2 (. *layer_land* :get_tile_flags (+ x (* *zoom* 27)) cy1))
					(if (and (bits? cflags1 +fmap_climb)
							 (bits? cflags2 +fmap_climb))
						(progn
							(setq y (- y walk_spd)
								  x (logand (+ x walk_spd) (neg tile_h))
								  xv 0)
							(. man :set_frame (+ +frm_climb (logand (>> (/ y *zoom*) 2) 3))))
						:nil))
				((bits? *game_controls* +fkey_down)
					; climb down ladder or crouch/duck
					(defq
						cy1 (+ y h)
						cflags1 (. *layer_land* :get_tile_flags (+ x (* *zoom* 4)) cy1)
						cflags2 (. *layer_land* :get_tile_flags (+ x (* *zoom* 27)) cy1))
					(if (and (bits? cflags1 +fmap_climb)
							 (bits? cflags2 +fmap_climb))
						(progn
							(setq y (+ y walk_spd)
								  x (logand (+ x walk_spd) (neg tile_h))
								  xv 0)
							(. man :set_frame (+ +frm_climb (logand (>> (/ y *zoom*) 2) 3))))
						(progn
							(. man :set_frame +frm_duck)
							(setq xv 0))))
				((bits? *game_controls* +fkey_left)
					; walk left
					(setq x (- x walk_spd)
						  y (logand y (neg tile_h))
						  xv (neg walk_spd))
					(set man :dir -1)
					(defq f (+ (get :frame man) 1))
					(if (or (< f +frm_walk) (>= f (+ +frm_walk 8)))
						(setq f +frm_walk))
					(. man :set_frame f))
				((bits? *game_controls* +fkey_right)
					; walk right
					(setq x (+ x walk_spd)
						  y (logand y (neg tile_h))
						  xv walk_spd)
					(set man :dir 1)
					(defq f (+ (get :frame man) 1))
					(if (or (< f +frm_walk) (>= f (+ +frm_walk 8)))
						(setq f +frm_walk))
					(. man :set_frame f))
				(:t
					(setq xv 0)
					(when (= (get :frame man) +frm_duck)
						(. man :set_frame +frm_walk))))
			(set man :xv xv :yv yv)))
	; clamp to map horizontal bounds
	(setq x (max 0 (min (- (* +map_width (* +tile_width *zoom*)) h) x)))
	(. man :set_pos x y)
	(list x y))

(defun field-sequence-start (&optional layer map_idx)
	; switch to field battle scene
	(setd map_idx *current_map_idx*)
	(reset-layers-field map_idx)
	; spawn player man at starting position (x=8, y=208)
	(defq man (ManSprite))
	(def man :xv 0 :yv 0 :dir 1)
	(. man :set_pos (* *zoom* 8) (* *zoom* 208))
	(. *layer_player* :add_front man)
	(setq *player_man* man)
	; position camera on player
	(update-camera (* *zoom* 8) (* *zoom* 208))
	man)

(defun field-sequence-update ()
	; tick field map state
	(when *player_man*
		(bind '(mx my) (player-man-update *player_man*))
		(update-camera mx my))
	; update all active sprites across all 7 layers
	(update-sprite-layers)
	(. *layer_sky* :dirty)
	(. *layer_land* :dirty)
	(. *world_layers* :dirty_all))
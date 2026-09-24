;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Onslaught Field Battle Sequence
; Handles field map loop, QAOP controls, player man physics & camera tracking
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

(defun clear-layer (layer)
	; Remove all child views from layer
	(each (# (. %0 :sub)) (cat (. layer :children))))

(defun update-camera (man_x man_y)
	; Center camera on player man
	(defq
		cam_x (- man_x 144)
		cam_y (- (+ man_y 16) 84)
		max_cam_x (- (* +map_width +tile_width) +window_width)    ; 2048 - 320 = 1728
		max_cam_y (- (* +map_height +tile_height) +window_height)) ; 256 - 168 = 88
	(setq *cam_x* (max 0 (min max_cam_x cam_x))
		  *cam_y* (max 0 (min max_cam_y cam_y)))
	; Scroll all world layers by negative camera offset
	(. *layer_sprites* :set_pos (neg *cam_x*) (neg *cam_y*))
	(. *layer_fx* :set_pos (neg *cam_x*) (neg *cam_y*))
	(. *layer_land* :set_pos (neg *cam_x*) (neg *cam_y*))
	(. *layer_sky* :set_pos (neg *cam_x*) (neg *cam_y*)))

(defun player-man-update (man)
	; Physics & QAOP controls for player man
	(bind '(x y) (. man :get_pos))
	(defq
		xv (or (get :xv man) 0)
		yv (or (get :yv man) 0)
		h 32
		y1 (+ h yv y)
		flags1 (. *layer_land* :get_tile_flags (+ x 8) y1)
		flags2 (. *layer_land* :get_tile_flags (+ x 23) y1))
	; Falling check
	(if (and (not (bits? flags1 +fmap_climb +fmap_stand))
			 (not (bits? flags2 +fmap_climb +fmap_stand)))
		(progn
			; Falling in air
			(. man :set_frame +frm_fall)
			(setq yv (min 10 (+ yv 1))
				  x (+ x xv)
				  y (+ y yv))
			(set man :xv xv :yv yv))
		(progn
			; On floor or ladder
			(when (/= yv 0)
				(if (= yv 10)
					(progn
						(. man :set_frame +frm_duck)
						(setq xv 0))
					(. man :set_frame +frm_walk))
				(setq y (- (logand y1 -16) 32)
					  yv 0))
			; Process QAOP movement controls
			(cond
				((bits? *game_controls* +fkey_up)
					; Climb up ladder
					(defq
						cy1 (+ y h -1)
						cflags1 (. *layer_land* :get_tile_flags (+ x 4) cy1)
						cflags2 (. *layer_land* :get_tile_flags (+ x 27) cy1))
					(if (and (bits? cflags1 +fmap_climb)
							 (bits? cflags2 +fmap_climb))
						(progn
							(setq y (- y 4)
								  x (logand (+ x 4) -16)
								  xv 0)
							(. man :set_frame (+ +frm_climb (logand (>> y 2) 3))))
						:nil))
				((bits? *game_controls* +fkey_down)
					; Climb down ladder or crouch/duck
					(defq
						cy1 (+ y h)
						cflags1 (. *layer_land* :get_tile_flags (+ x 4) cy1)
						cflags2 (. *layer_land* :get_tile_flags (+ x 27) cy1))
					(if (and (bits? cflags1 +fmap_climb)
							 (bits? cflags2 +fmap_climb))
						(progn
							(setq y (+ y 4)
								  x (logand (+ x 4) -16)
								  xv 0)
							(. man :set_frame (+ +frm_climb (logand (>> y 2) 3))))
						(progn
							(. man :set_frame +frm_duck)
							(setq xv 0))))
				((bits? *game_controls* +fkey_left)
					; Walk left
					(setq x (- x 4)
						  y (logand y -16)
						  xv -4)
					(set man :dir -1)
					(defq f (+ (get :frame man) 1))
					(if (or (< f +frm_walk) (>= f (+ +frm_walk 8)))
						(setq f +frm_walk))
					(. man :set_frame f))
				((bits? *game_controls* +fkey_right)
					; Walk right
					(setq x (+ x 4)
						  y (logand y -16)
						  xv 4)
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
	; Clamp to map horizontal bounds
	(setq x (max 0 (min (- (* +map_width +tile_width) 32) x)))
	(. man :set_pos x y)
	(list x y))

(defun field-sequence-start (&optional layer map_idx)
	; Switch to field battle scene
	(setd map_idx *current_map_idx*)
	; 1. Clear all old sprites and FX views
	(clear-layer *layer_sprites*)
	(clear-layer *layer_fx*)
	; 2. Set all world layers to full map dimensions (2048 x 256)
	(defq
		world_w (* +map_width +tile_width)
		world_h (* +map_height +tile_height))
	(. *layer_sprites* :set_size world_w world_h)
	(. *layer_fx* :set_size world_w world_h)
	(. *layer_land* :set_size world_w world_h)
	(. *layer_sky* :set_size world_w world_h)
	; 3. Setup pastel sky gradient
	(. *layer_sky* :set_random_sky)
	; 4. Load field terrain map
	(. *layer_land* :load_field_map map_idx)
	; 5. Spawn player man at starting position (x=8, y=208)
	(defq man (ManSprite))
	(def man :xv 0 :yv 0 :dir 1)
	(. man :set_pos 8 208)
	(. *layer_sprites* :add_front man)
	(setq *player_man* man)
	; 6. Position camera on player
	(update-camera 8 208)
	man)

(defun field-sequence-update (layer dt)
	; Tick field map state
	(when *player_man*
		(bind '(mx my) (player-man-update *player_man*))
		(update-camera mx my))
	; Update any other active sprites
	(each (# (when (Sprite? %0) (. %0 :update dt))) (cat (. *layer_sprites* :children)))
	(each (# (when (Sprite? %0) (. %0 :update dt))) (cat (. *layer_fx* :children)))
	; Redraw playfield
	(. layer :dirty_all))

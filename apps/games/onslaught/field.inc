;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught field battle state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *current_map_idx* 1)

;state control

(defun field-reset-layers (&optional map_idx)
	; field battle: clear sprites, size to full map world, pastel sky, load terrain map
	(clear-sprite-layers)
	(set-world-layers-pos 0 0)
	(set-world-layers-size (* *zoom* (* +map_width +tile_width)) (* *zoom* (* +map_height +tile_height)))
	(. *layer_sky* :set_random_sky)
	(. *layer_land* :load_field_map (ifn map_idx 1)))

(defun field-sequence-start (&optional layer map_idx)
	; switch to field battle scene
	(setd map_idx *current_map_idx*)
	(field-reset-layers map_idx)
	; spawn player man at starting position (x=8, y=208)
	(defq man (Fanatic))
	(def man :sp_dir 1 :man_xv 0 :man_yv 0 :man_walk_tick 0)
	(. man :sp_set_pos 8 208)
	(. *layer_player* :add_front man)
	(setq *player_man* man)
	; position camera on player
	(update-camera man)
	man)

(defun field-sequence-update ()
	; tick field map state
	(update-camera *player_man*)
	; update all active sprites across all 7 layers
	(update-sprite-layers))

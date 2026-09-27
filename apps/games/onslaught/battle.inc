;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught battle state
;;;;;;;;;;;;;;;;;;;;;;;;

(defq *current_map_idx* 1)

(defun battle-state-init ()
	(clear-sprite-layers)
	(set-world-layers-pos 0 0)
	(set-world-layers-size (* *zoom* (* +map_width +tile_width)) (* *zoom* (* +map_height +tile_height)))
	(. *layer_sky* :sky_random)
	(. *layer_land* :load_field_map *current_map_idx*)
	(def (defq man (Fanatic)) :sp_dir 1 :man_xv 0 :man_yv 0 :man_walk_tick 0)
	(. man :sp_set_pos 8 208)
	(. *layer_player* :add_front man)
	(setq *player_man* man))

(defun battle-state-update ()
	(update-camera *player_man*)
	(update-sprite-layers))

;other states - not done yet
;these should go in their own files
(defun menu-state-init ())
(defun battle-lost-state-init ())
(defun mind-state-init ())
(defun mind-won-state-init ())
(defun mind-lost-state-init ())
(defun credits-state-init ())
(defun oracle-state-init ())
(defun demo-state-init ())
;update functions
(defun menu-state-update ())
(defun map-state-update ())
(defun scores-state-update ())
(defun hiscore-state-update ())
(defun battle-won-state-update ())
(defun battle-lost-state-update ())
(defun mind-state-update ())
(defun mind-won-state-update ())
(defun mind-lost-state-update ())
(defun credits-state-update ())
(defun oracle-state-update ())
(defun demo-state-update ())

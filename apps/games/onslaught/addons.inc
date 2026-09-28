;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/onslaught/addons.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; weapon and item addon sprites, movement tables & animations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; movement tables for addon sprites relative to player (dx dy)

(defq
	+mt_upper_mase_l
	''((32 0) (32 0) (8 0) (8 0) (-16 0) (-16 0))
	+mt_upper_mase_r
	''((-16 0) (-16 0) (8 0) (8 0) (32 0) (32 0))
	+mt_lower_mase_l
	''((32 -8) (32 -8) (8 0) (8 0) (-16 0) (-16 0))
	+mt_lower_mase_r
	''((-16 -8) (-16 -8) (8 0) (8 0) (32 0) (32 0))
	+mt_big_crossbow_l
	''((-16 16) (-16 16) (-16 16) (-16 16) (-16 0) (-16 0) (-16 0) (-16 0))
	+mt_big_crossbow_r
	''((32 16) (32 16) (32 16) (32 16) (32 0) (32 0) (32 0) (32 0))
	+mt_small_crossbow_l
	''((0 -16) (-16 0) (-16 0))
	+mt_small_crossbow_r
	''((16 -16) (32 0) (32 0))
	+mt_naptha_l
	''((32 0) (32 0) (32 0) (0 0) (0 0) (0 0) (-16 0) (-16 0) (-16 0))
	+mt_naptha_r
	''((-16 0) (-16 0) (-16 0) (0 0) (0 0) (0 0) (32 0) (32 0) (32 0)))

; addon animation tables (terminate with -1 to trigger :sp_kill)

(defq
	+at_upper_mase
	''(1 2 3 -1)
	+at_lower_mase
	''(4 5 6 -1)
	+at_big_crossbow
	''(10 11 12 13 -1)
	+at_small_crossbow
	''(7 8 9 -1)
	+at_naptha
	''(14 14 15 -1))

; player man body animations during weapon use

(defq
	+at_man_upper_mase
	''(14 15 16 4)
	+at_man_lower_mase
	''(30 31 32 29)
	+at_man_big_crossbow
	''(20 21 22 23 0)
	+at_man_small_crossbow
	''(17 18 19 4)
	+at_man_naptha
	''(24 25 26 4))

; reusable 16x16 directional addon sprite class

(defclass Addon (man mt_table at_speed at_table) (Sprite *img_frm_16x16_l* *img_frm_16x16_r* 16 16 :16x16_lr)
	; (Addon man mt_table at_speed at_table) -> addon sprite
	(def this :sp_dir (or (get :sp_dir man) 1))
	; set initial position immediately to eliminate 1-frame spawn lag
	(bind '(mx my) (. man :sp_get_pos))
	(bind '(dx dy) (first mt_table))
	(. this :sp_set_pos (+ mx dx) (+ my dy))
	(defq at_comp (AT at_speed at_table :nil)
		mt_comp (MT 1 mt_table man))
	; start at index 1 since step 0 was applied at spawn
	(set at_comp :index 1 :count 0)
	(set mt_comp :index 1 :count 0)
	(.-> this
		(:sp_add_component mt_comp)
		(:sp_add_component at_comp)
		(:sp_set_frame (first at_table))))

; test trigger to cycle player actions

(defq *test_addon_idx* 0
	*test_addon_pressed* :nil)

(defun trigger-player-addon (man action_idx)
	; trigger weapon animation on man and spawn matching tracking addon
	(defq dir (or (get :sp_dir man) 1)
		man_at (elem-get (get :sp_components man) +man_comp_at))
	(cond
		((= action_idx 0)
			; upper mase
			(audio-play-rpc *sfx_throw*)
			(def man :action_playing :t)
			(. man :sp_set_frame (first +at_man_upper_mase))
			(set man_at :speed 2 :table +at_man_upper_mase :count 0 :index 1 :loop :nil)
			(defq mt (if (= dir 1) +mt_upper_mase_r +mt_upper_mase_l)
				addon (Addon man mt 2 +at_upper_mase))
			(. *layer_player* :add_front addon))
		((= action_idx 1)
			; lower mase
			(audio-play-rpc *sfx_throw*)
			(def man :action_playing :t)
			(. man :sp_set_frame (first +at_man_lower_mase))
			(set man_at :speed 2 :table +at_man_lower_mase :count 0 :index 1 :loop :nil)
			(defq mt (if (= dir 1) +mt_lower_mase_r +mt_lower_mase_l)
				addon (Addon man mt 2 +at_lower_mase))
			(. *layer_player* :add_front addon))
		((= action_idx 2)
			; big crossbow
			(audio-play-rpc *sfx_twang1*)
			(def man :action_playing :t)
			(. man :sp_set_frame (first +at_man_big_crossbow))
			(set man_at :speed 2 :table +at_man_big_crossbow :count 0 :index 1 :loop :nil)
			(defq mt (if (= dir 1) +mt_big_crossbow_r +mt_big_crossbow_l)
				addon (Addon man mt 2 +at_big_crossbow))
			(. *layer_player* :add_front addon))
		((= action_idx 3)
			; small crossbow
			(audio-play-rpc *sfx_twang2*)
			(def man :action_playing :t)
			(. man :sp_set_frame (first +at_man_small_crossbow))
			(set man_at :speed 1 :table +at_man_small_crossbow :count 0 :index 1 :loop :nil)
			(defq mt (if (= dir 1) +mt_small_crossbow_r +mt_small_crossbow_l)
				addon (Addon man mt 1 +at_small_crossbow))
			(. *layer_player* :add_front addon))
		((= action_idx 4)
			; naptha pot
			(audio-play-rpc *sfx_throw*)
			(def man :action_playing :t)
			(. man :sp_set_frame (first +at_man_naptha))
			(set man_at :speed 3 :table +at_man_naptha :count 0 :index 1 :loop :nil)
			(defq mt (if (= dir 1) +mt_naptha_r +mt_naptha_l)
				addon (Addon man mt 1 +at_naptha))
			(. *layer_player* :add_front addon))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught main menu and credits state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq
	*menu_selected* +menu_start
	*menu_timer* 0
	*menu_up_armed* :nil
	*menu_down_armed* :nil
	*menu_fire_armed* :nil
	*menu_difficulty* 0
	*menu_mode* +mode_auto
	*menu_sound* 0
	*menu_view* :nil
	*credits_view* :nil
	*credits_timer* 0
	*credits_exit_armed* :nil
	*scores_view* :nil
	*scores_timer* 0
	*scores_exit_armed* :nil
	*hiscore_view* :nil
	*hiscore_timer* 0
	*hiscore_exit_armed* :nil
	+menu_dif_names ''("EASY MODE" "HARD MODE")
	+menu_mode_names ''("AUTO MODE" "ASSIST MODE" "HUMAN MODE")
	+menu_sound_names ''("SOUND ON" "SOUND OFF")
	+at_smith `'(,+frm_32c32_smith ,+frm_32c32_smith1 ,+frm_32c32_smith2
				 ,+frm_32c32_smith1 ,+frm_32c32_smith -1))

(defclass Smith () (Sprite *img_frm_32c32* :nil 32 32 :32c32)
	; mr smith animated background face
	(.-> this
		(:sp_add_component (AT 4 +at_smith :nil))
		(:sp_set_frame +frm_32c32_smith)))

(defun game-mr-smiths ()
	; animate mr smith faces in *layer_bodies* behind transparent text overlay views
	(when (= (logand *game_frame_count* 3) 0)
		(defq sfx (Smith)
			sx (game-random (- +window_width 32))
			sy (game-random (- +window_height 32)))
		(. sfx :sp_set_pos sx sy)
		(. *layer_bodies* :add_front sfx)))

(defun menu-sfx (snd)
	(when (= *menu_sound* 0)
		(audio-play-rpc snd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 8x8 ascii text blitter helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-ascii-string (view txt x y col zoom tid)
	; (. view :ctx_blit tid col dx dy dw dh sx sy) using 8x8 white ascii texture atlas
	(each (# (defq ucode (code %0)
				idx (if (<= 32 ucode 95) (- ucode 32) 0)
				sy (* idx 8 zoom) gw (* 8 zoom) gh (* 8 zoom))
			(when (/= idx 0)
				(. view :ctx_blit tid col (* x zoom) (* y zoom) gw gh 0 sy))
			(++ x 8)) txt))

(defun draw-ascii-centered (view txt y col zoom tid)
	; center 8x8 text across unscaled view width
	(bind '(w &ignore) (. view :get_size))
	(defq unscaled_w (if (and zoom (> zoom 0)) (/ w zoom) 320)
		x (/ (- unscaled_w (* (length txt) 8)) 2))
	(draw-ascii-string view txt x y col zoom tid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; menu, credits, scores views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass MenuView () (View)
	; (MenuView) -> full-screen menu overlay view
	(def this :color 0)

	(defmethod :draw ()
		(defq zoom (get :zoom this) tid (get :tid this)
			selected (get :selected this)
			dif (get :difficulty this)
			mode (get :mode this)
			snd (get :sound this))
		(draw-ascii-centered this "MAIN MENU" 20 +argb_yellow zoom tid)
		(draw-ascii-centered this "START GAME" 44 (if (= selected 0) +argb_white +argb_red) zoom tid)
		(draw-ascii-centered this "LOAD GAME" 60 (if (= selected 1) +argb_white +argb_red) zoom tid)
		(draw-ascii-centered this (if (= dif 0) "EASY MODE" "HARD MODE") 76 (if (= selected 2) +argb_white +argb_red) zoom tid)
		(draw-ascii-centered this (case mode (0 "AUTO MODE") (1 "ASSIST MODE") (2 "HUMAN MODE")) 92 (if (= selected 3) +argb_white +argb_red) zoom tid)
		(draw-ascii-centered this (if (= snd 0) "SOUND ON" "SOUND OFF") 108 (if (= selected 4) +argb_white +argb_red) zoom tid)
		(draw-ascii-centered this "GAME CREDITS" 124 (if (= selected 5) +argb_white +argb_red) zoom tid)
		(draw-ascii-centered this "SELECT WITH UP DOWN FIRE" 144 +argb_cyan zoom tid)
		this)
	)

(defclass CreditsView () (View)
	; (CreditsView) -> full-screen credits overlay view
	(def this :color 0)

	(defmethod :draw ()
		(defq zoom (get :zoom this) tid (get :tid this))
		(draw-ascii-centered this "CREDITS" 16 +argb_cyan zoom tid)
		(draw-ascii-centered this "COPYRIGHT (C) 1989-2026 CHRIS HINSLEY" 32 +argb_white zoom tid)
		(draw-ascii-centered this "ALL RIGHTS RESERVED" 42 +argb_white zoom tid)
		(draw-ascii-centered this "GAME DESIGN AND CONCEPT" 58 +argb_green zoom tid)
		(draw-ascii-centered this "CHRIS HINSLEY AND NIGEL BROWNJOHN" 68 +argb_yellow zoom tid)
		(draw-ascii-centered this "PROGRAMMING" 82 +argb_green zoom tid)
		(draw-ascii-centered this "CHRIS HINSLEY" 92 +argb_yellow zoom tid)
		(draw-ascii-centered this "GRAPHICS" 106 +argb_green zoom tid)
		(draw-ascii-centered this "CHRIS HINSLEY AND NIGEL BROWNJOHN" 116 +argb_yellow zoom tid)
		(draw-ascii-centered this "SOUND AND MUSIC" 130 +argb_green zoom tid)
		(draw-ascii-centered this "CHRIS HINSLEY" 140 +argb_yellow zoom tid)
		(draw-ascii-centered this "PRESS FIRE TO RETURN" 154 +argb_cyan zoom tid)
		this)
	)

(defclass ScoresView () (View)
	; (ScoresView) -> full-screen hall of glory scores view
	(def this :color 0)

	(defmethod :draw ()
		(defq zoom (get :zoom this) tid (get :tid this)
			names (get :names this)
			values (get :values this)
			y 44)
		(draw-ascii-centered this "HALL OF GLORY" 28 +argb_red zoom tid)
		(each (#
			(draw-ascii-string this %0 72 y +argb_green zoom tid)
			(draw-ascii-string this (pad (str %1) 8 " ") 184 y +argb_yellow zoom tid)
			(setq y (+ y 8)))
			names values)
		(draw-ascii-centered this "PRESS FIRE TO RETURN" 136 +argb_cyan zoom tid)
		this)
	)

(defclass HiscoreView () (View)
	; (HiscoreView) -> full-screen glory attained message view
	(def this :color 0)

	(defmethod :draw ()
		(defq zoom (get :zoom this) tid (get :tid this)
			mode (get :mode this)
			score (get :score this)
			qualifies (get :qualifies this))
		(cond
			((= mode +mode_auto)
				(draw-ascii-centered this "AUTO MODE WILL NEVER ENABLE YOU" 76 +argb_white zoom tid)
				(draw-ascii-centered this "TO ENTER THE HISTORY BOOKS" 88 +argb_white zoom tid))
			(qualifies
				(draw-ascii-centered this "CONGRATULATIONS!" 76 +argb_yellow zoom tid)
				(draw-ascii-centered this "YOU HAVE ENTERED THE HISTORY BOOKS" 88 +argb_white zoom tid))
			(:t
				(draw-ascii-centered this "YOU HAVE NOT ATTAINED ENOUGH GLORY" 76 +argb_white zoom tid)
				(draw-ascii-centered this "TO ENTER THE HISTORY BOOKS" 88 +argb_white zoom tid)))
		(draw-ascii-centered this (cat "YOUR GLORY WAS " (str score)) 108 +argb_yellow zoom tid)
		(draw-ascii-centered this "PRESS FIRE TO CONTINUE" 144 +argb_cyan zoom tid)
		this)
	)

(defun menu-sync-view ()
	(when *menu_view*
		(def *menu_view*
			:selected *menu_selected*
			:difficulty *menu_difficulty*
			:mode *menu_mode*
			:sound *menu_sound*)
		(. *menu_view* :dirty)))

(defun menu-resize (zoom)
	(when (and *img_ascii* (defq c (getf *img_ascii* +canvas_texture 0)))
		(bind '(tid tw th) (texture-metrics c))
		(when *menu_view*
			(def *menu_view* :zoom zoom :tid tid)
			(. *menu_view* :change 0 0 (* zoom +screen_width) (* zoom +screen_height) :t))
		(when *credits_view*
			(def *credits_view* :zoom zoom :tid tid)
			(. *credits_view* :change 0 0 (* zoom +screen_width) (* zoom +screen_height) :t))
		(when *scores_view*
			(def *scores_view* :zoom zoom :tid tid
				:names *hiscore_names* :values *hiscore_values*)
			(. *scores_view* :change 0 0 (* zoom +screen_width) (* zoom +screen_height) :t))
		(when *hiscore_view*
			(def *hiscore_view* :zoom zoom :tid tid
				:mode *menu_mode* :score *man_score*)
			(. *hiscore_view* :change 0 0 (* zoom +screen_width) (* zoom +screen_height) :t))))

;;;;;;;;;;;;;;;;;;;;;;;
; menu state lifecycle
;;;;;;;;;;;;;;;;;;;;;;;

(defun menu-state-init ()
	(clear-sprite-layers)
	(set-world-layers-pos 0 0)
	(defq zoom *zoom*)
	(set-world-layers-size (* zoom +screen_width) (* zoom +screen_height))
	(. *layer_land* :clear)
	(. *layer_sky* :sky_black)
	(setq *menu_timer* 0 *menu_up_armed* :nil *menu_down_armed* :nil *menu_fire_armed* :nil)
	(unless *menu_view*
		(setq *menu_view* (MenuView)))
	(bind '(tid tw th) (texture-metrics (getf *img_ascii* +canvas_texture 0)))
	(def *menu_view* :tid tid :zoom zoom
		:selected *menu_selected*
		:difficulty *menu_difficulty*
		:mode *menu_mode*
		:sound *menu_sound*)
	(. *layer_fx* :add_front *menu_view*)
	(.-> *menu_view*
		(:change 0 0 (* zoom +screen_width) (* zoom +screen_height) :t))
	(. *menu_view* :dirty))

(defun menu-state-update ()
	(game-mr-smiths)
	(defq controls *game_controls*)
	; 1. Up navigation (armed on press down, triggered on key release)
	(if (bits? controls +fkey_up)
		(setq *menu_up_armed* :t
			  *menu_timer* 0)
		(when *menu_up_armed*
			(setq *menu_up_armed* :nil
				  *menu_selected* (if (= *menu_selected* 0) 5 (dec *menu_selected*)))
			(menu-sfx *sfx_clash1*)
			(menu-sync-view)))
	; 2. Down navigation (armed on press down, triggered on key release)
	(if (bits? controls +fkey_down)
		(setq *menu_down_armed* :t
			  *menu_timer* 0)
		(when *menu_down_armed*
			(setq *menu_down_armed* :nil
				  *menu_selected* (if (= *menu_selected* 5) 0 (inc *menu_selected*)))
			(menu-sfx *sfx_clash1*)
			(menu-sync-view)))
	; 3. Fire action (armed on press down, triggered on key release)
	(if (bits? controls +fkey_keya)
		(setq *menu_fire_armed* :t
			  *menu_timer* 0)
		(if *menu_fire_armed*
			(progn
				(setq *menu_fire_armed* :nil
					  *menu_timer* 0)
				(case *menu_selected*
					(+menu_start
						(. *menu_view* :sub)
						(goto-title +game_state_battle))
					(+menu_define
						(menu-sfx *sfx_spell*))
					(+menu_difficulty
						(setq *menu_difficulty* (logxor *menu_difficulty* 1))
						(menu-sfx *sfx_twang1*)
						(config-save)
						(menu-sync-view))
					(+menu_mode
						(setq *menu_mode* (% (inc *menu_mode*) 3)
							  *game_mode* *menu_mode*)
						(menu-sfx *sfx_twang1*)
						(config-save)
						(menu-sync-view))
					(+menu_sound
						(setq *menu_sound* (logxor *menu_sound* 1))
						(menu-sfx *sfx_twang1*)
						(config-save)
						(menu-sync-view))
					(+menu_credits
						(. *menu_view* :sub)
						(goto-title +game_state_credits))))
			(progn
				(++ *menu_timer*)
				(when (>= *menu_timer* 256)
					(. *menu_view* :sub)
					(if (and *best_demo_data* (> (length *best_demo_data*) 0))
						(goto-title +game_state_demo)
						(goto-title +game_state_credits)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; credits state lifecycle
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun credits-state-init ()
	(clear-sprite-layers)
	(set-world-layers-pos 0 0)
	(defq zoom *zoom*)
	(set-world-layers-size (* zoom +screen_width) (* zoom +screen_height))
	(. *layer_land* :clear)
	(. *layer_sky* :sky_black)
	(setq *credits_timer* 0 *credits_exit_armed* :nil)
	(unless *credits_view*
		(setq *credits_view* (CreditsView)))
	(bind '(tid tw th) (texture-metrics (getf *img_ascii* +canvas_texture 0)))
	(def *credits_view* :tid tid :zoom zoom)
	(. *layer_fx* :add_front *credits_view*)
	(.-> *credits_view*
		(:change 0 0 (* zoom +screen_width) (* zoom +screen_height) :t))
	(. *credits_view* :dirty))

(defun credits-state-update ()
	(game-mr-smiths)
	(defq controls *game_controls*)
	; armed on press down, triggered on key release
	(if (/= controls 0)
		(setq *credits_exit_armed* :t
			  *credits_timer* 0)
		(if *credits_exit_armed*
			(progn
				(setq *credits_exit_armed* :nil)
				(. *credits_view* :sub)
				(goto-title +game_state_menu))
			(progn
				(++ *credits_timer*)
				(when (>= *credits_timer* 256)
					(. *credits_view* :sub)
					(goto-title +game_state_scores))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
; scores state lifecycle
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scores-state-init ()
	(clear-sprite-layers)
	(set-world-layers-pos 0 0)
	(defq zoom *zoom*)
	(set-world-layers-size (* zoom +screen_width) (* zoom +screen_height))
	(. *layer_land* :clear)
	(. *layer_sky* :sky_black)
	(setq *scores_timer* 0 *scores_exit_armed* :nil)
	(unless *scores_view*
		(setq *scores_view* (ScoresView)))
	(bind '(tid tw th) (texture-metrics (getf *img_ascii* +canvas_texture 0)))
	(def *scores_view* :tid tid :zoom zoom
		:names *hiscore_names* :values *hiscore_values*)
	(. *layer_fx* :add_front *scores_view*)
	(.-> *scores_view*
		(:change 0 0 (* zoom +screen_width) (* zoom +screen_height) :t))
	(. *scores_view* :dirty))

(defun scores-state-update ()
	(game-mr-smiths)
	(defq controls *game_controls*)
	(if (/= controls 0)
		(setq *scores_exit_armed* :t
			  *scores_timer* 0)
		(if *scores_exit_armed*
			(progn
				(setq *scores_exit_armed* :nil)
				(. *scores_view* :sub)
				(goto-title +game_state_menu))
			(progn
				(++ *scores_timer*)
				(when (>= *scores_timer* 256)
					(. *scores_view* :sub)
					(goto-title +game_state_menu))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; hiscore state lifecycle
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hiscore-state-init ()
	(clear-sprite-layers)
	(set-world-layers-pos 0 0)
	(defq zoom *zoom*)
	(set-world-layers-size (* zoom +screen_width) (* zoom +screen_height))
	(. *layer_land* :clear)
	(. *layer_sky* :sky_black)
	(setq *hiscore_timer* 0 *hiscore_exit_armed* :nil)
	; check if player qualifies for high score list
	(defq qualifies :nil)
	(when (and (/= *menu_mode* +mode_auto) *hiscore_values*)
		(defq insert_idx -1 i 0)
		(while (and (< i (length *hiscore_values*)) (= insert_idx -1))
			(when (> *man_score* (elem-get *hiscore_values* i))
				(setq insert_idx i))
			(++ i))
		(when (/= insert_idx -1)
			(setq qualifies :t)
			(setq *hiscore_names* (slice (cat (slice *hiscore_names* 0 insert_idx) (list "FANATIC(YOU)") (slice *hiscore_names* insert_idx -1)) 0 10)
				  *hiscore_values* (slice (cat (slice *hiscore_values* 0 insert_idx) (list *man_score*) (slice *hiscore_values* insert_idx -1)) 0 10))
			(config-save)))
	(unless *hiscore_view*
		(setq *hiscore_view* (HiscoreView)))
	(bind '(tid tw th) (texture-metrics (getf *img_ascii* +canvas_texture 0)))
	(def *hiscore_view* :tid tid :zoom zoom
		:mode *menu_mode* :score *man_score* :qualifies qualifies)
	(. *layer_fx* :add_front *hiscore_view*)
	(.-> *hiscore_view*
		(:change 0 0 (* zoom +screen_width) (* zoom +screen_height) :t))
	(. *hiscore_view* :dirty))

(defun hiscore-state-update ()
	(game-mr-smiths)
	(defq controls *game_controls*)
	(if (/= controls 0)
		(setq *hiscore_exit_armed* :t
			  *hiscore_timer* 0)
		(if *hiscore_exit_armed*
			(progn
				(setq *hiscore_exit_armed* :nil)
				(. *hiscore_view* :sub)
				(goto-title +game_state_scores))
			(progn
				(++ *hiscore_timer*)
				(when (>= *hiscore_timer* 256)
					(. *hiscore_view* :sub)
					(goto-title +game_state_scores))))))

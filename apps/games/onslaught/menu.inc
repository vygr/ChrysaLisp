;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught main menu and credits state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *menu_selected* +menu_start *menu_timer* 0 *menu_up_armed* :nil
	*menu_down_armed* :nil *menu_fire_armed* :nil *menu_difficulty* 0
	*menu_mode* +mode_auto *menu_view* :nil
	*credits_view* :nil *credits_timer* 0 *credits_exit_armed* :nil
	*scores_view* :nil *scores_timer* 0 *scores_exit_armed* :nil
	*hiscore_view* :nil *hiscore_timer* 0 *hiscore_exit_armed* :nil
	*hiscore_entry* :nil *hiscore_text* 0 *hiscore_initials* "" *hiscore_sight* :nil
	; letter picker, [ is backspace, ] is enter
	+hiscore_letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ(). ?[]" +hiscore_pick 16
	+menu_dif_names ''("EASY MODE" "HARD MODE")
	+menu_mode_names ''("AUTO MODE" "ASSIST MODE" "HUMAN MODE")
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
	(draw-ascii-string view txt (/ (- (/ w zoom) (* (length txt) 8)) 2) y col zoom tid))

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
			mode (get :mode this))
		(draw-ascii-centered this "MAIN MENU" 20 +argb_yellow zoom tid)
		(draw-ascii-centered this "START GAME" 44 (if (= selected 0) +argb_white +argb_red) zoom tid)
		(draw-ascii-centered this "LOAD GAME" 60 (if (= selected 1) +argb_white +argb_red) zoom tid)
		(draw-ascii-centered this (elem-get +menu_dif_names dif) 76 (if (= selected 2) +argb_white +argb_red) zoom tid)
		(draw-ascii-centered this (elem-get +menu_mode_names mode) 92 (if (= selected 3) +argb_white +argb_red) zoom tid)
		(draw-ascii-centered this "GAME CREDITS" 108 (if (= selected 4) +argb_white +argb_red) zoom tid)
		(draw-ascii-centered this "SELECT WITH UP DOWN FIRE" 128 +argb_cyan zoom tid)
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
	; (HiscoreView) -> full-screen glory attained message and name entry view
	(def this :color 0)

	(defmethod :draw ()
		(defq zoom (get :zoom this) tid (get :tid this))
		(cond
			((defq letters (get :letters this))
				;enter initials, a 3 by 11 grid of letters under the sight
				(draw-ascii-string this "PLEASE ENTER INITIALS" 72 52 +argb_white zoom tid)
				(draw-ascii-string this "(   )" 136 68 +argb_green zoom tid)
				(draw-ascii-string this (get :initials this) 144 68 +argb_cyan zoom tid)
				(each (# (draw-ascii-string this %0 (+ 72 (* (% (!) 11) 16))
						(+ 84 (* (/ (!) 11) 16)) +argb_white zoom tid))
					letters))
			(:t (draw-ascii-centered this (if (= (get :mode this) +mode_auto)
						"AUTO MODE WILL NEVER ENABLE YOU" "YOU HAVE NOT ATTAINED ENOUGH GLORY")
					68 +argb_white zoom tid)
				(draw-ascii-centered this "TO ENTER THE HISTORY BOOKS" 76 +argb_white zoom tid)
				(draw-ascii-centered this (cat "YOUR GLORY WAS " (str (get :score this)))
					92 +argb_yellow zoom tid)))
		this)
	)

(defun menu-sync-view ()
	(when *menu_view*
		(def *menu_view*
			:selected *menu_selected*
			:difficulty *menu_difficulty*
			:mode *menu_mode*)
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
			(def *hiscore_view* :zoom zoom :tid tid)
			(. *hiscore_view* :change 0 0 (* zoom +screen_width) (* zoom +screen_height) :t))))

;;;;;;;;;;;;;;;;;;;;;;;
; menu state lifecycle
;;;;;;;;;;;;;;;;;;;;;;;

(defun menu-state-init ()
	(demo-restore-settings)
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
		:mode *menu_mode*)
	(. *layer_fx* :add_front *menu_view*)
	(.-> *menu_view*
		(:change 0 0 (* zoom +screen_width) (* zoom +screen_height) :t))
	(. *menu_view* :dirty))

(defun menu-state-update ()
	(game-mr-smiths)
	(defq controls *game_controls*)
	; 1. Up navigation (armed on press down, triggered on key release)
	(if (bits? controls +fkey_up)
		(setq *menu_up_armed* :t *menu_timer* 0)
		(when *menu_up_armed*
			(setq *menu_up_armed* :nil
				*menu_selected* (if (= *menu_selected* 0) +menu_credits (dec *menu_selected*)))
			(play-sfx *sfx_clash1*)
			(menu-sync-view)))
	; 2. Down navigation (armed on press down, triggered on key release)
	(if (bits? controls +fkey_down)
		(setq *menu_down_armed* :t *menu_timer* 0)
		(when *menu_down_armed*
			(setq *menu_down_armed* :nil
				*menu_selected* (if (= *menu_selected* +menu_credits) 0 (inc *menu_selected*)))
			(play-sfx *sfx_clash1*)
			(menu-sync-view)))
	; 3. Fire action (armed on press down, triggered on key release)
	(if (bits? controls +fkey_keya)
		(setq *menu_fire_armed* :t *menu_timer* 0)
		(if *menu_fire_armed*
			(progn
				(setq *menu_fire_armed* :nil *menu_timer* 0)
				(case *menu_selected*
					(+menu_start
						(. *menu_view* :sub)
						;new campaign, map-state-init generates the player info
						(setq *campaign_active* :nil *game_state* +game_state_map))
					(+menu_define
						;load game, map-state-init restores the saved campaign
						(ifn *campaign_state* (play-sfx *sfx_out*)
							(. *menu_view* :sub)
							(play-sfx *sfx_spell*)
							(setq *campaign_active* :load *game_state* +game_state_map)))
					(+menu_difficulty
						(setq *menu_difficulty* (logxor *menu_difficulty* 1))
						(play-sfx *sfx_twang1*)
						(config-save)
						(menu-sync-view))
					(+menu_mode
						(setq *menu_mode* (% (inc *menu_mode*) 3)
							*game_mode* *menu_mode*)
						(play-sfx *sfx_twang1*)
						(config-save)
						(menu-sync-view))
					(+menu_credits
						(. *menu_view* :sub)
						(goto-title +game_state_credits))))
			(++ *menu_timer*)
			(when (>= *menu_timer* 256)
				(. *menu_view* :sub)
				(goto-title +game_state_credits)))))

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
		(setq *credits_exit_armed* :t *credits_timer* 0)
		(if *credits_exit_armed*
			(progn
				(setq *credits_exit_armed* :nil)
				(. *credits_view* :sub)
				(goto-title +game_state_menu))
			(++ *credits_timer*)
			(when (>= *credits_timer* 256)
				(. *credits_view* :sub)
				(goto-title +game_state_scores)))))

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
		(setq *scores_exit_armed* :t *scores_timer* 0)
		(if *scores_exit_armed*
			(progn
				(setq *scores_exit_armed* :nil)
				(. *scores_view* :sub)
				(goto-title +game_state_menu))
			(++ *scores_timer*)
			(when (>= *scores_timer* 256)
				(. *scores_view* :sub)
				(if (and *best_demo_data* (nempty? *best_demo_data*))
					(goto-title +game_state_demo)
					(goto-title +game_state_menu))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; hiscore state lifecycle
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hiscore-sync-view ()
	; the letters rotate under the fixed sight
	(def *hiscore_view* :mode *menu_mode* :score *man_score*
		:initials *hiscore_initials*
		:letters (if *hiscore_entry*
			(cat (slice +hiscore_letters *hiscore_text* -1)
				(slice +hiscore_letters 0 *hiscore_text*)))))

(defun hiscore-insert ()
	; add the generated name and entered initials to the hall of glory
	(defq idx 0 name (cat *man_name* (slice "       " (length *man_name*) -1)
		"(" *hiscore_initials* (slice "   " (length *hiscore_initials*) -1) ")"))
	(while (<= *man_score* (elem-get *hiscore_values* idx)) (++ idx))
	(setq *hiscore_names* (slice (cat (slice *hiscore_names* 0 idx) (list name)
			(slice *hiscore_names* idx -1)) 0 10)
		*hiscore_values* (slice (cat (slice *hiscore_values* 0 idx) (list *man_score*)
			(slice *hiscore_values* idx -1)) 0 10))
	(config-save))

(defun hiscore-state-init ()
	(clear-sprite-layers)
	(set-world-layers-pos 0 0)
	(defq zoom *zoom*)
	(set-world-layers-size (* zoom +screen_width) (* zoom +screen_height))
	(. *layer_land* :clear)
	(. *layer_sky* :sky_black)
	;no glory in auto mode, or if not enough to beat the lowest
	(setq *hiscore_timer* 0 *hiscore_exit_armed* :nil *hiscore_text* 0
		*hiscore_initials* "" *game_flags* (logior *game_flags* +fflag_select)
		*hiscore_entry* (and (/= *menu_mode* +mode_auto)
			(> *man_score* (last *hiscore_values*))))
	(unless *hiscore_view*
		(setq *hiscore_view* (HiscoreView)))
	(bind '(tid tw th) (texture-metrics (getf *img_ascii* +canvas_texture 0)))
	(def *hiscore_view* :tid tid :zoom zoom)
	(hiscore-sync-view)
	(. *layer_fx* :add_front *hiscore_view*)
	(. *hiscore_view* :change 0 0 (* zoom +screen_width) (* zoom +screen_height) :t)
	(when *hiscore_entry*
		(.-> (setq *hiscore_sight* (Sprite *img_frm_16x16* :nil 16 16 :16x16))
			(:sp_set_frame +frm_16x16_sight)
			(:sp_set_pos (const (- (/ +window_width 2) 12)) (const (+ (/ +window_height 2) 12))))
		(. *layer_fx* :add_front *hiscore_sight*))
	(. *hiscore_view* :dirty))

(defun hiscore-entry-update ()
	; left and right spin the letters, fire picks the one under the sight
	(if (= (defq controls *game_controls*) 0)
		(setq *game_flags* (logand *game_flags* (lognot +fflag_select)))
		(unless (bits? *game_flags* +fflag_select)
			(setq *game_flags* (logior *game_flags* +fflag_select))
			(defq num (length +hiscore_letters))
			(cond
				((bits? controls +fkey_left)
					(setq *hiscore_text* (% (+ *hiscore_text* (dec num)) num)))
				((bits? controls +fkey_right)
					(setq *hiscore_text* (% (inc *hiscore_text*) num)))
				((bits? controls +fkey_keya)
					(dt-exp-fx *hiscore_sight*)
					(defq letter (elem-get +hiscore_letters
						(% (+ *hiscore_text* +hiscore_pick) num)))
					(cond
						((eql letter "[")
							(setq *hiscore_initials* (slice *hiscore_initials* 0
								(max 0 (dec (length *hiscore_initials*))))))
						((eql letter "]")
							(hiscore-insert)
							(. *hiscore_view* :sub)
							(goto-title +game_state_scores))
						((< (length *hiscore_initials*) 3)
							(setq *hiscore_initials* (cat *hiscore_initials* letter))))))
			(hiscore-sync-view))))

(defun hiscore-state-update ()
	(game-mr-smiths)
	(if *hiscore_entry* (hiscore-entry-update)
		(if (/= *game_controls* 0)
			(setq *hiscore_exit_armed* :t *hiscore_timer* 0)
			(when (or *hiscore_exit_armed* (>= (++ *hiscore_timer*) 256))
				(setq *hiscore_exit_armed* :nil)
				(. *hiscore_view* :sub)
				(goto-title +game_state_scores)))))

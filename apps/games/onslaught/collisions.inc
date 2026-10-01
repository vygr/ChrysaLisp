;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught collision handlers & tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *man_power* +max_power *man_strength* +max_strength *man_score* 0)

(defq +score_table
	''(50 5 10 15 20 10 10 10 25 25 80 5 50 10 50 20 1 0 0))

(defun play-death-score (node)
	(when (defq type (get :sp_type node))
		(when (/= type 0)
			(defq idx (log2 type))
			(when (< -1 idx (length +score_table))
				(setq *man_score* (+ *man_score* (elem-get +score_table idx)))))))

; collision handlers for player and weapons

(defun cl-man-hits-enemy (this hit)
	; player man colliding with enemy infantry / knight / skeleton
	(defq frame (get :sp_frame this)
		mv (elem-get (get :sp_components hit) +en_comp_mv))
	(when (or (<= +frm_fanatic_l_climb frame (+ +frm_fanatic_l_climb 3))
			  (<= (abs (- (+ (get :sp_y this) (get :sp_h this))
						  (+ (get :sp_y hit) (get :sp_h hit)))) 4))
		(when (and mv (= (get :ay mv) 0))
			(def hit :sp_flags (logior (get :sp_flags hit) +fsp_collide))
			(set mv :vx 0))))

(defun cl-man-hits-dragging-enemy (this hit)
	; player man hit by heavy dragging enemy (horse, tower, carpet, etc.)
	(defq hit_type (get :sp_type hit)
		hit_pnt (get :sp_hitpnt hit)
		drag_xv 0)
	(each (lambda (comp)
		(when (and (env? comp) (def? :vx comp))
			(setq drag_xv (get :vx comp))))
		(get :sp_components hit))
	(if (bits? hit_type +ftp_tower +ftp_carpet +ftp_skeleton_horse)
		(setq *man_power* (max 0 (- *man_power* (+ (>> hit_pnt 2) 1))))
		(setq *man_strength* (max 0 (- *man_strength* (+ (>> hit_pnt 1) 1)))))
	(when (/= drag_xv 0)
		(bind '(x y) (. this :sp_get_pos))
		(setq x (max 0 (min (- (* +map_width +tile_width) 32) (+ x (/ drag_xv 2)))))
		(. this :sp_set_pos x y))
	(if (or (<= *man_strength* 0) (<= *man_power* 0))
		(. this :sp_kill)))

(defun cl-man-hits-monk (this hit)
	; monk drains player strength and detonates
	(setq *man_strength* (max 0 (- *man_strength* 200)))
	(audio-play-rpc *sfx_death1*)
	(. hit :sp_kill)
	(if (or (<= *man_strength* 0) (<= *man_power* 0))
		(. this :sp_kill)))

(defun cl-man-hits (this hit)
	; general hit by missile / enemy fire / mine
	(defq hit_frame (get :sp_frame hit))
	(cond
		((and (or (= hit_frame +frm_16x16_mine) (= hit_frame (+ +frm_16x16_mine 1)))
			  (/= (+ (get :sp_y this) (get :sp_h this))
				  (+ (get :sp_y hit) (get :sp_h hit))))
			; jumped over mine safely
			:nil)
		(:t
			(defq hit_pnt (get :sp_hitpnt hit)
				death_hook (get :sp_death hit)
				dir (get :sp_dir hit))
			(. hit :sp_kill)
			(if (<= (+ +frm_16x16_wizshot 2) hit_frame (+ +frm_16x16_wizshot 3))
				; wizard power shot drains player power level
				(setq *man_power* (max 0 (- *man_power* hit_pnt)))
				; other missiles / strength shot / mine drain strength level
				(progn
					(setq *man_strength* (max 0 (- *man_strength* hit_pnt)))
					(unless death_hook
						; projectile had no death explosion: silent directional blood spray
						(bind '(mx my mw mh) (. this :sp_get_bounds))
						(defq bx (- (+ mx (/ mw 2)) 8)
							by (- (+ my (/ mh 2)) 8)
							spray (Blood bx by dir))
						(. *layer_fx* :add_front spray))))
			(if (or (<= *man_strength* 0) (<= *man_power* 0))
				(. this :sp_kill)))))

(defun cl-man-hits-banner (this hit)
	; touching player banner recovers power to at least half
	(when (< *man_power* (/ +max_power 2))
		(setq *man_power* (/ +max_power 2))))

(defun cl-man-hits-enemy-banner (this hit)
	; capturing enemy banner
	(when (<= (abs (- (+ (get :sp_y this) (get :sp_h this))
					  (+ (get :sp_y hit) (get :sp_h hit)))) 4)
		(audio-play-rpc *sfx_spell*)
		(setq *enemy_banner_captured* :t)
		(. hit :sp_kill)
		(setq *man_score* (+ *man_score* 500))
		(setq *man_power* (min +max_power (* *man_power* 2)))
		(if (= *game_state* +game_state_demo)
			(progn
				(setq *playback_stream* :nil *game_controls* 0)
				(goto-title +game_state_menu))
			(record-battle-finish)
			(setq *game_state* +game_state_battle_won))))

(defun cl-enemy-addon-hits-man (this hit)
	; enemy weapon addon hitting player man: play clash, spawn sparks, deduct strength
	(unless (get :has_hit this)
		(def this :has_hit :t)
		(defq hit_pnt (get :sp_hitpnt this))
		(setq *man_strength* (max 0 (- *man_strength* hit_pnt)))
		(audio-play-rpc (if (= (game-random 2) 0) *sfx_clash1* *sfx_clash2*))
		(bind '(ax ay aw ah) (. this :sp_get_bounds))
		(spawn-fizz-fx (+ ax (/ aw 2)) (+ ay (/ ah 2)))
		(if (or (<= *man_strength* 0) (<= *man_power* 0))
			(. hit :sp_kill))))

(defun cl-man-missile-hits (this hit)
	; player missile / arrow / fireball hitting target enemy or missile
	(audio-play-rpc *sfx_clash2*)
	(defq hit_type (get :sp_type hit)
		non_flesh (logior +ftp_enemy_fire +ftp_cannon +ftp_oil +ftp_balista
						  +ftp_tower +ftp_carpet +ftp_skeleton_monk
						  +ftp_skeleton_horse +ftp_skeleton))
	(bind '(mx my mw mh) (. this :sp_get_bounds))
	(if (/= (logand hit_type non_flesh) 0)
		(spawn-fizz-fx (+ mx (/ mw 2)) (+ my (/ mh 2)))
		(defq dir (get :sp_dir this)
			spray (Blood (- (+ mx (/ mw 2)) 8) (- (+ my (/ mh 2)) 8) dir))
		(. *layer_fx* :add_front spray))
	(defq missile_hp (get :sp_hitpnt this)
		enemy_hp (get :sp_hitpnt hit)
		new_missile_hp (- missile_hp enemy_hp)
		new_enemy_hp (- enemy_hp missile_hp))
	(if (<= new_missile_hp 0)
		(. this :sp_kill)
		(def this :sp_hitpnt new_missile_hp))
	(if (> new_enemy_hp 0)
		(def hit :sp_hitpnt new_enemy_hp)
		(play-death-score hit)
		(when (and (>= missile_hp 500) (eql (get :sp_death hit) dt-kill-enemy))
			(def hit :sp_death dt-kill-zap))
		(. hit :sp_kill)))

(defun cl-man-addon-hits (this hit)
	; player weapon mase hitting target enemy or missile
	(audio-play-rpc (if (= (game-random 2) 0) *sfx_clash1* *sfx_clash2*))
	(defq hit_type (get :sp_type hit)
		non_flesh (logior +ftp_enemy_fire +ftp_cannon +ftp_oil +ftp_balista
						  +ftp_tower +ftp_carpet +ftp_skeleton_monk
						  +ftp_skeleton_horse +ftp_skeleton))
	(bind '(mx my mw mh) (. this :sp_get_bounds))
	(if (/= (logand hit_type non_flesh) 0)
		(spawn-fizz-fx (+ mx (/ mw 2)) (+ my (/ mh 2)))
		(defq dir (get :sp_dir this)
			spray (Blood (- (+ mx (/ mw 2)) 8) (- (+ my (/ mh 2)) 8) dir))
		(. *layer_fx* :add_front spray))
	(defq dmg (get :sp_hitpnt this)
		hp (- (get :sp_hitpnt hit) dmg))
	(if (> hp 0)
		(def hit :sp_hitpnt hp)
		(play-death-score hit)
		(when (and (>= dmg 500) (eql (get :sp_death hit) dt-kill-enemy))
			(def hit :sp_death dt-kill-zap))
		(. hit :sp_kill)))

(defun collect-item (item)
	; collect item into inventory, topping out usage or filling first free slot
	(defq frame (get :sp_frame item)
		uses (get :sp_hitpnt item)
		slot -1
		empty_slot -1
		idx 7)
	(while (>= idx 0)
		(defq cur (elem-get *item_inventory* idx))
		(cond
			((= cur frame) (setq slot idx))
			((= cur 0) (setq empty_slot idx)))
		(-- idx))
	(defq target_slot (if (/= slot -1) slot empty_slot))
	(when (/= target_slot -1)
		(elem-set *item_inventory* target_slot frame)
		(elem-set *item_usage* target_slot (max (elem-get *item_usage* target_slot) uses))
		(audio-play-rpc *sfx_clash2*)
		(. item :sp_kill)))

(defun cl-man-hits-item (this hit)
	; player collecting an item on touch in auto mode, or ducking/down key in assist / human mode
	(when (or (= *game_mode* +mode_auto)
			  (= (get :sp_h this) 16)
			  (bits? *game_controls* +fkey_down))
		(collect-item hit)))

; collision dispatch tables indexed by (log2 type)

(defq *man_collision_table*
	(list
		(const cl-man-hits-dragging-enemy)	; 0: horse
		:nil								; 1: spearman
		:nil								; 2: wizard
		(const cl-man-hits-enemy)			; 3: footman
		(const cl-man-hits-enemy)			; 4: knight
		(const cl-man-hits)					; 5: balista
		(const cl-man-hits)					; 6: cannon
		(const cl-man-hits)					; 7: oil
		(const cl-man-hits-dragging-enemy)	; 8: boarrider
		(const cl-man-hits-dragging-enemy)	; 9: carpet
		(const cl-man-hits-dragging-enemy)	; 10: tower
		(const cl-man-hits-monk)			; 11: monk
		(const cl-man-hits)					; 12: beserk
		(const cl-man-hits-monk)			; 13: skeleton_monk
		(const cl-man-hits-dragging-enemy)	; 14: skeleton_horse
		(const cl-man-hits-enemy)			; 15: skeleton
		(const cl-man-hits)					; 16: enemy_fire
		(const cl-man-hits-item)			; 17: item
		(const cl-enemy-addon-hits-man)		; 18: man
		(const cl-man-hits-banner)			; 19: man_banner
		(const cl-man-hits-enemy-banner))	; 20: enemy_banner
	*man_missile_collision_table* (const cl-man-missile-hits)
	*man_addon_collision_table* (const cl-man-addon-hits))
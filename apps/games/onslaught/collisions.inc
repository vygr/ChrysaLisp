;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught collision handlers & tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *man_power* +max_power *man_strength* +max_strength *man_score* 0)

; collision handlers for player and weapons

(defun cl-man-hits-enemy (this hit)
	; player man colliding with enemy infantry / knight / skeleton
	(defq frame (get :sp_frame this)
		mv (elem-get (get :sp_components hit) +en_comp_mv))
	(when (or (<= +frm_fanatic_l_climb frame (+ +frm_fanatic_l_climb 3))
			  (<= (abs (- (+ (get :sp_y this) (get :sp_h this))
						  (+ (get :sp_y hit) (get :sp_h hit)))) 4))
		(when (and mv (= (get :ay mv) 0))
			(def hit :fsp_collide :t)
			(set mv :vx 0))))

(defun cl-man-hits-dragging-enemy (this hit)
	; player man hit by heavy dragging enemy (horse, tower, carpet, etc.)
	(defq hit_type (or (get :sp_type hit) 0)
		hit_pnt (or (get :sp_hitpnt hit) 10)
		drag_xv 0)
	(each (lambda (comp)
		(when (and (env? comp) (def? :vx comp))
			(setq drag_xv (get :vx comp))))
		(get :sp_components hit))
	(if (/= (logand hit_type (logior +ftp_tower +ftp_carpet +ftp_skeleton_horse)) 0)
		(setq *man_power* (max 0 (- *man_power* (+ (>> hit_pnt 2) 1))))
		(setq *man_strength* (max 0 (- *man_strength* (+ (>> hit_pnt 1) 1)))))
	(when (/= drag_xv 0)
		(bind '(x y) (. this :sp_get_pos))
		(setq x (max 0 (min (- (* +map_width +tile_width) 32) (+ x (/ drag_xv 2)))))
		(. this :sp_set_pos x y)))

(defun cl-man-hits-monk (this hit)
	; monk drains player strength and detonates
	(setq *man_strength* (max 0 (- *man_strength* 200)))
	(audio-play-rpc *sfx_death1*)
	(. hit :sp_kill))

(defun cl-man-hits (this hit)
	; general hit by missile / enemy fire / mine
	(defq hit_frame (or (get :sp_frame hit) 0))
	(cond
		((and (or (= hit_frame +frm_16x16_mine) (= hit_frame (+ +frm_16x16_mine 1)))
			  (/= (+ (get :sp_y this) (get :sp_h this))
				  (+ (get :sp_y hit) (get :sp_h hit))))
			; jumped over mine safely
			:nil)
		(:t
			(defq hit_pnt (or (get :sp_hitpnt hit) 10)
				death_hook (get :sp_death hit)
				dir (or (get :sp_dir hit) 1))
			(setq *man_strength* (max 0 (- *man_strength* hit_pnt)))
			(. hit :sp_kill)
			(unless death_hook
				; projectile had no death explosion: silent directional blood spray
				(bind '(mx my mw mh) (. this :sp_get_bounds))
				(defq bx (- (+ mx (/ mw 2)) 8)
					by (- (+ my (/ mh 2)) 8)
					spray (Blood bx by dir))
				(. *layer_fx* :add_front spray))
			(if (<= *man_strength* 0)
				(. this :sp_kill)))))

(defun cl-man-hits-banner (this hit)
	; touching player banner recovers power to at least half
	(when (< *man_power* (/ +max_power 2))
		(setq *man_power* (/ +max_power 2))))

(defun cl-man-hits-enemy-banner (this hit)
	; capturing enemy banner
	(when (= (+ (get :sp_y this) (get :sp_h this))
			 (+ (get :sp_y hit) (get :sp_h hit)))
		(audio-play-rpc *sfx_spell*)
		(. hit :sp_kill)
		(setq *man_power* (min +max_power (* *man_power* 2)))))

(defun cl-enemy-addon-hits-man (this hit)
	; enemy weapon addon hitting player man: play clash, spawn sparks, deduct strength
	(unless (get :has_hit this)
		(def this :has_hit :t)
		(defq hit_pnt (or (get :sp_hitpnt this) 8))
		(setq *man_strength* (max 0 (- *man_strength* hit_pnt)))
		(audio-play-rpc (if (= (random 2) 0) *sfx_clash1* *sfx_clash2*))
		(bind '(ax ay aw ah) (. this :sp_get_bounds))
		(spawn-fizz-fx (+ ax (/ aw 2)) (+ ay (/ ah 2)))
		(if (<= *man_strength* 0)
			(. hit :sp_kill))))

(defun cl-man-missile-hits (this hit)
	; player missile / arrow / fireball hitting target enemy or missile
	(audio-play-rpc *sfx_clash2*)
	(defq hit_type (or (get :sp_type hit) 0)
		non_flesh (logior +ftp_enemy_fire +ftp_cannon +ftp_oil +ftp_balista
						  +ftp_tower +ftp_carpet +ftp_skeleton_monk
						  +ftp_skeleton_horse +ftp_skeleton))
	(bind '(mx my mw mh) (. this :sp_get_bounds))
	(if (/= (logand hit_type non_flesh) 0)
		(spawn-fizz-fx (+ mx (/ mw 2)) (+ my (/ mh 2)))
		(defq dir (or (get :sp_dir this) (neg (or (get :sp_dir hit) 1)))
			spray (Blood (- (+ mx (/ mw 2)) 8) (- (+ my (/ mh 2)) 8) dir))
		(. *layer_fx* :add_front spray))
	(defq missile_hp (or (get :sp_hitpnt this) 1)
		enemy_hp (or (get :sp_hitpnt hit) 1)
		new_missile_hp (- missile_hp enemy_hp)
		new_enemy_hp (- enemy_hp missile_hp))
	(if (<= new_missile_hp 0)
		(. this :sp_kill)
		(def this :sp_hitpnt new_missile_hp))
	(if (<= new_enemy_hp 0)
		(progn
			(when (>= missile_hp 500)
				(when (eql (get :sp_death hit) dt-kill-enemy)
					(def hit :sp_death dt-kill-zap)))
			(. hit :sp_kill))
		(def hit :sp_hitpnt new_enemy_hp)))

(defun cl-man-addon-hits (this hit)
	; player weapon mase hitting target enemy or missile
	(audio-play-rpc (if (= (random 2) 0) *sfx_clash1* *sfx_clash2*))
	(defq hit_type (or (get :sp_type hit) 0)
		non_flesh (logior +ftp_enemy_fire +ftp_cannon +ftp_oil +ftp_balista
						  +ftp_tower +ftp_carpet +ftp_skeleton_monk
						  +ftp_skeleton_horse +ftp_skeleton))
	(bind '(mx my mw mh) (. this :sp_get_bounds))
	(if (/= (logand hit_type non_flesh) 0)
		(spawn-fizz-fx (+ mx (/ mw 2)) (+ my (/ mh 2)))
		(defq dir (or (get :sp_dir this) (neg (or (get :sp_dir hit) 1)))
			spray (Blood (- (+ mx (/ mw 2)) 8) (- (+ my (/ mh 2)) 8) dir))
		(. *layer_fx* :add_front spray))
	(defq dmg (or (get :sp_hitpnt this) 1)
		hp (- (or (get :sp_hitpnt hit) 1) dmg))
	(if (<= hp 0)
		(progn
			(when (>= dmg 500)
				(when (eql (get :sp_death hit) dt-kill-enemy)
					(def hit :sp_death dt-kill-zap)))
			(. hit :sp_kill))
		(def hit :sp_hitpnt hp)))

(defun collect-item (item)
	; collect item into inventory, updating usage or filling first free slot
	(defq frame (or (get :sp_frame item) 0)
		uses (or (get :sp_hitpnt item) 1)
		slot -1
		empty_slot -1
		idx 7)
	(while (>= idx 0)
		(defq cur (elem-get *item_inventory* idx))
		(cond
			((= cur frame) (setq slot idx))
			((= cur 0) (setq empty_slot idx)))
		(-- idx))
	(if (/= slot -1)
		(elem-set *item_usage* slot (+ (elem-get *item_usage* slot) uses))
		(when (/= empty_slot -1)
			(elem-set *item_inventory* empty_slot frame)
			(elem-set *item_usage* empty_slot uses)))
	(audio-play-rpc *sfx_clash2*)
	(. item :sp_kill))

(defun cl-man-hits-item (this hit)
	; player collecting an item on touch
	(collect-item hit))

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
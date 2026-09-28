;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/onslaught/collisions.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught collision handlers & tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *man_power* +max_power
	*man_strength* +max_strength)

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
	(defq hit_pnt (or (get :sp_hitpnt hit) 10))
	(setq *man_strength* (max 0 (- *man_strength* hit_pnt)))
	(audio-play-rpc *sfx_explode*)
	(. hit :sp_kill)
	(if (<= *man_strength* 0)
		(. this :sp_kill)))

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
	; enemy weapon addon hitting player man
	(defq hit_pnt (or (get :sp_hitpnt hit) 10))
	(setq *man_strength* (max 0 (- *man_strength* hit_pnt)))
	(audio-play-rpc *sfx_clash1*)
	(if (<= *man_strength* 0)
		(. this :sp_kill)))

(defun cl-man-missile-hits (this hit)
	; player missile / arrow hitting target
	(audio-play-rpc *sfx_clash2*)
	(. hit :sp_kill)
	(. this :sp_kill))

(defun cl-man-addon-hits (this hit)
	; player weapon mase hitting target
	(audio-play-rpc *sfx_clash1*)
	(. hit :sp_kill))

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
		:nil								; 17: item
		(const cl-enemy-addon-hits-man)		; 18: man
		(const cl-man-hits-banner)			; 19: man_banner
		(const cl-man-hits-enemy-banner))	; 20: enemy_banner
	*man_missile_collision_table* (const cl-man-missile-hits)
	*man_addon_collision_table* (const cl-man-addon-hits))
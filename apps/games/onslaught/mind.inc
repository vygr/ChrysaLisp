;;;;;;;;;;;;;;;;;;;;;;;
; onslaught mind combat
;;;;;;;;;;;;;;;;;;;;;;;

(defq +mind_y (- (/ +window_height 2) 16) +mind_fire_max 3
	*mind_count* 0 *mind_firecnt* 0 *mind_itemindex* 0 *mind_power* 0
	*mind_map* :nil *mind_arms* :nil *mind_sprite* :nil *mind_hand* :nil
	+at_mind_arm `'(,+frm_16x16_segments ,+frm_16x16_segments ,+frm_16x16_segments
		,+frm_16x16_segments ,+frm_16x16_segments ,+frm_16x16_segments
		,+frm_16x16_segments ,+frm_16x16_segments ,+frm_16x16_segments
		,+frm_16x16_segments ,+frm_16x16_segments1)
	+at_hand_fire `'(,+frm_16x16_pshot ,+frm_16x16_pshot1)
	+at_mind_fire `'(,+frm_16x16_mshot ,+frm_16x16_mshot1)
	+mt_mind_arm
	''((12 2) (10 3) (8 4) (6 5) (4 6) (2 7) (2 7) (4 6) (6 5) (8 4) (10 3) (12 2) (12 0)
		(12 -2) (10 -3) (8 -4) (6 -5) (4 -6) (2 -7) (2 -7) (4 -6) (6 -5) (8 -4) (10 -3) (12 -2) (12 0))
	; the four arms (ox oy xx xy yx yy end_frame), each segment
	; steps by (dx * xx + dy * xy, dx * yx + dy * yy)
	+mind_arms
	`'((24 8 1 0 0 1 ,+frm_16x16_tends1)
		(-8 8 -1 0 0 1 ,+frm_16x16_tends3)
		(8 24 0 1 1 0 ,+frm_16x16_tends2)
		(8 -8 0 1 -1 0 ,+frm_16x16_tends))
	; items a temple mind gives up, after its terrain talisman
	+mind_items
	`'(,+frm_16x16_diamond ,+frm_16x16_spells5 ,+frm_16x16_diamond5
		,+frm_16x16_spells5 ,+frm_16x16_diamond8 ,+frm_16x16_spells5
		,+frm_16x16_diamond7 ,+frm_16x16_spells5 ,+frm_16x16_diamond6
		,+frm_16x16_spells5 ,+frm_16x16_diamond9 ,+frm_16x16_spells5
		,+frm_16x16_diamond4))

(defun get-xy-inner (w h)
	; pick a random point on the inside edge of the playable area
	(case (game-random 4)
		(0 (list (game-random (- +window_width w)) 0))
		(1 (list (game-random (- +window_width w)) (- +window_height h)))
		(2 (list 0 (game-random (- +window_height h))))
		(:t (list (- +window_width w) (game-random (- +window_height h))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mind map and arms overlays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mind-blit (view tid th ts zoom frame x y)
	; blit a square strip frame at unscaled view position x, y
	(defq raw_y (* frame ts))
	(. view :ctx_blit tid +argb_white (* zoom x) (* zoom y) ts ts
		(* (/ raw_y th) ts) (% raw_y th)))

(defclass MindMap () (View)
	; (MindMap) -> two plane tiled mind background view
	(def this :color 0 :canvas :nil :x1 0 :y1 0 :x2 0 :y2 0)

	(defmethod :draw ()
		(when (and (defq c (get :canvas this)) (defq texture (getf c +canvas_texture 0)))
			(bind '(tid tw th) (texture-metrics texture))
			(defq zoom (get :zoom this) ts (* zoom +tile_width) y (neg (get :y1 this)))
			;plane 1
			(while (< y +window_height)
				(defq x (neg (get :x1 this)))
				(while (< x +window_width)
					(mind-blit this tid th ts zoom 0 x y)
					(++ x +tile_width))
				(++ y +tile_height))
			;plane 2
			(setq y (neg (get :y2 this)))
			(while (< y +window_height)
				(defq x (neg (get :x2 this)))
				(while (< x +window_width)
					(mind-blit this tid th ts zoom 1 x y)
					(mind-blit this tid th ts zoom 3 (+ x +tile_width) y)
					(mind-blit this tid th ts zoom 2 x (+ y +tile_height))
					(++ x (const (* +tile_width 2))))
				(++ y (const (* +tile_height 2)))))
		this)
	)

(defclass MindArms () (View)
	; (MindArms) -> full screen overlay drawing the four arms of the mind
	(def this :color 0 :canvas :nil :mx 0 :my 0 :segs 0 :arm 0 :frame_count 0)

	(defmethod :draw ()
		(when (and (> (defq segs (get :segs this)) 0) (defq c (get :canvas this))
				(defq texture (getf c +canvas_texture 0)))
			(bind '(tid tw th) (texture-metrics texture))
			(defq zoom (get :zoom this) ts (* zoom 16) arm (get :arm this)
				seg (% (- 15 (% (get :frame_count this) (const (length +at_mind_arm))))
					(const (length +at_mind_arm))))
			(each (lambda ((ox oy xx xy yx yy end_frame))
				(defq sx (+ (get :mx this) ox) sy (+ (get :my this) oy))
				(times segs
					(bind '(dx dy) (elem-get +mt_mind_arm arm))
					(mind-blit this tid th ts zoom (elem-get +at_mind_arm seg) sx sy)
					(setq seg (% (inc seg) (const (length +at_mind_arm)))
						arm (% (inc arm) (const (length +mt_mind_arm)))
						sx (+ sx (* dx xx) (* dy xy)) sy (+ sy (* dx yx) (* dy yy))))
				(mind-blit this tid th ts zoom end_frame sx sy))
				+mind_arms))
		this)
	)

(defun mind-resize (zoom)
	(defq w (* zoom +screen_width) h (* zoom +screen_height))
	(when *mind_map*
		(def *mind_map* :zoom zoom :canvas *img_blocks3*)
		(. *mind_map* :change 0 0 w h :t))
	(when *mind_arms*
		(def *mind_arms* :zoom zoom :canvas *img_frm_16x16*)
		(. *mind_arms* :change 0 0 w h :t)))

(defun mind-map-init ()
	; show the mind background, behind all the sprites
	(unless *mind_map* (setq *mind_map* (MindMap)))
	(def *mind_map* :x1 0 :y1 0 :x2 0 :y2 0)
	(. *layer_bans* :add_back *mind_map*)
	(mind-resize *zoom*))

(defun mind-map-scroll (dx1 dy1 dx2 dy2)
	; scroll the two planes of the mind map, each wraps at its pattern size
	(def *mind_map*
		:x1 (logand (+ (get :x1 *mind_map*) dx1) 0xf)
		:y1 (logand (+ (get :y1 *mind_map*) dy1) 0xf)
		:x2 (logand (+ (get :x2 *mind_map*) dx2) 0x1f)
		:y2 (logand (+ (get :y2 *mind_map*) dy2) 0x1f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; death hooks and collision handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dt-hand-fire (this)
	(-- *mind_firecnt*)
	(dt-fizz-fx this))

(defun dt-hand-fire1 (this)
	(-- *mind_firecnt*)
	(dt-exp-fx this))

(defun dt-mind (this)
	(. *mind_arms* :sub)
	(dt-blat-fx this))

(defun cl-hand-hits-fire (this hit)
	; mind shot drains the player's power
	(def hit :sp_death dt-exp-fx)
	(. hit :sp_kill)
	(when (= (setq *man_power* (max 0 (- *man_power* (get :sp_hitpnt hit)))) 0)
		(. this :sp_kill)))

(defun cl-hand-hits-item (this hit)
	(cond
		((= (get :sp_frame hit) +frm_16x16_spells5)
			;blue spell restores power, less if carrying the mind talisman
			(setq *man_power* (min +max_power (+ *man_power*
				(if (find +frm_16x16_diamond4 *item_inventory*)
					(const (/ +max_power 10)) (const (/ +max_power 5))))))
			(play-sfx *sfx_clash2* this)
			(. hit :sp_kill))
		(:t (collect-item hit))))

(defun cl-mind-hits-fire (this hit)
	(def hit :sp_death dt-hand-fire1)
	(. hit :sp_kill)
	;open mouth
	(. this :sp_set_frame (+ (logior (- (get :sp_frame this) +frm_32c32_face) 1) +frm_32c32_face))
	(defq hitpnt (- (get :sp_hitpnt this) (get :sp_hitpnt hit)))
	(def this :sp_hitpnt hitpnt)
	(when (<= hitpnt 0)
		;lose an arm segment
		(def this :sp_hitpnt 8)
		(if (= (def this :mind_segs (max 0 (dec (get :mind_segs this)))) 0)
			(. this :sp_kill))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; player hand and mind sprites
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cp-hand-fire (this that)
	; shots die on reaching their target
	(if (= (get 'count (elem-get (get :sp_components this) +comp_move)) -1)
		(. this :sp_kill)))

(defclass HandFire () (Sprite *img_frm_16x16* :nil 16 16 :16x16 dt-hand-fire)
	; (HandFire) -> player mind shot
	(def this :sp_type +ftp_man :sp_hitpnt 1)
	(.-> this
		(:sp_add_component (ML 8))
		(:sp_add_component (AT 2 +at_hand_fire))
		(:sp_add_component (CP cp-hand-fire))
		(:sp_set_frame +frm_16x16_pshot)))

(defclass MindFire () (Sprite *img_frm_16x16* :nil 16 16 :16x16 dt-fizz-fx)
	; (MindFire) -> enemy mind shot
	(def this :sp_type +ftp_enemy_fire :sp_hitpnt 200)
	(.-> this
		(:sp_add_component (ML 5))
		(:sp_add_component (AT 1 +at_mind_fire))
		(:sp_add_component (CP cp-hand-fire))
		(:sp_set_frame +frm_16x16_mshot)))

(defclass Talisman (frame) (Sprite *img_frm_16x16* :nil 16 16 :16x16 dt-fizz-fx)
	; (Talisman frame) -> static collectible item
	(def this :sp_type +ftp_item :sp_hitpnt 3)
	(. this :sp_set_frame frame))

(defun cp-hand (this that)
	; the hand runs around the inside edge of the screen
	(bind '(x y) (. this :sp_get_pos))
	(defq controls *game_controls*
		mx (const (- +window_width 16)) my (const (- +window_height 16)))
	(cond
		((bits? controls +fkey_left +fkey_up)
			;clockwise
			(cond
				((= x 0) (when (>= (setq y (+ y 8)) my) (setq y my x (+ x 8))))
				((= x mx) (when (<= (setq y (- y 8)) 0) (setq y 0 x (- x 8))))
				((= y 0) (when (<= (setq x (- x 8)) 0) (setq x 0 y (+ y 8))))
				(:t (when (>= (setq x (+ x 8)) mx) (setq x mx y (- y 8))))))
		((bits? controls +fkey_right +fkey_down)
			;anticlockwise
			(cond
				((= x 0) (when (<= (setq y (- y 8)) 0) (setq y 0 x (+ x 8))))
				((= x mx) (when (>= (setq y (+ y 8)) my) (setq y my x (- x 8))))
				((= y 0) (when (>= (setq x (+ x 8)) mx) (setq x mx y (+ y 8))))
				(:t (when (<= (setq x (- x 8)) 0) (setq x 0 y (- y 8)))))))
	(. this :sp_set_pos x y)
	;fire at the mind
	(if (bits? controls +fkey_keya)
		(unless (bits? *game_flags* +fflag_select)
			(setq *game_flags* (logior *game_flags* +fflag_select))
			(when (< *mind_firecnt* +mind_fire_max)
				(++ *mind_firecnt*)
				(bind '(tx ty) (if (get :sp_dead *mind_sprite*)
					(list (const (- (/ +window_width 2) 8)) (const (- (/ +window_height 2) 8)))
					(map (# (+ %0 8)) (. *mind_sprite* :sp_get_pos))))
				(init-ml (defq fire (HandFire)) (elem-get (get :sp_components fire) +comp_move) x y tx ty 8)
				(. *layer_player* :add_front fire)
				(play-sfx *sfx_spell* this)))
		(setq *game_flags* (logand *game_flags* (lognot +fflag_select))))
	;face inwards
	(. this :sp_set_frame (cond
		((= x 0) +frm_16x16_manmind3)
		((= x mx) +frm_16x16_manmind1)
		((= y 0) +frm_16x16_manmind)
		(:t +frm_16x16_manmind2)))
	;parallax scroll the map away from the hand, far plane at twice the speed
	(defq xs (cond
			((< x (const (- (/ +window_width 3) 8))) -1)
			((< x (const (- +window_width (- (/ +window_width 3) 8)))) 0)
			(:t 1))
		ys (cond
			((< y (const (- (/ +window_height 3) 8))) -1)
			((< y (const (- +window_height (- (/ +window_height 3) 8)))) 0)
			(:t 1)))
	(mind-map-scroll xs ys (* xs 2) (* ys 2)))

(defclass Hand () (Sprite *img_frm_16x16* :nil 16 16 :16x16 dt-exp-fx)
	; (Hand) -> player mind combat sprite
	(.-> this
		(:sp_add_component (CP cp-hand))
		(:sp_add_component (CL *layer_items* +ftp_item cl-hand-hits-item))
		(:sp_add_component (CL *layer_missiles* +ftp_enemy_fire cl-hand-hits-fire))
		(:sp_set_frame +frm_16x16_manmind)))

(defun cp-mind (this that)
	(bind '(x y) (. this :sp_get_pos))
	(defq delay (dec (get :mind_delay this)) frame_count *game_frame_count*
		arm (% (inc (get :mind_arm this)) (const (length +mt_mind_arm))))
	(when (<= delay 0)
		;new delay and homing position
		(game-random 16)
		(setq delay (+ delay 32))
		(def this :mind_tx (+ (game-random 32) 128) :mind_ty (+ (game-random 16) +mind_y)))
	(def this :mind_delay delay :mind_arm arm)
	;home in
	(set (elem-get (get :sp_components this) +en_comp_mv)
		'ax (if (>= (get :mind_tx this) x) 1 -1)
		'ay (if (>= (get :mind_ty this) y) 1 -1))
	;close mouth
	(if (= (logand frame_count 0xf) 0)
		(. this :sp_set_frame (+ (logand (- (get :sp_frame this) +frm_32c32_face) -2) +frm_32c32_face)))
	;fire at the hand, or at the screen edge
	(when (= (% frame_count 10) 0)
		(bind '(tx ty) (if (or (get :sp_dead *mind_hand*) (> (game-random 4) 0))
			(get-xy-outer 16 16)
			(. *mind_hand* :sp_get_pos)))
		(init-ml (defq fire (MindFire)) (elem-get (get :sp_components fire) +comp_move)
			(+ x 8) (+ y 8) tx ty 5)
		(. *layer_missiles* :add_front fire))
	;waggle arms
	(def *mind_arms* :mx x :my y :segs (get :mind_segs this) :arm arm :frame_count frame_count))

(defclass Mind (frame) (Sprite *img_frm_32c32* :nil 32 32 :32c32 dt-mind)
	; (Mind frame) -> wizardlord mind sprite, the arms are drawn by *mind_arms*
	(def this :mind_segs 8 :mind_delay 0 :mind_tx 0 :mind_ty 0 :mind_arm 0)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 8 8))
		(:sp_add_component (CP cp-mind))
		(:sp_add_component (CL *layer_player* +ftp_man cl-mind-hits-fire))
		(:sp_set_frame frame)))

;;;;;;;;;;;;;
; mind combat
;;;;;;;;;;;;;

(defun mind-combat-init (frame)
	; (mind-combat-init frame) set up combat against the wizardlord face frame
	(clear-sprite-layers)
	(set-world-layers-pos 0 0)
	(set-world-layers-size (* *zoom* +screen_width) (* *zoom* +screen_height))
	(. *layer_land* :clear)
	(. *layer_sky* :sky_black)
	(mind-map-init)
	(unless *mind_arms* (setq *mind_arms* (MindArms)))
	(def *mind_arms* :segs 0)
	(. *layer_enemies* :add_back *mind_arms*)
	(mind-resize *zoom*)
	(bind '(x y) (get-xy-inner 16 16))
	(. *layer_player* :add_front
		(setq *mind_hand* (. (Hand) :sp_set_pos (logand x -8) (logand y -8))))
	(. *layer_enemies* :add_front
		(setq *mind_sprite* (. (Mind frame) :sp_set_pos 144 +mind_y)))
	(setq *mind_count* 128 *mind_firecnt* 0 *mind_itemindex* 0 *mind_power* *man_power*
		*game_flags* (logior *game_flags* +fflag_select)))

(defun mind-combat-update (tile)
	; (mind-combat-update tile) -> :nil | :won | :lost
	(when (= (-- *mind_count*) 0)
		;mr smith drops a new item
		(each (# (. %0 :sp_kill)) (. *layer_items* :children))
		(defq frame +frm_16x16_spells5)
		(when (<= +frm_campain_twater tile +frm_campain_tmountain)
			;temples give up their terrain talisman first, then the rest
			(setq frame (if (= *mind_itemindex* 0)
					(+ +frm_16x16_diamond (- tile +frm_campain_twater))
					(elem-get +mind_items *mind_itemindex*))
				*mind_itemindex* (% (inc *mind_itemindex*) (const (length +mind_items)))))
		(bind '(x y) (get-xy-inner 16 16))
		(. *layer_items* :add_front (. (Talisman frame) :sp_set_pos x y))
		(. *layer_bodies* :add_front (. (Smith) :sp_set_pos (- x 8) (- y 8)))
		(setq *mind_count* 96))
	(cond
		((get :sp_dead *mind_sprite*) :won)
		((= *man_power* 0) :lost)))

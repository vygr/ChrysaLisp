;;;;;;;;;;;;;;;;;;;;;
; tower enemy entity
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants and animation tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +at_tower `'(,+frm64x64_tower ,+frm64x64_tower1 ,+frm64x64_tower2 ,+frm64x64_tower3
		,+frm64x64_tower ,+frm64x64_tower1 ,+frm64x64_tower2
		,+frm64x64_tower ,+frm64x64_tower1 ,+frm64x64_tower2))

;;;;;;;;;;;;;;;;
; death handlers
;;;;;;;;;;;;;;;;

(defun dt-kill-tower (this)
	; tower death: 4 spearmen emerge from the 4 corners, blat explosion
	(-- *enemy_count*)
	(bind '(x y) (. this :sp_get_pos))
	(each (lambda ((ox oy sdir))
		(defq sp (Spearman)
			mv (elem-get (get :sp_components sp) +en_comp_mv))
		(def sp :sp_dir sdir)
		(set mv 'vx (* (get 'max_vx mv) sdir))
		(. sp :sp_set_pos (+ x ox) (+ y oy))
		(. *layer_enemies* :add_front sp)
		(++ *enemy_count*))
		'((0 0 -1) (32 0 1) (0 32 -1) (32 32 1)))
	(dt-blat-fx this))

;;;;;;;;;;;;;;;;;;;;
; component handlers
;;;;;;;;;;;;;;;;;;;;

(defun cp-tower-patrol (this that)
	; tower patrols platform back and forth without falling off, and fires 4 spears on index 4
	(bind '(x y w h) (. this :sp_get_bounds))
	(defq dir (get :sp_dir this)
		comps (get :sp_components this)
		mv (elem-get comps +en_comp_mv)
		at_comp (elem-get comps +en_comp_at)
		probe_x (if (= dir 1) (+ x w -1) x)
		probe_y (+ y h)
		flags (. *layer_land* :get_tile_flags probe_x probe_y))
	(unless (bits? flags +fmap_stand)
		(setq dir (neg dir))
		(def this :sp_dir dir)
		(set mv 'vx (* (get 'max_vx mv) dir)))
	(when (and at_comp (= (get 'index at_comp) 4) (= (get 'count at_comp) 0))
		(each (lambda ((ox oy sdir))
			(defq spear (Spear (+ x ox) (+ y oy) sdir))
			(. *layer_missiles* :add_front spear))
			'((0 0 -1) (0 32 -1) (32 0 1) (32 32 1))))
	this)

;;;;;;;;;;;;;;;;;;;;
; tower sprite class
;;;;;;;;;;;;;;;;;;;;

(defclass Tower () (Sprite *img_frm_64x64* :nil 64 64 :64x64 dt-kill-tower)
	(def this :sp_type +ftp_tower :sp_hitpnt 8)
	(.-> this
		(:sp_add_component (MV 0 0 0 0 6 0))
		(:sp_add_component (AT 2 +at_tower))
		(:sp_add_component (CP cp-tower-patrol))
		(:sp_add_component (CP cp-enemy-offscreen))
		(:sp_set_frame +frm64x64_tower)))

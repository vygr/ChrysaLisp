;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; layer management helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-canvas-pos (c x &optional y)
	; set position of canvas or view
	(ifn y
		(setq y x x c c *img_panel*))
	(. c :set_pos x y))

(defun set-canvas-size (c w &optional h)
	; size a canvas or view
	(ifn h
		(setq h w w c c *img_panel*))
	(. c :set_size w h))

(defun clear-layer (layer)
	; remove all child views from layer
	(each (# (. %0 :sub)) (cat (. layer :children))))

(defun clear-sprite-layers ()
	; remove all active sprites from all 7 sprite layers
	(each (# (clear-layer %0)) +sprite_layers))

(defun set-world-layers-pos (x y)
	; position all children of *world_layers*
	(each (# (. %0 :set_pos x y)) +world_layers))

(defun set-world-layers-size (w h)
	; size all children of *world_layers*
	(each (# (. %0 :set_size w h)) +world_layers))

(defun reset-layers-title ()
	; title sequence: clear sprites, size to screen at (0, 0), black sky
	(clear-sprite-layers)
	(. *layer_land* :clear)
	(. *layer_sky* :set_black)
	(set-world-layers-pos 0 0)
	(set-world-layers-size (* *zoom* +screen_width) (* *zoom* +screen_height))
	(. *layer_sky* :dirty))

(defun reset-layers-field (&optional map_idx)
	; field battle: clear sprites, size to full map world, pastel sky, load terrain map
	(setd map_idx 1)
	(clear-sprite-layers)
	(defq
		world_w (* *zoom* (* +map_width +tile_width))
		world_h (* *zoom* (* +map_height +tile_height)))
	(set-world-layers-size world_w world_h)
	(. *layer_sky* :set_random_sky)
	(. *layer_land* :load_field_map map_idx)
	(. *layer_sky* :dirty)
	(. *layer_land* :dirty)
	(. *world_layers* :dirty_all))

(defun update-sprite-canvas (sp)
	(when (def? :canvas_tag sp)
		(case (get :canvas_tag sp)
			(:fanatic
				(set sp :canvas_l *img_fanatic_l* :canvas_r *img_fanatic_r*))
			(:32c32
				(set sp :canvas_l *img_frm_32c32*))
			(:16x16
				(set sp :canvas_l *img_frm_16x16*))
			(:16x16_lr
				(set sp :canvas_l *img_frm_16x16_l* :canvas_r *img_frm_16x16_r*))
			(:16x64
				(set sp :canvas_l *img_frm_16x64*))
			(:32x32_lr
				(set sp :canvas_l *img_frm_32x32_l* :canvas_r *img_frm_32x32_r*))
			(:32c32_lr
				(set sp :canvas_l *img_frm_32c32_l* :canvas_r *img_frm_32c32_r*))
			(:32x16_lr
				(set sp :canvas_l *img_frm_32x16_l* :canvas_r *img_frm_32x16_r*))
			(:64x32_lr
				(set sp :canvas_l *img_frm_64x32_l* :canvas_r *img_frm_64x32_r*))
			(:64x64_lr
				(set sp :canvas_l *img_frm_64x64_l* :canvas_r *img_frm_64x64_r*))
			(:64x64
				(set sp :canvas_l *img_frm_64x64*)))))

(defun rescale-active-sprites (old_z new_z)
	; rescale positions, dimensions, canvases, and motion components of all live sprites
	(each (lambda (layer)
		(each (lambda (sp)
			(when (Sprite? sp)
				(bind '(x y) (. sp :get_pos))
				(defq
					new_x (/ (* x new_z) old_z)
					new_y (/ (* y new_z) old_z)
					base_w (get :base_w sp)
					base_h (get :base_h sp)
					new_w (* base_w new_z)
					new_h (* base_h new_z))
				(. sp :set_bounds new_x new_y new_w new_h)

				(update-sprite-canvas sp)

				(when (def? :mv_vx sp)
					(set sp
						:mv_vx (/ (* (get :mv_vx sp) new_z) old_z)
						:mv_vy (/ (* (get :mv_vy sp) new_z) old_z)
						:mv_ax (/ (* (get :mv_ax sp) new_z) old_z)
						:mv_ay (/ (* (get :mv_ay sp) new_z) old_z)
						:mv_max_vx (/ (* (get :mv_max_vx sp) new_z) old_z)
						:mv_max_vy (/ (* (get :mv_max_vy sp) new_z) old_z)))

				(when (def? :ml_speed sp)
					(defq speed (get :ml_speed sp))
					(when (/= speed 0)
						(defq
							tx (/ (* (get :ml_target_x sp) new_z) old_z)
							ty (/ (* (get :ml_target_y sp) new_z) old_z)
							new_spd (/ (* speed new_z) old_z))
						(init-ml sp new_x new_y tx ty new_spd)))

				(when (def? :bounds_max_x sp)
					(set sp
						:bounds_min_x (/ (* (get :bounds_min_x sp) new_z) old_z)
						:bounds_max_x (/ (* (get :bounds_max_x sp) new_z) old_z)
						:bounds_min_y (/ (* (get :bounds_min_y sp) new_z) old_z)
						:bounds_max_y (/ (* (get :bounds_max_y sp) new_z) old_z)))

				(when (def? :xv sp)
					(set sp
						:xv (/ (* (get :xv sp) new_z) old_z)
						:yv (/ (* (get :yv sp) new_z) old_z)))))
			(cat (. layer :children))))
		+sprite_layers))

(defun update-sprite-layers ()
	; tick all active sprites across all 7 sprite layers
	(each (# (each (lambda (sp) (when (Sprite? sp) (. sp :update))) (cat (. %0 :children))))
		+sprite_layers))

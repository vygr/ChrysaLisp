;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; layer management helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-canvas-pos (c x &optional y)
	; set position of canvas or view
	(ifn y (setq y x x c c *img_panel*))
	(. c :set_pos x y))

(defun set-canvas-size (c w &optional h)
	; size a canvas or view
	(ifn h (setq h w w c c *img_panel*))
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
	(set-world-layers-pos 0 0)
	(set-world-layers-size (* *zoom* +screen_width) (* *zoom* +screen_height))
	(. *layer_land* :clear)
	(. *layer_sky* :set_black))

(defun reset-layers-field (&optional map_idx)
	; field battle: clear sprites, size to full map world, pastel sky, load terrain map
	(clear-sprite-layers)
	(set-world-layers-pos 0 0)
	(set-world-layers-size (* *zoom* (* +map_width +tile_width)) (* *zoom* (* +map_height +tile_height)))
	(. *layer_sky* :set_random_sky)
	(. *layer_land* :load_field_map (ifn map_idx 1)))

(defun update-sprite-canvas (sp)
	(when (def? :sp_canvas_tag sp)
		(case (get :sp_canvas_tag sp)
			(:fanatic
				(set sp :sp_canvas_l *img_fanatic_l* :sp_canvas_r *img_fanatic_r*))
			(:32c32
				(set sp :sp_canvas_l *img_frm_32c32*))
			(:16x16
				(set sp :sp_canvas_l *img_frm_16x16*))
			(:16x16_lr
				(set sp :sp_canvas_l *img_frm_16x16_l* :sp_canvas_r *img_frm_16x16_r*))
			(:16x64
				(set sp :sp_canvas_l *img_frm_16x64*))
			(:32x32_lr
				(set sp :sp_canvas_l *img_frm_32x32_l* :sp_canvas_r *img_frm_32x32_r*))
			(:32c32_lr
				(set sp :sp_canvas_l *img_frm_32c32_l* :sp_canvas_r *img_frm_32c32_r*))
			(:32x16_lr
				(set sp :sp_canvas_l *img_frm_32x16_l* :sp_canvas_r *img_frm_32x16_r*))
			(:64x32_lr
				(set sp :sp_canvas_l *img_frm_64x32_l* :sp_canvas_r *img_frm_64x32_r*))
			(:64x64_lr
				(set sp :sp_canvas_l *img_frm_64x64_l* :sp_canvas_r *img_frm_64x64_r*))
			(:64x64
				(set sp :sp_canvas_l *img_frm_64x64*)))))

(defun rescale-active-sprites ()
	; rescale positions, dimensions and canvases of all live sprites
	(each (lambda (layer)
		(each (lambda (sp)
			(when (Sprite? sp)
				(bind '(x y w h) (. sp :sp_get_bounds))
				(. sp :sp_set_bounds x y w h)
				(update-sprite-canvas sp)))
			(cat (. layer :children))))
		+sprite_layers))

(defun update-sprite-layers ()
	; tick all active sprites across all 7 sprite layers
	(each (# (each (lambda (sp) (when (Sprite? sp) (. sp :update))) (cat (. %0 :children))))
		+sprite_layers))

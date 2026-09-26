;;;;;;;;;;;;;;;;;;;;;;;;;;
; layer management helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun get-xy-outer (w h)
	; pick a random start point just outside the playable area
	(case (random 4)
		(0 (list (- (random (+ +window_width (* 2 w))) w) (neg h)))
		(1 (list (- (random (+ +window_width (* 2 w))) w) +window_height))
		(2 (list (neg w) (- (random (+ +window_height (* 2 h))) h)))
		(3 (list +window_width (- (random (+ +window_height (* 2 h))) h)))))

(defun update-camera (sprite)
	; center camera on sprite
	(bind '(x y w h) (. sprite :sp_get_bounds))
	(defq cam_x (- (+ x (/ w 2)) (const (/ +window_width 2)))
		cam_y (- (+ y (/ h 2)) (const (/ +window_height 2)))
		max_cam_x (- (* +map_width +tile_width) +window_width)
		max_cam_y (- (* +map_height +tile_height) +window_height))
	; scroll all world layers by negative camera offset
	(set-world-layers-pos
		(* *zoom* (neg (max 0 (min max_cam_x cam_x))))
		(* *zoom* (neg (max 0 (min max_cam_y cam_y))))))

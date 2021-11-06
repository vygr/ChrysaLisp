;;;;;;;;;;;;;
; layer class
;;;;;;;;;;;;;

(import "lib/math/vector.inc")

;module
(env-push)

(enums +layer_line 0
	(enum x1 y1 x2 y2 radius gap))

(enums +layers_line 0
	(enum x1 y1 z1 x2 y2 z2 radius gap))

(defmacro Layer-line (x1 y1 x2 y2 radius gap)
	`(fixeds ,x1 ,y1 ,x2 ,y2 ,radius ,gap))

(defmacro Layers-line (x1 y1 z1 x2 y2 z2 radius gap)
	`(fixeds ,x1 ,y1 ,z1 ,x2 ,y2 ,z2 ,radius ,gap))

(enums +record 0
	(enum id layer_line p1 p2 norm dist))

(defun Record (id layer_line)
	(defq p1 (slice +layer_line_x1 +layer_line_x2 layer_line)
		p2 (slice +layer_line_x2 +layer_line_radius layer_line)
		pv (apply fixeds (vec-perp-2d (vec-sub p2 p1)))
		norm (vec-norm pv) dist (vec-dot norm p1))
	(list id layer_line p1 p2 norm dist))

(defun hit-line? (record layer_line d)
	(defq p1 (slice +layer_line_x1 +layer_line_x2 layer_line)
		p2 (slice +layer_line_x2 +layer_line_radius layer_line)
		dp1 (- (vec-dot (elem +record_norm record) p1) (elem +record_dist record))
		dp2 (- (vec-dot (elem +record_norm record) p2) (elem +record_dist record)))
	(cond
		((and (> dp1 d) (> dp2 d)) nil)
		((and (< dp1 (neg d)) (< dp2 (neg d))) nil)
		((vec-collide-thick-lines-2d p1 p2 (elem +record_p1 record) (elem +record_p2 record) d))))

(defclass Layer (width height scale) nil
	; (Layer width height scale) -> layer
	(def this :width width :height height :scale scale :id 0
		:buckets (defq buckets (cap (* width height) (list))))
	(times (* width height) (push buckets (list)))

	(defmethod :aabb (this layer_line)
		; (. layer :aabb layer_line) -> aabb
		(raise :width :height :scale)
		(bind '(x1 y1 x2 y2 radius gap) layer_line)
		(if (> x1 x2) (defq lt x1 x1 x2 x2 lt))
		(if (> y1 y2) (defq lt y1 y1 y2 y2 lt))
		(defq radius (+ radius gap) width (dec width) height (dec height))
		(list (min width (max 0 (n2i (* (- x1 radius) scale))))
			(min height (max 0 (n2i (* (- y1 radius) scale))))
			(min width (max 0 (n2i (* (+ x2 radius) scale))))
			(min height (max 0 (n2i (* (+ y2 radius) scale))))))

	(defmethod :add_line (this layer_line)
		; (. layer :add_line layer_line) -> layer
		(bind '(minx miny maxx maxy) (. this :aabb layer_line))
		(defq record (Record 0 layer_line))
		(raise :width :buckets)
		(setq miny (* (dec miny) width) maxy (* maxy width))
		(while (<= (setq miny (+ miny width)) maxy)
			(each! (+ miny minx) (+ miny maxx 1)
				(lambda (bucket)
					(push bucket record))
				(list buckets)))
		this)

	(defmethod :sub_line (this layer_line)
		; (. layer :sub_line layer_line) -> layer
		(bind '(minx miny maxx maxy) (. this :aabb layer_line))
		(raise :width :buckets)
		(setq miny (* (dec miny) width) maxy (* maxy width))
		(while (<= (setq miny (+ miny width)) maxy)
			(each! (+ miny minx) (+ miny maxx 1)
				(lambda (bucket)
					(when (defq idx (some
							(lambda (record)
								(if (eql (elem +record_layer_line record) layer_line) _))
							bucket))
						(elem-set idx bucket (elem -2 bucket))
						(pop bucket)))
				(list buckets)))
		this)

	(defmethod :hit_line (this layer_line)
		; (. layer :hit_line layer_line) -> nil | t
		(bind '(minx miny maxx maxy) (. this :aabb layer_line))
		(raise :width :buckets :id)
		(defq id (inc id) miny (* (dec miny) width) maxy (* maxy width))
		(lower :id)
		(while (and (<= (setq miny (+ miny width)) maxy)
					(not (defq hit (some! (+ miny minx) (+ miny maxx 1) nil
						(lambda (bucket)
							(some (lambda (record)
								(when (/= (elem +record_id record) id)
									(elem-set +record_id record id)
									(hit-line? record layer_line (+
										(elem +layer_line_radius layer_line)
										(elem +layer_line_radius (elem +record_layer_line record))
										(max (elem +layer_line_gap layer_line)
											(elem +layer_line_gap (elem +record_layer_line record)))))))
								bucket))
						(list buckets))))))
		hit)
	)

(defclass Layers (width height depth scale) nil
	; (Layers width height depth scale) -> layers
	(def this :depth depth :layers (defq layers (list)))
	(times depth (push layers (Layer width height scale)))

	(defmethod :add_line (this p1 p2 radius gap)
		; (. layers :add_line p1 p2 radius gap) -> layers
		(bind '(x1 y1 z1) p1)
		(bind '(x2 y2 z2) p2)
		(defq layer_line (Layer-line x1 y1 x2 y2 radius gap))
		(if (> z1 z2) (defq zt z1 z1 z2 z2 zt))
		(raise :layers)
		(while (<= z1 z2)
			(. (elem (n2i z1) layers) :add_line layer_line)
			(setq z1 (+ z1 1.0)))
		this)

	(defmethod :sub_line (this p1 p2 radius gap)
		; (. layers :sub_line p1 p2 radius gap) -> layers
		(bind '(x1 y1 z1) p1)
		(bind '(x2 y2 z2) p2)
		(defq layer_line (Layer-line x1 y1 x2 y2 radius gap))
		(if (> z1 z2) (defq zt z1 z1 z2 z2 zt))
		(raise :layers)
		(while (<= z1 z2)
			(. (elem (n2i z1) layers) :sub_line layer_line)
			(setq z1 (+ z1 1.0)))
		this)

	(defmethod :hit_line (this p1 p2 radius gap)
		; (. layers :hit_line p1 p2 radius gap) -> nil | t
		(bind '(x1 y1 z1) p1)
		(bind '(x2 y2 z2) p2)
		(defq layer_line (Layer-line x1 y1 x2 y2 radius gap))
		(if (> z1 z2) (defq zt z1 z1 z2 z2 zt))
		(raise :layers)
		(while (and (<= z1 z2)
					(not (defq hit (. (elem (n2i z1) layers) :hit_line layer_line))))
			(setq z1 (+ z1 1.0)))
		hit)

	(defmethod :add_layers_line (this layers_line)
		; (. layers :add_layers_line layers_line) -> layers
		(bind '(x1 y1 z1 x2 y2 z2 radius gap) layers_line)
		(defq layer_line (Layer-line x1 y1 x2 y2 radius gap))
		(if (> z1 z2) (defq zt z1 z1 z2 z2 zt))
		(raise :layers)
		(while (<= z1 z2)
			(. (elem (n2i z1) layers) :add_line layer_line)
			(setq z1 (+ z1 1.0)))
		this)

	(defmethod :sub_layers_line (this layers_line)
		; (. layers :sub_layers_line layers_line) -> layers
		(bind '(x1 y1 z1 x2 y2 z2 radius gap) layers_line)
		(defq layer_line (Layer-line x1 y1 x2 y2 radius gap))
		(if (> z1 z2) (defq zt z1 z1 z2 z2 zt))
		(raise :layers)
		(while (<= z1 z2)
			(. (elem (n2i z1) layers) :sub_line layer_line)
			(setq z1 (+ z1 1.0)))
		this)

	(defmethod :hit_layers_line (this layers_line)
		; (. layers :hit_layers_line layers_line) -> nil | t
		(bind '(x1 y1 z1 x2 y2 z2 radius gap) layers_line)
		(defq layer_line (Layer-line x1 y1 x2 y2 radius gap))
		(if (> z1 z2) (defq zt z1 z1 z2 z2 zt))
		(raise :layers)
		(while (and (<= z1 z2)
					(not (defq hit (. (elem (n2i z1) layers) :hit_line layer_line))))
			(setq z1 (+ z1 1.0)))
		hit)
	)

;module
(export-symbols
	Layer-line Layers-line)
(export-classes
	Layer Layers)
(env-pop)
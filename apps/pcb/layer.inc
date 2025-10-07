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
	(defq p1 (slice layer_line +layer_line_x1 +layer_line_x2)
		p2 (slice layer_line +layer_line_x2 +layer_line_radius)
		pv (apply fixeds (vec-perp-2d (vec-sub p2 p1)))
		l (vec-length pv))
	(if (= l 0.0)
		(defq norm +fixeds_zero2 dist 0.0)
		(defq norm (vec-scale pv (recip l)) dist (vec-dot norm p1)))
	(list id layer_line p1 p2 norm dist))

(defun hit-line? (record layer_line)
	(defq d (+ (elem-get layer_line +layer_line_radius)
				(elem-get (elem-get record +record_layer_line) +layer_line_radius)
				(max (elem-get layer_line +layer_line_gap)
					(elem-get (elem-get record +record_layer_line) +layer_line_gap)))
		p1 (slice layer_line +layer_line_x1 +layer_line_x2)
		p2 (slice layer_line +layer_line_x2 +layer_line_radius)
		dp1 (- (vec-dot (elem-get record +record_norm) p1) (elem-get record +record_dist))
		dp2 (- (vec-dot (elem-get record +record_norm) p2) (elem-get record +record_dist)))
	(cond
		((and (> dp1 d) (> dp2 d)) :nil)
		((and (< dp1 (neg d)) (< dp2 (neg d))) :nil)
		((vec-collide-thick-lines-2d p1 p2 (elem-get record +record_p1) (elem-get record +record_p2) d))))

(defun hit-line (buckets minx miny maxx maxy width id func line)
	(defq miny (* (dec miny) width) maxy (* maxy width))
	(while (and (<= (++ miny width) maxy)
				(not (defq hit (some!
					(lambda (bucket)
						(some (lambda (record)
							(when (/= (elem-get record +record_id) id)
								(elem-set record +record_id id)
								(func record line)))
							bucket))
					(list buckets) :nil (+ miny minx) (+ miny maxx 1)))))
	hit))

(defun add-line (buckets minx miny maxx maxy width record)
	(defq miny (* (dec miny) width) maxy (* maxy width))
	(while (<= (++ miny width) maxy)
		(each! (lambda (bucket) (push bucket record))
			(list buckets) (+ miny minx) (+ miny maxx 1))))

(defun sub-line (buckets minx miny maxx maxy width layer_line)
	(defq miny (* (dec miny) width) maxy (* maxy width))
	(while (<= (++ miny width) maxy)
		(each! (lambda (bucket)
			(when (defq idx (some
					(lambda (record)
						(if (eql (elem-get record +record_layer_line) layer_line) _))
					bucket))
				(elem-set bucket idx (last bucket))
				(pop bucket)))
			(list buckets) (+ miny minx) (+ miny maxx 1))))

;native versions
(ffi "apps/pcb/hit_line" hit-line)
; (hit-line buckets minx miny maxx maxy width id func line) -> flag
(ffi "apps/pcb/add_line" add-line)
; (add-line buckets minx miny maxx maxy width record) -> record
(ffi "apps/pcb/sub_line" sub-line)
; (sub-line buckets minx miny maxx maxy width line) -> buckets

(defclass Layer (width height scale) :nil
	; (Layer width height scale) -> layer
	(def this :width width :height height :scale scale :id 0
		:buckets (defq buckets (lists (* width height))))

	(defmethod :aabb (layer_line)
		; (. layer :aabb layer_line) -> aabb
		(bind '(x1 y1 x2 y2 radius gap) layer_line)
		(raise :scale (radius (+ radius gap)
			width (dec (get :width this))
			height (dec (get :height this))))
		(if (< x1 x2)
			(if (< y1 y2)
				(list (min width (max 0 (n2i (* (- x1 radius) scale))))
					(min height (max 0 (n2i (* (- y1 radius) scale))))
					(min width (max 0 (n2i (* (+ x2 radius) scale))))
					(min height (max 0 (n2i (* (+ y2 radius) scale)))))
				(list (min width (max 0 (n2i (* (- x1 radius) scale))))
					(min height (max 0 (n2i (* (- y2 radius) scale))))
					(min width (max 0 (n2i (* (+ x2 radius) scale))))
					(min height (max 0 (n2i (* (+ y1 radius) scale))))))
			(if (< y1 y2)
				(list (min width (max 0 (n2i (* (- x2 radius) scale))))
					(min height (max 0 (n2i (* (- y1 radius) scale))))
					(min width (max 0 (n2i (* (+ x1 radius) scale))))
					(min height (max 0 (n2i (* (+ y2 radius) scale)))))
				(list (min width (max 0 (n2i (* (- x2 radius) scale))))
					(min height (max 0 (n2i (* (- y2 radius) scale))))
					(min width (max 0 (n2i (* (+ x1 radius) scale))))
					(min height (max 0 (n2i (* (+ y1 radius) scale))))))))

	(defmethod :add_line (layer_line)
		; (. layer :add_line layer_line) -> layer
		(bind '(minx miny maxx maxy) (. this :aabb layer_line))
		(add-line (get :buckets this) minx miny maxx maxy (get :width this) (Record 0 layer_line))
		this)

	(defmethod :sub_line (layer_line)
		; (. layer :sub_line layer_line) -> layer
		(bind '(minx miny maxx maxy) (. this :aabb layer_line))
		(sub-line (get :buckets this) minx miny maxx maxy (get :width this) layer_line)
		this)

	(defmethod :hit_line (layer_line)
		; (. layer :hit_line layer_line) -> :nil | :t
		(set this :id (defq id (inc (get :id this))))
		(bind '(minx miny maxx maxy) (. this :aabb layer_line))
		(hit-line (get :buckets this) minx miny maxx maxy (get :width this) id (const hit-line?) layer_line))
	)

(defclass Layers (width height depth scale) :nil
	; (Layers width height depth scale) -> layers
	(def this :depth depth :layers (defq layers (list)))
	(times depth (push layers (Layer width height scale)))

	(defmethod :add_line (p1 p2 radius gap)
		; (. layers :add_line p1 p2 radius gap) -> layers
		(bind '(x1 y1 z1) p1)
		(bind '(x2 y2 z2) p2)
		(defq layer_line (Layer-line x1 y1 x2 y2 radius gap))
		(if (> z1 z2) (defq zt z1 z1 z2 z2 zt))
		(raise :layers)
		(while (<= z1 z2)
			(. (elem-get layers (n2i z1)) :add_line layer_line)
			(setq z1 (+ z1 1.0)))
		this)

	(defmethod :sub_line (p1 p2 radius gap)
		; (. layers :sub_line p1 p2 radius gap) -> layers
		(bind '(x1 y1 z1) p1)
		(bind '(x2 y2 z2) p2)
		(defq layer_line (Layer-line x1 y1 x2 y2 radius gap))
		(if (> z1 z2) (defq zt z1 z1 z2 z2 zt))
		(raise :layers)
		(while (<= z1 z2)
			(. (elem-get layers (n2i z1)) :sub_line layer_line)
			(setq z1 (+ z1 1.0)))
		this)

	(defmethod :hit_line (p1 p2 radius gap)
		; (. layers :hit_line p1 p2 radius gap) -> :nil | :t
		(bind '(x1 y1 z1) p1)
		(bind '(x2 y2 z2) p2)
		(defq layer_line (Layer-line x1 y1 x2 y2 radius gap))
		(if (> z1 z2) (defq zt z1 z1 z2 z2 zt))
		(raise :layers)
		(while (and (<= z1 z2)
					(not (defq hit (. (elem-get layers (n2i z1)) :hit_line layer_line))))
			(setq z1 (+ z1 1.0)))
		hit)

	(defmethod :add_layers_line (layers_line)
		; (. layers :add_layers_line layers_line) -> layers
		(bind '(x1 y1 z1 x2 y2 z2 radius gap) layers_line)
		(defq layer_line (Layer-line x1 y1 x2 y2 radius gap))
		(if (> z1 z2) (defq zt z1 z1 z2 z2 zt))
		(raise :layers)
		(while (<= z1 z2)
			(. (elem-get layers (n2i z1)) :add_line layer_line)
			(setq z1 (+ z1 1.0)))
		this)

	(defmethod :sub_layers_line (layers_line)
		; (. layers :sub_layers_line layers_line) -> layers
		(bind '(x1 y1 z1 x2 y2 z2 radius gap) layers_line)
		(defq layer_line (Layer-line x1 y1 x2 y2 radius gap))
		(if (> z1 z2) (defq zt z1 z1 z2 z2 zt))
		(raise :layers)
		(while (<= z1 z2)
			(. (elem-get layers (n2i z1)) :sub_line layer_line)
			(setq z1 (+ z1 1.0)))
		this)

	(defmethod :hit_layers_line (layers_line)
		; (. layers :hit_layers_line layers_line) -> :nil | :t
		(bind '(x1 y1 z1 x2 y2 z2 radius gap) layers_line)
		(defq layer_line (Layer-line x1 y1 x2 y2 radius gap))
		(if (> z1 z2) (defq zt z1 z1 z2 z2 zt))
		(raise :layers)
		(while (and (<= z1 z2)
					(not (defq hit (. (elem-get layers (n2i z1)) :hit_line layer_line))))
			(setq z1 (+ z1 1.0)))
		hit)
	)

;module
(export-symbols '(Layer-line Layers-line))
(export-classes '(Layer Layers))
(env-pop)

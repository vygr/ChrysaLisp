;;;;;;;;;;;;;
; layer class
;;;;;;;;;;;;;

(import "lib/math/vector.inc")

;module
(env-push)

(enums +line 0
	(enum x1 y1 x2 y2 radius gap))

(enums +record 0
	(enum id line p1 p2 norm dist))

(defmacro Line (x1 y1 x2 y2 radius gap)
	`(fixeds ,x1 ,y1 ,x2 ,y2 ,radius ,gap))

(defun Record (id line)
	(defq p1 (slice +line_x1 +line_x2 line)
		p2 (slice +line_x2 +line_radius line)
		pv (apply fixeds (vec-perp-2d (vec-sub p2 p1)))
		norm (vec-norm pv) dist (vec-dot norm p1))
	(list id line p1 p2 norm dist))

(defun hit-record? (record line d)
	(defq p1 (slice +line_x1 +line_x2 line)
		p2 (slice +line_x2 +line_radius line)
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

	(defmethod :aabb (this line)
		; (. layer :aabb line) -> aabb
		(raise :width :height :scale)
		(bind '(x1 y1 x2 y2 radius gap) line)
		(if (> x1 x2) (defq lt x1 x1 x2 x2 lt))
		(if (> y1 y2) (defq lt y1 y1 y2 y2 lt))
		(defq radius (+ radius gap) width (dec width) height (dec height))
		(list (min width (max 0 (n2i (* (- x1 radius) scale))))
			(min height (max 0 (n2i (* (- y1 radius) scale))))
			(min width (max 0 (n2i (* (+ x2 radius) scale))))
			(min height (max 0 (n2i (* (+ y2 radius) scale))))))

	(defmethod :add_line (this line)
		; (. layer :add_line line)
		(bind '(minx miny maxx maxy) (. this :aabb line))
		(defq record (Record 0 line))
		(raise :width :buckets)
		(setq miny (* miny width) maxy (* maxy width))
		(while (<= miny maxy)
			(each! (+ miny minx) (+ miny maxx 1)
				(lambda (bucket)
					(push bucket record))
				(list buckets))
			(setq miny (+ miny width))))

	(defmethod :sub_line (this line)
		; (. layer :sub_line line)
		(bind '(minx miny maxx maxy) (. this :aabb line))
		(raise :width :buckets)
		(setq miny (* miny width) maxy (* maxy width))
		(while (<= miny maxy)
			(each! (+ miny minx) (+ miny maxx 1)
				(lambda (bucket)
					(when (defq idx (some
							(lambda (record)
								(if (eql (elem +record_line record) line) _))
							bucket))
						(elem-set idx bucket (elem -2 bucket))
						(pop bucket)))
				(list buckets))
			(setq miny (+ miny width))))

	(defmethod :hit_line (this line)
		; (. layer :hit_line line) -> nil | t
		(bind '(minx miny maxx maxy) (. this :aabb line))
		(raise :width :buckets :id)
		(defq id (inc id) idx nil miny (* miny width) maxy (* maxy width))
		(lower :id)
		(while (and (<= miny maxy) (not idx))
			(some! (+ miny minx) (+ miny maxx 1) nil
				(lambda (bucket)
					(setq idx (some
						(lambda (record)
							(when (/= (elem +record_id record) id)
								(elem-set +record_id record id)
								(defq d (+ (elem +line_radius line)
											(elem +line_radius (elem +record_line record))
											(max (elem +line_gap line)
												(elem +line_gap (elem +record_line record)))))
								(if (hit-record? record line d) _)))
						bucket)))
				(list buckets))
			(setq miny (+ miny width)))
		idx)
	)

;module
(export-symbols
	Line)
(export-classes
	Layer)
(env-pop)

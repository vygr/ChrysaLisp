;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; design rule check, of a board as the router prints it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the board as (pcb-read) gives it, what the router printed, checked from
;that alone: no copper of one net nearer to another's than both their
;radii and the larger of their gaps, every net's pads joined by its own
;copper, and all of it on the board. Its own geometry, in reals, none of
;the router's: a pad with a shape is the polygon filled, a pad without
;one, a via, and the ends of a track are round.

;module
(env-push)

(enums +piece 0
	(enum net z1 z2 kind geom radius gap box))

(defq +drc_eps (n2r 0.001) +drc_far (n2r 1000000000))

(defun drc-sq (x) (* x x))

(defun drc-pt-seg (px py x1 y1 x2 y2)
	;the square of the distance from a point to a segment
	(defq dx (- x2 x1) dy (- y2 y1) l (+ (drc-sq dx) (drc-sq dy)))
	(cond
		((= l (n2r 0)) (+ (drc-sq (- px x1)) (drc-sq (- py y1))))
		(:t (defq t (/ (+ (* (- px x1) dx) (* (- py y1) dy)) l))
			(cond
				((< t (n2r 0)) (setq t (n2r 0)))
				((> t (n2r 1)) (setq t (n2r 1))))
			(+ (drc-sq (- px (+ x1 (* t dx)))) (drc-sq (- py (+ y1 (* t dy))))))))

(defun drc-side (ax ay bx by cx cy)
	;which side of a to b the point c is
	(- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax))))

(defun drc-cross? (x1 y1 x2 y2 x3 y3 x4 y4)
	;do two segments cross, each end of one on its own side of the other
	(defq d1 (drc-side x3 y3 x4 y4 x1 y1) d2 (drc-side x3 y3 x4 y4 x2 y2)
		d3 (drc-side x1 y1 x2 y2 x3 y3) d4 (drc-side x1 y1 x2 y2 x4 y4) z (n2r 0))
	(and (or (and (> d1 z) (< d2 z)) (and (< d1 z) (> d2 z)))
		(or (and (> d3 z) (< d4 z)) (and (< d3 z) (> d4 z)))))

(defun drc-seg-seg (x1 y1 x2 y2 x3 y3 x4 y4)
	;the square of the distance between two segments: 0 if they cross,
	;else the nearest of an end of one to the other
	(if (drc-cross? x1 y1 x2 y2 x3 y3 x4 y4)
		(n2r 0)
		(min (drc-pt-seg x1 y1 x3 y3 x4 y4) (drc-pt-seg x2 y2 x3 y3 x4 y4)
			(drc-pt-seg x3 y3 x1 y1 x2 y2) (drc-pt-seg x4 y4 x1 y1 x2 y2))))

(defun drc-inside? (px py pts)
	;is the point inside the polygon (x y ...), by how many of its edges a
	;ray to the right of it crosses
	(defq in :nil n (length pts) j (- n 2))
	(each! (lambda (i)
			(defq xi (elem-get pts i) yi (elem-get pts (inc i))
				xj (elem-get pts j) yj (elem-get pts (inc j)))
			(and (not (eql (> yi py) (> yj py)))
				(< px (+ xi (/ (* (- xj xi) (- py yi)) (- yj yi))))
				(setq in (not in)))
			(setq j i))
		(list (range 0 n 2)))
	in)

(defun drc-edges (pts)
	;the edges of a polygon (x y ...), (x1 y1 x2 y2) each, closed
	(defq n (length pts))
	(map (lambda (i)
			(defq k (% (+ i 2) n))
			(list (elem-get pts i) (elem-get pts (inc i)) (elem-get pts k) (elem-get pts (inc k))))
		(range 0 n 2)))

(defun drc-seg-poly (x1 y1 x2 y2 pts)
	;the square of the distance from a segment to a filled polygon
	(if (or (drc-inside? x1 y1 pts) (drc-inside? x2 y2 pts))
		(n2r 0)
		(reduce (lambda (d (x3 y3 x4 y4)) (min d (drc-seg-seg x1 y1 x2 y2 x3 y3 x4 y4)))
			(drc-edges pts) +drc_far)))

(defun drc-poly-poly (a b)
	;the square of the distance between two filled polygons
	(if (or (drc-inside? (first a) (second a) b) (drc-inside? (first b) (second b) a))
		(n2r 0)
		(reduce (lambda (d (x1 y1 x2 y2)) (min d (drc-seg-poly x1 y1 x2 y2 b)))
			(drc-edges a) +drc_far)))

(defun drc-dist2 (a b)
	;the square of the distance between the centre lines or areas of two pieces
	(defq ga (elem-get a +piece_geom) gb (elem-get b +piece_geom))
	(cond
		((eql (elem-get a +piece_kind) :seg)
			(if (eql (elem-get b +piece_kind) :seg)
				(apply (const drc-seg-seg) (cat ga gb))
				(drc-seg-poly (elem-get ga 0) (elem-get ga 1) (elem-get ga 2) (elem-get ga 3) gb)))
		((eql (elem-get b +piece_kind) :seg)
			(drc-seg-poly (elem-get gb 0) (elem-get gb 1) (elem-get gb 2) (elem-get gb 3) ga))
		(:t (drc-poly-poly ga gb))))

(defun drc-piece (net z1 z2 kind geom radius gap)
	;a piece of copper, and the box it and its gap cover
	(defq xs (map (const first) (partition geom 2)) ys (map (const second) (partition geom 2))
		grow (+ radius gap))
	(list net z1 z2 kind geom radius gap
		(list (- (reduce (const min) xs (first xs)) grow) (- (reduce (const min) ys (first ys)) grow)
			(+ (reduce (const max) xs (first xs)) grow) (+ (reduce (const max) ys (first ys)) grow))))

(defun drc-near? (a b)
	;do their boxes and layers meet
	(bind '(ax1 ay1 ax2 ay2) (elem-get a +piece_box))
	(bind '(bx1 by1 bx2 by2) (elem-get b +piece_box))
	(and (<= ax1 bx2) (<= bx1 ax2) (<= ay1 by2) (<= by1 ay2)
		(<= (elem-get a +piece_z1) (elem-get b +piece_z2))
		(<= (elem-get b +piece_z1) (elem-get a +piece_z2))))

(defun drc-track (net pts radius via gap depth)
	;the pieces of a track or a wire: a segment for each step on a layer,
	;a via through every layer where it changes layer
	(defq out (list) p0 :nil)
	(each (lambda ((x y z))
			(setq x (n2r x) y (n2r y) z (n2i z))
			(when p0
				(bind '(x0 y0 z0) p0)
				(push out (if (= z0 z)
					(drc-piece net z z :seg (list x0 y0 x y) radius gap)
					(drc-piece net 0 (dec depth) :seg (list x0 y0 x0 y0) via gap))))
			(setq p0 (list x y z)))
		pts)
	out)

(defun drc-pieces (pcb_list)
	; -> (pieces pads), pads (net routed (index ...)) of each net
	(bind '(w h depth) (first pcb_list))
	(defq pieces (list) pads (list))
	(each! (lambda ((id track_radius via_radius gap net_pads wires &optional paths))
			(defq tr (n2r track_radius) vr (n2r via_radius) g (n2r gap) mine (list))
			(each (lambda ((radius pad_gap (x y z) shape))
					(defq x (n2r x) y (n2r y) z (n2i z) r (n2r radius) pg (n2r pad_gap))
					(push mine (length pieces))
					(push pieces (if (empty? shape)
						(drc-piece id z z :seg (list x y x y) r pg)
						(drc-piece id z z :poly
							(reduce (lambda (out (sx sy)) (push out (+ x (n2r sx)) (+ y (n2r sy))))
								shape (list))
							r pg))))
				net_pads)
			(push pads (list id (> track_radius 0) mine))
			(each (lambda (pts) (each (# (push pieces %0)) (drc-track id pts tr vr g depth)))
				(cat wires (ifn paths '()))))
		(list pcb_list) 1)
	(list pieces pads))

(defun drc-root (set i)
	(while (/= (elem-get set i) i) (setq i (elem-get set i)))
	i)

(defun pcb-drc (pcb_list)
	; (pcb-drc pcb_list) -> (shorts unjoined outside)
	;shorts, (net_a net_b layer x y) each, two nets' copper nearer than
	;their rules allow, at about there; unjoined, the ids of the nets whose
	;pads are not all joined by their own copper; outside, the ids of the
	;nets with copper off the board
	(bind '(w h depth) (first pcb_list))
	(bind '(pieces pads) (drc-pieces pcb_list))
	(defq shorts (list) unjoined (list) outside (list)
		set (map (const identity) (range 0 (length pieces))) w (n2r w) h (n2r h) z (n2r 0))
	;every pair that is near: a short between two nets, or one net joined
	(each! (lambda (a)
			(defq i (!))
			(each! (lambda (b)
					(when (drc-near? a b)
						(defq d2 (drc-dist2 a b))
						(cond
							((eql (first a) (first b))
								(defq need (+ (elem-get a +piece_radius) (elem-get b +piece_radius) +drc_eps))
								(if (<= d2 (* need need))
									(elem-set set (drc-root set (!)) (drc-root set i))))
							(:t (defq need (- (+ (elem-get a +piece_radius) (elem-get b +piece_radius)
										(max (elem-get a +piece_gap) (elem-get b +piece_gap))) +drc_eps))
								(if (< d2 (* need need))
									(push shorts (list (first a) (first b)
										(max (elem-get a +piece_z1) (elem-get b +piece_z1))
										(n2f (first (elem-get a +piece_geom)))
										(n2f (second (elem-get a +piece_geom))))))))))
				(list pieces) (inc i)))
		(list pieces))
	;a pad through the board is one of each layer at the one point, joined
	;by its hole
	(each (lambda ((id routed mine))
			(each (lambda (i)
					(defq gi (elem-get (elem-get pieces i) +piece_geom))
					(each (lambda (j)
							(if (eql (str (slice (elem-get (elem-get pieces j) +piece_geom) 0 2)) (str (slice gi 0 2)))
								(elem-set set (drc-root set j) (drc-root set i))))
						mine))
				mine))
		pads)
	;each routed net, its pads in one group
	(each (lambda ((id routed mine))
			(and routed (nempty? mine)
				(notevery (# (= (drc-root set %0) (drc-root set (first mine)))) mine)
				(push unjoined id)))
		pads)
	;all on the board
	(each (lambda (p)
			(defq geom (elem-get p +piece_geom))
			(and (or (< (elem-get p +piece_z1) 0) (>= (elem-get p +piece_z2) depth)
					(some (# (if (= (% (!) 2) 0) (or (< %0 z) (> %0 w)) (or (< %0 z) (> %0 h)))) geom))
				(not (find (first p) outside))
				(push outside (first p))))
		pieces)
	(list shorts unjoined outside))

;module
(export-symbols '(pcb-drc))
(env-pop)

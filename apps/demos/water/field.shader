;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water, the reef, functions for a shader to import. How much water
; there is at a place, more than 0 in the water and less in the rock,
; near enough its distance to the rock: the seabed, outcrops of rock
; on it, and caves under it. Everything is of the place, so a chunk
; made by any child, in any order, is the same every time and meets
; its neighbours.
;
; The caves are a lattice of junctions, one in each cell, where the
; hash of its cell puts it, and tunnels between neighbours where the
; hash of the pair says so, so the two cells either side agree. Three
; levels of them: two of caves under the bed, and one over it, in the
; open water, whose junctions are all joined, the ways the auto pilot
; always has. Shafts join the levels, and open in the seabed.
;
; It all goes round every 18496 across, the noise's 289 of its lattice
; at the biggest size, 64, so every size of noise here is a whole
; number of times round it, and the cells and the chunks are too.
; No inputs, no main.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/gpu/shaders/noise.shader")

;how far round the world is, and how big a cell of the lattice is, how
;many go round, and how far apart its levels are
(defconst world 18496.0)
(defconst cell 68.0)
(defconst cells 272.0)
(defconst level 36.0)
;how high the junctions of the open water are over the bed, where the
;surface of the water is, and how near it the diver comes at most
(defconst over_bed 6.0)
(defconst surface 30.0)
(defconst under_surface 5.0)
;the chance of a tunnel along a level of the caves, and of a shaft up
;from each
(defconst cave_link 0.68)
(defconst shaft_link 0.4)

;the hash of a cell, a number from 0 to 1, and k another of the same cell
(defun cell_hash :float ((c :vec3) (k :float))
	(lattice3 (+ (vec3 (mod (:x c) cells) (+ (:y c) 8.0) (mod (:z c) cells)) (* k 17.0))))

;how much a big part of the world is lifted, 0 to 1, smooth across
;squares of four cells, of the hash of their corners: the shallows
(defun region :float ((p :vec2))
	(defq g (/ p (* 4.0 cell)) i (floor g) f (fract g) n (* 0.25 cells))
	(setq f (* f f (- 3.0 (* 2.0 f))))
	(defq a (lattice3 (vec3 (mod (:x i) n) 5.0 (mod (:y i) n)))
		b (lattice3 (vec3 (mod (+ (:x i) 1.0) n) 5.0 (mod (:y i) n)))
		c (lattice3 (vec3 (mod (:x i) n) 5.0 (mod (+ (:y i) 1.0) n)))
		d (lattice3 (vec3 (mod (+ (:x i) 1.0) n) 5.0 (mod (+ (:y i) 1.0) n))))
	(mix (mix a b (:x f)) (mix c d (:x f)) (:y f)))

;how high the seabed is: rolling, and dunes on it, lifted in the shallows
;up to the surface and through it
(defun bed :float ((p :vec2))
	(defq q (* p 0.015625))
	(+ -6.0 (* 10.0 (noise2 q)) (* 4.0 (noise2 (* q 2.0))) (* 1.5 (noise2 (* q 4.0)))
		(* 34.0 (ease 0.5 0.95 (region p)))))

;where the junction of a cell is: anywhere in the middle of the cell, and
;over the bed on the level of the open water
(defun junction :vec3 ((c :vec3))
	(defq x (* (+ (:x c) 0.3 (* 0.4 (cell_hash c 0.0))) cell)
		z (* (+ (:z c) 0.3 (* 0.4 (cell_hash c 1.0))) cell)
		y (* (:y c) level))
	(if (< (:y c) 0.0)
		(setq y (+ y (* 8.0 (- (cell_hash c 2.0) 0.5))))
		(setq y (min (+ (bed (vec2 x z)) over_bed (* 3.0 (cell_hash c 2.0))) (- surface under_surface))))
	(vec3 x y z))

;is there a tunnel from cell c the way of k, 3 to 6: across, along, and
;the two diagonals, and 7 up. It is of the pair, the cell it starts from
;and the way, so both ends agree. All are there over the bed
(defun linked :bool ((c :vec3) (k :float))
	(defq h (cell_hash c k) chance cave_link)
	(if (> k 6.5) (setq chance shaft_link))
	(if (> (:y c) -0.5) (setq chance 1.1))
	(< h chance))

;how far a place is from the curve of a tunnel from a to b, bent through
;a point the hash of the pair moves it to, a curve of two halves
(defun along :float ((p :vec3) (a :vec3) (b :vec3) (bend :vec3))
	(defq m (+ (* 0.5 (+ a b)) bend)
		ab (- m a) bm (- b m)
		t (clamp (/ (dot (- p a) ab) (dot ab ab)) 0.0 1.0)
		u (clamp (/ (dot (- p m) bm) (dot bm bm)) 0.0 1.0))
	(min (length (- p (+ a (* ab t)))) (length (- p (+ m (* bm u))))))

;how far the middle of the tunnel of the pair, c the way of k, is moved
;off the straight, so it bends
(defun bend :vec3 ((c :vec3) (k :float))
	(* (- (vec3 (cell_hash c (+ k 5.0)) (cell_hash c (+ k 9.0)) (cell_hash c (+ k 13.0))) 0.5)
		(vec3 22.0 6.0 22.0)))

;1 if the pair has a tunnel, 0 if not, for Lisp
(defun link_open :float ((c :vec3) (k :float))
	(if (linked c k) 1.0 0.0))

;how far into the open the tunnel of the pair is at p, c the way of k
;from junction a to b, its radius less how far p is from it, less than 0
;outside it, or far below 0 if the pair has none
(defun tunnel :float ((p :vec3) (c :vec3) (k :float) (a :vec3) (b :vec3) (r :float))
	(defq o -100.0)
	(when (linked c k)
		(setq o (- r (along p a b (bend c k)))))
	o)

;a chamber at a junction of the caves, its size of the hash, none at some:
;how far into it p is, less than 0 outside it, near enough a distance
(defun chamber :float ((p :vec3) (c :vec3) (j :vec3))
	(defq o -100.0)
	(when (and (< (:y c) -0.5) (< (cell_hash c 20.0) 0.6))
		(defq size (vec3 (+ 9.0 (* 5.0 (cell_hash c 21.0))) (+ 5.0 (* 3.0 (cell_hash c 22.0)))
				(+ 9.0 (* 5.0 (cell_hash c 23.0)))))
		(setq o (* (:y size) (- 1.0 (length (/ (- p j) size))))))
	o)

;how far into the open of the caves p is, of each level of the caves
;near it: the chamber of its cell, the tunnels of its cell and those that
;come to it, and the diagonals of the cells beside it that cross its
;corners, the nine junctions round it worked out the once; and the shafts,
;up to the junctions of the open water, which are their mouths. The ways
;of the open water carve nothing, through an outcrop they would leave a
;shell of it floating: they are only the auto pilot's
(defun caves :float ((p :vec3) (r :float))
	(defq cx (floor (/ (:x p) cell)) cz (floor (/ (:z p) cell)) o -100.0)
	(for (l 0 2)
		(defq c (vec3 cx (- (float l) 2.0) cz) j11 (junction c))
		;a level is only looked at near it, a tunnel keeps near its level
		(when (< (abs (- (:y p) (:y j11))) 30.0)
			(defq j00 (junction (+ c (vec3 -1.0 0.0 -1.0))) j01 (junction (+ c (vec3 -1.0 0.0 0.0)))
				j02 (junction (+ c (vec3 -1.0 0.0 1.0))) j10 (junction (+ c (vec3 0.0 0.0 -1.0)))
				j12 (junction (+ c (vec3 0.0 0.0 1.0))) j20 (junction (+ c (vec3 1.0 0.0 -1.0)))
				j21 (junction (+ c (vec3 1.0 0.0 0.0))) j22 (junction (+ c (vec3 1.0 0.0 1.0)))
				cxm (+ c (vec3 -1.0 0.0 0.0)) czm (+ c (vec3 0.0 0.0 -1.0)))
			(setq o (max o (chamber p c j11)))
			(setq o (max o (tunnel p c 3.0 j11 j21 r)))
			(setq o (max o (tunnel p c 4.0 j11 j12 r)))
			(setq o (max o (tunnel p c 5.0 j11 j22 r)))
			(setq o (max o (tunnel p c 6.0 j11 j20 r)))
			(setq o (max o (tunnel p cxm 3.0 j01 j11 r)))
			(setq o (max o (tunnel p czm 4.0 j10 j11 r)))
			(setq o (max o (tunnel p (+ c (vec3 -1.0 0.0 -1.0)) 5.0 j00 j11 r)))
			(setq o (max o (tunnel p (+ c (vec3 -1.0 0.0 1.0)) 6.0 j02 j11 r)))
			(setq o (max o (tunnel p (+ c (vec3 0.0 0.0 1.0)) 6.0 j12 j21 r)))
			(setq o (max o (tunnel p cxm 5.0 j01 j12 r)))
			(setq o (max o (tunnel p cxm 6.0 j01 j10 r)))
			(setq o (max o (tunnel p czm 5.0 j10 j21 r)))))
	;the shafts up from the two levels of the caves
	(defq c0 (vec3 cx -2.0 cz) c1 (vec3 cx -1.0 cz) c2 (vec3 cx 0.0 cz) a0 (junction c0) a1 (junction c1))
	(max (max o (tunnel p c0 7.0 a0 a1 r)) (tunnel p c1 7.0 a1 (junction c2) r)))

;the reef's rock over the bed: outcrops, arches and overhangs of noise,
;fewer the higher, quickly enough that none floats free of the bed, and
;the seabed under them
(defun rock :float ((p :vec3))
	(defq q (* p 0.03125)
		n (+ (* 0.6 (noise3 q)) (* 0.3 (noise3 (* q 2.0))) (* 0.1 (noise3 (* q 4.0))))
		h (- (:y p) (bed (:xz p))))
	(min h (* 18.0 (- (+ 0.6 (* h 0.032)) n))))

;stalactites down from the roof of a chamber and stalagmites up from its
;floor, on a grid of 4.5 across, a spike where the hash says so: the least
;distance to one, of the chamber of p's own cell on the level nearest it.
;A spike is never so far from the middle of its square of the grid, nor so
;fat, that it reaches the next square, where it would be cut off flat.
;None stands in the middle of a chamber, where its tunnels meet, so the way
;through is open, and few are long enough to meet as a column
(defun spikes :float ((p :vec3))
	(defq ly (clamp (floor (+ (/ (:y p) level) 0.5)) -2.0 -1.0)
		c (vec3 (floor (/ (:x p) cell)) ly (floor (/ (:z p) cell))) o 100.0)
	(when (< (cell_hash c 20.0) 0.6)
		(defq g (floor (/ (:xz p) 4.5)) s (vec3 (:x g) ly (:y g))
			at (* (+ g 0.35 (* 0.3 (vec2 (cell_hash s 30.0) (cell_hash s 31.0)))) 4.5)
			j (junction c)
			size (vec3 (+ 9.0 (* 5.0 (cell_hash c 21.0))) (+ 5.0 (* 3.0 (cell_hash c 22.0)))
				(+ 9.0 (* 5.0 (cell_hash c 23.0))))
			e (/ (- at (:xz j)) (:xz size))
			rr (dot e e))
		(when (and (> rr 0.11) (< rr 0.8) (< (cell_hash s 32.0) 0.35))
			(defq half (* (:y size) (sqrt (- 1.0 rr)))
				top (+ (:y j) half) floor_at (- (:y j) half)
				hd (length (- (:xz p) at))
				;lumpy and ringed, as if built up drip by drip: the radius
				;of a spike swells and narrows along it, never to nothing, and never
				;past the margin of its square
				lumpy (+ 0.7 (* 0.4 (noise3 (+ (* p 0.9) (* s 7.0))))
					(* 0.1 (sin (+ (* (:y p) 2.7) (* 6.0 (noise3 (* p 0.45)))))))
				;down from the roof, long and thin, to its tip
				lt (* 2.0 half (+ 0.15 (* 0.45 (cell_hash s 33.0))))
				bt (+ 0.45 (* 0.5 (cell_hash s 34.0)))
				tt (clamp (/ (- (:y p) (- top lt)) lt) 0.0 1.0)
				down (max (max (- hd (* bt (sqrt tt) lumpy)) (- (- top lt) (:y p))) (- (:y p) (+ top 6.0)))
				;up from the floor, short and broad
				lm (* 2.0 half (+ 0.1 (* 0.3 (cell_hash s 35.0))))
				bm (+ 0.6 (* 0.6 (cell_hash s 36.0)))
				tm (clamp (/ (- (+ floor_at lm) (:y p)) lm) 0.0 1.0)
				up (max (max (- hd (* bm (sqrt tm) lumpy)) (- (:y p) (+ floor_at lm))) (- (- floor_at 6.0) (:y p))))
			(setq o (min down up))))
	o)

;how far a point is moved before the caves are worked out, so they wind:
;a tunnel is open where a point moved so is near its curve, so the middle
;of it near a point of the curve is near that point less this
(defun warp :vec3 ((p :vec3))
	(defq q (* p 0.09375))
	(* (- (vec3 (noise3 q) (noise3 (+ q 31.0)) (noise3 (+ q 63.0))) 0.5) 7.0))

;how much water there is at p: the rock, the caves taken out of it, of
;the point moved, their width of noise too, and the spikes of the
;chambers put back
(defun water :float ((p :vec3))
	(defq r (+ 2.8 (* 2.4 (noise3 (* p 0.0625)))))
	(min (max (rock p) (caves (+ p (warp p)) r)) (spikes p)))

;how near the caves are, more than 0 in them, for nothing to block their
;ways in and out
(defun cave_near :float ((p :vec3))
	(caves (+ p (warp p)) (+ 2.8 (* 2.4 (noise3 (* p 0.0625))))))

;the way to the sun, high and a little to one side
(defconst sun_way (vec3 0.35 0.88 0.32))

;how much of the light from a way reaches p, 0 to 1: a place in the rock
;first goes out of it that way, a little, so a place just under a face
;has the light of the face; then a march toward the light to over the
;surface of the water, as far as rock that breaks it, a soft shadow of
;how near the rock comes to the way, k the softness
(defun lit_by :float ((p :vec3) (way :vec3) (k :float))
	(defq t 0.0 lit 1.0)
	(for (i 0 12)
		(when (> (water (+ p (* way t))) 0.0) (break))
		(setq t (+ t 0.5)))
	(defq top (/ (- (+ surface 20.0) (:y p)) (:y way)) start t)
	(setq t (+ t 0.3))
	(for (i 0 14)
		(when (> t top) (break))
		(defq h (water (+ p (* way t))))
		(when (< h 0.02) (return 0.0))
		(setq lit (min lit (/ (* k h) (- t start)))
			t (+ t (clamp h 0.5 5.0))))
	lit)

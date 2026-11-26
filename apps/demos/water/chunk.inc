;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water's chunks of the reef, and the job a child of its farm does to make
; one. A chunk is a cube of the world 17 across, named by its three whole
; numbers. Its field is worked out by a kernel, grid.shader, at so many
; places each way and two more all round, which of the corners of each of
; its cubes are in the rock by another, cases.shader, and only the cubes
; the surface goes through are marched, by the tables of surface.inc. A
; vertex is where the surface cuts an edge of a cube, its normal the slope
; of the field there, of the grid, so the chunks either side of a face
; meet with no seam: all of it is of the place, field.shader. A chunk is
; marched a cube past each face, over its neighbours': where they are as
; fine it is the very same triangles, and where one is coarser it covers
; the gap between surfaces marched at two sizes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/task/jobs.inc")
(import "lib/consts/numbers.inc")
(import "lib/math/surface.inc")
(import "lib/gpu/vp.inc")
(import "apps/demos/water/stones.inc")
(import "apps/demos/water/weeds.inc")

;a chunk for a child to make: the app's number of it, where it is, and
;how many cubes it is each way
(structure +chunk +job_size
	(uint id cubes)
	(int x y z))

;its answer, the stones on its floors, +stone_size numbers each, and then
;the mesh as the shaders want it, a vertex where it is in the chunk, 0 to
;its size each way, its normal, and the light of the sun and of the sky
;there
(structure +chunk_reply +job_reply_size
	(uint id stones)
	(offset data))

;a stone: where it lies, the way that is up for it, how far it is turned
;about that, how big, which kind, stones.inc, the sun's and the sky's
;light there, 0 sandstone or 1 basalt, and how high the floor is
(defq +stone_size 13)

;how big a chunk is
(defq +chunk_side 17
	;how many steps each way the coarse grid of its light is
	+light_steps 8)

;module
(env-push)

;the corners of a cube in the order the tables have them, 0 to 3 round
;its near face, 4 to 7 round its far, and the two corners of each edge
(defq +corners ''((0 0 0) (1 0 0) (1 1 0) (0 1 0) (0 0 1) (1 0 1) (1 1 1) (0 1 1))
	+edges ''((0 1) (1 2) (2 3) (3 0) (4 5) (5 6) (6 7) (7 4) (0 4) (1 5) (2 6) (3 7))
	+chunk_d (n2d +chunk_side) +light_d (n2d +light_steps)
	;the stones: how many places each way of a chunk one may lie, how far
	;the floor is looked for a step, how far apart the places are round
	;the world, the range of a hash; how many lie where most do, of 1, and
	;how big the clumps of them are; the size of a sandstone region, how
	;much a floor faces up to have one, how much room over it a stone wants
	;and how much of that it may be, and the least it may be; how far up is
	;looked for open water, and how open, that wants no room counted
	+stone_grid 8 +stone_d (/ (n2d +chunk_side) (n2d 8)) +floor_step +double_1 +grid_round 8704
	+hash_range (n2d 16777216) +stones_most (n2d 0.2) +clump_scale (n2d 0.06) +region_scale (n2d 0.025)
	+basalt_from (n2d 0.55) +facing_up (n2d 0.5) +room +double_2 +room_share (n2d 0.7) +bulk_least (n2d 0.25)
	+room_probe (n2d 6) +room_open +double_4
	+slope_e +double_1/4
	;the slabs of whole beds: how many places of their coarse grid each way
	;of a chunk, how far apart, how many round the world, how many places
	;have one, how far from the middle of one a smaller stone may lie, of
	;its size, and how many places round a smaller stone may have one near
	;enough to be in
	+slabs_across 2 +slab_d (/ (n2d +chunk_side) (n2d 2)) +slab_round 2176 +slab_chance (n2d 0.35)
	+slab_clear (n2d 0.75) +slab_near 2
	;how far past its size a stone keeps from the ways into the caves
	+cave_clear +double_2
	;the patches of weed: how big, from where in them, over how much they
	;grow from none to all, how much of the sky a place must have, and how
	;far a clump is sunk, of its height
	+weed_scale (n2d 0.035) +weed_from (n2d 0.5) +weed_edge (n2d 0.15) +weed_sky (n2d 0.3) +weed_sink (n2d 0.03)
	;how much thicker a patch of weed grows than one of stones, more of 1
	+weed_thicker +double_1/2)

(defun chunk-kernels ()
	; (chunk-kernels) -> kernels
	;the two kernels, each its program and native code, for a child to
	;make the once and keep
	(defq grid_prog (shader-load "apps/demos/water/grid.shader")
		case_prog (shader-load "apps/demos/water/cases.shader")
		light_prog (shader-load "apps/demos/water/light.shader")
		field (shader-load "apps/demos/water/field.shader"))
	(list grid_prog (shader-vp-kernel grid_prog) case_prog (shader-vp-kernel case_prog)
		light_prog (shader-vp-kernel light_prog) (shader-vp-func field 'water) (shader-vp-func field 'noise3)
		(shader-vp-func field 'cave_near)))

(defun hash-on (h)
	; (hash-on h) -> num
	;the next hash of a hash
	(logand (+ (* (logand h 0x7fffffff) 1103515245) 12345) 0x7fffffff))

(defun hash-of (a b c)
	; (hash-of a b c) -> num
	;a hash of three whole numbers, 0 to 0x7fffffff
	(hash-on (logxor (* a 73856093) (* b 19349663) (* c 83492791))))

(defun hash-unit (h)
	; (hash-unit h) -> double
	;a hash as a number from 0 to 1
	(/ (n2d (>> h 7)) +hash_range))

(defun light-at (light k x y z)
	; (light-at light k x y z) -> doubles
	;the sun's and the sky's light at a place of a chunk, of the coarse
	;grid of it, k the step of the grid, between its eight places round it
	;a place a cube past a face has the light of the face
	(defq side (inc +light_steps) gx (max +double_0 (min +light_d (/ x k))) gy (max +double_0 (min +light_d (/ y k)))
		gz (max +double_0 (min +light_d (/ z k)))
		ix (min (dec +light_steps) (n2i gx)) iy (min (dec +light_steps) (n2i gy)) iz (min (dec +light_steps) (n2i gz))
		fx (- gx (n2d ix)) fy (- gy (n2d iy)) fz (- gz (n2d iz)) sun +double_0 sky +double_0)
	(each (lambda ((ox oy oz))
			(defq w (* (if (= ox 0) (- +double_1 fx) fx) (if (= oy 0) (- +double_1 fy) fy) (if (= oz 0) (- +double_1 fz) fz))
				at (* 2 (+ ix ox (* side (+ iy oy (* side (+ iz oz)))))))
			(setq sun (+ sun (* w (n2d (elem-get light at)))) sky (+ sky (* w (n2d (elem-get light (inc at)))))))
		+corners)
	(doubles sun sky))

(defun slope (grid g side s2)
	; (slope grid g side s2) -> doubles
	;the slope of the field at a place of the grid, of its neighbours
	(doubles (n2d (- (elem-get grid (inc g)) (elem-get grid (dec g))))
		(n2d (- (elem-get grid (+ g side)) (elem-get grid (- g side))))
		(n2d (- (elem-get grid (+ g s2)) (elem-get grid (- g s2))))))

(defun wrap-of (i n)
	; (wrap-of i n) -> num
	;a whole number round the world, 0 to n
	(% (+ (% i n) n) n))

(defun basalt-at? (noise x z)
	; (basalt-at? noise x z) -> :t | :nil
	;is a place of the basalt, or of the sandstone, the regions of them
	;going all the way down
	(> (noise (doubles (* x +region_scale) +double_0 (* z +region_scale))) +basalt_from))

(defun slab-at (noise ci cj)
	; (slab-at noise ci cj) -> :nil | (x z kind bulk h)
	;the slab of a whole bed of a place of the coarse grid, if it has one,
	;where its middle is and how big, of a hash of the place alone, so the
	;smaller stones round it know where it is, whichever chunk it is in
	(defq h (hash-of (wrap-of ci +slab_round) (wrap-of cj +slab_round) 7))
	(when (< (hash-unit h) +slab_chance)
		(defq h (hash-on h) x (* +slab_d (+ (n2d ci) (hash-unit h)))
			h (hash-on h) z (* +slab_d (+ (n2d cj) (hash-unit h)))
			h (hash-on h) r (hash-unit h))
		(unless (basalt-at? noise x z)
			(bind '(kind bulk) (stone-slab h r))
			(list x z kind bulk h))))

(defun under-slab? (noise x z)
	; (under-slab? noise x z) -> :t | :nil
	;would a smaller stone here be in a slab, more than at its edge
	(defq ci (n2i (floor (/ x +slab_d))) cj (n2i (floor (/ z +slab_d))) under :nil)
	(each! (lambda ()
			(defq j (!))
			(each! (lambda ()
					(when (defq slab (slab-at noise (!) j))
						(bind '(sx sz & bulk &ignore) slab)
						(defq dx (- x sx) dz (- z sz) clear (* bulk +slab_clear))
						(if (< (+ (* dx dx) (* dz dz)) (* clear clear)) (setq under :t))))
				'() (- ci +slab_near) (+ ci +slab_near 1)))
		'() (- cj +slab_near) (+ cj +slab_near 1))
	under)

(defun floors-in (water px pz oy top)
	; (floors-in water px pz oy top) -> ((y n) ...)
	;the floors down a line of a chunk, water over rock, that face up enough
	;for a stone, each how high and the way up of it
	(defq out (list) y top fa (water (doubles px y pz)))
	(while (> y oy)
		(defq yb (- y +floor_step) fb (water (doubles px yb pz)))
		(when (and (> fa +double_0) (<= fb +double_0))
			(defq py (- y (/ fa (- fa fb))) e +slope_e
				g (doubles (- (water (doubles (+ px e) py pz)) (water (doubles (- px e) py pz)))
					(- (water (doubles px (+ py e) pz)) (water (doubles px (- py e) pz)))
					(- (water (doubles px py (+ pz e))) (water (doubles px py (- pz e)))))
				gg (nums-dot g g))
			(when (> gg +double_0)
				(defq n (nums-scale g (recip (sqrt gg))))
				(if (> (second n) +facing_up) (push out (list py n)))))
		(setq y yb fa fb))
	out)

(defun stone-at (water light light_k p n kind bulk h basalt ox oy oz)
	; (stone-at water light light_k p n kind bulk h basalt ox oy oz) -> doubles
	;a stone lying on a floor at p, up n, as its kind lies, turned and
	;leaning by its hash, no bigger than the water over it has room for
	(defq h (hash-on h) yaw (* (hash-unit h) +double_2pi) h (hash-on h) lx (- (* +double_2 (hash-unit h)) +double_1)
		h (hash-on h) lz (- (* +double_2 (hash-unit h)) +double_1))
	(bind '(up sink) (stone-lie kind n lx lz))
	(bind '(px py pz) p)
	;open water over it, or so much room
	(defq bulk (if (> (water (nums-add p (nums-scale n +room_probe))) +room_open) bulk
		(max +bulk_least (min bulk (* +room_share (water (nums-add p (nums-scale n +room))))))))
	(cat (nums-sub p (nums-scale up (* sink bulk))) up (doubles yaw bulk (n2d kind))
		(light-at light light_k (- px ox) (- py oy) (- pz oz)) (doubles (if basalt +double_1 +double_0) py)))

(defun clear-of-caves? (cave p n bulk)
	; (clear-of-caves? cave p n bulk) -> :t | :nil
	;is a stone of a size on a floor at p, up n, clear of the ways in and
	;out of the caves
	(< (cave (nums-add p n)) (neg (+ bulk +cave_clear))))

(defun weedy (noise x z)
	; (weedy noise x z) -> double
	;how much a place is a patch of weed, 0 none to 1, the chance a spot
	;there grows weed and not a stone
	(max +double_0 (min +double_1 (/ (- (noise (doubles (* x +weed_scale) +double_0 (* z +weed_scale))) +weed_from)
		+weed_edge))))

(defun weed-at (water light light_k p n kind bulk h grass ox oy oz)
	; (weed-at water light light_k p n kind bulk h grass ox oy oz) -> doubles
	;a clump of weed growing on a floor at p, up n, near upright, turned by
	;its hash, no taller than the water over it has room for
	(defq h (hash-on h) yaw (* (hash-unit h) +double_2pi) up (weed-lie n))
	(bind '(px py pz) p)
	(defq bulk (if (> (water (nums-add p (nums-scale n +room_probe))) +room_open) bulk
		(max +bulk_least (min bulk (* +room_share (water (nums-add p (nums-scale n +room))))))))
	(cat (nums-sub p (nums-scale up (* +weed_sink bulk))) up (doubles yaw bulk (n2d kind))
		(light-at light light_k (- px ox) (- py oy) (- pz oz)) (doubles grass py)))

(defun chunk-stones (water noise cave light light_k cx cy cz)
	; (chunk-stones water noise cave light light_k cx cy cz) -> doubles
	;the stones on the floors of a chunk: the slabs of whole beds whose
	;middles are in it, of the coarse grid; then at each place of the fine
	;grid, moved a little, on each floor that faces up enough, by a hash of
	;the place and the floor, a smaller stone, unless it would be in a slab,
	;its kind by the region, sandstone or basalt, and by the hash; none in
	;the ways into the caves
	(defq out (list (doubles)) ox (* (n2d cx) +chunk_d) oy (* (n2d cy) +chunk_d) oz (* (n2d cz) +chunk_d)
		top (+ oy +chunk_d) ci (* cx +slabs_across) cj (* cz +slabs_across))
	(each! (lambda ()
			(defq j (!))
			(each! (lambda ()
					(when (defq slab (slab-at noise (!) j))
						(bind '(x z kind bulk h) slab)
						(each (lambda ((py n))
								(if (clear-of-caves? cave (defq p (doubles x py z)) n bulk)
									(push out (stone-at water light light_k p n kind bulk h :nil ox oy oz))))
							(floors-in water x z oy top))))
				'() ci (+ ci +slabs_across)))
		'() cj (+ cj +slabs_across))
	(each! (lambda ()
			(defq gz (!) wz (wrap-of (+ (* cz +stone_grid) gz) +grid_round))
			(each! (lambda ()
					(defq gx (!) wx (wrap-of (+ (* cx +stone_grid) gx) +grid_round)
						h (hash-of wx wz 0) px (+ ox (* +stone_d (+ (n2d gx) (hash-unit h))))
						h (hash-on h) pz (+ oz (* +stone_d (+ (n2d gz) (hash-unit h)))))
					(each (lambda ((py n))
							(defq p (doubles px py pz) h (hash-of wx wz (+ 1000 (n2i (floor py)))))
							;a patch of weed grows thicker than one of stones
							(when (and (< (hash-unit h) (* +stones_most (defq c (* +double_2 (noise (nums-scale p +clump_scale)))) c c
										(+ +double_1 (* +weed_thicker (weedy noise px pz)))))
									(not (under-slab? noise px pz)))
								(defq basalt (basalt-at? noise px pz) h (hash-on h) pick (hash-unit h) h (hash-on h) r (hash-unit h)
									h (hash-on h) sky (second (light-at light light_k (- px ox) (- py oy) (- pz oz))))
								(cond
									;in a patch of weed, where the sky reaches, a clump
									((and (> sky +weed_sky) (< (hash-unit h) (weedy noise px pz)))
										(bind '(kind bulk grass) (weed-pick pick (hash-on h) r))
										(if (clear-of-caves? cave p n bulk)
											(push out (weed-at water light light_k p n (+ +stone_kinds kind) bulk h grass ox oy oz))))
									(:t (bind '(kind bulk) (stone-pick basalt pick h r))
										(if (clear-of-caves? cave p n bulk)
											(push out (stone-at water light light_k p n kind bulk h basalt ox oy oz)))))))
						(floors-in water px pz oy top)))
				'() 0 +stone_grid))
		'() 0 +stone_grid)
	(apply (const cat) out))

(defun chunk-mesh (kernels cx cy cz cubes)
	; (chunk-mesh kernels cx cy cz cubes) -> (stones mesh)
	;the triangles of the surface in a chunk, cubes of them each way, each
	;corner x y z 1, its normal, and its sun and sky, in the chunk; and the
	;stones on its floors, none if it has no surface
	(bind '(grid_prog grid_native case_prog case_native light_prog light_native water noise cave) kernels)
	;the grid two places past each face, one for the cubes marched past it
	;and one for their slopes; the cubes marched, one past each face
	(defq side (+ cubes 5) s2 (* side side) n (* s2 side) marched (+ cubes 2) nc (* marched marched marched)
		step (/ +chunk_d (n2d cubes)) grid (fill (floats) n +float_0) cases (str-alloc (* nc 4))
		;where each corner of a cube is in the grid from its first, and where
		;it is, as doubles, from the cube's
		offs (map (lambda ((ox oy oz)) (+ ox (* side oy) (* s2 oz))) +corners)
		at (map (lambda ((ox oy oz)) (list (n2d ox) (n2d oy) (n2d oz))) +corners)
		out (list (doubles)) light_side (inc +light_steps) light_k (/ +chunk_d (n2d +light_steps))
		light (fill (floats) (* 2 light_side light_side light_side) +float_0) lit :nil)
	(shader-vp-run grid_prog grid_native (shader-vp-frame grid_prog grid_native (list
			(list 'origin (doubles (- (* (n2d cx) +chunk_d) (* +double_2 step)) (- (* (n2d cy) +chunk_d) (* +double_2 step))
				(- (* (n2d cz) +chunk_d) (* +double_2 step))))
			(list 'step step) (list 'side side))) 0 n (list grid))
	(shader-vp-run case_prog case_native (shader-vp-frame case_prog case_native (list
			(list 'side side) (list 'cubes marched))) 0 nc (list grid cases))
	(each! (lambda ()
			(defq c (get-uint cases (* (!) 4)))
			(when (< 0 c 255)
				;the light worked out at the first cube with a surface, a chunk
				;all rock or all water has none
				(unless lit
					(setq lit :t)
					(shader-vp-run light_prog light_native (shader-vp-frame light_prog light_native (list
							(list 'origin (doubles (* (n2d cx) +chunk_d) (* (n2d cy) +chunk_d) (* (n2d cz) +chunk_d)))
							(list 'step light_k) (list 'side light_side))) 0 (* light_side light_side light_side) (list light)))
				;a cube's place in the chunk, the first one a cube before it
				(defq i (!) z (/ i (* marched marched)) rest (- i (* z marched marched)) y (/ rest marched)
					x (- rest (* y marched)) base (+ (inc x) (* side (inc y)) (* s2 (inc z)))
					fx (n2d (dec x)) fy (n2d (dec y)) fz (n2d (dec z)) verts (list :nil :nil :nil :nil :nil :nil :nil :nil :nil :nil :nil :nil))
				(each (lambda (tri)
						;the vertex of each edge, the once a cube
						(each (lambda (e)
								(unless (elem-get verts e)
									(bind '(a b) (elem-get +edges e))
									(defq ga (+ base (elem-get offs a)) gb (+ base (elem-get offs b))
										va (n2d (elem-get grid ga)) vb (n2d (elem-get grid gb))
										t (/ va (- va vb)))
									(bind '((ax ay az) (bx by bz)) (list (elem-get at a) (elem-get at b)))
									(defq na (slope grid ga side s2) nb (slope grid gb side s2)
										nn (nums-add na (nums-scale (nums-sub nb na) t)) d (nums-dot nn nn))
									(if (> d +double_0) (nums-scale nn (recip (sqrt d)) nn))
									(defq px (* step (+ fx ax (* (- bx ax) t))) py (* step (+ fy ay (* (- by ay) t)))
										pz (* step (+ fz az (* (- bz az) t))))
									(elem-set verts e (cat (doubles px py pz +double_1) nn
										(light-at light light_k px py pz)))))
							tri)
						(bind '(e0 e1 e2) tri)
						(push out (elem-get verts e0) (elem-get verts e2) (elem-get verts e1)))
					(elem-get +tri_table c))))
		'() 0 nc)
	(list (if lit (chunk-stones water noise cave light light_k cx cy cz) (doubles)) (apply (const cat) out)))

;module
(export-symbols '(chunk-kernels chunk-mesh))
(env-pop)

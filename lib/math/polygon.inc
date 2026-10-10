;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; set logic of polygons
;
; Two sets of polygons, each a list of paths as (:fpoly) fills them, and
; what is in both, in either, in the first and not the second, or in just
; one: (poly-op). What comes back is a list of paths, to fill by either
; winding rule, a hole goes round the other way to what it is a hole in.
;
; A path is fixed point, 48.16, and so is all of this. The fixed numbers
; are taken as the whole numbers they are held as, and every sum is of
; whole numbers: nothing is rounded but where a point has to be made
; that was not there, where two edges cross, and that is made once.
;
; How. It is the scan line of a fill, with bands for lines:
;
; * every y a point is at cuts the plane into bands, and within a band
;   every y at which two edges cross cuts it again. So in a band no two
;   edges cross, and each edge that is in it goes from its top to its
;   bottom.
; * in a band the edges are put in order across, and gone along with a
;   count of how many times each set is wound round. Where what is asked
;   for begins to be so and where it ends are an edge each, and between
;   them is a piece of the answer, with four corners.
; * the left of a piece is a line going down and the right a line going
;   up. Along the line between two bands, where one has a piece and the
;   other has none there is a line across. Each line begins where
;   another ends, they are joined up into rings, and those are the answer.
; * lines that follow one another along the one edge are one line, so
;   a ring has a point only where the edge it follows changes.
;
; No function here calls itself.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/vector.inc")

;two edges of a band this close at both ends, in the units a fixed number
;is held in, are the one edge: an edge of each set that lie along each
;other are worked out apart, and may not come to the very same number
(defq +poly_near 2)

(defun poly-raw (paths)
	; (poly-raw paths) -> ((x y ...) ...)
	;the paths with each number as the whole number it is held as
	(map (lambda (p) (map (# (n2i (* %0 65536.0))) p)) paths))

(defun poly-num (raw)
	; (poly-num raw) -> fixed
	;the fixed number that is held as this whole number, its whole part
	;and its fraction made apart, so a big one has the room
	(+ (n2f (>>> raw 16)) (/ (n2f (logand raw 0xffff)) 65536.0)))

(defun poly-fixed (raw)
	; (poly-fixed ((x y ...) ...)) -> paths
	(map (lambda (p) (apply (const path) (map (const poly-num) p))) raw))

(defun poly-edges (raw set edges ys)
	; (poly-edges raw set edges ys) -> edges
	;the edges of a set, each (ytop ybot xtop xbot wind set id), those that
	;lie flat left out, and every y a point is at
	(each (lambda (p)
		(defq n (length p))
		(when (>= n 6)
			(defq x0 (elem-get p (- n 2)) y0 (elem-get p (- n 1)) i 0)
			(while (< i n)
				(defq x1 (elem-get p i) y1 (elem-get p (inc i)))
				(push ys y1)
				(cond
					((< y0 y1) (push edges (list y0 y1 x0 x1 1 set (length edges))))
					((> y0 y1) (push edges (list y1 y0 x1 x0 -1 set (length edges)))))
				(setq x0 x1 y0 y1 i (+ i 2)))))
		raw)
	edges)

(defun poly-x (e y)
	; (poly-x edge y) -> x
	;where an edge is at a y, its ends as they are
	(bind '(yt yb xt xb &ignore) e)
	(cond
		((= y yt) xt)
		((= y yb) xb)
		(:t (+ xt (/ (* (- xb xt) (- y yt)) (- yb yt))))))

(defun poly-order (xs)
	; (poly-order xs) -> (index ...)
	;the places of a list of numbers, the least first. Each number is put
	;with its place into one number, and those sorted, which is the sort
	;with no function of Lisp called for each two it holds together
	(map (# (logand %0 0xfffff)) (sort (map (# (+ (<< %0 20) (!))) xs) (const -))))

(defun poly-cuts (order xa xb ya yb)
	; (poly-cuts order xa xb ya yb) -> (y ...)
	;the ys within a band at which two of its edges cross, in order, with
	;the top and the bottom of the band. order is the edges across the
	;top, xa and xb where each is at the top and at the bottom
	(defq cuts (list ya yb) n (length order) i 0)
	(while (< i (dec n))
		(defq e (elem-get order i) x0 (elem-get xa e) x1 (elem-get xb e) j (inc i))
		(while (< j n)
			(defq f (elem-get order j) x3 (elem-get xb f))
			(when (> x1 x3)
				;as far down the band as the gap at its top is of the
				;gap at its top and the gap, the other way, at its bottom
				(defq num (- (elem-get xa f) x0) den (+ num (- x1 x3))
					y (+ ya (/ (* (- yb ya) num) den)))
				(if (< ya y yb) (push cuts y)))
			(setq j (inc j)))
		(setq i (inc i)))
	(usort cuts (const -)))

(defun poly-in? (wind mode)
	; (poly-in? wind mode) -> :t | :nil
	(if (= mode 0) (/= 0 (logand wind 1)) (/= 0 wind)))

(defun poly-spans (active order x0s x1s op mode_a mode_b)
	; (poly-spans active order x0s x1s op mode_a mode_b) -> ((xl0 xl1 xr0 xr1 idl idr) ...)
	;the pieces of the answer in a band no edges cross in, from the left.
	;order is its edges across, x0s and x1s where each is at its top and
	;at its bottom
	(defq out (list) wa 0 wb 0 inside :nil px0 +min_long px1 +min_long
		gx0 :nil gx1 0 gid 0 lx0 0 lx1 0 lid 0)
	(each (lambda (i)
		;none is to the left of the one before it, at either end
		(defq e (elem-get active i) x0 (max (elem-get x0s i) px0) x1 (max (elem-get x1s i) px1))
		(setq px0 x0 px1 x1)
		(cond
			((and gx0 (<= (- x0 gx0) +poly_near) (<= (- x1 gx1) +poly_near))
				;one edge with the last
				(setq gid (min gid (elem-get e 6))))
			(:t (when gx0
					(defq now (case op
						(:union (or (poly-in? wa mode_a) (poly-in? wb mode_b)))
						(:intersect (and (poly-in? wa mode_a) (poly-in? wb mode_b)))
						(:difference (and (poly-in? wa mode_a) (not (poly-in? wb mode_b))))
						(:t (not (eql (poly-in? wa mode_a) (poly-in? wb mode_b))))))
					(cond
						((eql now inside))
						(now (setq lx0 gx0 lx1 gx1 lid gid inside :t))
						(:t (push out (list lx0 lx1 gx0 gx1 lid gid)) (setq inside :nil))))
				(setq gx0 x0 gx1 x1 gid (elem-get e 6))))
		(if (= (elem-get e 5) 0)
			(setq wa (+ wa (elem-get e 4)))
			(setq wb (+ wb (elem-get e 4)))))
		order)
	;after the last edge nothing is wound round, so a piece that is open ends at it
	(if (and gx0 inside) (push out (list lx0 lx1 gx0 gx1 lid gid)))
	out)

(defun poly-across (above below y segs)
	; (poly-across above below y segs) -> segs
	;the lines across at a y: where what is above, (u v) each, is and what
	;is below is not, a bottom, left to right, and where it is the other
	;way, a top, right to left
	(when (or (nempty? above) (nempty? below))
		(defq at (list) ia :nil ib :nil px 0)
		;each end as one number, where it is and which of the two it is of
		(each (lambda ((u v)) (push at (<< u 1) (<< v 1))) above)
		(each (lambda ((u v)) (push at (inc (<< u 1)) (inc (<< v 1)))) below)
		(each (lambda (end)
			(defq x (>>> end 1))
			(when (> x px)
				(cond
					((and ia (not ib)) (push segs (list px y x y -1)))
					((and ib (not ia)) (push segs (list x y px y -1)))))
			(if (= (logand end 1) 0) (setq ia (not ia)) (setq ib (not ib)))
			(setq px x))
			(sort at (const -))))
	segs)

(defun poly-rings (segs)
	; (poly-rings segs) -> ((x y ...) ...)
	;the lines, (x0 y0 x1 y1 id) each, joined end to start into rings.
	;Lines that follow one another with the one id are one line
	(defq from (Fmap 101) used (map (lambda (_) 0) segs) out (list))
	(each (lambda ((x y &ignore))
		(defq key (+ (<< x 32) (+ y 0x80000000)))
		(if (defq l (. from :find key)) (push l (!)) (. from :insert key (list (!)))))
		segs)
	(each (lambda (seg)
		(when (= 0 (elem-get used (!)))
			(defq ring (list) ids (list) i (!))
			(while i
				(elem-set used i 1)
				(bind '(x0 y0 x1 y1 id) (elem-get segs i))
				;a point, unless this goes on along the line before it
				(unless (and (nempty? ids) (= id (last ids))) (push ring x0 y0) (push ids id))
				(defq l (. from :find (+ (<< x1 32) (+ y1 0x80000000))))
				(setq i :nil)
				(while (and l (nempty? l) (not i))
					(defq j (pop l))
					(if (= 0 (elem-get used j)) (setq i j))))
			;and round to its start, the last may go on along the first
			(if (and (> (length ids) 1) (= (first ids) (last ids)))
				(setq ring (slice ring 2 -1)))
			;a point that is on the line from the one before it to the one
			;after it is not a corner, two edges that lay along each other met there
			(defq n (length ring) tidy (list) k 0)
			(while (< k n)
				(defq ax (elem-get ring (% (+ k n -2) n)) ay (elem-get ring (% (+ k n -1) n))
					bx (elem-get ring k) by (elem-get ring (inc k))
					cx (elem-get ring (% (+ k 2) n)) cy (elem-get ring (% (+ k 3) n)))
				(unless (= 0 (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax)))) (push tidy bx by))
				(setq k (+ k 2)))
			(if (>= (length tidy) 6) (push out tidy))))
		segs)
	out)

(defun poly-op (op a b &optional mode_a mode_b)
	; (poly-op op a b [mode_a mode_b]) -> paths
	;op is :union, :intersect, :difference, a and not b, or :xor.
	;a and b are lists of paths, either may be empty. A mode is the
	;winding rule a set is filled by, as (:fpoly) has them, 0 odd even,
	;the default, or 1 non zero
	(setd mode_a 0 mode_b 0)
	(defq ys (list) edges (list) segs (list) above (list))
	(poly-edges (poly-raw a) 0 edges ys)
	(poly-edges (poly-raw b) 1 edges ys)
	(setq ys (usort ys (const -)))
	(sort edges (# (- (first %0) (first %1))))
	(defq next 0 n (length edges) active (list) yi 0)
	(while (< yi (dec (length ys)))
		(defq ya (elem-get ys yi) yb (elem-get ys (inc yi)))
		;those that end here go, those that begin here come
		(setq active (filter (# (> (second %0) ya)) active))
		(while (and (< next n) (= (first (elem-get edges next)) ya))
			(push active (elem-get edges next))
			(setq next (inc next)))
		(when (nempty? active)
			;across the top, and is each no further left at the bottom than
			;all before it: then none cross, and that is their order all down
			(defq xa (map (# (poly-x %0 ya)) active) xb (map (# (poly-x %0 yb)) active)
				order (poly-order xa) top +min_long cross :nil)
			(each (# (defq x (elem-get xb %0)) (if (< x top) (setq cross :t) (setq top x))) order)
			(cond
				(cross
					(defq cuts (poly-cuts order xa xb ya yb) ci 0)
					(while (< ci (dec (length cuts)))
						(defq y0 (elem-get cuts ci) y1 (elem-get cuts (inc ci))
							x0s (map (# (poly-x %0 y0)) active) x1s (map (# (poly-x %0 y1)) active)
							spans (poly-spans active (poly-order (map (const +) x0s x1s)) x0s x1s op mode_a mode_b))
						(poly-across above (map (lambda ((xl0 _ xr0 &ignore)) (list xl0 xr0)) spans) y0 segs)
						(each (lambda ((xl0 xl1 xr0 xr1 idl idr))
							(push segs (list xl0 y0 xl1 y1 idl) (list xr1 y1 xr0 y0 idr))) spans)
						(setq above (map (lambda ((_ xl1 _ xr1 &ignore)) (list xl1 xr1)) spans) ci (inc ci))))
				(:t (defq spans (poly-spans active order xa xb op mode_a mode_b))
					(poly-across above (map (lambda ((xl0 _ xr0 &ignore)) (list xl0 xr0)) spans) ya segs)
					(each (lambda ((xl0 xl1 xr0 xr1 idl idr))
						(push segs (list xl0 ya xl1 yb idl) (list xr1 yb xr0 ya idr))) spans)
					(setq above (map (lambda ((_ xl1 _ xr1 &ignore)) (list xl1 xr1)) spans)))))
		(when (empty? active)
			(poly-across above (list) ya segs)
			(setq above (list)))
		(setq yi (inc yi)))
	(if (nempty? ys) (poly-across above (list) (last ys) segs))
	(poly-fixed (poly-rings segs)))

(defun poly-union (a b &optional mode_a mode_b) (poly-op :union a b mode_a mode_b))
(defun poly-intersect (a b &optional mode_a mode_b) (poly-op :intersect a b mode_a mode_b))
(defun poly-difference (a b &optional mode_a mode_b) (poly-op :difference a b mode_a mode_b))
(defun poly-xor (a b &optional mode_a mode_b) (poly-op :xor a b mode_a mode_b))

(defun poly-area (paths)
	; (poly-area paths) -> fixed
	;the area of a list of paths, one that goes round the other way taken
	;off, so what (poly-op) gives has the area that is filled. Not less
	;than 0. Each path is summed about its first point, and the sum kept
	;in two parts, so nothing is lost and nothing runs out of room
	(defq high 0 low 0)
	(each (lambda (p)
		(defq n (length p))
		(when (>= n 6)
			(defq ox (first p) oy (second p) x0 (- (elem-get p (- n 2)) ox) y0 (- (elem-get p (- n 1)) oy) i 0)
			(while (< i n)
				(defq x1 (- (elem-get p i) ox) y1 (- (elem-get p (inc i)) oy)
					cross (- (* x0 y1) (* x1 y0)))
				(setq high (+ high (>>> cross 16)) low (+ low (logand cross 0xffff))
					x0 x1 y0 y1 i (+ i 2)))))
		(poly-raw paths))
	(poly-num (abs (>>> (+ high (>>> low 16)) 1))))

(defun poly-hull (paths)
	; (poly-hull paths) -> path
	;the least path with no dent in it that has every point of the paths
	;in it or on it. Its points go round the one way, whatever the paths
	;do. Points that are all in one line give the two ends of it
	(defq keys (list))
	(each (lambda (p)
		(defq i 0 n (length p))
		(while (< i n)
			;a point as one number, to sort by x and then y
			(push keys (+ (<< (elem-get p i) 32) (+ (elem-get p (inc i)) 0x80000000)))
			(setq i (+ i 2))))
		(poly-raw paths))
	(setq keys (usort keys (const -)))
	(defq xs (map (# (>>> %0 32)) keys) ys (map (# (- (logand %0 0xffffffff) 0x80000000)) keys)
		n (length keys) hx (list) hy (list) i 0 base 1 pass 0)
	(cond
		((< n 3))
		(:t ;along the bottom from the left, then back along the top
			(while (< pass 2)
				(while (if (= pass 0) (< i n) (>= i 0))
					(defq x (elem-get xs i) y (elem-get ys i) more :t)
					(while (and more (> (length hx) base))
						(defq ax (elem-get hx -2) ay (elem-get hy -2) bx (elem-get hx -3) by (elem-get hy -3))
						(if (<= (- (* (- ax bx) (- y by)) (* (- ay by) (- x bx))) 0)
							(progn (pop hx) (pop hy))
							(setq more :nil)))
					(push hx x) (push hy y)
					(setq i (if (= pass 0) (inc i) (dec i))))
				(setq pass (inc pass) base (length hx) i (- n 2)))
			;the last is the first again
			(pop hx) (pop hy)
			(setq xs hx ys hy)))
	(defq out (list))
	(each (# (push out %0 %1)) xs ys)
	(first (poly-fixed (list out))))

(defun poly-sweep (shape points)
	; (poly-sweep shape points) -> paths
	;what a shape covers as it is moved along a line. shape is a path
	;about 0 0 with no dent in it, points the line it goes along, x y
	;x y. At each point is the shape, each two that follow one another
	;are joined by the hull of them both, and all of the hulls are made
	;one
	(defq hulls (list) n (length points) i 0 last_at :nil)
	(while (< i n)
		(defq x (elem-get points i) y (elem-get points (inc i)) at (path) j 0)
		(while (< j (length shape))
			(push at (+ x (elem-get shape j)) (+ y (elem-get shape (inc j))))
			(setq j (+ j 2)))
		(if last_at (push hulls (poly-hull (list last_at at))))
		(setq last_at at i (+ i 2)))
	(cond
		((empty? hulls) (if last_at (list (poly-hull (list last_at))) (list)))
		(:t (poly-union hulls (list) 1))))

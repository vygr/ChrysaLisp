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
; The work of a band is native code, lib/math/lisp.vp: (poly-band),
; (poly-cuts) and (poly-lines), each with the Lisp it was written from
; beside it, a -ref, which a test holds it to. All else is Lisp. No function here calls itself.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./vector.inc")

;two edges of a band this close at both ends, in the units a fixed number
;is held in, are the one edge: an edge of each set that lie along each
;other are worked out apart, and may not come to the very same number
(defq +poly_near 2)

;the edges are in one array of whole numbers, this many to an edge: its
;top and bottom y, its x at each, which way it goes, 1 down and -1 up,
;which set it is of, 0 or 1, its number, and, for the band it is in, its
;x at the top of the band and at the bottom, and what it is ordered by
(defq +poly_yt 0 +poly_yb 1 +poly_xt 2 +poly_xb 3 +poly_wind 4 +poly_set 5
	+poly_id 6 +poly_x0 7 +poly_x1 8 +poly_key 9 +poly_edge 10)

;what is asked of a band, in one number: the op, the winding rule of each
;set, and that the edges are to be ordered by their middle and not tried
;for crossing
(defq +poly_op_union 0 +poly_op_intersect 1 +poly_op_difference 2 +poly_op_xor 3
	+poly_mode_a 4 +poly_mode_b 8 +poly_by_middle 16 +poly_goes_on 32)

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
	;the edges of a set, each (ytop ybot xtop xbot wind set), those that
	;lie flat left out, and every y a point is at
	(each (lambda (p)
		(defq n (length p))
		(when (>= n 6)
			(defq x0 (elem-get p (- n 2)) y0 (elem-get p (- n 1)) i 0)
			(while (< i n)
				(defq x1 (elem-get p i) y1 (elem-get p (inc i)))
				(push ys y1)
				(cond
					((< y0 y1) (push edges (list y0 y1 x0 x1 1 set)))
					((> y0 y1) (push edges (list y1 y0 x1 x0 -1 set))))
				(setq x0 x1 y0 y1 i (+ i 2)))))
		raw)
	edges)

(defun poly-band-ref (edges active y0 y1 mode out)
	; (poly-band-ref edges active y0 y1 mode out) -> :nil | out
	;the pieces of the answer in a band, from the left, put in out, 6
	;numbers each: its left at the top and at the bottom, its right at
	;each, the number of its left edge and of its right.
	;
	;active is the edges that are in the band, the place of each in
	;edges. Those that end at its top are taken out of it, and it is put
	;in order across. With +poly_by_middle in the mode
	;that is by the middle of each, for a band none cross in. Without, it
	;is by the top, and if any two cross in the band nothing is made and
	;:nil comes back: the band is to be cut where they cross.
	;
	;This is the reference, in Lisp, and is not what is used: the native
	;(poly-band) below does the very same, and a test holds the two
	;together on every machine, a band at a time.
	(defq by_middle (/= 0 (logand mode +poly_by_middle)))
	;those that end at or above the top of the band are not in it, and go
	(defq stay (filter (# (> (elem-get edges (+ (* %0 +poly_edge) +poly_yb)) y0)) active))
	(clear active)
	(each (# (push active %0)) stay)
	(defq n (length active) i 0)
	;where each is at the top and the bottom of the band, its ends as they are
	(each (lambda (e)
		(defq o (* e +poly_edge) yt (elem-get edges o) yb (elem-get edges (+ o +poly_yb))
			xt (elem-get edges (+ o +poly_xt)) xb (elem-get edges (+ o +poly_xb))
			x0 (cond ((= y0 yt) xt) ((= y0 yb) xb) (:t (+ xt (/ (* (- xb xt) (- y0 yt)) (- yb yt)))))
			x1 (cond ((= y1 yt) xt) ((= y1 yb) xb) (:t (+ xt (/ (* (- xb xt) (- y1 yt)) (- yb yt))))))
		(elem-set edges (+ o +poly_x0) x0)
		(elem-set edges (+ o +poly_x1) x1)
		(elem-set edges (+ o +poly_key) (if by_middle (+ x0 x1) x0)))
		active)
	;in order, each put back past those that are after it. They were in
	;order in the band above, so few move, and not far
	(setq i 1)
	(while (< i n)
		(defq v (elem-get active i) kv (elem-get edges (+ (* v +poly_edge) +poly_key))
			xv (elem-get edges (+ (* v +poly_edge) +poly_x1)) j i more :t)
		(while (and more (> j 0))
			(defq u (elem-get active (dec j)) ku (elem-get edges (+ (* u +poly_edge) +poly_key)))
			(cond
				((or (< ku kv) (and (= ku kv) (<= (elem-get edges (+ (* u +poly_edge) +poly_x1)) xv)))
					(setq more :nil))
				(:t (elem-set active j u) (setq j (dec j)))))
		(elem-set active j v)
		(setq i (inc i)))
	;do any cross: is one further left at the bottom than one before it
	(defq cross :nil)
	(unless by_middle
		(defq top +min_long)
		(each (lambda (e)
			(defq x (elem-get edges (+ (* e +poly_edge) +poly_x1)))
			(if (< x top) (setq cross :t) (setq top x)))
			active))
	(cond
		(cross :nil)
		(:t (clear out)
			(defq wa 0 wb 0 inside 0 px0 +min_long px1 +min_long
				gx0 0 gx1 0 gid -1 lx0 0 lx1 0 lid 0 op (logand mode 3))
			(each (lambda (e)
				;none is to the left of the one before it, at either end
				(defq o (* e +poly_edge) x0 (max (elem-get edges (+ o +poly_x0)) px0)
					x1 (max (elem-get edges (+ o +poly_x1)) px1))
				(setq px0 x0 px1 x1)
				(cond
					((and (>= gid 0) (<= (- x0 gx0) +poly_near) (<= (- x1 gx1) +poly_near))
						;one edge with the last
						(setq gid (min gid (elem-get edges (+ o +poly_id)))))
					(:t (when (>= gid 0)
							;is what is wound round here in the answer
							(defq ia (if (= 0 (logand mode +poly_mode_a)) (logand wa 1) (if (= wa 0) 0 1))
								ib (if (= 0 (logand mode +poly_mode_b)) (logand wb 1) (if (= wb 0) 0 1))
								now (case op
									(0 (logior ia ib))
									(1 (logand ia ib))
									(2 (logand ia (logxor ib 1)))
									(:t (logxor ia ib))))
							(cond
								((= now inside))
								((= now 1) (setq lx0 gx0 lx1 gx1 lid gid inside 1))
								(:t (push out lx0 lx1 gx0 gx1 lid gid) (setq inside 0))))
						(setq gx0 x0 gx1 x1 gid (elem-get edges (+ o +poly_id)))))
				(if (= (elem-get edges (+ o +poly_set)) 0)
					(setq wa (+ wa (elem-get edges (+ o +poly_wind))))
					(setq wb (+ wb (elem-get edges (+ o +poly_wind))))))
				active)
			;after the last edge nothing is wound round, so a piece that is open ends at it
			(if (and (>= gid 0) (= inside 1)) (push out lx0 lx1 gx0 gx1 lid gid))
			out)))

;the native one, lib/math/lisp.vp
(ffi (path-to-absolute "./poly_band") poly-band)
; (poly-band edges active y0 y1 mode out) -> :nil | out

(defun poly-cuts-ref (edges active ya yb cuts)
	; (poly-cuts-ref edges active ya yb cuts) -> cuts
	;the ys within a band at which two of its edges cross, put on the end
	;of cuts. As (poly-band) left them: active in order across the top,
	;and each edge with where it is at the top and at the bottom.
	;The reference, as (poly-band-ref) is
	(defq n (length active) i 0)
	(while (< i (dec n))
		(defq o (* (elem-get active i) +poly_edge) x0 (elem-get edges (+ o +poly_x0))
			x1 (elem-get edges (+ o +poly_x1)) j (inc i))
		(while (< j n)
			(defq o (* (elem-get active j) +poly_edge) x3 (elem-get edges (+ o +poly_x1)))
			(when (> x1 x3)
				;as far down the band as the gap at its top is of the
				;gap at its top and the gap, the other way, at its bottom
				(defq num (- (elem-get edges (+ o +poly_x0)) x0) den (+ num (- x1 x3))
					y (+ ya (/ (* (- yb ya) num) den)))
				(if (< ya y yb) (push cuts y)))
			(setq j (inc j)))
		(setq i (inc i)))
	cuts)

;the native one
(ffi (path-to-absolute "./poly_cuts") poly-cuts)
; (poly-cuts edges active ya yb cuts) -> cuts

(defun poly-lines-ref (out y0 y1 above below segs openl openr)
	; (poly-lines-ref out y0 y1 above below segs openl openr) -> below
	;the lines of the pieces of a band, as (poly-band) gave them, put on
	;segs, 5 numbers each, from x y to x y and the number of the edge it
	;is along, -1 for one that goes across: down the left of each piece,
	;up its right, and across at the top of the band where it or the band
	;above, above, u v u v ..., has a piece and the other has none.
	;
	;A side that goes on from where the same side of the same edge ended
	;in the band above is not a new line, that one is made longer: openl
	;and openr have, for each edge, where in segs its last left and its
	;last right are, -1 for none.
	;
	;below is made what is along the bottom, for the band below.
	;The reference, as (poly-band-ref) is
	(clear below)
	(defq k 0 n (length out))
	(while (< k n)
		(defq xl0 (elem-get out k) xl1 (elem-get out (+ k 1)) xr0 (elem-get out (+ k 2))
			xr1 (elem-get out (+ k 3)) idl (elem-get out (+ k 4)) idr (elem-get out (+ k 5))
			i (elem-get openl idl))
		(push below xl1 xr1)
		(cond
			((and (>= i 0) (= (elem-get segs (+ i 2)) xl0) (= (elem-get segs (+ i 3)) y0))
				(elem-set segs (+ i 2) xl1) (elem-set segs (+ i 3) y1))
			(:t (elem-set openl idl (length segs))
				(push segs xl0 y0 xl1 y1 idl)))
		(setq i (elem-get openr idr))
		(cond
			((and (>= i 0) (= (elem-get segs i) xr0) (= (elem-get segs (+ i 1)) y0))
				(elem-set segs i xr1) (elem-set segs (+ i 1) y1))
			(:t (elem-set openr idr (length segs))
				(push segs xr1 y1 xr0 y0 idr)))
		(setq k (+ k 6)))
	;across: the ends of what is above and of the tops of these, each in
	;order, taken the least first
	(defq pa 0 na (length above) pb 0 nb (/ n 3) ia :nil ib :nil px 0)
	(while (or (< pa na) (< pb nb))
		(defq xa (if (< pa na) (elem-get above pa) +max_long)
			xb (if (< pb nb) (elem-get out (+ (* (>>> pb 1) 6) (* (logand pb 1) 2))) +max_long)
			x (min xa xb))
		(when (> x px)
			(cond
				((and ia (not ib)) (push segs px y0 x y0 -1))
				((and ib (not ia)) (push segs x y0 px y0 -1))))
		(if (<= xa xb)
			(setq ia (not ia) pa (inc pa))
			(setq ib (not ib) pb (inc pb)))
		(setq px x))
	below)

;the native one
(ffi (path-to-absolute "./poly_lines") poly-lines)
; (poly-lines out y0 y1 above below segs openl openr) -> below

(defun poly-rings-ref (segs)
	; (poly-rings-ref segs) -> ((x y ...) ...)
	;the lines, 5 numbers each, joined end to start into rings. Lines that
	;follow one another with the one number are one line
	(defq n (/ (length segs) 5) from (Fmap (max 31 n)) used (cap n (list)) out (list) s 0)
	(while (< s n)
		(defq key (+ (<< (elem-get segs (* s 5)) 32) (+ (elem-get segs (inc (* s 5))) 0x80000000)))
		(if (defq l (. from :find key)) (push l s) (. from :insert key (list s)))
		(push used 0)
		(setq s (inc s)))
	(setq s 0)
	(while (< s n)
		(when (= 0 (elem-get used s))
			(defq ring (list) ids (list) i s)
			(while i
				(elem-set used i 1)
				(defq o (* i 5) id (elem-get segs (+ o 4)))
				;a point, unless this goes on along the line before it
				(unless (and (nempty? ids) (= id (last ids)))
					(push ring (elem-get segs o) (elem-get segs (+ o 1)))
					(push ids id))
				(defq l (. from :find (+ (<< (elem-get segs (+ o 2)) 32) (+ (elem-get segs (+ o 3)) 0x80000000))))
				(setq i :nil)
				(while (and l (nempty? l) (not i))
					(defq j (pop l))
					(if (= 0 (elem-get used j)) (setq i j))))
			;and round to its start, the last may go on along the first
			(if (and (> (length ids) 1) (= (first ids) (last ids)))
				(setq ring (slice ring 2 -1)))
			;a point that is on the line from the one before it to the one
			;after it is not a corner, two edges that lay along each other met there
			(defq m (length ring) tidy (list) k 0)
			(while (< k m)
				(defq ax (elem-get ring (% (+ k m -2) m)) ay (elem-get ring (% (+ k m -1) m))
					bx (elem-get ring k) by (elem-get ring (inc k))
					cx (elem-get ring (% (+ k 2) m)) cy (elem-get ring (% (+ k 3) m)))
				(unless (= 0 (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax)))) (push tidy bx by))
				(setq k (+ k 2)))
			(if (>= (length tidy) 6) (push out tidy)))
		(setq s (inc s)))
	out)

(ffi (path-to-absolute "./poly_rings") poly-rings)
; (poly-rings segs) -> paths

(defun poly-scan-ref (edges ys openl openr mode segs)
	; (poly-scan-ref edges ys openl openr mode segs) -> segs
	;every band, from the top to the bottom, its lines put on segs
	(defq above (nums) below (nums) out (nums) cuts (nums))
	(defq next 0 n (length openl) active (nums) yi 0 swap :nil)
	(while (< yi (dec (length ys)))
		(defq ya (elem-get ys yi) yb (elem-get ys (inc yi)))
		;those that begin here come, those that end here (poly-band) takes out
		(while (and (< next n) (= (elem-get edges (* next +poly_edge)) ya))
			(push active next)
			(setq next (inc next)))
		(cond
			((poly-band edges active ya yb mode out)
				;none cross in it
				(poly-lines out ya yb above below segs openl openr)
				(setq swap above above below below swap))
			(:t ;some do, and it is cut where they do
				(clear cuts)
				(push cuts ya yb)
				(poly-cuts edges active ya yb cuts)
				(defq at (usort (map (# %0) cuts) (const -)) ci 0)
				(while (< ci (dec (length at)))
					(defq y0 (elem-get at ci) y1 (elem-get at (inc ci)))
					(poly-band edges active y0 y1 (+ mode +poly_by_middle) out)
					(poly-lines out y0 y1 above below segs openl openr)
					(setq swap above above below below swap ci (inc ci)))))
		(setq yi (inc yi)))
	;and across the bottom of the last
	(when (nempty? ys)
		(clear out)
		(poly-lines out (last ys) (last ys) above below segs openl openr))
	segs)

(ffi (path-to-absolute "./poly_scan") poly-scan)
; (poly-scan edges ys openl openr mode segs) -> segs

(ffi (path-to-absolute "./poly_setup") poly-setup)
; (poly-setup a b edges ys openl openr) -> edges

(defun poly-segs (op a b &optional mode_a mode_b)
	; (poly-segs op a b [mode_a mode_b]) -> segs
	;the lines of what (poly-op) gives, 5 numbers each, not yet joined
	;into rings: where one starts and ends, and the edge it is of
	;op is :union, :intersect, :difference, a and not b, or :xor.
	;a and b are lists of paths, either may be empty. A mode is the
	;winding rule a set is filled by, as (:fpoly) has them, 0 odd even,
	;the default, or 1 non zero
	(setd mode_a 0 mode_b 0)
	(defq ys (nums) edges (nums) openl (nums) openr (nums)
		mode (+ (case op (:union +poly_op_union) (:intersect +poly_op_intersect)
				(:difference +poly_op_difference) (:t +poly_op_xor))
			(if (= mode_a 0) 0 +poly_mode_a) (if (= mode_b 0) 0 +poly_mode_b)))
	;the edges, the highest first, each with its place for a number
	(poly-setup a b edges ys openl openr)
	(poly-scan edges ys openl openr mode (nums)))

(defun poly-op (op a b &optional mode_a mode_b)
	; (poly-op op a b [mode_a mode_b]) -> paths
	;op is :union, :intersect, :difference, a and not b, or :xor.
	;a and b are lists of paths, either may be empty. A mode is the
	;winding rule a set is filled by, as (:fpoly) has them, 0 odd even,
	;the default, or 1 non zero
	(poly-rings (poly-segs op a b mode_a mode_b)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Do two shapes with no dent in them touch: GJK
;
; The set logic above is exact and is not cheap. Before it, to throw out
; what is near and does not touch, the hulls of two things are asked this.
; It is the method of Gilbert, Johnson and Keerthi, in the plane, after
; the 3D one Chris gave, gjk.cpp: the points of one shape less the points
; of the other are a shape too, with no dent, and the two touch if 0 0
; is in it. That shape is never made. All that is asked of it is its
; furthest point some way, which is the furthest point of the one that
; way less the furthest of the other the other way. Three such points
; are a triangle, and each turn the triangle is moved toward 0 0, till
; it has it in it, or a point is found that is short of 0 0 the way it
; was asked for, and then nothing of the shape is past it: they are apart.
;
; The furthest point of a hull is found by climbing. Its points go
; round, so how far each is the way asked rises to one top and falls
; again: from where it was last time, step to the neighbour that is
; further till neither is. The way asked turns only a little from one
; turn to the next, so that is a step or two, however many points.
;
; It is in fixeds, as the paths are. A sum that comes out a hair wrong
; can only matter to two shapes a hair apart, and what is in doubt is
; said to touch: the set logic then has the last word.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun poly-support (hull at dx dy)
	; (poly-support hull at dx dy) -> at
	;the point of a hull that is furthest the way of a vector, as where
	;it is in the hull, an even index. Climbed to from at, where it was
	;when last asked, 0 if never
	(defq n (length hull) best (+ (* (elem-get hull at) dx) (* (elem-get hull (inc at)) dy)) moved :nil)
	(when (> n 2)
		(while (progn (defq next (% (+ at 2) n)
				d (+ (* (elem-get hull next) dx) (* (elem-get hull (inc next)) dy))) (> d best))
			(setq at next best d moved :t))
		(unless moved
			(while (progn (defq next (% (+ at (- n 2)) n)
					d (+ (* (elem-get hull next) dx) (* (elem-get hull (inc next)) dy))) (> d best))
				(setq at next best d))))
	at)

(defun poly-convex-hit? (ha hb)
	; (poly-convex-hit? hull_a hull_b) -> :t | :nil
	;do two paths with no dent in them touch, (poly-hull) gives such a
	;path. :nil only if they are apart for sure
	(defq ia 0 ib 0 hit :t turns 0
		far (lambda (dx dy)
			;the furthest point that way of the one less the other
			(setq ia (poly-support ha ia dx dy) ib (poly-support hb ib (neg dx) (neg dy)))
			(list (- (elem-get ha ia) (elem-get hb ib)) (- (elem-get ha (inc ia)) (elem-get hb (inc ib))))))
	(bind '(cx cy) (far 1.0 0.0))
	(cond
		((< cx 0.0) (setq hit :nil))
		((and (= cx 0.0) (= cy 0.0)))
		(:t (bind '(bx by) (far (neg cx) (neg cy)))
			(cond
				((> (+ (* bx cx) (* by cy)) 0.0) (setq hit :nil))
				(:t ;a line, b to c. The way to look next is square to it, toward 0 0
					(defq dx (- by cy) dy (- cx bx))
					(if (> (+ (* dx bx) (* dy by)) 0.0) (setq dx (neg dx) dy (neg dy)))
					(while (and hit (< turns 32) (not (and (= dx 0.0) (= dy 0.0))))
						(setq turns (inc turns))
						(bind '(ax ay) (far dx dy))
						(cond
							((< (+ (* ax dx) (* ay dy)) 0.0) (setq hit :nil))
							(:t ;a triangle, a b c. Is 0 0 outside its side a b, or a c
								(defq abx (- bx ax) aby (- by ay) acx (- cx ax) acy (- cy ay)
									px (neg aby) py abx qx (neg acy) qy acx)
								;square to a b, away from c, and to a c, away from b
								(if (> (+ (* px acx) (* py acy)) 0.0) (setq px (neg px) py (neg py)))
								(if (> (+ (* qx abx) (* qy aby)) 0.0) (setq qx (neg qx) qy (neg qy)))
								(cond
									((< (+ (* px ax) (* py ay)) 0.0)
										(setq cx bx cy by bx ax by ay dx px dy py))
									((< (+ (* qx ax) (* qy ay)) 0.0)
										(setq bx ax by ay dx qx dy qy))
									;inside it
									(:t (setq turns 32))))))))))
	hit)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Collide, the bodies of a world in a grid of space: what is in a region,
; what a way meets first, and which touch, and which have begun or ended
; touching. A body is only an id, a centre, a bound and a turn: a ball,
; (:sphere r), or a box, (:box hx hy hz) its half sizes, turned by a
; quaternion, lib/math/quaternion.inc, or square to the world; and a mask
; of what it may meet. What it is, how it moves and what a touch does are
; the app's. A touch can be made exact by a function of the app's, its
; true shape.
;
; Each body's box square to the world is kept, made again only as it moves,
; turns or changes, and the app may ask it, (:aabb): two bodies whose boxes
; do not meet do not, the first test of a pair, the cheapest.
;
; A thing of the app's can have many bodies, its owner's, each at its own
; offset from the owner's centre, turned and moved with it, never meeting
; each other: a tight one to collide with, a wide one for what is near it.
; A body may be a sensor, a region that says what is in it and never
; pushes: two sensors do not meet, a ray goes through one, and a sensor's
; touches begun and ended are things come into it and gone out of it.
;
; Each body may have a narrow test, a plugin of the app's: (fn collide a b)
; -> :t | :nil, do two bodies whose bounds meet really touch, the app's own
; exact test of its own shapes, with what it keeps for that, the body's
; shape, (:shape). :nil, the bound is the answer. The depth and the way
; are the bounds', the plugin says only yes or no. A body's bound holds all
; its shape, so the boxes and bounds are still the first tests, and the
; cheap ones.
;
; The touches of a world can be spread over calls, as a world of many
; bodies must: each body is tested every so many calls, (:every), the
; player's every call, a drifting rock every few, staggered by its id so
; each call has its share. A pair is tested when the one of it tested more
; often is due, once, and says how many calls its test stands for, its
; every, so the app scales what it does by it. It is one more cull, in time, on those of
; space, the cells, and of the bounds, the boxes square to the world, the
; bounds, then the narrow tests.
;
; The grid is cells of a size the app gives, kept in a map by their place,
; so it has no edge; a world that wraps, as a field that goes on round,
; gives how far round it is each way, 0 not that way. A body small for a
; cell, its reach no more than half of one, is in the cell of its centre,
; and meets only those of the cells round it; a bigger one is on a list of
; its own, met with all
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/quaternion.inc")
(import "lib/collections/fmap.inc")

(ffi "class/doubles/lisp_box_near" collide-box-near)
;of those, the ones a body does not lead and the balls apart from a ball
;let go of, lib/physics/lisp.vp
(ffi (path-to-absolute "./led_near") collide-led-near)
; (collide-box-near centres sizes wrap body [out]) -> nums

;module
(env-push)

(defq +collide_none (doubles +double_0 +double_0 +double_0) +collide_bits 0x1fffff
	+collide_tiny (/ +double_1 (n2d 1000000000))
	;the half sizes of a body gone, so no box meets it
	+collide_gone (neg (n2d 1000000000))
	;the bits of a rank: of a body's :every, the most it may be in them, and
	;of its id
	+collide_every_bits 20 +collide_every_most 0xfffff +collide_id_bits 32
	+collide_x (doubles +double_1 +double_0 +double_0) +collide_y (doubles +double_0 +double_1 +double_0)
	+collide_z (doubles +double_0 +double_0 +double_1))

(defun collide-reach (bound)
	; (collide-reach bound) -> double
	;how far from its centre a bound reaches at most, however turned
	(if (eql (first bound) :sphere)
		(second bound)
		(sqrt (nums-dot (defq h (apply (const doubles) (rest bound))) h))))

(defun collide-near-of (d wrap)
	; (collide-near-of d wrap) -> doubles
	;a way from one place to another, the shortest round a world that wraps
	(each (lambda (round_by v)
			(if (> round_by +double_0)
				(elem-set d (!) (- v (* round_by (floor (+ (/ v round_by) +double_1/2)))))))
		wrap d)
	d)

(defun collide-cross (a b)
	; (collide-cross a b) -> doubles
	(bind '(ax ay az) a)
	(bind '(bx by bz) b)
	(doubles (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx))))

(defun collide-axes (q)
	; (collide-axes q) -> (x y z)
	;the ways of a box turned by q, the world's if :nil
	(if q (list (quat-vec3-rot q +collide_x) (quat-vec3-rot q +collide_y) (quat-vec3-rot q +collide_z))
		(list +collide_x +collide_y +collide_z)))

(defun collide-local (q v)
	; (collide-local q v) -> doubles
	;a way of the world as it is to a thing turned by q
	(if q (quat-vec3-rot (quat-conj q) v) v))

(defun collide-world (q v)
	; (collide-world q v) -> doubles
	;a way of a thing turned by q as it is to the world
	(if q (quat-vec3-rot q v) v))

(defun collide-extent (bound q &optional frame)
	; (collide-extent bound q [frame]) -> doubles
	;the half sizes of the box square to the world, or to a frame turned so,
	;a bound turned by q is in
	(cond
		((eql (first bound) :sphere) (doubles (second bound) (second bound) (second bound)))
		((and (not q) (not frame)) (apply (const doubles) (rest bound)))
		(:t (bind '(hx hy hz) (rest bound))
			(bind '(ax ay az) (map (# (nums-abs (collide-local frame %0))) (collide-axes q)))
			(nums-add (nums-add (nums-scale ax hx) (nums-scale ay hy)) (nums-scale az hz)))))

(defun collide-sphere-box (r h q)
	; (collide-sphere-box r h q) -> :nil | (depth way)
	;a ball of r at q from the middle of a box of half sizes h, square to
	;its ways: how deep in, and the way out of the box, the ball's from it
	(bind '(qx qy qz) q)
	(bind '(hx hy hz) h)
	(defq c (doubles (max (neg hx) (min hx qx)) (max (neg hy) (min hy qy)) (max (neg hz) (min hz qz)))
		v (nums-sub q c) dd (nums-dot v v))
	(cond
		((> dd (* r r)) :nil)
		((> dd +double_0)
			(defq dist (sqrt dd))
			(list (- r dist) (nums-scale v (recip dist))))
		(:t ;its centre in the box: out by the nearest face
			(defq ox (- hx (abs qx)) oy (- hy (abs qy)) oz (- hz (abs qz)))
			(cond
				((<= ox (min oy oz)) (list (+ r ox) (doubles (if (< qx +double_0) +double_-1 +double_1) +double_0 +double_0)))
				((<= oy oz) (list (+ r oy) (doubles +double_0 (if (< qy +double_0) +double_-1 +double_1) +double_0)))
				((list (+ r oz) (doubles +double_0 +double_0 (if (< qz +double_0) +double_-1 +double_1))))))))

(defun collide-boxes (ha qa hb qb d)
	; (collide-boxes ha qa hb qb d) -> :nil | (depth way)
	;two boxes, half sizes and turns, the second d from the first: do they
	;meet, by the ways that could part them, the faces of each and the
	;crosses of their edges; how deep by the one that parts them least, and
	;the way from the first to the second along it
	(defq as (collide-axes qa) bs (collide-axes qb) ways (cat as bs) best :nil best_way :nil out :t)
	(each (lambda (a) (each (lambda (b) (push ways (collide-cross a b))) bs)) as)
	(each (lambda (l)
			(when (and out (> (defq ll (nums-dot l l)) +collide_tiny))
				(defq l (nums-scale l (recip (sqrt ll))) along (nums-dot d l)
					o (- (+ (reduce! (lambda (n h a) (+ n (* h (abs (nums-dot a l))))) (list ha as) +double_0)
						(reduce! (lambda (n h b) (+ n (* h (abs (nums-dot b l))))) (list hb bs) +double_0))
						(abs along)))
				(cond
					((<= o +double_0) (setq out :nil))
					((or (not best) (< o best))
						(setq best o best_way (if (< along +double_0) (nums-scale l +double_-1) l))))))
		ways)
	(if out (list best best_way)))

(defun collide-meet (ba qa bb qb d)
	; (collide-meet ba qa bb qb d) -> :nil | (depth way)
	;do two bounds meet, each with its turn, the second d from the first:
	;how deep, and the way from the first to the second, of length 1
	(defq ka (first ba) kb (first bb))
	(cond
		((and (eql ka :sphere) (eql kb :sphere))
			(defq r (+ (second ba) (second bb)) dd (nums-dot d d))
			(cond
				((>= dd (* r r)) :nil)
				((> dd +double_0) (defq dist (sqrt dd)) (list (- r dist) (nums-scale d (recip dist))))
				((list r (doubles +double_0 +double_1 +double_0)))))
		((eql ka :sphere)
			;the ball in the box's frame, the way out of the box turned back to
			;the world, and from the ball to the box
			(if (defq m (collide-sphere-box (second ba) (apply (const doubles) (rest bb))
					(collide-local qb (nums-scale d +double_-1))))
				(list (first m) (nums-scale (collide-world qb (second m)) +double_-1))))
		((eql kb :sphere)
			(if (defq m (collide-sphere-box (second bb) (apply (const doubles) (rest ba)) (collide-local qa d)))
				(list (first m) (collide-world qa (second m)))))
		((or qa qb)
			(collide-boxes (rest ba) qa (rest bb) qb d))
		(:t (bind '(ax ay az) (rest ba))
			(bind '(bx by bz) (rest bb))
			(bind '(dx dy dz) d)
			(defq ox (- (+ ax bx) (abs dx)) oy (- (+ ay by) (abs dy)) oz (- (+ az bz) (abs dz)))
			(cond
				((or (<= ox +double_0) (<= oy +double_0) (<= oz +double_0)) :nil)
				((<= ox (min oy oz)) (list ox (doubles (if (< dx +double_0) +double_-1 +double_1) +double_0 +double_0)))
				((<= oy oz) (list oy (doubles +double_0 (if (< dy +double_0) +double_-1 +double_1) +double_0)))
				((list oz (doubles +double_0 +double_0 (if (< dz +double_0) +double_-1 +double_1))))))))

(defun collide-ray (bound q c from way far r)
	; (collide-ray bound q c from way far r) -> :nil | t
	;how far along a way, of length 1, from a place a ball of r first meets
	;a bound turned by q at c, no further than far; 0 if it starts in it. A
	;box is met as one grown by r each way, in its own frame, near enough a
	;box with round edges
	(defq o (nums-sub from c))
	(cond
		((eql (first bound) :sphere)
			(defq rr (+ (second bound) r) b (nums-dot o way) cc (- (nums-dot o o) (* rr rr)))
			(cond
				((<= cc +double_0) +double_0)
				((> b +double_0) :nil)
				((< (defq disc (- (* b b) cc)) +double_0) :nil)
				((<= (defq t (- (neg b) (sqrt disc))) far) t)))
		(:t (defq lo +double_0 hi far out :nil)
			(each (lambda (h oi wi)
					(unless out
						(defq h (+ h r))
						(cond
							((< (abs wi) +collide_tiny)
								(if (> (abs oi) h) (setq out :t)))
							(:t (defq t0 (/ (- (neg h) oi) wi) t1 (/ (- h oi) wi))
								(if (> t0 t1) (defq t2 t0 t0 t1 t1 t2))
								(setq lo (max lo t0) hi (min hi t1))
								(if (> lo hi) (setq out :t))))))
				(rest bound) (collide-local q o) (collide-local q way))
			(unless out lo))))

(defun collide-holds (region qr b qb d)
	; (collide-holds region qr b qb d) -> :t | :nil
	;is a bound b turned by qb, d from the middle of a region turned by qr,
	;a bound too, all in it
	(cond
		((eql (first region) :sphere)
			(<= (+ (sqrt (nums-dot d d)) (collide-reach b)) (second region)))
		(:t (defq out :t)
			(each (lambda (h v e) (if (> (+ (abs v) e) h) (setq out :nil)))
				(rest region) (collide-local qr d) (collide-extent b qb qr))
			out)))

(defun collide-key (cx cy cz)
	; (collide-key cx cy cz) -> num
	(+ (logand cx +collide_bits) (<< (logand cy +collide_bits) 21) (<< (logand cz +collide_bits) 42)))

(defclass Collide (cell &optional wrap) :nil
	; (Collide cell [wrap]) -> collide
	;a world with a grid of cells so big, round so far each way if it wraps
	(def this :cell cell :wrap (ifn wrap +collide_none) :free (list) :grid (Fmap 1009) :big (list)
		:touching (Fmap 31) :owned (Fmap 31)
		;each body's centre, bound, turn, the half sizes of its box square to
		;the world, mask, cell, owner, offset from it, and is it a sensor
		:centres (list) :bounds (list) :turns (list) :extents (list) :masks (list) :keys (list)
		:owners (list) :offsets (list) :sensors (list) :narrows (list) :shapes (list) :everys (list)
		;how many calls apart a new body is tested, and the calls so far
		:every 1 :call 0
		;how many cells round the world is each way, 0 a way it does not wrap
		:counts (map (lambda (w) (if (> w +double_0) (n2i (/ w cell)) 0)) (ifn wrap +collide_none))
		;for the native sweep, collide-box-near: each body's centre, held, and
		;its half sizes, flat, three a body; and for collide-led-near each
		;body's reach if it is a ball, -1 if not, and its rank, which leads
		:flat_centres (list) :flat_sizes (doubles) :flat_reaches (doubles) :flat_ranks (nums))
	;a world that wraps every way has so many cells, a list of each, by
	;number; else they are in the map by key
	(bind '(nx ny nz) (get :counts this))
	(def this :cells_of (and (> nx 0) (> ny 0) (> nz 0) (lists (* nx ny nz))))

	(defmethod :cell_of (p)
		; (. collide :cell_of p) -> (cx cy cz)
		;the cell of a place, round the world if it wraps
		(defq cell (get :cell this))
		(map (lambda (v n)
				(defq c (n2i (floor (/ v cell))))
				(if (> n 0) (% (+ (% c n) n) n) c))
			p (get :counts this)))

	(defmethod :reflat (id)
		; (. collide :reflat id) -> collide
		;a body's centre and half sizes for the native sweep as they are now,
		;one gone no box meets
		(defq src (get :flat_centres this) sizes (get :flat_sizes this) reaches (get :flat_reaches this)
			ranks (get :flat_ranks this) c (elem-get (get :centres this) id) e (elem-get (get :extents this) id)
			bound (elem-get (get :bounds this) id))
		(while (<= (length src) id)
			(push src +collide_none)
			(push sizes +collide_gone +collide_gone +collide_gone)
			(push reaches +double_-1)
			(push ranks 0))
		(elem-set src id (ifn c +collide_none))
		(elem-set reaches id (if (and c (eql (first bound) :sphere)) (second bound) +double_-1))
		;its rank: a leads b if a's is the less, as :leads? has it, a sensor
		;first, then the one tested more often, then a big one, then the less;
		;one gone none
		(elem-set ranks id (if c (+ (<< (+ (<< (+ (<< (if (elem-get (get :sensors this) id) 0 1) +collide_every_bits)
				(min (elem-get (get :everys this) id) +collide_every_most)) 1)
			(if (eql (elem-get (get :keys this) id) :big) 0 1)) +collide_id_bits) id) 0))
		(each! (lambda (v) (elem-set sizes (+ (* id 3) (!)) (if c v +collide_gone))) (list (ifn e +collide_none)))
		this)

	(defmethod :key_of (p)
		; (. collide :key_of p) -> num
		;the key of the cell of a place, its number in a world that wraps
		;every way
		(bind '(x y z) (. this :cell_of p))
		(if (get :cells_of this)
			(+ x (* (first (get :counts this)) (+ y (* (second (get :counts this)) z))))
			(collide-key x y z)))

	(defmethod :file (id)
		; (. collide :file id) -> collide
		;a body put in the cell of its centre, or with the big ones
		(cond
			((> (collide-reach (elem-get (get :bounds this) id)) (* (get :cell this) +double_1/2))
				(push (get :big this) id)
				(elem-set (get :keys this) id :big))
			(:t (defq key (. this :key_of (elem-get (get :centres this) id)))
				(push (if (defq cells (get :cells_of this)) (elem-get cells key)
					(. (get :grid this) :memoize key (const list))) id)
				(elem-set (get :keys this) id key)))
		this)

	(defmethod :unfile (id)
		; (. collide :unfile id) -> collide
		(defq key (elem-get (get :keys this) id)
			l (cond
				((eql key :big) (get :big this))
				((defq cells (get :cells_of this)) (elem-get cells key))
				((. (get :grid this) :find key))))
		(when (defq i (find id l))
			(elem-set l i (last l))
			(pop l))
		this)

	(defmethod :add (centre bound &optional mask sensor turn)
		; (. collide :add centre bound [mask sensor turn]) -> id
		;a body, its centre, its bound, (:sphere r) or (:box hx hy hz), its
		;mask, 1 if not said: two bodies meet if their masks share a bit; is
		;it a sensor, and a box's turn, a quaternion, square to the world if
		;not said. The centre is held, not copied: an app may move it in
		;place, then (:refile)
		(defq id (if (nempty? (get :free this)) (pop (get :free this))
				(each (# (push (get %0 this) :nil))
					'(:centres :bounds :turns :extents :masks :keys :owners :offsets :sensors :narrows :shapes :everys))
				(dec (length (get :centres this)))))
		(elem-set (get :centres this) id centre)
		(elem-set (get :bounds this) id bound)
		(elem-set (get :turns this) id turn)
		(elem-set (get :extents this) id (collide-extent bound turn))
		(elem-set (get :masks this) id (ifn mask 1))
		(elem-set (get :sensors this) id sensor)
		(elem-set (get :narrows this) id :nil)
		(elem-set (get :shapes this) id :nil)
		(elem-set (get :everys this) id (get :every this))
		(. (. this :file id) :reflat id)
		id)

	(defmethod :attach (owner centre offset bound &optional mask sensor turn)
		; (. collide :attach owner centre offset bound [mask sensor turn]) -> id
		;a body of an owner, the app's, whose centre is centre and turn turn,
		;so far from it, turned with it; moved with it, (:move_owner), never
		;meeting its others. Of no offset, :nil, its centre is the owner's,
		;held as it is
		(defq id (. this :add (if offset (nums-add centre (collide-world turn offset)) centre) bound mask sensor turn))
		(elem-set (get :owners this) id owner)
		(elem-set (get :offsets this) id (if offset (cat offset)))
		(defq owned (get :owned this))
		(push (. owned :memoize owner (const list)) id)
		id)

	(defmethod :remove (id)
		; (. collide :remove id) -> collide
		(. this :unfile id)
		(when (defq owner (elem-get (get :owners this) id))
			(defq l (. (get :owned this) :find owner) i (find id l))
			(elem-set l i (last l))
			(pop l)
			(if (empty? l) (. (get :owned this) :erase owner)))
		(each (# (elem-set (get %0 this) id :nil)) '(:centres :turns :extents :keys :owners :offsets :sensors :narrows :shapes :everys))
		(. this :reflat id)
		(push (get :free this) id)
		this)

	(defmethod :detach (owner)
		; (. collide :detach owner) -> collide
		;all the bodies of an owner gone
		(if (defq l (. (get :owned this) :find owner)) (each (# (. this :remove %0)) (cat l)))
		this)

	(defmethod :bodies (owner)
		; (. collide :bodies owner) -> (id ...)
		(ifn (. (get :owned this) :find owner) (list)))

	(defmethod :owner (id)
		; (. collide :owner id) -> :nil | owner
		(elem-get (get :owners this) id))

	(defmethod :sensor? (id)
		; (. collide :sensor? id) -> :t | :nil
		(elem-get (get :sensors this) id))

	(defmethod :move (id centre)
		; (. collide :move id centre) -> collide
		;a body moved, its centre now that one, held, to another cell only if
		;it has changed cell
		(elem-set (get :centres this) id centre)
		(. this :reflat id)
		(defq key (elem-get (get :keys this) id))
		(unless (or (eql key :big) (= key (. this :key_of centre)))
			(. this :unfile id)
			(. this :file id))
		this)

	(defmethod :refile ()
		; (. collide :refile) -> collide
		;each body to the cell its centre is in now, those moved in place
		(each! (lambda (c key)
				(and c (nql key :big) (/= key (. this :key_of c))
					(. (. this :unfile (!)) :file (!))))
			(list (get :centres this) (get :keys this)))
		this)

	(defmethod :turn (id turn)
		; (. collide :turn id turn) -> collide
		;a body turned, a quaternion, or square to the world, :nil
		(elem-set (get :turns this) id turn)
		(elem-set (get :extents this) id (collide-extent (elem-get (get :bounds this) id) turn))
		(. this :reflat id))

	(defmethod :move_owner (owner centre &optional turn)
		; (. collide :move_owner owner centre [turn]) -> collide
		;an owner moved, and turned if said, all its bodies with it, each at
		;its offset turned as the owner is
		(defq offsets (get :offsets this))
		(each (lambda (id)
				(when turn (. this :turn id turn))
				(. this :move id (if (defq offset (elem-get offsets id))
					(nums-add centre (collide-world (elem-get (get :turns this) id) offset)) centre)))
			(. this :bodies owner))
		this)

	(defmethod :narrow (id fnc &optional shape)
		; (. collide :narrow id fnc [shape]) -> collide
		;a body's narrow test, (fnc collide a b) -> :t | :nil, the app's own,
		;and what it keeps for it, its shape; :nil, the bound is the answer
		(elem-set (get :narrows this) id fnc)
		(elem-set (get :shapes this) id shape)
		this)

	(defmethod :mask (id mask)
		; (. collide :mask id mask) -> collide
		;what a body may meet now, 0 nothing, a body put by for a while
		(elem-set (get :masks this) id mask)
		this)

	(defmethod :every (id n)
		; (. collide :every id n) -> collide
		;a body tested every n calls of :pairs or :touches, 1 each call
		(elem-set (get :everys this) id n)
		(. this :reflat id))

	(defmethod :every_new (n)
		; (. collide :every_new n) -> collide
		;bodies added from now tested every n calls
		(def this :every n)
		this)

	(defmethod :due? (id)
		; (. collide :due? id) -> :t | :nil
		;is a body tested this call, staggered by its id
		(= (% (+ (get :call this) id) (elem-get (get :everys this) id)) 0))

	(defmethod :leads? (a b)
		; (. collide :leads? a b) -> :t | :nil
		;does a lead a pair with b, its being due testing it: a sensor, as
		;often as it asks; else the one tested more often; else a big one,
		;so a small one need not look at the big ones it is tested as often
		;as; else the less
		(defq sa (elem-get (get :sensors this) a) sb (elem-get (get :sensors this) b)
			ea (elem-get (get :everys this) a) eb (elem-get (get :everys this) b)
			ba (eql (elem-get (get :keys this) a) :big) bb (eql (elem-get (get :keys this) b) :big))
		(cond
			((nql sa sb) sa)
			((/= ea eb) (< ea eb))
			((nql ba bb) ba)
			((< a b))))

	(defmethod :leader (a b)
		; (. collide :leader a b) -> id
		;of a pair, the one whose being due tests it, (:leads?)
		(if (. this :leads? a b) a b))

	(defmethod :shape (id)
		; (. collide :shape id) -> :nil | shape
		(elem-get (get :shapes this) id))

	(defmethod :exact? (a b)
		; (. collide :exact? a b) -> :t | :nil
		;do two bodies whose bounds meet really touch: the first's narrow test
		;says, else the second's, else yes
		(cond
			((defq fnc (elem-get (get :narrows this) a)) (fnc this a b))
			((defq fnc (elem-get (get :narrows this) b)) (fnc this a b))
			(:t)))

	(defmethod :reshape (id bound)
		; (. collide :reshape id bound) -> collide
		(. this :unfile id)
		(elem-set (get :bounds this) id bound)
		(elem-set (get :extents this) id (collide-extent bound (elem-get (get :turns this) id)))
		(. (. this :file id) :reflat id))

	(defmethod :centre (id)
		; (. collide :centre id) -> :nil | doubles
		(elem-get (get :centres this) id))

	(defmethod :bound (id)
		; (. collide :bound id) -> :nil | bound
		(elem-get (get :bounds this) id))

	(defmethod :turned (id)
		; (. collide :turned id) -> :nil | quaternion
		(elem-get (get :turns this) id))

	(defmethod :aabb (id)
		; (. collide :aabb id) -> (lo hi)
		;the box square to the world a body is in, its least and most corners
		(defq c (elem-get (get :centres this) id) e (elem-get (get :extents this) id))
		(list (nums-sub c e) (nums-add c e)))

	(defmethod :aabb_owner (owner)
		; (. collide :aabb_owner owner) -> :nil | (lo hi)
		;the box square to the world all an owner's bodies are in
		(when (nempty? (defq ids (. this :bodies owner)))
			(reduce (lambda (box id)
					(bind '(lo hi) (. this :aabb id))
					(list (nums-min (first box) lo) (nums-max (second box) hi)))
				ids (. this :aabb (first ids)))))

	(defmethod :apart (a b)
		; (. collide :apart a b) -> doubles
		;the way from one place to another, the shortest round the world
		(collide-near-of (nums-sub b a) (get :wrap this)))

	(defmethod :may? (a b)
		; (. collide :may? a b) -> :t | :nil
		;may two bodies meet: their masks share a bit, they are not of the
		;one owner, and not both sensors
		(defq owners (get :owners this) sensors (get :sensors this))
		(and (/= 0 (logand (elem-get (get :masks this) a) (elem-get (get :masks this) b)))
			(not (and (elem-get owners a) (eql (elem-get owners a) (elem-get owners b))))
			(not (and (elem-get sensors a) (elem-get sensors b)))))

	(defmethod :cells (p reach fnc)
		; (. collide :cells p reach fnc) -> collide
		;(fnc id) of each small body in the cells round a place, as far as
		;reach and the most a small body reaches, each cell once round a
		;world that wraps, by their numbers
		(defq cell (get :cell this) grid (get :grid this)
			n (n2i (ceil (/ (+ reach (* cell +double_1/2)) cell)))
			spans (map (lambda (c count)
					(cond
						((and (> count 0) (>= (inc (* 2 n)) count)) (map! (# (!)) '() 0 count))
						((> count 0) (map! (# (% (+ c (!) count) count)) '() (neg n) (inc n)))
						((map! (# (+ c (!))) '() (neg n) (inc n)))))
				(. this :cell_of p) (get :counts this)))
		(bind '(xs ys zs) spans)
		(cond
			((defq cells (get :cells_of this))
				;a world that wraps every way, its cells by number
				(bind '(nx ny &ignore) (get :counts this))
				(defq nxy (* nx ny))
				(each (lambda (z)
						(defq z (* z nxy))
						(each (lambda (y)
								(defq y (+ z (* y nx)))
								(each (lambda (x) (each fnc (elem-get cells (+ y x)))) xs))
							ys))
					zs))
			(:t (each (lambda (z)
					(each (lambda (y)
							(each (lambda (x)
									(if (defq l (. grid :find (collide-key x y z))) (each fnc l)))
								xs))
						ys))
				zs)))
		this)

	(defmethod :round (p reach fnc)
		; (. collide :round p reach fnc) -> collide
		;(fnc id) of each body that may be within reach of a place: the small
		;ones of the cells round it, (:cells), and the big ones; or every body,
		;if that is fewer than the cells
		(defq cell (get :cell this) n (inc (* 2 (n2i (ceil (/ (+ reach (* cell +double_1/2)) cell))))))
		(cond
			((> (* n n n) (length (get :centres this)))
				(each! (lambda (c) (if c (fnc (!)))) (list (get :centres this))))
			(:t (. this :cells p reach fnc)
				(each fnc (get :big this))))
		this)

	(defmethod :within (region p &optional mask sensors turn)
		; (. collide :within region p [mask sensors turn]) -> ((id depth way all) ...)
		;the bodies a region meets, a bound at a place turned by turn, of
		;those whose mask shares a bit with mask, all if not said, and not
		;sensors unless asked: each how deep, the way from the place to it,
		;and is it all in the region. What a buoy has near it, what a blast
		;reaches
		(defq out (list) centres (get :centres this) bounds (get :bounds this) turns (get :turns this)
			extents (get :extents this) masks (get :masks this) sensor_of (get :sensors this)
			wrap (get :wrap this) region_e (collide-extent region turn))
		(. this :round p (collide-reach region) (lambda (id)
			(and (or (not mask) (/= 0 (logand mask (elem-get masks id))))
				(or sensors (not (elem-get sensor_of id)))
				(defq d (collide-near-of (nums-sub (elem-get centres id) p) wrap))
				;the boxes square to the world first
				(every (# (< (abs %0) (+ %1 %2))) d region_e (elem-get extents id))
				(defq b (elem-get bounds id) q (elem-get turns id) m (collide-meet region turn b q d))
				(push out (cat (list id) m (list (collide-holds region turn b q d)))))))
		out)

	(defmethod :near (p r &optional mask)
		; (. collide :near p r [mask]) -> ((id depth way all) ...)
		;the bodies a ball of r at a place meets, :within
		(. this :within (list :sphere r) p mask))

	(defmethod :cast (from way far &optional r mask)
		; (. collide :cast from way far [r mask]) -> :nil | (id t)
		;what a ball of r, 0 if not said, going a way of length 1 from a place
		;meets first, no further than far, and how far along: a ray, or a
		;thing of a size swept along it. Sensors it passes through
		(defq r (ifn r +double_0) half (* far +double_1/2) mid (nums-add from (nums-scale way half))
			best :nil best_t far centres (get :centres this) bounds (get :bounds this) turns (get :turns this)
			masks (get :masks this) sensor_of (get :sensors this) wrap (get :wrap this))
		(. this :round mid (+ half r) (lambda (id)
			(when (and (or (not mask) (/= 0 (logand mask (elem-get masks id)))) (not (elem-get sensor_of id)))
				;the body where it is nearest from, round the world
				(defq c (nums-add from (collide-near-of (nums-sub (elem-get centres id) from) wrap)))
				(and (defq t (collide-ray (elem-get bounds id) (elem-get turns id) c from way best_t r))
					(< t best_t) (setq best id best_t t)))))
		(if best (list best best_t)))

	(defmethod :pairs (&optional narrow_fn)
		; (. collide :pairs [narrow_fn]) -> ((a b depth way stands) ...)
		;the bodies that touch, of the pairs whose leader is due this call,
		;(:leads?), each pair once, a the less, how deep, the way from a to b,
		;and how many calls its test stands for, its leader's :every; of
		;those that may meet, (:may?). Each body due, the boxes of all the
		;bodies square to the world that meet its, by the native sweep,
		;collide-box-near, less those it does not lead and the balls clearly
		;apart from a ball, collide-led-near, then their bounds, then their
		;narrow tests, (:exact?); and (narrow_fn collide a b depth way) ->
		;:nil | (depth way), of all pairs, if given. The next call after
		(defq out (list) call (get :call this) centres (get :centres this) bounds (get :bounds this)
			turns (get :turns this) wrap (get :wrap this) keys (get :keys this)
			everys (get :everys this) sensors (get :sensors this) masks (get :masks this)
			owners (get :owners this) narrows (get :narrows this)
			flat (apply (const cat) (get :flat_centres this)) sizes (get :flat_sizes this)
			reaches (get :flat_reaches this) ranks (get :flat_ranks this) hits (nums))
		(bind '(wx wy wz) wrap)
		(defq leads (lambda (a b)
				(defq sa (elem-get sensors a) sb (elem-get sensors b) ea (elem-get everys a) eb (elem-get everys b)
					ba (eql (elem-get keys a) :big) bb (eql (elem-get keys b) :big))
				(cond ((nql sa sb) sa) ((/= ea eb) (< ea eb)) ((nql ba bb) ba) ((< a b))))
			meet (lambda (a b)
				(and (/= 0 (logand (elem-get masks a) (elem-get masks b)))
					(not (and (defq oa (elem-get owners a)) (eql oa (elem-get owners b))))
					(not (and (elem-get sensors a) (elem-get sensors b)))
					;their boxes meet, the way between them round the world
					(defq ca (elem-get centres a) cb (elem-get centres b)
						dx (- (elem-get cb 0) (elem-get ca 0)) dy (- (elem-get cb 1) (elem-get ca 1))
						dz (- (elem-get cb 2) (elem-get ca 2))
						d (doubles (if (> wx +double_0) (- dx (* wx (floor (+ (/ dx wx) +double_1/2)))) dx)
							(if (> wy +double_0) (- dy (* wy (floor (+ (/ dy wy) +double_1/2)))) dy)
							(if (> wz +double_0) (- dz (* wz (floor (+ (/ dz wz) +double_1/2)))) dz))
						m (collide-meet (elem-get bounds a) (elem-get turns a) (elem-get bounds b) (elem-get turns b) d))
					(or (not (or (elem-get narrows a) (elem-get narrows b))) (. this :exact? a b))
					(or (not narrow_fn) (setq m (narrow_fn this a b (first m) (second m))))
					(push out (cat (list a b) m (list (elem-get everys (if (leads a b) a b))))))))
		;each body due with the bodies whose boxes meet its, the pairs it leads
		(each! (lambda (c e)
				(when (and c (= 0 (% (+ call (!)) e)))
					(defq a (!))
					(each (lambda (b) (meet (min a b) (max a b)))
						(collide-led-near flat reaches ranks wrap a (collide-box-near flat sizes wrap a hits)))))
			(list centres everys))
		(def this :call (inc call))
		out)

	(defmethod :touches (&optional narrow_fn)
		; (. collide :touches [narrow_fn]) -> (began held ended)
		;the pairs touching now, as :pairs, split into those that began
		;touching since last asked, those that still do, and the ones that
		;ended, each of those (a b): a pair not tested this call, its leader
		;not due, as it was
		(defq was (get :touching this) now (Fmap 31) began (list) held (list) ended (list)
			call (get :call this) centres (get :centres this))
		(each (lambda (pair)
				(bind '(a b &ignore) pair)
				(. now :insert (defq key (+ a (<< b 32))) :t)
				(push (if (. was :find key) held began) pair))
			(. this :pairs narrow_fn))
		;which were tested: those with one due on the call just made
		(def this :call call)
		(. was :each (lambda (key &ignore)
			(defq a (logand key 0xffffffff) b (>> key 32))
			(unless (. now :find key)
				(if (and (elem-get centres a) (elem-get centres b) (not (. this :due? (. this :leader a b))))
					(. now :insert key :t)
					(push ended (list a b))))))
		(def this :call (inc call) :touching now)
		(list began held ended)))

;module
(export-classes '(Collide))
(export-symbols
	'(collide-reach collide-near-of collide-cross collide-axes collide-local collide-world collide-extent
	collide-sphere-box collide-boxes collide-meet collide-ray collide-holds))
(env-pop)

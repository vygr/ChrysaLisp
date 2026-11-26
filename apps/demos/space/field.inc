;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space's field of rocks: where they are, how they drift, and how they
; move each other, pulled together and bounced apart. It is a box of rocks
; that goes on the same every way, so the ship never comes to its end: a
; rock that drifts out of one side comes in at the other, and is drawn and
; met where it is nearest. Rocks are only met by those near them, the
; box cut into a grid of cells, a rock asked only of its own and those
; round it. No GUI here, so it is timed and tested on its own.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/vector.inc")
(import "apps/demos/space/sky.inc")

;module
(env-push)

;how many kinds of rock, how many rocks, and how big the box of them
(defq +rock_kinds 8 +rock_count 450 +field (n2d 1000)
	;the rocks, each (at drift bulk kind spin patch), make-rocks, and the
	;clusters, each the numbers of its rocks, its big one first
	rocks (list) clusters (list)
	;the marker buoys, make-buoys, how many, how many at swarms, how far
	;round one it counts
	;rocks, how soon its light follows its hazard, and its colours, safe,
	;crowded and dangerous, and safe in open space
	buoys (list) +buoys 32 +buoys_at_swarms 20 +buoy_round (n2d 100) +buoy_close (n2d 24) +buoy_ease (n2d 1.5)
	;a buoy at a swarm moved to another when its own has gone: under what
	;hazard it is calm, for how many looks running, and how far round a big
	;rock its cluster's rocks count as there
	+buoy_calm +double_1/20 +calm_looks 50 +crowd_round (n2d 120)
	;a buoy goes nuclear in a heap too crowded to draw: how many rocks round
	;it, within +buoy_round, are too many, for how many looks running, and
	;how far round it the small rocks are shattered
	+nuke_crowd 18 +nuke_looks 3 +nuke_core (n2d 130)
	+buoy_green (doubles (n2d 0.2) (n2d 1.0) (n2d 0.35)) +buoy_yellow (doubles (n2d 1.0) (n2d 0.82) (n2d 0.15))
	+buoy_red (doubles (n2d 1.0) (n2d 0.15) (n2d 0.1)) +buoy_blue (doubles (n2d 0.25) (n2d 0.6) (n2d 1.0))
	;how far out each kind of rock reaches from its middle, as it turns, of
	;its size, its bound, near 1 till its mesh has come
	kind_reach (fill (doubles) +rock_kinds (n2d 0.9))
	;how fast at most they drift at first, a second; how many clusters of
	;them; in how many parts, a part a frame, they are moved by each other;
	;cells each way of the box, of the coarse grid and the fine, and how
	;far a small rock reaches at most, half a fine cell, so two small ones
	;that touch are in cells side by side; how strong the pull, how far it reaches,
	;squared, how soft it is close to, how big one of a pair is for it,
	;and in one round of meetings in how many it is, as much the more, it
	;changing slowly, and the round it is; and how much of its speed a hit
	;keeps, little, rock on rock is a crunch, and how much of the sideways
	;part of it is ground down
	+drift_most +double_3 +clusters 45 +meet_parts 12 +cells_side 8 +fine_side 16 +pull (n2d 0.025)
	+small_reach (/ +field (n2d (* 2 +fine_side))) +pull_least (n2d 25) +pull_every 3 meet_round 0 +pull_reach2 (n2d 62500) +pull_soft (n2d 400) +bounce (n2d 0.15) +grind (n2d 0.4)
	;a blast: how far it reaches, how hard it throws, a rock of size 1 at
	;its middle thrown so fast, one of size b a b'th of it, how fast its
	;front of pressure goes out, a second, and how much of how hard a rock is
	;thrown sets it tumbling; how fast a rock struck by a missile spins, at
	;its edge and of size 1; how fast a rock tumbles at most, and its surface
	;goes round at most, so a big rock the slower; how well two rocks' faces
	;grip as they grind past; and how much of a rock's spin a nuke's blast
	;leaves it
	+blast_reach (n2d 140) +blast_strength (n2d 150) +blast_front (n2d 280) +blast_spin (n2d 0.15)
	+strike_spin (n2d 40) +spin_most +double_4 +rim_most (n2d 40) +grip (n2d 0.15) +blast_calm +double_1/5
	;the ship keeps clear of rocks: how far ahead it looks, each way, and
	;how many seconds ahead
	+look_far (n2d 380) +look_time +double_10
	;threading: how far a heading tried is from the way wanted, at most, a
	;tangent, and how far past a rock's reach is clear
	+thread_cone (n2d 0.9) +thread_safe (n2d 1.5)
	;the widest a gap is that is a gap, past it all are as dull
	+gap_most (n2d 80)
	;the headings tried, as far across and up from the way wanted: it, and
	;five rings of eight round it
	thread_offsets (cat (list (list +double_0 +double_0))
		(reduce (lambda (out ring)
				(defq r (* +thread_cone (/ (n2d ring) +double_5)))
				(each! (lambda () (defq a (/ (* +double_2pi (n2d (!))) +double_8))
						(push out (list (* r (cos a)) (* r (sin a))))) '() 0 8)
				out)
			'(1 2 3 4 5) (list)))
	;how many hits there have been, and the hits since the app last took
	;them, each (x y z speed), where they touched and how hard, for it to
	;hear and see
	bumps 0 hits (list)
	;the cells round a cell, its own among them, each a step across, up and
	;along
	cell_steps (reduce (lambda (out i) (push out (list (dec (% i 3)) (dec (% (/ i 3) 3)) (dec (/ i 9)))))
		(map! (const !) '() 0 27) (list))
	;and in the fine grid, inside the box, how many cells on each is
	fine_steps (map (lambda ((sx sy sz)) (+ sx (* +fine_side (+ sy (* +fine_side sz))))) cell_steps)
	;how far off a rock starts to go into the sky behind it and is gone into
	;it, and is drawn: all of it inside half the box, as past that a rock is
	;drawn on the other side of the eye, where it is next as the field goes
	;on, and would come from nowhere
	+fade_near (* +field (n2d 0.30)) +fade_far (* +field (n2d 0.47)) +rock_far (* +field (n2d 0.48)))

;the numbers of the functions below: the steps of (rnd); a rock's patch
;of the surface, the least and the spread past it; the most a rock
;tumbles; the gap round a cluster's big rock its small ones orbit in and
;its spread, how much of an orbit's speed they hold and its spread, and
;how far off it they wander; the size of a big rock, of a small one, and
;of a loner, and a loner's drift; the most a blast turns a rock; how far
;behind the eye, in its sizes, a rock is still near; the weight of a
;heading's way off on the tour; the gap off a cluster a buoy keeps
;station at and its spread, and its least drift; how near past a rock's
;reach a buoy pushes it off; the hazard of a crowd, how many is the most
;of it, and the hazard of a rock close
(defq +rnd_steps (n2d 1000000) +patch_least (n2d 0.06) +patch_spread (n2d 0.12)
	+tumble_most (n2d 0.35) +orbit_gap (n2d 15) +orbit_spread (n2d 90) +orbit_hold_least (n2d 0.75)
	+orbit_hold_spread (n2d 0.35) +orbit_wander (n2d 0.8) +big_least (n2d 35) +big_spread (n2d 25)
	+small_least +double_4 +small_spread (n2d 16) +loner_least +double_5 +loner_spread (n2d 55)
	+loner_drift_least (n2d 0.15) +loner_drift_spread (n2d 0.85) +blast_turn_most (n2d 12)
	+behind_bulks (n2d 1.5) +thrill_weight +double_8 +station_gap (n2d 45) +station_spread (n2d 40)
	+buoy_drift_least (n2d 0.4) +buoy_near +double_4 +crowd_hazard (n2d 0.65) +crowd_full +double_5
	+close_hazard (n2d 100))

(defun rnd ()
	; (rnd) -> double
	;0 to 1
	(/ (n2d (random 1000000)) +rnd_steps))


(defun rnd-way ()
	; (rnd-way) -> doubles
	;a way at random, of length 1, as likely any way
	(defq y (- (* +double_2 (rnd)) +double_1) c (sqrt (- +double_1 (* y y))) a (* +double_2pi (rnd)))
	(doubles (* c (cos a)) y (* c (sin a))))

(defun rock-tumble ()
	; (rock-tumble) -> doubles
	;a rock's tumble, (wx wy wz qw qx qy qz): how fast it turns, about which
	;way, radians a second along it, and how it is turned now, a quaternion,
	;both at random, slow
	(defq way (nums-scale (rnd-way) (* (rnd) +tumble_most)) axis (rnd-way) half (* (rnd) +double_pi)
		s (sin half))
	(cat way (doubles (cos half) (* s (first axis)) (* s (second axis)) (* s (third axis)))))

(defun make-rock (at drift bulk)
	; (make-rock at drift bulk) -> rock
	;a rock (at drift bulk kind spin patch) at a place, drifting, of a size:
	;its kind, its tumble, rock-tumble, and the patch of the moon's map on
	;it, where and how much, at random
	(list at drift bulk (random +rock_kinds) (rock-tumble)
		(doubles (rnd) (rnd) (+ +patch_least (* +patch_spread (rnd))))))

(defun rock-spin (spin bulk ax ay az)
	;a rock's tumble turned faster about a way by so much, no faster than
	;its surface going round no faster than +rim_most, so a big rock, much
	;the heavier, spins the slower, and none faster than +spin_most in all
	(defq wx (+ (elem-get spin 0) ax) wy (+ (elem-get spin 1) ay) wz (+ (elem-get spin 2) az)
		w (sqrt (+ (* wx wx) (* wy wy) (* wz wz))) most (min +spin_most (/ +rim_most bulk))
		k (if (> w most) (/ most w) +double_1))
	(elem-set spin 0 (* wx k))
	(elem-set spin 1 (* wy k))
	(elem-set spin 2 (* wz k)))

(defun rock-turned (spin)
	; (rock-turned spin) -> mat4x4
	;how a rock is turned now, the matrix of its quaternion
	(bind '(& & & w x y z) spin)
	(doubles (- +double_1 (* +double_2 (+ (* y y) (* z z)))) (* +double_2 (- (* x y) (* w z)))
			(* +double_2 (+ (* x z) (* w y))) +double_0
		(* +double_2 (+ (* x y) (* w z))) (- +double_1 (* +double_2 (+ (* x x) (* z z))))
			(* +double_2 (- (* y z) (* w x))) +double_0
		(* +double_2 (- (* x z) (* w y))) (* +double_2 (+ (* y z) (* w x)))
			(- +double_1 (* +double_2 (+ (* x x) (* y y)))) +double_0
		+double_0 +double_0 +double_0 +double_1))

(defun orbit-at (middle drift big small)
	; (orbit-at middle drift big small) -> (at drift)
	;a place and a drift for a small rock in orbit round a big one at
	;middle, drifting: clear of it, going round it, across the way out at
	;the speed the big one's pull holds it there, with its softness, a
	;little more or less, so its orbit is an ellipse, some of them dipping
	;close, and their orbits crossing, so they meet and crunch
	(defq way (rnd-way) out_by (+ big small +orbit_gap (* +orbit_spread (rnd)))
		round (sky-unit (sky-cross way (rnd-way)))
		hold (sqrt (/ (* +pull big big big out_by) (+ (* out_by out_by) +pull_soft))))
	(list (apply (const doubles) (map (lambda (m w)
				(defq v (+ m (* w out_by)))
				(cond ((< v +double_0) (+ v +field)) ((>= v +field) (- v +field)) (v))) middle way))
		(nums-add (nums-add drift (nums-scale round (* hold (+ +orbit_hold_least (* +orbit_hold_spread (rnd))))))
			(nums-scale way (* (- (rnd) +double_1/2) +orbit_wander)))))

(defun make-rocks ()
	; (make-rocks) -> rocks
	;the rocks of the field: clusters, each a big rock with smaller ones in
	;orbit round it, drifting together, orbit-at, each cluster kept, the
	;numbers of its rocks, to put them in orbit again; and the rest loose,
	;anywhere, each its own way. Sizes most of them middling and a few big,
	;as asteroids are
	(defq out (list))
	(setq clusters (list))
	(times +clusters
		(defq middle (doubles (* (rnd) +field) (* (rnd) +field) (* (rnd) +field))
			drift (nums-scale (rnd-way) (* +drift_most (rnd)))
			big (+ +big_least (* +big_spread (rnd))) members (list (length out)))
		(push out (make-rock middle (cat drift) big))
		(times (+ 5 (random 7))
			(defq small (+ +small_least (* +small_spread (rnd) (rnd))))
			(bind '(at go) (orbit-at middle drift big small))
			(push members (length out))
			(push out (make-rock at go small)))
		(push clusters members))
	(while (< (length out) +rock_count)
		(push out (make-rock (doubles (* (rnd) +field) (* (rnd) +field) (* (rnd) +field))
			(nums-scale (rnd-way) (* +drift_most (+ +loner_drift_least (* +loner_drift_spread (rnd)))))
			(+ +loner_least (* +loner_spread (rnd) (rnd))))))
	out)

(defun near-way (v)
	; (near-way v) -> double
	;of a way across the box, the shortest, as the box goes on every way
	(cond
		((> v (const (* +field +double_1/2))) (- v +field))
		((< v (const (* +field +double_-1/2))) (+ v +field))
		(v)))

(defun out-of-sight? (at eye far)
	; (out-of-sight? at eye far) -> :t | :nil
	;is a place further from the eye than far
	(defq rx (near-way (- (first at) (first eye))) ry (near-way (- (second at) (second eye)))
		rz (near-way (- (third at) (third eye))))
	(> (+ (* rx rx) (* ry ry) (* rz rz)) (* far far)))

(defun cluster-again (members eye far)
	;a cluster's small rocks put in orbit round its big one again, where
	;the big one is now and going as it goes, their tumbles calmed: a
	;cluster settled into a heap made lively again. Only those out of
	;sight, further from the eye than far, and to a place out of sight: one
	;blown near the eye stays, not to be seen to go, nor one seen to come
	(bind '((at drift big &ignore)) (list (elem-get rocks (first members))))
	(each! (lambda (i)
			(bind '(rock_at rock_drift small & spin &ignore) (elem-get rocks i))
			(bind '(new_at new_drift) (orbit-at at drift big small))
			(when (and (out-of-sight? rock_at eye far) (out-of-sight? new_at eye far))
				(each! (lambda (v) (elem-set rock_at (!) v)) (list new_at))
				(each! (lambda (v) (elem-set rock_drift (!) v)) (list new_drift))
				(each! (lambda (v) (elem-set spin (!) v)) (list (nums-scale (rnd-way) (* (rnd) +tumble_most))))))
		(list members) 1))

(defun buoy-station (members)
	; (buoy-station members) -> (at drift)
	;where a buoy keeps station at a cluster, just outside its big rock and
	;the swarm round it, and its drift, the big one's, so it goes along with
	;the cluster
	(bind '((at drift big kind &ignore)) (list (elem-get rocks (first members))))
	(defq way (rnd-way) out_by (+ (* big (elem-get kind_reach kind)) +station_gap (* +station_spread (rnd))))
	(list (apply (const doubles) (map (lambda (m w)
				(defq v (+ m (* w out_by)))
				(cond ((< v +double_0) (+ v +field)) ((>= v +field) (- v +field)) (v))) at way))
		(cat drift)))

(defun buoy-again (cluster eye far)
	;the buoy of a cluster, if it has one, put back on station, if it is
	;out of sight, further from the eye than far
	(each (lambda (buoy)
			(when (and (eql (elem-get buoy 4) cluster) (out-of-sight? (first buoy) eye far))
				(bind '(at drift) (buoy-station (elem-get clusters cluster)))
				(each! (lambda (v) (elem-set (first buoy) (!) v)) (list at))
				(each! (lambda (v) (elem-set (second buoy) (!) v)) (list drift))))
		buoys))

(defun clusters-refresh (eye far)
	;a cluster at random put in orbit again, cluster-again, if it is out of
	;sight, its big rock further from the eye than far, so it is never seen
	;to happen; a few tries for one that is
	(defq tries 5)
	(while (> tries 0)
		(defq cluster (random (length clusters)) members (elem-get clusters cluster)
			at (first (elem-get rocks (first members)))
			rx (near-way (- (first at) (first eye))) ry (near-way (- (second at) (second eye)))
			rz (near-way (- (third at) (third eye))))
		(cond
			((> (+ (* rx rx) (* ry ry) (* rz rz)) (* far far))
				(cluster-again members eye far) (buoy-again cluster eye far) (setq tries 0))
			((-- tries)))))

(defun rocks-move (dt)
	;each rock on by its drift, kept in the box of the field, and turned on
	;as fast as it tumbles about the way it does, which a knock may change:
	;its quaternion turned by its turn in the time, and kept of length 1
	(defq h (* dt +double_1/2))
	(each (lambda ((at drift & & spin &ignore))
			(each! (lambda ()
					(defq i (!) v (+ (elem-get at i) (* (elem-get drift i) dt)))
					(elem-set at i (cond ((< v +double_0) (+ v +field)) ((>= v +field) (- v +field)) (v))))
				'() 0 3)
			(bind '(wx wy wz qw qx qy qz) spin)
			(defq nw (- qw (* h (+ (* wx qx) (* wy qy) (* wz qz))))
				nx (+ qx (* h (+ (* wx qw) (- (* wy qz) (* wz qy)))))
				ny (+ qy (* h (+ (* wy qw) (- (* wz qx) (* wx qz)))))
				nz (+ qz (* h (+ (* wz qw) (- (* wx qy) (* wy qx)))))
				k (/ +double_1 (sqrt (+ (* nw nw) (* nx nx) (* ny ny) (* nz nz)))))
			(elem-set spin 3 (* nw k))
			(elem-set spin 4 (* nx k))
			(elem-set spin 5 (* ny k))
			(elem-set spin 6 (* nz k)))
		rocks))


(defun cell-at (x y z)
	; (cell-at x y z) -> num
	;the cell of the grid of a place, a place a cell over an edge the cell
	;on the other side, as the field goes on
	(+ (% (+ x +cells_side) +cells_side) (* +cells_side (+ (% (+ y +cells_side) +cells_side)
		(* +cells_side (% (+ z +cells_side) +cells_side))))))


(defun rock-push (v nx ny nz by)
	;v moved along the way n by so much
	(elem-set v 0 (+ (elem-get v 0) (* nx by)))
	(elem-set v 1 (+ (elem-get v 1) (* ny by)))
	(elem-set v 2 (+ (elem-get v 2) (* nz by))))


(defun cell-wrap (c side)
	; (cell-wrap c side) -> double
	;the way across the box to a cell c across, up or along of a grid of
	;side cells each way: minus the box past its low edge, the box past its
	;high edge, and 0 in it
	(cond ((< c 0) (neg +field)) ((>= c side) +field) (+double_0)))

(defun fine-at (x y z)
	; (fine-at x y z) -> num
	;the cell of the fine grid of a place, a place over an edge the cell on
	;the other side, as the field goes on
	(+ (% (+ x +fine_side) +fine_side) (* +fine_side (+ (% (+ y +fine_side) +fine_side)
		(* +fine_side (% (+ z +fine_side) +fine_side))))))

(defun rocks-grid ()
	; (rocks-grid) -> (cells at_cell smalls at_fine big_list)
	;each rock put in the cells of the two grids it is in, made once for a
	;round of meetings, a rock moves little in it. The coarse grid, for the
	;pulls: its cells, each a list of the rocks in it, and each rock's cell
	;across, up and along. The fine one, for touches: the small rocks in
	;the cell they are in, and each rock's cell across, up and along, and
	;if it is big; and the big ones
	(defq side (/ +field (n2d +cells_side)) fine (/ +field (n2d +fine_side))
		cells (lists (* +cells_side +cells_side +cells_side))
		smalls (lists (* +fine_side +fine_side +fine_side))
		at_fine (list) big_list (list)
		at_cell (map (lambda ((at & bulk kind &ignore))
				(defq i (!) reach (* bulk (elem-get kind_reach kind)) big (>= reach +small_reach))
				(bind '(x y z) (map (lambda (v) (min (dec +cells_side) (n2i (/ v side)))) at))
				(push (elem-get cells (cell-at x y z)) i)
				(bind '(fx fy fz) (map (lambda (v) (min (dec +fine_side) (n2i (/ v fine)))) at))
				(push at_fine (list fx fy fz big))
				(if big (push big_list i) (push (elem-get smalls (fine-at fx fy fz)) i))
				(list x y z))
			rocks))
	(list cells at_cell smalls at_fine big_list))

(defun rocks-touch (rock ri rock_j dx dy dz d2)
	;two rocks, the first reaching ri, the second dx dy dz from it, d2 that
	;squared: if they touch and are coming together, bounced apart, along
	;the way between them, the bigger moved the less, losing some of their
	;speed, and pushed apart so they do not sink into each other
	(bind '(& & bulk_j kind_j &ignore) rock_j)
	(defq reach (+ ri (* bulk_j (elem-get kind_reach kind_j))))
	(when (< d2 (* reach reach))
		(bind '(at drift bulk & spin &ignore) rock)
		(bind '(at_j drift_j & & spin_j &ignore) rock_j)
		(defq d (sqrt (+ d2 (const (/ +double_1 (n2d 1000000)))))
			nx (/ dx d) ny (/ dy d) nz (/ dz d) mi (* bulk bulk bulk) mj (* bulk_j bulk_j bulk_j)
			closing (+ (* (- (elem-get drift_j 0) (elem-get drift 0)) nx)
				(* (- (elem-get drift_j 1) (elem-get drift 1)) ny)
				(* (- (elem-get drift_j 2) (elem-get drift 2)) nz))
			wi (/ mj (+ mi mj)) wj (/ mi (+ mi mj)))
		(when (< closing +double_0)
			;a crunch: little rebound, the sideways part of how they move
			;ground down, and each set turning by it as their faces grip,
			;about the way across the touch and the slide, the smaller the
			;faster, as a wheel rolls
			(++ bumps)
			(push hits (list (+ (elem-get at 0) (* nx ri)) (+ (elem-get at 1) (* ny ri))
				(+ (elem-get at 2) (* nz ri)) (neg closing)))
			(defq kick (* (+ +double_1 +bounce) closing)
				tx (- (elem-get drift_j 0) (elem-get drift 0) (* closing nx))
				ty (- (elem-get drift_j 1) (elem-get drift 1) (* closing ny))
				tz (- (elem-get drift_j 2) (elem-get drift 2) (* closing nz)))
			(rock-push drift nx ny nz (* kick wi))
			(rock-push drift_j nx ny nz (neg (* kick wj)))
			(rock-push drift tx ty tz (* +grind wi))
			(rock-push drift_j tx ty tz (neg (* +grind wj)))
			(defq cx (- (* ny tz) (* nz ty)) cy (- (* nz tx) (* nx tz)) cz (- (* nx ty) (* ny tx))
				gi (/ (* +grip wi) ri) gj (/ (* +grip wj) (- reach ri)))
			(rock-spin spin bulk (* cx gi) (* cy gi) (* cz gi))
			(rock-spin spin_j bulk_j (* cx gj) (* cy gj) (* cz gj)))
		;and apart, the bigger moved the less
		(defq over (- reach d))
		(rock-push at nx ny nz (neg (* over wi)))
		(rock-push at_j nx ny nz (* over wj))))

(defun rocks-pull (rock rock_j dx dy dz d2 pull_dt)
	;two rocks, the second dx dy dz from the first, d2 that squared, pulled
	;each toward the other as much as the other is big, pull_dt the pull's
	;strength by how long
	(bind '(& drift bulk &ignore) rock)
	(bind '(& drift_j bulk_j &ignore) rock_j)
	(defq by (/ pull_dt (* (+ d2 +pull_soft) (sqrt (+ d2 (const (/ +double_1 (n2d 1000000))))))))
	(rock-push drift dx dy dz (* by bulk_j bulk_j bulk_j))
	(rock-push drift_j dx dy dz (neg (* by bulk bulk bulk))))

(defun meet-cell (i rock ri xi yi zi members ox oy oz)
	;a rock, i, reaching ri and at xi yi zi, with each rock after it of
	;the members of a cell, ox oy oz the way across the box to the cell
	(each (lambda (j)
			(when (> j i)
				(defq rock_j (elem-get rocks j) at_j (first rock_j)
					dx (+ (- (elem-get at_j 0) xi) ox) dy (+ (- (elem-get at_j 1) yi) oy)
					dz (+ (- (elem-get at_j 2) zi) oz))
				(rocks-touch rock ri rock_j dx dy dz (+ (* dx dx) (* dy dy) (* dz dz)))))
		members))

(defun rocks-meet (dt grid part)
	;the rocks of a part moved by those near them, dt seconds of it, a part
	;every +meet_parts rock from part on, so each part has as many of the
	;rocks after it as another. A round is all the parts, a part a frame,
	;so no frame has it all.
	;Touches, rocks-touch, each pair once: a small rock with each small one
	;after it in its cell of the fine grid and those round it; a big rock
	;with each big one after it, and with each small one in the cells it
	;may touch one in. Pulls, rocks-pull, in one round of +pull_every,
	;+pull_every times as much, as it changes slowly: a rock as big as
	;+pull_least with each rock round it in the coarse grid in reach of a
	;pull, but one as big after it only, so once a pair; two small rocks'
	;pull on each other is too small to be seen, and most of the pairs
	(bind '(cells at_cell smalls at_fine big_list) grid)
	(if (= part 0) (setq meet_round (inc meet_round)))
	(defq pulling (= (% meet_round +pull_every) 0) pull_dt (* +pull dt (n2d +pull_every)))
	(each! (lambda ()
			(defq i (+ (* (!) +meet_parts) part) rock (elem-get rocks i))
			(bind '(at & bulk kind &ignore) rock)
			(bind '(xi yi zi) at)
			(bind '(fx fy fz big) (elem-get at_fine i))
			(defq ri (* bulk (elem-get kind_reach kind)))
			(cond
				(big
					(each (lambda (j)
							(when (> j i)
								(defq rock_j (elem-get rocks j) at_j (first rock_j)
									dx (near-way (- (elem-get at_j 0) xi)) dy (near-way (- (elem-get at_j 1) yi))
									dz (near-way (- (elem-get at_j 2) zi)))
								(rocks-touch rock ri rock_j dx dy dz (+ (* dx dx) (* dy dy) (* dz dz)))))
						big_list)
					;the small ones, each in one cell, in the cells it may
					;touch one in, as far as its reach and a small one's
					(defq out (+ ri +small_reach) fine (/ +field (n2d +fine_side))
						lo (map (lambda (v) (n2i (floor (/ (- v out) fine)))) at)
						hi (map (lambda (v) (n2i (floor (/ (+ v out) fine)))) at))
					(each! (lambda ()
							(defq cx (!) ox (cell-wrap cx +fine_side))
							(each! (lambda ()
									(defq cy (!) oy (cell-wrap cy +fine_side))
									(each! (lambda ()
											(defq cz (!))
											(each (lambda (j)
													(defq rock_j (elem-get rocks j) at_j (first rock_j)
														dx (+ (- (elem-get at_j 0) xi) ox) dy (+ (- (elem-get at_j 1) yi) oy)
														dz (+ (- (elem-get at_j 2) zi) (cell-wrap cz +fine_side)))
													(rocks-touch rock ri rock_j dx dy dz (+ (* dx dx) (* dy dy) (* dz dz))))
												(elem-get smalls (fine-at cx cy cz))))
										'() (third lo) (inc (third hi))))
								'() (second lo) (inc (second hi))))
						'() (first lo) (inc (first hi))))
				((and (< 0 fx (dec +fine_side)) (< 0 fy (dec +fine_side)) (< 0 fz (dec +fine_side)))
					;inside the box, the cells round it its own and steps on,
					;none over an edge
					(defq c (fine-at fx fy fz))
					(each (lambda (step) (meet-cell i rock ri xi yi zi (elem-get smalls (+ c step))
							+double_0 +double_0 +double_0))
						fine_steps))
				(:t (each (lambda ((sx sy sz))
						(defq cx (+ fx sx) cy (+ fy sy) cz (+ fz sz))
						(meet-cell i rock ri xi yi zi (elem-get smalls (fine-at cx cy cz))
							(cell-wrap cx +fine_side) (cell-wrap cy +fine_side) (cell-wrap cz +fine_side)))
					cell_steps)))
			(when (and pulling (>= bulk +pull_least))
				(bind '(cx cy cz) (elem-get at_cell i))
				(each (lambda ((sx sy sz))
						(defq ox (cell-wrap (+ cx sx) +cells_side) oy (cell-wrap (+ cy sy) +cells_side)
							oz (cell-wrap (+ cz sz) +cells_side))
						(each (lambda (j)
								(defq rock_j (elem-get rocks j))
								(when (and (/= j i) (or (> j i) (< (third rock_j) +pull_least)))
									(defq at_j (first rock_j) dx (+ (- (elem-get at_j 0) xi) ox)
										dy (+ (- (elem-get at_j 1) yi) oy) dz (+ (- (elem-get at_j 2) zi) oz)
										d2 (+ (* dx dx) (* dy dy) (* dz dz)))
									(if (< d2 +pull_reach2) (rocks-pull rock rock_j dx dy dz d2 pull_dt))))
							(elem-get cells (cell-at (+ cx sx) (+ cy sy) (+ cz sz)))))
					cell_steps)))
		'() 0 (/ (+ (- (length rocks) part) (dec +meet_parts)) +meet_parts)))

(defun rocks-blast (x y z from to &optional reach strength calm)
	; (rocks-blast x y z from to [reach strength calm]) -> num
	;a blast at x y z, its front of pressure gone on from from to to out
	;from it: each rock the front has passed over in that thrown out from
	;the middle, as hard as the pressure on its face, so the less the
	;further and the bigger the rock, its face as its size squared and its
	;weight cubed, a big one moved little and a small one far; and set
	;tumbling the faster, as an uneven face takes the pressure, along the
	;spin it has, or about a way across the way out if it has none. How many it moved. Nothing is
	;broken, the rocks only go. Of +blast_reach and +blast_strength if not
	;said, a missile's. A calm blast, a nuke's, first takes each rock's
	;spin down to +blast_calm of it, so a heap it breaks settles
	(defq moved 0 reach (ifn reach +blast_reach) strength (ifn strength +blast_strength))
	(each (lambda ((at drift bulk & spin &ignore))
			(defq dx (near-way (- (first at) x)) dy (near-way (- (second at) y)) dz (near-way (- (third at) z))
				d (sqrt (+ (* dx dx) (* dy dy) (* dz dz) +double_1)))
			(when (and (>= d from) (< d to) (< d reach))
				(defq by (/ (* strength (- +double_1 (/ d reach))) bulk))
				(rock-push drift dx dy dz (/ by d))
				(if calm (each! (lambda (v) (elem-set spin (!) (* v +blast_calm))) (list spin) 0 3))
				;its tumble along the spin it has, so blast on blast spins it
				;up, and only one all but still is set going about a way
				;across the way out, at random
				(defq w (sqrt (+ (* (first spin) (first spin)) (* (second spin) (second spin)) (* (third spin) (third spin)))))
				(bind '(ax ay az) (if (> w +double_1/20)
					(doubles (/ (first spin) w) (/ (second spin) w) (/ (third spin) w))
					(sky-unit (sky-cross (doubles dx dy dz) (rnd-way)))))
				(defq turn (* (min by +blast_turn_most) +blast_spin))
				(rock-spin spin bulk (* ax turn) (* ay turn) (* az turn))
				(++ moved)))
		rocks)
	moved)

(defun rock-at (x y z)
	; (rock-at x y z) -> :nil | rock
	;the rock a place is in the reach of, if any
	(some (lambda (rock)
			(bind '(at & bulk kind &ignore) rock)
			(defq dx (near-way (- (first at) x)) dy (near-way (- (second at) y)) dz (near-way (- (third at) z))
				r (* bulk (elem-get kind_reach kind)))
			(if (< (+ (* dx dx) (* dy dy) (* dz dz)) (* r r)) rock))
		rocks))

(defun rock-strike (rock x y z way)
	;a rock struck at x y z by something going way, of length 1: spun
	;about the way across from its middle to where it was struck and the
	;way, as hard as it was struck off its middle, the smaller the faster;
	;struck through its middle, not spun
	(bind '(at & bulk kind spin &ignore) rock)
	(defq rx (near-way (- x (first at))) ry (near-way (- y (second at))) rz (near-way (- z (third at)))
		k (/ +strike_spin (* bulk bulk (elem-get kind_reach kind))))
	(bind '(wx wy wz) way)
	(rock-spin spin bulk (* k (- (* ry wz) (* rz wy))) (* k (- (* rz wx) (* rx wz))) (* k (- (* rx wy) (* ry wx)))))

(defun rocks-near (eye ahead)
	; (rocks-near eye ahead) -> ((rx ry rz dx dy dz reach) ...)
	;the rocks near enough to the eye to matter to its way, and not behind
	;it, ahead its way, each where it is from the eye, nearest, its drift,
	;and how far it reaches
	(defq out (list))
	(bind '(ex ey ez) eye)
	(bind '(ax ay az) ahead)
	(each (lambda ((at drift bulk kind &ignore))
			(defq rx (near-way (- (first at) ex)) ry (near-way (- (second at) ey)) rz (near-way (- (third at) ez)))
			(and (< (abs rx) +look_far) (< (abs ry) +look_far) (< (abs rz) +look_far)
				(> (+ (* rx ax) (* ry ay) (* rz az)) (neg (* bulk +behind_bulks)))
				(push out (list rx ry rz (first drift) (second drift) (third drift) (* bulk (elem-get kind_reach kind))))))
		rocks)
	out)

(defun way-clear (near vx vy vz)
	; (way-clear near vx vy vz) -> (least next)
	;how near, past their reach, the ship going at vx vy vz comes to the
	;nearest two of the near rocks in the time it looks ahead, the nearest
	;and the next: two near is a gap between them; +look_far for none
	(defq least +look_far next +look_far)
	(each (lambda ((rx ry rz dx dy dz reach))
			(defq wx (- dx vx) wy (- dy vy) wz (- dz vz)
				ww (+ (* wx wx) (* wy wy) (* wz wz) (const (/ +double_1 (n2d 1000000))))
				t (max +double_0 (min +look_time (/ (neg (+ (* rx wx) (* ry wy) (* rz wz))) ww)))
				cx (+ rx (* wx t)) cy (+ ry (* wy t)) cz (+ rz (* wz t))
				clear (- (sqrt (+ (* cx cx) (* cy cy) (* cz cz))) reach))
			(cond
				((< clear least) (setq next least least clear))
				((< clear next) (setq next clear))))
		near)
	(list least next))

(defun thread-start (eye speed want right up thrill)
	; (thread-start eye speed want right up thrill) -> thread
	;a choice of the way to go begun, made a few headings at a time,
	;(thread-step), so no frame has all of it: the rocks near, the speed,
	;the way wanted and its across and up, whether a tour guide's, the
	;heading tried next, the best yet and its score, and the clearest
	(list (rocks-near eye want) speed want right up thrill 0 want :nil +double_0 want))

(defun thread-step (thread count)
	; (thread-step thread count) -> thread | doubles
	;so many more headings of a choice tried, round the way wanted, as far
	;as +thread_cone from it, of those that clear every rock by
	;+thread_safe: with thrill, a tour guide's, the one through the tightest
	;gap, the two rocks it passes nearest the nearer each other, the way
	;wanted only to choose between those alike, threading the needle; else
	;the nearest the way wanted. With none that clear them, the one that
	;clears them the most. The thread, or with all tried, the way chosen
	(bind '(near speed want right up thrill at best best_score most most_way) thread)
	(each! (lambda ((across upward))
			(defq way (sky-unit (nums-add want (nums-add (nums-scale right across) (nums-scale up upward))))
				off (+ (abs across) (abs upward)))
			(bind '(least next) (way-clear near (* speed (first way)) (* speed (second way)) (* speed (third way))))
			(when (> least most) (setq most least most_way way))
			(when (>= least +thread_safe)
				(defq score (if thrill (+ (min next +gap_most) (* off +thrill_weight)) off))
				(when (or (not best_score) (< score best_score)) (setq best way best_score score))))
		(list thread_offsets) at (min (length thread_offsets) (+ at count)))
	(setq at (+ at count))
	(cond
		((< at (length thread_offsets))
			(list near speed want right up thrill at best best_score most most_way))
		(best_score best)
		(most_way)))

(defun make-buoys ()
	; (make-buoys) -> buoys
	;the marker buoys of the field, each (at drift hazard colour station
	;calm over armed), calm how many looks running it has read calm, over
	;how many it has read too crowded, buoy-look, and armed when it was
	;armed to go nuclear, 0 if not, the app's:
	;most at the swarms, a buoy keeping station at each of the first
	;clusters, station its number, buoy-station, and a few in open space
	;between, marking the safe way, drifting slowly. Their hazard is 0 till
	;looked at, buoy-look
	(defq out (list))
	(each! (lambda (members)
			(bind '(at drift) (buoy-station members))
			(push out (list at drift +double_0 (cat +buoy_green) (!) 0 0 0)))
		(list clusters) 0 (min +buoys_at_swarms (length clusters)))
	(while (< (length out) +buoys)
		(push out (list (doubles (* (rnd) +field) (* (rnd) +field) (* (rnd) +field))
			(nums-scale (rnd-way) (+ +buoy_drift_least (rnd))) +double_0 (cat +buoy_blue) :nil 0 0 0)))
	out)

(defun rgb-mix (a b t)
	; (rgb-mix a b t) -> doubles
	(nums-add a (nums-scale (nums-sub b a) t)))

(defun buoys-move (dt)
	;each buoy on by its drift, kept in the box, and its light's colour eased
	;toward the colour of its hazard, calm green at a swarm, ice blue in
	;open space
	(each (lambda ((at drift hazard colour station &ignore))
			(each! (lambda ()
					(defq i (!) v (+ (elem-get at i) (* (elem-get drift i) dt)))
					(elem-set at i (cond ((< v +double_0) (+ v +field)) ((>= v +field) (- v +field)) (v))))
				'() 0 3)
			(defq want (if (< hazard +double_1/2)
					(rgb-mix (if station +buoy_green +buoy_blue) +buoy_yellow (* hazard +double_2))
					(rgb-mix +buoy_yellow +buoy_red (* (- hazard +double_1/2) +double_2)))
				k (min +double_1 (* dt +buoy_ease)))
			(each! (lambda (w c) (elem-set colour (!) (+ c (* k (- w c))))) (list want colour)))
		buoys))

(defun buoy-look (i)
	;a buoy's hazard, 0 to 1, of the rocks round it, how many within
	;+buoy_round and how near the nearest, green to red; and if it is in a
	;rock, out of it
	(bind '(at & &ignore) (elem-get buoys i))
	(defq count 0 nearest +buoy_round)
	(each (lambda ((rock_at & bulk kind &ignore))
			(defq rx (near-way (- (first rock_at) (first at))) ry (near-way (- (second rock_at) (second at)))
				rz (near-way (- (third rock_at) (third at))))
			(when (and (< (abs rx) +buoy_round) (< (abs ry) +buoy_round) (< (abs rz) +buoy_round))
				(defq d (sqrt (+ (* rx rx) (* ry ry) (* rz rz) +double_1)) reach (* bulk (elem-get kind_reach kind)))
				(when (< d +buoy_round)
					(++ count)
					(setq nearest (min nearest (- d reach)))
					(if (< d (+ reach +buoy_near))
						(rock-push at (/ rx d) (/ ry d) (/ rz d) (- d (+ reach +buoy_near)))))))
		rocks)
	;crowded, from one rock round it, at most yellow orange; red for a rock
	;within +buoy_close, the redder the closer; and how many looks running
	;it has been calm
	(defq buoy (elem-get buoys i)
		hazard (max (* +crowd_hazard (max +double_0 (min +double_1 (/ (n2d (- count 1)) +crowd_full))))
			(if (< nearest +buoy_close) (- +double_1 (/ nearest +close_hazard)) +double_0)))
	(elem-set buoy 2 hazard)
	(elem-set buoy 5 (if (< hazard +buoy_calm) (inc (elem-get buoy 5)) 0))
	(elem-set buoy 6 (if (>= count +nuke_crowd) (inc (elem-get buoy 6)) 0)))

(defun rocks-nuke (x y z eye far)
	; (rocks-nuke x y z eye far) -> ((x y z) ...)
	;a buoy gone nuclear at x y z: each small rock in its core shattered,
	;where it was kept to be seen, and sent off out of sight, further from
	;the eye than far, drifting a way of its own and no more of its
	;cluster, so a cluster put in orbit again does not gather it back. The
	;big ones are left to its blast. Where those shattered were
	(defq out (list) gone (list))
	(each (lambda (rock)
			(bind '(at drift bulk & spin &ignore) rock)
			(defq dx (near-way (- (first at) x)) dy (near-way (- (second at) y)) dz (near-way (- (third at) z)))
			(when (and (< bulk +pull_least) (< (+ (* dx dx) (* dy dy) (* dz dz)) (const (* +nuke_core +nuke_core))))
				(push out (cat at))
				(push gone (!))
				(defq to (doubles (* (rnd) +field) (* (rnd) +field) (* (rnd) +field)) tries 10)
				(while (and (> (-- tries) 0) (not (out-of-sight? to eye far)))
					(setq to (doubles (* (rnd) +field) (* (rnd) +field) (* (rnd) +field))))
				(each! (lambda (v) (elem-set at (!) v)) (list to))
				(each! (lambda (v) (elem-set drift (!) v)) (list (nums-scale (rnd-way) (* +drift_most (rnd)))))
				(each! (lambda (v) (elem-set spin (!) v)) (list (nums-scale (rnd-way) (* (rnd) +tumble_most))))))
		rocks)
	(setq clusters (map (lambda (members) (filter (# (not (find %0 gone))) members)) clusters))
	out)

(defun cluster-crowd (members)
	; (cluster-crowd members) -> num
	;how many of a cluster's rocks are still round its big one, few when it
	;has been blown apart or its rocks have gone
	(bind '((at & big kind &ignore)) (list (elem-get rocks (first members))))
	(defq round (+ (* big (elem-get kind_reach kind)) +crowd_round))
	(reduce! (lambda (n i)
			(bind '(rock_at &ignore) (elem-get rocks i))
			(defq dx (near-way (- (first rock_at) (first at))) dy (near-way (- (second rock_at) (second at)))
				dz (near-way (- (third rock_at) (third at))))
			(if (< (+ (* dx dx) (* dy dy) (* dz dz)) (* round round)) (inc n) n))
		(list members) 0 1 -1))

(defun buoys-restation (eye far)
	;a buoy at a swarm that has read calm for +calm_looks looks running,
	;its swarm gone, put on station at the busiest cluster with no buoy,
	;both out of sight, further from the eye than far, so it is never seen
	;to go or come; one at a time. Those in open space stay, the safe way
	(defq calm (some (lambda (buoy)
			(and (elem-get buoy 4) (>= (elem-get buoy 5) +calm_looks) (out-of-sight? (first buoy) eye far) buoy))
		buoys))
	(when calm
		(defq taken (map (# (elem-get %0 4)) buoys) best :nil best_crowd -1)
		(each (lambda (members)
				(and (not (find (!) taken))
					(out-of-sight? (first (elem-get rocks (first members))) eye far)
					(> (defq crowd (cluster-crowd members)) best_crowd)
					(setq best (!) best_crowd crowd)))
			clusters)
		(when best
			(bind '(at drift) (buoy-station (elem-get clusters best)))
			(each! (lambda (v) (elem-set (first calm) (!) v)) (list at))
			(each! (lambda (v) (elem-set (second calm) (!) v)) (list drift))
			(elem-set calm 2 +double_0)
			(elem-set calm 4 best)
			(elem-set calm 5 0))))

;module, what the app and the tests use: its own numbers, prebound into
;its functions as they are made, go with it
(export-symbols
	'(rocks clusters buoys hits bumps kind_reach meet_round thread_offsets cell_steps fine_steps
	+rock_kinds +rock_count +field +clusters +meet_parts +rock_far +fade_near +fade_far
	+buoys +buoy_red +buoy_green +buoy_blue +calm_looks +nuke_looks +rim_most +spin_most
	+blast_front +blast_reach +blast_strength
	rnd rnd-way near-way out-of-sight? make-rocks make-buoys rock-at rock-spin rock-strike
	rock-turned rocks-blast rocks-grid rocks-meet rocks-move rocks-nuke clusters-refresh
	cluster-again buoy-look buoys-move buoys-restation thread-start thread-step))
(env-pop)

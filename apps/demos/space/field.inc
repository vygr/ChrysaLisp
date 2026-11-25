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

;how many kinds of rock, how many rocks, and how big the box of them
(defq +rock_kinds 8 +rock_count 450 +field (n2d 1000)
	;the rocks, each (at drift bulk kind spin patch), make-rocks, and the
	;clusters, each the numbers of its rocks, its big one first
	rocks (list) clusters (list)
	;the marker buoys, make-buoys, how many, how many at swarms, how far
	;round one it counts
	;rocks, how soon its light follows its hazard, and its colours, safe,
	;crowded and dangerous
	buoys (list) +buoys 24 +buoys_at_swarms 20 +buoy_round (n2d 100) +buoy_close (n2d 24) +buoy_ease (n2d 1.5)
	+buoy_green (doubles (n2d 0.2) (n2d 1.0) (n2d 0.35)) +buoy_yellow (doubles (n2d 1.0) (n2d 0.82) (n2d 0.15))
	+buoy_red (doubles (n2d 1.0) (n2d 0.15) (n2d 0.1))
	;how far out each kind of rock reaches from its middle, as it turns, of
	;its size, its bound, near 1 till its mesh has come
	kind_reach (fill (doubles) +rock_kinds (n2d 0.9))
	;how fast at most they drift at first, a second; how many clusters of
	;them; in how many parts, a part a frame, they are moved by each other;
	;cells each way of the box; how strong the pull, how far it reaches,
	;squared, and how soft it is close to; and how much of its speed a hit
	;keeps, little, rock on rock is a crunch, how much of the sideways part
	;of it is ground down, and how hard it jolts a tumble, a second for a
	;speed of 1
	+drift_most (n2d 3) +clusters 45 +meet_parts 12 +cells_side 8 +pull (n2d 0.025)
	+pull_reach2 (n2d 62500) +pull_soft (n2d 400) +bounce (n2d 0.15) +grind (n2d 0.4) +jolt (n2d 0.25)
	;the ship keeps clear of rocks: how far ahead it looks, each way, and
	;how many seconds ahead
	+look_far (n2d 380) +look_time (n2d 10)
	;threading: how far a heading tried is from the way wanted, at most, a
	;tangent, and how far past a rock's reach is clear
	+thread_cone (n2d 0.9) +thread_safe (n2d 1.5)
	;the widest a gap is that is a gap, past it all are as dull
	+gap_most (n2d 80)
	;the headings tried, as far across and up from the way wanted: it, and
	;five rings of eight round it
	thread_offsets (cat (list (list +double_0 +double_0))
		(reduce (lambda (out ring)
				(defq r (* +thread_cone (/ (n2d ring) (const (n2d 5)))))
				(each! (lambda () (defq a (/ (* +double_2pi (n2d (!))) (const (n2d 8))))
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
	;how far off a rock starts to go into the sky behind it and is gone into
	;it, and is drawn: all of it inside half the box, as past that a rock is
	;drawn on the other side of the eye, where it is next as the field goes
	;on, and would come from nowhere
	+fade_near (* +field (n2d 0.30)) +fade_far (* +field (n2d 0.47)) +rock_far (* +field (n2d 0.48)))

(defun rnd ()
	; (rnd) -> double
	;0 to 1
	(/ (n2d (random 1000000)) (const (n2d 1000000))))


(defun rnd-way ()
	; (rnd-way) -> doubles
	;a way at random, of length 1, as likely any way
	(defq y (- (* +double_2 (rnd)) +double_1) c (sqrt (- +double_1 (* y y))) a (* +double_2pi (rnd)))
	(doubles (* c (cos a)) y (* c (sin a))))

(defun make-rock (at drift bulk)
	; (make-rock at drift bulk) -> rock
	;a rock (at drift bulk kind spin patch) at a place, drifting, of a size:
	;its kind, how fast it tumbles about two ways and how far it has, and the
	;patch of the moon's map on it, where and how much, at random
	(list at drift bulk (random +rock_kinds)
		(doubles (* (- (rnd) +double_1/2) (const (n2d 0.6))) (* (- (rnd) +double_1/2) (const (n2d 0.6)))
			(* (rnd) +double_2pi) (* (rnd) +double_2pi))
		(doubles (rnd) (rnd) (+ (const (n2d 0.06)) (* (const (n2d 0.12)) (rnd))))))

(defun orbit-at (middle drift big small)
	; (orbit-at middle drift big small) -> (at drift)
	;a place and a drift for a small rock in orbit round a big one at
	;middle, drifting: clear of it, going round it, across the way out at
	;the speed the big one's pull holds it there, with its softness, a
	;little more or less, so its orbit is an ellipse, some of them dipping
	;close, and their orbits crossing, so they meet and crunch
	(defq way (rnd-way) out_by (+ big small (const (n2d 15)) (* (const (n2d 90)) (rnd)))
		round (sky-unit (sky-cross way (rnd-way)))
		hold (sqrt (/ (* +pull big big big out_by) (+ (* out_by out_by) +pull_soft))))
	(list (apply (const doubles) (map (lambda (m w)
				(defq v (+ m (* w out_by)))
				(cond ((< v +double_0) (+ v +field)) ((>= v +field) (- v +field)) (v))) middle way))
		(nums-add (nums-add drift (nums-scale round (* hold (+ (const (n2d 0.75)) (* (const (n2d 0.35)) (rnd))))))
			(nums-scale way (* (- (rnd) +double_1/2) (const (n2d 0.8)))))))

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
			big (+ (const (n2d 35)) (* (const (n2d 25)) (rnd))) members (list (length out)))
		(push out (make-rock middle (cat drift) big))
		(times (+ 5 (random 7))
			(defq small (+ (const (n2d 4)) (* (const (n2d 16)) (rnd) (rnd))))
			(bind '(at go) (orbit-at middle drift big small))
			(push members (length out))
			(push out (make-rock at go small)))
		(push clusters members))
	(while (< (length out) +rock_count)
		(push out (make-rock (doubles (* (rnd) +field) (* (rnd) +field) (* (rnd) +field))
			(nums-scale (rnd-way) (* +drift_most (+ (const (n2d 0.15)) (* (const (n2d 0.85)) (rnd)))))
			(+ (const (n2d 5)) (* (const (n2d 55)) (rnd) (rnd))))))
	out)

(defun cluster-again (members)
	;a cluster's small rocks put in orbit round its big one again, where
	;the big one is now and going as it goes, their tumbles calmed: a
	;cluster settled into a heap made lively again
	(bind '((at drift big &ignore)) (list (elem-get rocks (first members))))
	(each! (lambda (i)
			(bind '(rock_at rock_drift small & spin &ignore) (elem-get rocks i))
			(bind '(new_at new_drift) (orbit-at at drift big small))
			(each! (lambda (v) (elem-set rock_at (!) v)) (list new_at))
			(each! (lambda (v) (elem-set rock_drift (!) v)) (list new_drift))
			(elem-set spin 0 (* (- (rnd) +double_1/2) (const (n2d 0.6))))
			(elem-set spin 1 (* (- (rnd) +double_1/2) (const (n2d 0.6)))))
		(list members) 1))

(defun buoy-again (cluster)
	;the buoy of a cluster, if it has one, put back on station
	(each (lambda (buoy)
			(when (eql (elem-get buoy 4) cluster)
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
				(cluster-again members) (buoy-again cluster) (setq tries 0))
			((-- tries)))))

(defun near-way (v)
	; (near-way v) -> double
	;of a way across the box, the shortest, as the box goes on every way
	(cond
		((> v (const (* +field +double_1/2))) (- v +field))
		((< v (const (* +field (n2d -0.5)))) (+ v +field))
		(v)))


(defun rocks-move (dt)
	;each rock on by its drift, kept in the box of the field, and turned on
	;as fast as it tumbles, which a knock may change
	(each (lambda ((at drift & & spin &ignore))
			(each! (lambda ()
					(defq i (!) v (+ (elem-get at i) (* (elem-get drift i) dt)))
					(elem-set at i (cond ((< v +double_0) (+ v +field)) ((>= v +field) (- v +field)) (v))))
				'() 0 3)
			(elem-set spin 2 (+ (elem-get spin 2) (* (elem-get spin 0) dt)))
			(elem-set spin 3 (+ (elem-get spin 3) (* (elem-get spin 1) dt))))
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


(defun rocks-grid ()
	; (rocks-grid) -> (cells at_cell)
	;each rock put in the cell of the grid it is in: the cells, each a list
	;of the rocks in it, and each rock's cell across, up and along. Made
	;once for a round of meetings, a rock moves little in it
	(defq side (/ +field (n2d +cells_side))
		cells (lists (* +cells_side +cells_side +cells_side))
		at_cell (map (lambda ((at &ignore))
			(bind '(x y z) (map (lambda (v) (min (dec +cells_side) (n2i (/ v side)))) at))
			(push (elem-get cells (cell-at x y z)) (!))
			(list x y z)) rocks))
	(list cells at_cell))

(defun rock-jolt (spin by)
	;a tumble jolted by a knock, as hard as by, either way at random
	(elem-set spin 0 (+ (elem-get spin 0) (* by (- (rnd) +double_1/2) +jolt)))
	(elem-set spin 1 (+ (elem-get spin 1) (* by (- (rnd) +double_1/2) +jolt))))

(defun rocks-meet (dt grid part)
	;the rocks of a part moved by those near them, dt seconds of it, a part
	;every +meet_parts rock from part on, so each part has as many of the
	;rocks after it as another:
	;each with each other rock after it in its cell and those round it, of
	;the grid, so once a pair, pulled together as much as the other is big,
	;and if they touch and are coming together, bounced apart, along the
	;way between them, the bigger moved the less, losing some of their
	;speed, and pushed apart so they do not sink into each other. A round
	;is all the parts, a part a frame, so no frame has it all
	(bind '(cells at_cell) grid)
	(each! (lambda ()
			(defq i (* (!) +meet_parts))
			(bind '(at drift bulk kind spin &ignore) (elem-get rocks (+ i part)))
			(setq i (+ i part))
			(defq mi (* bulk bulk bulk) ri (* bulk (elem-get kind_reach kind)))
			(bind '(cx cy cz) (elem-get at_cell i))
			(each (lambda ((sx sy sz))
					(each (lambda (j)
							(when (> j i)
								(bind '(at_j drift_j bulk_j kind_j spin_j &ignore) (elem-get rocks j))
								(defq dx (near-way (- (elem-get at_j 0) (elem-get at 0)))
									dy (near-way (- (elem-get at_j 1) (elem-get at 1)))
									dz (near-way (- (elem-get at_j 2) (elem-get at 2)))
									d2 (+ (* dx dx) (* dy dy) (* dz dz)))
								(when (< d2 +pull_reach2)
									(defq d (sqrt (+ d2 (const (/ +double_1 (n2d 1000000)))))
										mj (* bulk_j bulk_j bulk_j) nx (/ dx d) ny (/ dy d) nz (/ dz d)
										f (/ (* +pull dt) (+ d2 +pull_soft))
										reach (+ ri (* bulk_j (elem-get kind_reach kind_j))))
									;the pull, each toward the other
									(rock-push drift nx ny nz (* f mj))
									(rock-push drift_j nx ny nz (neg (* f mi)))
									(when (< d reach)
										;touching: coming together, a bounce
										(defq closing (+ (* (- (elem-get drift_j 0) (elem-get drift 0)) nx)
												(* (- (elem-get drift_j 1) (elem-get drift 1)) ny)
												(* (- (elem-get drift_j 2) (elem-get drift 2)) nz))
											wi (/ mj (+ mi mj)) wj (/ mi (+ mi mj)))
										(when (< closing +double_0)
											;a crunch: little rebound, the sideways part of
											;how they move ground down, and each tumble
											;jolted, the smaller rock the more
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
											(rock-jolt spin (* closing wi))
											(rock-jolt spin_j (* closing wj)))
										;and apart, the bigger moved the less
										(defq over (- reach d))
										(rock-push at nx ny nz (neg (* over wi)))
										(rock-push at_j nx ny nz (* over wj))))))
						(elem-get cells (cell-at (+ cx sx) (+ cy sy) (+ cz sz)))))
				cell_steps))
		'() 0 (/ (+ (- (length rocks) part) (dec +meet_parts)) +meet_parts)))

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
				(> (+ (* rx ax) (* ry ay) (* rz az)) (neg (* bulk (const (n2d 1.5)))))
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
				(defq score (if thrill (+ (min next +gap_most) (* off (const (n2d 8)))) off))
				(when (or (not best_score) (< score best_score)) (setq best way best_score score))))
		(list thread_offsets) at (min (length thread_offsets) (+ at count)))
	(setq at (+ at count))
	(cond
		((< at (length thread_offsets))
			(list near speed want right up thrill at best best_score most most_way))
		(best_score best)
		(most_way)))

(defun buoy-station (members)
	; (buoy-station members) -> (at drift)
	;where a buoy keeps station at a cluster, just outside its big rock and
	;the swarm round it, and its drift, the big one's, so it goes along with
	;the cluster
	(bind '((at drift big kind &ignore)) (list (elem-get rocks (first members))))
	(defq way (rnd-way) out_by (+ (* big (elem-get kind_reach kind)) (const (n2d 45)) (* (const (n2d 40)) (rnd))))
	(list (apply (const doubles) (map (lambda (m w)
				(defq v (+ m (* w out_by)))
				(cond ((< v +double_0) (+ v +field)) ((>= v +field) (- v +field)) (v))) at way))
		(cat drift)))

(defun make-buoys ()
	; (make-buoys) -> buoys
	;the marker buoys of the field, each (at drift hazard colour station):
	;most at the swarms, a buoy keeping station at each of the first
	;clusters, station its number, buoy-station, and a few in open space
	;between, marking the safe way, drifting slowly. Their hazard is 0 till
	;looked at, buoy-look
	(defq out (list))
	(each! (lambda (members)
			(bind '(at drift) (buoy-station members))
			(push out (list at drift +double_0 (cat +buoy_green) (!))))
		(list clusters) 0 (min +buoys_at_swarms (length clusters)))
	(while (< (length out) +buoys)
		(push out (list (doubles (* (rnd) +field) (* (rnd) +field) (* (rnd) +field))
			(nums-scale (rnd-way) (+ (const (n2d 0.4)) (rnd))) +double_0 (cat +buoy_green) :nil)))
	out)

(defun buoys-move (dt)
	;each buoy on by its drift, kept in the box, and its light's colour eased
	;toward the colour of its hazard
	(each (lambda ((at drift hazard colour &ignore))
			(each! (lambda ()
					(defq i (!) v (+ (elem-get at i) (* (elem-get drift i) dt)))
					(elem-set at i (cond ((< v +double_0) (+ v +field)) ((>= v +field) (- v +field)) (v))))
				'() 0 3)
			(defq want (if (< hazard +double_1/2)
					(rgb-mix +buoy_green +buoy_yellow (* hazard +double_2))
					(rgb-mix +buoy_yellow +buoy_red (* (- hazard +double_1/2) +double_2)))
				k (min +double_1 (* dt +buoy_ease)))
			(each! (lambda (w c) (elem-set colour (!) (+ c (* k (- w c))))) (list want colour)))
		buoys))

(defun rgb-mix (a b t)
	; (rgb-mix a b t) -> doubles
	(nums-add a (nums-scale (nums-sub b a) t)))

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
					(if (< d (+ reach (const (n2d 4))))
						(rock-push at (/ rx d) (/ ry d) (/ rz d) (- d (+ reach (const (n2d 4)))))))))
		rocks)
	;crowded, from one rock round it, at most yellow orange; red for a rock
	;within +buoy_close, the redder the closer
	(elem-set (elem-get buoys i) 2
		(max (* (const (n2d 0.65)) (max +double_0 (min +double_1 (/ (n2d (- count 1)) (const (n2d 5))))))
			(if (< nearest +buoy_close) (- +double_1 (/ nearest (const (n2d 100)))) +double_0))))

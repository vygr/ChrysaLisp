;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water's weed, the clumps of fronds the seabed grows in patches, and the job
; a child of its farm does to make one. A clump is blades rising from a
; base, each a ribbon, its centre line leaning and curving out as it rises,
; turning a little about it, broad in the middle and pointed at its tip: kelp
; of a few broad tall blades, or a tuft of sea grass of many thin short
; ones. A vertex where it is and its normal, as the shaders want it, the
; base at 0 and the tallest tip at 1 up, so the vertex shader knows how far
; up a blade a place is and sways it the more the higher. A few of each kind
; made the once, at three sizes, and each clump on the floor one of them,
; turned and sized; chunk.inc says where they grow.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/consts/numbers.inc")
(import "lib/gpu/vp.inc")

;module
(env-push)

;the kinds of weed: kelp and sea grass, how many of each, and of all
(defq +kelps 4 +grasses 3 +weed_kinds (+ +kelps +grasses))

;the numbers of the shapes: the range of a pick; how many steps up a blade
;at each fineness; of kelp and grass, each: how many blades at least and
;their spread, how far from the middle of the base, how tall at least and
;the spread, how wide, how far it leans out at least and the spread, and
;how far it turns about itself
(defq +rnd_range (n2d 8388608) +weed_steps ''(10 5 3)
	+kelp_form ''(5 5 0.14 0.55 0.45 0.085 0.15 0.4 1.6)
	+grass_form ''(12 8 0.15 0.5 0.5 0.035 0.15 0.4 0.6)
	;how tall a clump is, kelp at least and more, grass at least and more,
	;and how much it leans to the floor it grows on
	+kelp_least (n2d 2.5) +kelp_more (n2d 3.5) +grass_least (n2d 0.5) +grass_more (n2d 0.6)
	+weed_tilt (n2d 0.3)
	;a blade's width at its foot, how much more in its belly, how much less
	;at its tip; the slope of its bow, h to the 1.5, and the least h of it
	+blade_foot (n2d 0.3) +blade_belly (n2d 2.4) +blade_point (n2d 0.7) +bow_slope (n2d 1.5) +bow_least (n2d 0.01))

(defun weed-picks (seed n)
	; (weed-picks seed n) -> (number ...)
	;numbers from 0 to 1 of a seed, the same every time and on every node
	(defq state seed)
	(map! (lambda ()
			(setq state (logand (+ (* state 1103515245) 12345) 0x7fffffff))
			(/ (n2d (>> state 8)) +rnd_range))
		'() 0 n))

(defun corner (p n)
	; (corner p n) -> doubles
	(cat p (const (doubles +double_1)) n))

(defun cross-of (a b)
	; (cross-of a b) -> doubles
	;a cross b, of length 1
	(bind '(ax ay az) a)
	(bind '(bx by bz) b)
	(defq c (doubles (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx))) cc (nums-dot c c))
	(if (> cc +double_0) (nums-scale c (recip (sqrt cc))) (doubles +double_0 +double_1 +double_0)))

(defun weed-blade (out picks at form steps)
	; (weed-blade out picks at form steps) -> out
	;one blade of picks from at on, triangles pushed on out, each corner x y
	;z 1 and its normal
	(bind '(& & base high high_spread wide lean lean_spread turn) form)
	(defq a (* (elem-get picks at) +double_2pi) far (* base (elem-get picks (inc at)))
		bx (* far (cos a)) bz (* far (sin a))
		lean_way (* (elem-get picks (+ at 2)) +double_2pi) lx (cos lean_way) lz (sin lean_way)
		tall (+ high (* high_spread (elem-get picks (+ at 3))))
		out_by (+ lean (* lean_spread (elem-get picks (+ at 4))))
		twist (* turn (- (* +double_2 (elem-get picks (+ at 5))) +double_1))
		rows (map! (lambda ()
				(defq h (/ (n2d (!)) (n2d steps)) bow (* out_by h (sqrt h))
					c (doubles (+ bx (* lx bow)) (* tall h) (+ bz (* lz bow)))
					;the way across it, square to the way it leans, turned
					s (+ lean_way (const (* +double_pi +double_1/2)) (* twist h))
					across (doubles (cos s) +double_0 (sin s))
					w (* wide (+ +blade_foot (* +blade_belly h (- +double_1 h))) (- +double_1 (* +blade_point h h h)))
					;its way up, of the bow's slope
					rise (* out_by +bow_slope (sqrt (max h +bow_least)))
					up (doubles (* lx rise) tall (* lz rise))
					n (cross-of across up))
				(list (nums-sub c (nums-scale across w)) (nums-add c (nums-scale across w)) n))
			'() 0 (inc steps)))
	(each (lambda ((l0 r0 n0) (l1 r1 n1))
			(push out (corner l0 n0) (corner r0 n0) (corner r1 n1)
				(corner l0 n0) (corner r1 n1) (corner l1 n1)))
		(slice rows 0 -2) (slice rows 1 -1))
	out)

(defun weed-mesh (kind fine)
	; (weed-mesh kind fine) -> str
	;the mesh of a kind of weed, as the shaders want it; fine, 0 near to,
	;1 further off, 2 far off, fewer steps up each blade
	(defq picks (weed-picks (+ 4111 (* kind 7919)) 200) form (if (< kind +kelps) +kelp_form +grass_form)
		blades (+ (first form) (n2i (* (elem-get picks 0) (n2d (second form)))))
		form (map (const n2d) form) out (list) steps (elem-get +weed_steps fine))
	(each! (lambda () (weed-blade out picks (inc (* (!) 6)) form steps)) '() 0 blades)
	(shader-verts-str (apply (const cat) out)))

(defun weed-pick (pick h r)
	; (weed-pick pick h r) -> (kind bulk grass)
	;which kind of weed, how tall, and 0 kelp or 1 grass, of two numbers
	;from 0 to 1 and a hash: as much grass as kelp
	(if (< pick +double_1/2)
		(list (% h +kelps) (+ +kelp_least (* +kelp_more r)) +double_0)
		(list (+ +kelps (% h +grasses)) (+ +grass_least (* +grass_more r)) +double_1)))

(defun weed-lie (n)
	; (weed-lie n) -> doubles
	;the way up of a clump growing on a floor of up n, near upright
	(defq u (nums-add (doubles +double_0 +double_1 +double_0) (nums-scale n +weed_tilt)))
	(nums-scale u (recip (sqrt (nums-dot u u)))))

;module
(export-symbols '(weed-mesh weed-pick weed-lie +weed_kinds))
(env-pop)

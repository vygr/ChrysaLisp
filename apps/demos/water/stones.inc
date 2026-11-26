;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water's stones, the shapes the reef is strewn with, and the job a child of
; its farm does to make one. Each is a field in the cube -1 to 1, less than
; 0 inside, made a mesh by (Mesh-iso) at 0 and lit smooth, as Space's rocks
; are, a few of each kind made the once, at two sizes, and each stone on
; the floor one of them, turned and sized; chunk.inc says where they lie.
;
; Sandstone: layered, each bed of it set in or out a little from the next,
; ledges all round it; a slab of a whole bed, wide and flat, lying level,
; or a piece broken off one, blockier, lying as the floor does. Basalt:
; a ragged boulder, ridged and cut by planes meeting at sharp edges, pitted
; with the holes of the gas it cooled round. And basalt's columns: a cluster
; of six sided pillars, each its own height, their tops broken off askew.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/surface.inc")
(import "lib/math/mesh.inc")
(import "lib/math/scene.inc")
(import "lib/gpu/shader.inc")
(import "lib/gpu/vp.inc")

;module
(env-push)

;the kinds of stone: sandstone slabs, basalt boulders, basalt columns, how
;many of each, and of all; how fine each of the two meshes of a kind is,
;near to and further off
(defq +sandstones 6 +boulders 5 +columns 3 +stone_kinds (+ +sandstones +boulders +columns)
	+stone_sizes ''(22 16 9) +stone_fines 3
	;the sandstones that are slabs of a whole bed, the rest pieces broken off
	+slabs 3)

;how many of basalt are columns; how big
;each kind is at least and how much more at most; how far in each is sunk,
;of its size, a slab the more the steeper the floor, as it lies level and
;the sand has settled round it; and how far a broken piece leans
(defq +columns_share (n2d 0.15) +column_least (n2d 1.8) +column_bulk (n2d 2.5) +boulder_least (n2d 0.25)
	+boulder_bulk (n2d 1.8) +slab_least (n2d 2.5) +slab_bulk (n2d 4.5) +piece_least (n2d 0.4) +piece_bulk (n2d 1.8)
	+column_sink (n2d -0.8) +sink (n2d 0.1) +slab_sink (n2d 0.12) +slab_steep (n2d 0.8) +slab_lean (n2d 0.025) +slab_rise (n2d 0.1) +piece_sink (n2d 0.2)
	+piece_lean (n2d 0.12))

;the numbers of the shapes: the range of (stone-rnd); a sandstone's
;wear, how much its outline wavers and how big the waves, how big the
;patches its top is eaten down in, from where, how many steps and how deep
;each, and its grit; a slab's and a piece's form, each its least width and
;spread, least depth and spread, least height and spread, how many beds at
;least and their spread, how far a bed is set in or out at most, how
;square it is, how far in its cuts are and their spread, and how steep
;they may be from upright, 1 any way; a boulder's lumps, ridges, cuts and pits; a cluster's
;columns, how far apart, how thick, how high at most and least, and how
;askew their tops, which of those at the rim are kept, how much the tops
;rise to the high side and how much each is its own, and how near a column
;must be to count
(defq +rnd_range (n2d 8388608) +slab_wear (n2d 0.05) +wear_scale +double_4 +outline_waver (n2d 0.35) +waver_scale (n2d 1.8)
	+eaten_scale (n2d 2.3) +eaten_from (n2d 0.42) +eaten_steps (n2d 9) +eaten_step (n2d 0.1) +slab_grit (n2d 0.025) +grit_scale (n2d 9)
	+lumps_mid (n2d 0.6) +lumps_big (n2d 1.7) +lumps_small (n2d 4.3) +ridge_scale (n2d 3.1) +ridge_height (n2d 0.12)
	+lumps_spread (n2d 50) +boulder_cut (n2d 0.38) +boulder_cut_spread (n2d 0.25) +pits 7 +pit_out (n2d 0.62)
	+pit_least (n2d 0.05) +pit_spread (n2d 0.09)
	+squash_least (n2d 0.75) +squash_spread +double_1/4 +squash_low (n2d 0.55) +squash_low_spread (n2d 0.3)
	+column_apart (n2d 0.28) +column_thick (n2d 0.132) +column_high (n2d 0.95) +column_low (n2d -0.1) +column_askew (n2d 0.3)
	+column_kept (n2d 0.35) +hex_across (n2d 0.866) +column_rise (n2d 0.8) +column_jitter (n2d 0.5) +column_reach (n2d 0.4) +tiny (/ +double_1 (n2d 1000000)))

(defq +slab_form ''(0.85 0.15 0.6 0.4 0.25 0.15 8 8 0.04 2.8 0.7 0.25 0.3)
	+piece_form ''(0.6 0.3 0.5 0.35 0.3 0.3 3 4 0.06 3.0 0.35 0.3 0.35))

(defun stone-rnd (state)
	; (stone-rnd state) -> (number state)
	;a number from 0 to 1 of a state, and the next: the same of the same
	;seed, every time and on every node
	(setq state (logand (+ (* state 1103515245) 12345) 0x7fffffff))
	(list (/ (n2d (>> state 8)) +rnd_range) state))

(defun stone-picks (seed n)
	; (stone-picks seed n) -> (number ...)
	(defq state seed)
	(map! (lambda () (bind '(r s) (stone-rnd state)) (setq state s) r) '() 0 n))

(defun way-of (a b)
	; (way-of a b) -> (x y z)
	;a way out of two numbers from 0 to 1, of length 1
	(defq a (* a +double_2pi) b (- (* b +double_2) +double_1) c (sqrt (- +double_1 (* b b))))
	(list (* c (cos a)) b (* c (sin a))))

(defclass Iso-sandstone (size picks broken) (Iso size size size)
	; (Iso-sandstone size picks broken) -> iso
	;sandstone of its picks, numbers from 0 to 1: a slab of a whole bed, wide
	;and flat, of many thin beds, only its edges broken; or broken, a piece
	;broken off one, blockier, of fewer beds, cut more
	(defq k (/ +double_2 (n2d size)) c (* (n2d (dec size)) +double_1/2)
		form (map (const n2d) (if broken +piece_form +slab_form))
		beds (n2i (+ (elem-get form 6) (* (elem-get picks 3) (elem-get form 7)))))
	(bind '(wide wide_spread deep deep_spread high high_spread & & bed_in square cut cut_spread steep) form)
	(def this :points (map! (# (* (- (n2d (!)) c) k)) '() 0 size)
		:wide (+ wide (* (elem-get picks 0) wide_spread))
		:deep (+ deep (* (elem-get picks 1) deep_spread))
		:high (+ high (* (elem-get picks 2) high_spread))
		:beds beds :square square
		;each bed set in or out of the stone a little
		:insets (map! (# (* bed_in (- (* +double_2 (elem-get picks (+ 28 (% (!) 16)))) +double_1))) '() 0 beds)
		;broken off by planes, near upright, joints, so it breaks along its
		;beds and keeps a flat top and bottom, a piece more than a slab
		:cuts (map (lambda (at)
				(defq a (* (elem-get picks at) +double_2pi) b (* steep (- (* +double_2 (elem-get picks (inc at))) +double_1))
					across (sqrt (- +double_1 (* b b))))
				(list (* (cos a) across) b (* (sin a) across) (+ cut (* (elem-get picks (+ at 2)) cut_spread))))
			'(4 7 10 13 16 19))
		:noise (shader-vp-func (shader-load "lib/gpu/shaders/noise.shader") 'noise3)
		:seed (doubles (* (elem-get picks 26) +lumps_spread) (* (elem-get picks 27) +lumps_spread) +double_0))

	(defmethod :get_scalar (x y z)
		; (. iso :get_scalar x y z) -> scalar
		(raise :points :wide :deep :high :beds :square :insets :cuts :noise :seed)
		(defq px (elem-get points x) py (elem-get points y) pz (elem-get points z)
			;which bed, from the bottom, and how far it is set in
			bed (max 0 (min (dec beds) (n2i (* (/ (+ py high) (* +double_2 high)) (n2d beds)))))
			inset (elem-get insets bed)
			qx (/ px (+ wide inset)) qz (/ pz (+ deep inset)) qy (/ py high)
			;a rounded box, squarer across than up, its outline wavering
			across (* (pow (+ (pow (abs qx) square) (pow (abs qz) square)) (recip square))
				(+ +double_1 (* +outline_waver (- (noise (nums-add (doubles (* px +waver_scale) +double_0 (* pz +waver_scale)) seed))
					+double_1/2))))
			d (- (max across (abs qy)) +double_1))
		(each (lambda ((nx ny nz cd))
				(setq d (max d (- (+ (* px nx) (* py ny) (* pz nz)) cd))))
			cuts)
		;its top eaten down in patches, a step at a time, so the beds under
		;show through it, ledges round each
		(defq eaten (n2i (* (max +double_0 (- (noise (nums-add (doubles (* px +eaten_scale) +double_0 (* pz +eaten_scale)) seed))
				+eaten_from)) +eaten_steps))
			d (max d (- py (- high (* (n2d eaten) +eaten_step)))))
		;worn, and rough at two scales, so it is ragged even far off
		(+ d (* +slab_wear (noise (nums-add (nums-scale (doubles px py pz) +wear_scale) seed)))
			(* +slab_grit (noise (nums-add (nums-scale (doubles px py pz) +grit_scale) seed))))))

(defclass Iso-boulder (size picks) (Iso size size size)
	; (Iso-boulder size picks) -> iso
	;a ragged boulder of basalt of its picks
	(defq k (/ +double_2 (n2d size)) c (* (n2d (dec size)) +double_1/2))
	(def this :points (map! (# (* (- (n2d (!)) c) k)) '() 0 size)
		:squash (list (+ +squash_least (* (elem-get picks 0) +squash_spread)) (+ +squash_low (* (elem-get picks 1) +squash_low_spread))
			(+ +squash_least (* (elem-get picks 2) +squash_spread)))
		:seed (doubles (* (elem-get picks 3) +lumps_spread) (* (elem-get picks 4) +lumps_spread) (* (elem-get picks 5) +lumps_spread))
		:cuts (map (lambda (at)
				(bind '(nx ny nz) (way-of (elem-get picks at) (elem-get picks (inc at))))
				(list nx ny nz (+ +boulder_cut (* (elem-get picks (+ at 2)) +boulder_cut_spread))))
			'(6 9 12 15 18))
		:pits (map! (lambda ()
				(bind '(nx ny nz) (way-of (elem-get picks (+ 21 (* (!) 2))) (elem-get picks (+ 22 (* (!) 2)))))
				(list (* nx +pit_out) (* ny +pit_out) (* nz +pit_out)
					(+ +pit_least (* (elem-get picks (+ 36 (!))) +pit_spread))))
			'() 0 +pits)
		:noise (shader-vp-func (shader-load "lib/gpu/shaders/noise.shader") 'noise3))

	(defmethod :get_scalar (x y z)
		; (. iso :get_scalar x y z) -> scalar
		(raise :points :squash :seed :cuts :pits :noise)
		(bind '(sx sy sz) squash)
		(defq px (elem-get points x) py (elem-get points y) pz (elem-get points z)
			qx (/ px sx) qy (/ py sy) qz (/ pz sz)
			r (sqrt (+ (* qx qx) (* qy qy) (* qz qz) +tiny))
			way (nums-add (doubles (/ qx r) (/ qy r) (/ qz r)) seed)
			ridge (- +double_1 (abs (- (* +double_2 (noise (nums-scale way +ridge_scale))) +double_1)))
			lumps (+ +lumps_mid (* +double_1/4 (noise (nums-scale way +lumps_big)))
				(* +double_1/10 (noise (nums-scale way +lumps_small))) (* +ridge_height ridge ridge))
			d (- r lumps))
		(each (lambda ((nx ny nz cd))
				(setq d (max d (- (+ (* px nx) (* py ny) (* pz nz)) cd))))
			cuts)
		(each (lambda ((bx by bz br))
				(defq ex (- px bx) ey (- py by) ez (- pz bz))
				(setq d (max d (- br (sqrt (+ (* ex ex) (* ey ey) (* ez ez)))))))
			pits)
		d))

(defclass Iso-columns (size picks) (Iso size size size)
	; (Iso-columns size picks) -> iso
	;a cluster of basalt columns of its picks, as the lava cracked cooling:
	;six sided, upright along y, packed edge to edge on a hexagonal lattice
	;two round one in the middle, a thin crack between, some at the rim left
	;out; their tops stepping down from a high side, each broken off askew
	(defq k (/ +double_2 (n2d size)) c (* (n2d (dec size)) +double_1/2)
		high_way (* (elem-get picks 0) +double_2pi) hx (cos high_way) hz (sin high_way))
	(def this :points (map! (# (* (- (n2d (!)) c) k)) '() 0 size)
		:columns (reduce (lambda (out (q r))
				(defq i (length out) at (+ 1 (* i 4))
					cx (* +column_apart q +hex_across) cz (* +column_apart (+ r (* q +double_1/2))))
				;the middle and the ring round it, and some of the rim
				(if (or (< (+ (abs q) (abs r) (abs (+ q r))) +double_4) (> (elem-get picks at) +column_kept))
					(push out (list cx cz
						;its top, higher to the high side, and how askew
						(- (+ +column_low (* (- +column_high +column_low)
							(max +double_0 (min +double_1 (+ +double_1/2 (* (+ (* cx hx) (* cz hz)) +column_rise)
								(* (- (elem-get picks (+ at 1)) +double_1/2) +column_jitter))))))
							+double_1)
						(* +column_askew (- (elem-get picks (+ at 2)) +double_1/2))
						(* +column_askew (- (elem-get picks (+ at 3)) +double_1/2))))
					out))
			(map (lambda ((q r)) (list (n2d q) (n2d r)))
				'((0 0) (1 0) (-1 0) (0 1) (0 -1) (1 -1) (-1 1)
				(2 0) (-2 0) (0 2) (0 -2) (2 -2) (-2 2) (2 -1) (-2 1) (1 1) (-1 -1) (1 -2) (-1 2)))
			(list)))

	(defmethod :get_scalar (x y z)
		; (. iso :get_scalar x y z) -> scalar
		(raise :points :columns)
		(defq px (elem-get points x) py (elem-get points y) pz (elem-get points z) d +double_1)
		(each (lambda ((cx cz top tx tz))
				(defq qx (abs (- px cx)) qz (abs (- pz cz)))
				;only the columns near enough to count
				(when (< (+ qx qz) +column_reach)
					(defq hex (- (max (+ (* qx +hex_across) (* qz +double_1/2)) qz) +column_thick)
						up (max (- +double_-1 py) (- py (+ top (* tx (- px cx)) (* tz (- pz cz))))))
					(setq d (min d (max hex up)))))
			columns)
		d))

(defun stone-slab (h r)
	; (stone-slab h r) -> (kind bulk)
	;which slab of a whole bed and how big, of a hash and a number 0 to 1
	(list (% h +slabs) (+ +slab_least (* +slab_bulk r))))

(defun stone-pick (basalt pick h r)
	; (stone-pick basalt pick h r) -> (kind bulk)
	;which kind of stone and how big, of its rock, two numbers from 0 to 1
	;and a hash: a piece of sandstone, or basalt, some of it columns; mostly
	;small
	(cond
		((not basalt) (list (+ +slabs (% h (- +sandstones +slabs))) (+ +piece_least (* +piece_bulk r r))))
		((< pick +columns_share) (list (+ +sandstones +boulders (% h +columns)) (+ +column_least (* +column_bulk r r))))
		((list (+ +sandstones (% h +boulders)) (+ +boulder_least (* +boulder_bulk r r r))))))

(defun stone-lie (kind n lx lz)
	; (stone-lie kind n lx lz) -> (up sink)
	;how a kind of stone lies on a floor of up n, lx and lz from -1 to 1:
	;columns stand up, boulders lie as the floor does, a whole slab lies
	;near level, a little higher or lower, so that where slabs overlap they
	;are one bed bent and broken, the sand settled round it, deeper the
	;steeper the floor, and a
	;broken piece lies in line with the floor, a little off it; and how
	;far in it is sunk, of its size
	(cond
		((>= kind (+ +sandstones +boulders)) (list (doubles +double_0 +double_1 +double_0) +column_sink))
		((>= kind +sandstones) (list n +sink))
		((< kind +slabs)
			(defq u (doubles (* lx +slab_lean) +double_1 (* lz +slab_lean)))
			(list (nums-scale u (recip (sqrt (nums-dot u u))))
				(+ +slab_sink (* +slab_rise lx) (* +slab_steep (- +double_1 (second n))))))
		((defq u (nums-add n (doubles (* lx +piece_lean) +double_0 (* lz +piece_lean))))
			(list (nums-scale u (recip (sqrt (nums-dot u u)))) +piece_sink))))

(defun stone-mesh (kind fine)
	; (stone-mesh kind fine) -> str
	;the mesh of a kind of stone, as the shaders want it, a vertex where it
	;is and its normal, lit smooth; fine, 0 near to, 1 further off, 2 far off
	(defq picks (stone-picks (+ 7919 (* kind 104729)) 80) size (elem-get +stone_sizes fine))
	(mesh-corners-smooth (Mesh-iso (cond
		((< kind +sandstones) (Iso-sandstone size picks (>= kind +slabs)))
		((< kind (+ +sandstones +boulders)) (Iso-boulder size picks))
		((Iso-columns (* size 2) picks))) +double_0)))

;module
(export-symbols '(stone-mesh stone-slab stone-pick stone-lie +stone_kinds +stone_fines))
(export-classes '(Iso-sandstone Iso-boulder Iso-columns))
(env-pop)

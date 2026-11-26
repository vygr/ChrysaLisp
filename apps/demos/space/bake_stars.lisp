;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space's stars, made once and kept: data/stars.cpm, a map of all of the
; sky as the nebula's but of the size they are seen at, 11 pixels a degree,
; black but for them, which the sky shader adds over the nebula. At random,
; so a bake of it is new stars:
;
;	lisp -r (import "apps/demos/space/bake_stars.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/lisp.inc")
(import "lib/image/cpm.inc")
(import "apps/demos/space/sky.inc")


;the stars, a map of all of the sky as the nebula's but of the size they
;are seen at, 11 pixels a degree, black but for them: so they are sharp,
;and it packs small. Each is drawn where a way at random is, as many to an
;area of sky anywhere, and stretched across as much as the map stretches
;the sky there, so it is round on the sky; one near the map's edge is
;drawn on the other edge too, where the sky goes on
(defq +stars_w 4096 +stars_h 2048 +stars_out "apps/demos/space/data/stars.cpm"
	+stars_faint 26000 +stars_middling 1600 +stars_bright 170)

;the steps of (rnd); the most of a colour's channel; how wide the
;galaxy's band is, and how many stars off it and in it; a faint star's
;least size and its brightness; a middling one's glow, how bright and its
;size, and the star, how bright, the least and the spread, and its size;
;a bright one's spikes, how long, the least and the spread, of the least
;and the spread of a part, its halo, glow, ring and core, the least of its
;ring, its spikes' width and their brightness
(defq +rnd_steps (n2d 1000000) +channel_most (n2d 255) +band_wide (n2d 0.3) +off_band (n2d 0.45)
	+in_band (n2d 0.55) +faint_least (n2d 0.4) +faint_bright (n2d 0.75) +middling_glow (n2d 0.12)
	+middling_glow_size (n2d 2.6) +middling_least (n2d 0.6) +middling_spread (n2d 0.4)
	+middling_size (n2d 1.1) +spike_least +double_8 +spike_spread (n2d 34)
	+spike_least_part (n2d 0.35) +spike_spread_part (n2d 0.65) +bright_halo +double_9
	+bright_glow +double_5 +bright_ring (n2d 2.6) +bright_ring_least (n2d 0.6)
	+bright_core (n2d 1.4) +spike_wide (n2d 0.7) +spike_bright (n2d 0.45))

(defun rnd ()
	; (rnd) -> double
	;0 to 1
	(/ (n2d (random 1000000)) +rnd_steps))


;the corners of an eight sided ellipse of radius 1, sine and cosine
(defq star_corners (map (lambda (i) (defq a (/ (* +double_2pi (n2d i)) +double_8)) (list (sin a) (cos a)))
	'(0 1 2 3 4 5 6 7)))

(defun star-shape (cx cy rx ry)
	; (star-shape cx cy rx ry) -> path
	;an ellipse of eight sides, enough for a dot a pixel or two across
	(defq out (path))
	(each (lambda ((s c)) (push out (n2q (+ cx (* rx s))) (n2q (+ cy (* ry c))))) star_corners)
	out)

(defun star-argb (rgb a)
	; (star-argb (r g b) a) -> argb
	(+ (<< (max 0 (min 255 (n2i (* a +channel_most)))) 24)
		(reduce (lambda (v c) (+ (<< v 8) (max 0 (min 255 (n2i (* c +channel_most)))))) rgb 0)))

(defun star-mix (a b t)
	(map (lambda (x y) (+ x (* t (- y x)))) a b))

(defun star-draw (fn x reach)
	; (star-draw fn x reach)
	;fn draws a star at x across the map, and again a map's width over if it
	;reaches past an edge
	(fn x)
	(if (< (- x reach) +double_0) (fn (+ x (n2d +stars_w))))
	(if (> (+ x reach) (n2d +stars_w)) (fn (- x (n2d +stars_w)))))

(defun star-at (pu pv bright_band)
	; (star-at pu pv bright_band) -> (x y stretch way lon lat)
	;a way at random, as many to an area of sky anywhere, more of them in
	;the galaxy's band if bright_band; where it is on the map, pu and pv its
	;pixels to a radian, and how much the map stretches the sky across there
	(defq found :nil)
	(until found
		(defq y (- (* +double_2 (rnd)) +double_1) cl (sqrt (- +double_1 (* y y)))
			lon (* +double_2pi (- (rnd) +double_1/2))
			way (doubles (* cl (sin lon)) y (* (neg cl) (cos lon)))
			band (- +double_1 (min +double_1 (/ (abs (nums-dot way band_pole)) +band_wide))))
		(if (or (not bright_band) (< (rnd) (+ +off_band (* +in_band band))))
			(setq found (list (n2d (path-angle (n2q cl) (n2q y))) lon way cl))))
	(bind '(lat lon way cl) found)
	(list (* pu (+ lon +double_pi)) (* pv (- +double_hpi lat))
		(/ +double_1 (max cl +double_1/20)) way lon lat))

(defun bake-stars ()
	; (bake-stars) -> :t
	(defq canvas (Canvas +stars_w +stars_h 1)
		pu (/ (n2d +stars_w) +double_2pi) pv (/ (n2d +stars_h) +double_pi)
		diagonal (sky-unit (nums-add pillars_across pillars_up)))
	(.-> canvas (:set_canvas_flags +canvas_flag_antialias) (:fill 0xff000000))
	;the faint, many, a pixel or so, blue white to warm
	(times +stars_faint
		(bind '(x y stretch &ignore) (star-at pu pv :t))
		(defq k (rnd) r (+ +faint_least (* +double_1/2 k))
			colour (star-mix (const `'(,(n2d 0.72) ,(n2d 0.82) ,(n2d 1.0))) (const `'(,+double_1 ,(n2d 0.86) ,(n2d 0.68))) (rnd)))
		(. canvas :set_color (star-argb colour (+ +double_1/4 (* +faint_bright k k))))
		(star-draw (lambda (x) (. canvas :fpoly 0.0 0.0 +winding_none_zero (list (star-shape x y (* r stretch) r))))
			x (* r stretch)))
	;the middling, a little bigger and a glow round them
	(times +stars_middling
		(bind '(x y stretch &ignore) (star-at pu pv :t))
		(defq k (rnd) colour (star-mix (const `'(,(n2d 0.8) ,(n2d 0.87) ,+double_1)) (const `'(,+double_1 ,(n2d 0.84) ,(n2d 0.66))) (rnd)))
		(star-draw (lambda (x)
				(. canvas :set_color (star-argb colour +middling_glow))
				(. canvas :fpoly 0.0 0.0 +winding_none_zero (list (star-shape x y (* +middling_glow_size stretch) +middling_glow_size)))
				(. canvas :set_color (star-argb colour (+ +middling_least (* +middling_spread k))))
				(. canvas :fpoly 0.0 0.0 +winding_none_zero (list (star-shape x y (* +middling_size stretch) +middling_size))))
			x (* +middling_glow_size stretch)))
	;the bright, Hubble's: a core, rings of glow, and four spikes that thin
	;to nothing, on the Pillars' diagonals laid on the sky where each is,
	;of many lengths
	(times +stars_bright
		(bind '(x y stretch way lon lat) (star-at pu pv :nil))
		(defq k (rnd) colour (star-mix (const `'(,+double_1 ,(n2d 0.93) ,(n2d 0.82))) (const `'(,+double_1 ,(n2d 0.72) ,(n2d 0.55))) (rnd))
			long (* (+ +spike_least (* +spike_spread k)) (+ +spike_least_part (* +spike_spread_part (rnd))))
			t1 (sky-unit (nums-sub diagonal (nums-scale way (nums-dot diagonal way))))
			t2 (sky-cross way t1)
			east (doubles (cos lon) +double_0 (sin lon))
			north (doubles (* (neg (sin lat)) (sin lon)) (cos lat) (* (sin lat) (cos lon)))
			;each spike's way across and down the map, a pixel of sky long
			spikes (map (lambda (t) (list (* (nums-dot t east) stretch) (neg (nums-dot t north))))
				(list t1 t2 (nums-scale t1 +double_-1) (nums-scale t2 +double_-1))))
		(star-draw (lambda (x)
				(each (lambda ((r a))
						(. canvas :set_color (star-argb colour a))
						(. canvas :fpoly 0.0 0.0 +winding_none_zero (list (star-shape x y (* r stretch) r))))
					(list (list (* +bright_halo (+ +double_1/2 k)) +double_1/20)
						(list (* +bright_glow (+ +double_1/2 k)) +double_1/10)
						(list (* +bright_ring (+ +bright_ring_least k)) +double_1/4)
						(list +bright_core +double_1)))
				(each (lambda ((dx dy))
						(defq tx (+ x (* dx long)) ty (+ y (* dy long))
							wide +spike_wide nx (* (neg dy) wide) ny (* dx wide (/ +double_1 stretch)))
						(. canvas :set_linear (n2q x) (n2q y) (n2q tx) (n2q ty)
							(list (list 0.0 (star-argb colour (+ +spike_bright (* +double_1/2 k))))
								(list 1.0 (star-argb colour +double_0))))
						(. canvas :fpoly 0.0 0.0 +winding_none_zero (list (path (n2q (+ x nx)) (n2q (+ y ny))
							(n2q tx) (n2q ty) (n2q (- x nx)) (n2q (- y ny))))))
					spikes))
			x (* long stretch)))
	(defq stream (file-stream +stars_out +file_open_write))
	(CPM-save canvas stream 24 :nil :t)
	(stream-flush stream)
	:t)

(time-it "space stars baked" (bake-stars))

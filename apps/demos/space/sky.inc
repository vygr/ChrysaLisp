;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Where things are on Space's sky, for the app and for bake_nebula.lisp, which
; makes the nebula round the Pillars from these: the nebula is made for the
; picture where it is, so a move of it is a bake again.
;
; The sky is a ball round the eye, its directions those of the world: the
; Pillars in one way, a flat picture seen through as a window of the sky
; a distance of 1 away, so far across and up; and the sun in another;
; the galaxy's band round all of it, and a nebula on its far side.
; The nebula is a map of all of the ball, across it the way round, minus z
; in its middle and x a quarter of the way on, and down it from up to
; down, lib/gpu/shaders/sphere.shader.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/vector.inc")

(defun sky-unit (v)
	; (sky-unit v) -> doubles
	;the way of v, made of length 1
	(nums-scale v (/ +double_1 (sqrt (nums-dot v v)))))

(defun sky-way (x y z)
	; (sky-way x y z) -> doubles
	;a way, of numbers given, made of length 1
	(sky-unit (doubles (n2d x) (n2d y) (n2d z))))

(defun sky-cross (a b)
	; (sky-cross a b) -> doubles
	(bind '(ax ay az) a)
	(bind '(bx by bz) b)
	(doubles (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx))))

;the Pillars: the way to their middle, ahead of the eye at first, a little
;to the right and up; their across, level, and their up; and how far they
;reach, across and up, at a distance of 1, the tangents of half the angle
;they fill, 51 degrees up and Hubble's picture's shape across, 6780 by
;7071, whatever size the texture made of it is
(defq pillars_at (sky-way 0.25 0.12 -1.0)
	pillars_across (sky-unit (sky-cross pillars_at (doubles +double_0 +double_1 +double_0)))
	pillars_up (sky-cross pillars_across pillars_at)
	+pillars_tall (n2d 0.4770)
	pillars_size (doubles (* +pillars_tall (/ (n2d 6780) (n2d 7071))) +pillars_tall)
	;the sun, off to one side and behind, so it lights what is seen from
	;the side
	sun_at (sky-way 0.85 0.3 0.45)
	;the pole of the galaxy's band, which goes round all of the sky, and the
	;way to a nebula of the Pillars' colours on the far side of it from them
	band_pole (sky-way 0.30 0.85 0.43)
	far_at (sky-way -0.45 -0.22 0.86)
	;the light on the rocks: the sun's, warm white; the Pillars' nebula's,
	;from their way, its teal and blue, nearly half the sun's; and the far
	;nebula's, its amber, a fifth. A nebula is a great glow of the sky, not a
	;point: its light wraps round a rock further than the sun's
	sun_light (doubles (n2d 1.0) (n2d 0.94) (n2d 0.84))
	pillars_light (nums-scale (doubles (n2d 0.55) (n2d 0.85) (n2d 1.0)) (n2d 0.45))
	far_light (nums-scale (doubles (n2d 1.0) (n2d 0.75) (n2d 0.5)) (n2d 0.18)))

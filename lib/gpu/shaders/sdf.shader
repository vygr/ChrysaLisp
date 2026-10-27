;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Distance fields and noise, functions for a shader to import,
; (import "lib/gpu/shaders/sdf.shader"). A field gives how far a
; point is from the surface of a shape, less than 0 inside it, as a
; raymarch steps by. No inputs, no main, as any imported file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;a number from 0 to 1 that looks random, of a number
(defun hash :float ((n :float))
	(fract (* (sin n) 43758.5453)))

;three of them, of a point
(defun hash33 :vec3 ((p :vec3))
	(defq n (sin (dot p (vec3 7 157 113))))
	(fract (* (vec3 2097152 262144 32768) n)))

;value noise, smooth from 0 to 1 across space
(defun noise :float ((x :vec3))
	(defq p (floor x) f (fract x))
	(setq f (* f f (- 3.0 (* 2.0 f))))
	(defq n (+ (* (:x p) 7.0) (* (:y p) 57.0) (* 111.0 (:z p))))
	(mix
		(mix (mix (hash (+ n 0.0)) (hash (+ n 7.0)) (:x f))
			(mix (hash (+ n 57.0)) (hash (+ n 64.0)) (:x f)) (:y f))
		(mix (mix (hash (+ n 111.0)) (hash (+ n 118.0)) (:x f))
			(mix (hash (+ n 168.0)) (hash (+ n 175.0)) (:x f)) (:y f))
		(:z f)))

;field equation for a sphere
(defun sphere :float ((p :vec3) (center :vec3) (radius :float))
	(- (length (- p center)) radius))

;field equation for a cube
(defun box :float ((p :vec3) (b :vec3))
	(defq d (- (abs p) b))
	(+ (min (max (:x d) (max (:y d) (:z d))) 0.0) (length (max d 0.0))))

;field equation for a rounded cube
(defun rounded_cube :float ((p :vec3) (xt :vec3) (r :float))
	(- (length (max (+ (- (abs p) xt) (vec3 r)) 0.0)) r))

;smooth min between two values
(defun smin :float ((a :float) (b :float) (k :float))
	(defq h (clamp (+ 0.5 (/ (* 0.5 (- b a)) k)) 0.0 1.0))
	(- (mix b a h) (* k h (- 1.0 h))))

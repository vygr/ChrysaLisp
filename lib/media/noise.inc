;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; noise, the random numbers of the coded sound of films, AAC's noise
; bands and AC-3's dither, both of the same generator
;
; A seed is a number of 32 bits. (noise-next seed) the next, the seed
; held by the caller in a local, set in place; (noise-real seed) a real
; of -1 to 1 of one.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defmacro noise-next (seed)
	; (noise-next seed) -> seed
	`(setq ,seed (logand (+ (* ,seed 1664525) 1013904223) 0xffffffff)))

(defmacro noise-real (seed)
	; (noise-real seed) -> real
	`(- (/ (n2r ,seed) (const (n2r 0x80000000))) (const (n2r 1))))

;module
(export-symbols '(noise-next noise-real))
(env-pop)

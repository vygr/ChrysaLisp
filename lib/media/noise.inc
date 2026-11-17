;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; noise, the random numbers of the coded sound of films, AAC's noise
; bands and AC-3's dither, both of the same generator
;
; A seed is a number of 32 bits. (noise-next seed) the next, the seed
; held by the caller in a local, set in place; (noise-double seed) a double
; of -1 to 1 of one. (noise-fill doubles start end seed scale) -> seed,
; native, the doubles from start to end each the next less 2^31 by scale,
; so a scale of k/2^31 is noise of -k to k.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(ffi (path-to-absolute "./media_noise") noise-fill)
; (noise-fill doubles start end seed scale) -> seed

(defmacro noise-next (seed)
	; (noise-next seed) -> seed
	`(setq ,seed (logand (+ (* ,seed 1664525) 1013904223) 0xffffffff)))

(defmacro noise-double (seed)
	; (noise-double seed) -> double
	`(- (/ (n2d ,seed) (const (n2d 0x80000000))) (const (n2d 1))))

;module
(export-symbols '(noise-next noise-double noise-fill))
(env-pop)

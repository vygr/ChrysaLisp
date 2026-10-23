;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sound of 4 bits a sample, and PCM, as an AVI or a WAV has them
;
; Microsoft's ADPCM, 0x0002: a block begins, for each channel, with which
; pair of coefficients predicts it, its step, and its two samples before;
; each sample after is the prediction of the two before it and a nibble,
; high first, of the step, which grows and shrinks with them. IMA's, 0x0011:
; a block begins, for each channel, with its sample and the index of its
; step; then words of 8 samples a channel, a channel a word, low nibble
; first, each a step's worth more or less. And PCM of 8, 16 or 24 bits, low
; byte first. Each is made the channels as reals at the scale of 16 bits,
; the first two, the one of one on both, as lib/media/flac.inc's are.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./reel.inc")

;module
(env-push)

(defq +adpcm (Emap))
(scatter +adpcm
	:adapt (nums 230 230 230 230 307 409 512 614 768 614 512 409 307 230 230 230)
	:coefs (list 256 0 512 -256 0 0 192 64 240 0 460 -208 392 -232)
	:ima_index (nums -1 -1 -1 -1 2 4 6 8 -1 -1 -1 -1 2 4 6 8)
	:ima_steps (nums 7 8 9 10 11 12 13 14 16 17 19 21 23 25 28 31 34 37 41 45 50 55 60 66 73 80 88 97 107 118
		130 143 157 173 190 209 230 253 279 307 337 371 408 449 494 544 598 658 724 796 876 963 1060 1166 1282
		1411 1552 1707 1878 2066 2272 2499 2749 3024 3327 3660 4026 4428 4871 5358 5894 6484 7132 7845 8630 9493
		10442 11487 12635 13899 15289 16818 18500 20350 22385 24623 27086 29794 32767))


(defun adpcm-ms-coefs (extra)
	; (adpcm-ms-coefs extra) -> (coef1 coef2 ...)
	;the pairs of coefficients a format of Microsoft ADPCM says, after its
	;samples a block and how many pairs, or the seven every one has
	(defq n (if (>= (length extra) 4) (code extra 2 2) 0))
	(if (and (> n 0) (>= (length extra) (+ 4 (* n 4))))
		(map (# (get-short extra (+ 4 (* %0 2)))) (range 0 (* n 2)))
		(. +adpcm :find :coefs)))

(defun adpcm-ms-block (s channels coefs)
	; (adpcm-ms-block str channels coefs) -> (left right)
	;a block of Microsoft ADPCM
	(defq adapt (. +adpcm :find :adapt) at 0
		most (dec (/ (length coefs) 2))
		preds (map (# (min most (code s 1 (+ at %0)))) (range 0 channels)) at (+ at channels)
		deltas (map (# (get-short s (+ at (* 2 %0)))) (range 0 channels)) at (+ at (* 2 channels))
		s1 (map (# (get-short s (+ at (* 2 %0)))) (range 0 channels)) at (+ at (* 2 channels))
		s2 (map (# (get-short s (+ at (* 2 %0)))) (range 0 channels)) at (+ at (* 2 channels))
		chans (map (# (nums (elem-get s2 %0) (elem-get s1 %0))) (range 0 channels)) c 0)
	(defun nibble (n)
		(defq p (* 2 (elem-get preds c))
			;a signed shift, >>>, the prediction may be less than 0
			predict (>>> (+ (* (elem-get s1 c) (elem-get coefs p)) (* (elem-get s2 c) (elem-get coefs (inc p)))) 8)
			delta (elem-get deltas c)
			v (clamp (+ predict (* (if (>= n 8) (- n 16) n) delta)) -32768 32767))
		(elem-set deltas c (max 16 (>> (* (elem-get adapt n) delta) 8)))
		(elem-set s2 c (elem-get s1 c))
		(elem-set s1 c v)
		(push (elem-get chans c) v)
		(setq c (if (= (inc c) channels) 0 (inc c))))
	(while (< at (length s))
		(defq b (code s 1 at))
		(nibble (>> b 4))
		(nibble (logand b 15))
		(setq at (inc at)))
	(reel-pcm chans))

(defun adpcm-ima-block (s channels)
	; (adpcm-ima-block str channels) -> (left right)
	;a block of IMA ADPCM, as WAV has it
	(defq steps (. +adpcm :find :ima_steps) index_table (. +adpcm :find :ima_index)
		samples (map (# (get-short s (* 4 %0))) (range 0 channels))
		indexes (map (# (clamp (code s 1 (+ 2 (* 4 %0))) 0 88)) (range 0 channels))
		chans (map (# (nums (elem-get samples %0))) (range 0 channels)) at (* 4 channels))
	(defun nibble (c n)
		(defq step (elem-get steps (elem-get indexes c)) diff (>> step 3))
		(if (/= 0 (logand n 4)) (setq diff (+ diff step)))
		(if (/= 0 (logand n 2)) (setq diff (+ diff (>> step 1))))
		(if (/= 0 (logand n 1)) (setq diff (+ diff (>> step 2))))
		(defq v (clamp (if (/= 0 (logand n 8)) (- (elem-get samples c) diff) (+ (elem-get samples c) diff)) -32768 32767))
		(elem-set samples c v)
		(elem-set indexes c (clamp (+ (elem-get indexes c) (elem-get index_table n)) 0 88))
		(push (elem-get chans c) v))
	;a word a channel, 8 samples, low nibble first
	(while (<= (+ at (* 4 channels)) (length s))
		(each (lambda (c)
				(each (lambda (i)
						(defq b (code s 1 (+ at (* 4 c) i)))
						(nibble c (logand b 15))
						(nibble c (>> b 4)))
					(range 0 4)))
			(range 0 channels))
		(setq at (+ at (* 4 channels))))
	(reel-pcm chans))

(defun adpcm-pcm (s channels bits)
	; (adpcm-pcm str channels bits) -> (left right)
	;PCM of 8 bits, offset by 128, or of 16 or 24 signed, low byte first,
	;channels one after another in each frame
	(defq width (/ bits 8) frames (/ (length s) (* width channels)))
	;the native taken as it is used, as (reel-pcm) does
	(reel-pcm ((ffi "lib/media/media_pcm") (map (lambda (_) (fill (nums) frames 0)) (range 0 channels)) s width)))

;module
(export-symbols '(adpcm-ms-coefs adpcm-ms-block adpcm-ima-block adpcm-pcm))
(env-pop)

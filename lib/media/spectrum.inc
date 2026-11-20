;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the spectrum of frames of 16 bit stereo, as bars: an FFT of n of them by
; media_spectrum, lib/media/lisp_tracker.vp, its frequencies gathered into
; bands, each the same part of an octave, and each band's power in decibels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi "lib/media/media_spectrum" media-spectrum)

(defun spectrum-make (n rate bands &optional low)
	; (spectrum-make n rate bands [low]) -> spectrum
	;what the FFT of n frames needs, made once: the twiddles, the bit
	;reversal and a Hann window, and the bins of each of so many bands from
	;low Hz, 40 if not said, to half the rate
	(defq bits 0 two_pi (const (str-to-double "6.283185307179586"))
		hz (/ (n2d rate) (n2d n)) lo (n2d (ifn low 40)) hi (/ (n2d rate) (const (n2d 2)))
		ratio (/ (log (/ hi lo)) (n2d bands)) edges (list))
	(while (< (<< 1 bits) n) (setq bits (inc bits)))
	;each band's first bin, the last the end
	(each! (# (push edges (max 1 (min (/ n 2) (n2i (/ (* lo (exp (* ratio (n2d (!))))) hz))))))
		'() 0 (inc bands))
	(scatter (Emap) :n n :bands bands
		:x (fill (doubles) (* 2 n) (const (n2d 0)))
		:pow (fill (doubles) (/ n 2) (const (n2d 0)))
		:tw (apply (const doubles) (apply (const cat) (map! (lambda ()
				(defq a (/ (* two_pi (n2d (!))) (n2d n)))
				(list (cos a) (neg (sin a)))) '() 0 (/ n 2))))
		:rev (apply (const nums) (map! (lambda ()
				(defq k (!) r 0)
				(times bits (setq r (logior (<< r 1) (logand k 1)) k (>> k 1)))
				r) '() 0 n))
		:win (apply (const doubles) (map! (# (* (const (n2d 0.5))
				(- (const (n2d 1)) (cos (/ (* two_pi (n2d (!))) (n2d (dec n))))))) '() 0 n))
		:edges edges))

(defun spectrum-bands (spectrum pcm from)
	; (spectrum-bands spectrum pcm from) -> (decibels ...)
	;the power of each band of n frames of pcm from frame from, in decibels,
	;the loudest a full scale sine wave about 90, nothing 0
	(defq pow (media-spectrum pcm from (. spectrum :find :x) (. spectrum :find :tw)
			(. spectrum :find :rev) (. spectrum :find :win) (. spectrum :find :pow))
		edges (. spectrum :find :edges) ten (const (n2d 10)) tenth (/ (const (n2d 1)) (log (const (n2d 10)))))
	(map! (lambda ()
			(defq lo (elem-get edges (!)) hi (max (inc lo) (elem-get edges (inc (!)))) sum (const (n2d 1)))
			(each! (# (setq sum (+ sum %0))) (list pow) lo hi)
			(max (const (n2d 0)) (- (* ten (log sum) tenth) (const (n2d 60)))))
		'() 0 (. spectrum :find :bands)))

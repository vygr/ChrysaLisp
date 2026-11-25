;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space's crunches at the loudness a hit is heard at, made of the crunches
; there are: each near one at 60% and 35%, crunch_1_60.wav and the like, and
; the far one at 50% down to 10%, so the app picks one as loud as a hit is
; for how hard it was and how far off, and the mixer pans it. Run it after
; bake_crunch.lisp; it changes no sound, only how loud:
;
;	lisp -r (import "apps/demos/space/bake_levels.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/audio/wav.inc")

;the crunches and the levels of each, percent
(defq +levels_dir "apps/demos/space/data/"
	levels_of (list (list "crunch_1" '(60 35)) (list "crunch_2" '(60 35)) (list "crunch_3" '(60 35))
		(list "crunch_far" '(50 30 18 10))))

(defun level-save (name level)
	; (level-save name level) -> :nil | :t
	;the crunch of that name read, its 16 bit samples made level percent as
	;loud, and written beside it as name_level.wav
	(when (defq in (file-stream (cat +levels_dir name ".wav")))
		(bind '(rate channels bits bytes) (WAV-info in))
		(when (and (= channels 1) (= bits 16) (defq data (read-blk in bytes)))
			;the samples, read as their bits, made signed again and scaled
			(defq pcm (reduce (lambda (out v)
						(push out (/ (* (if (> v 32767) (- v 65536) v) level) 100)))
					(push (shorts) data) (shorts))
				count (* (length pcm) 2) out (file-stream (cat +levels_dir name "_" (str level) ".wav") +file_open_write))
			(WAV-head out rate)
			(write-blk out (obj-set (str-alloc count) +str_data count pcm))
			(WAV-tail out rate count)
			:t)))

(time-it "space crunch levels baked"
	(each (lambda ((name levels)) (each (lambda (level) (level-save name level)) levels)) levels_of))

;;;;;;;;;;;;
; wav format
;;;;;;;;;;;;

;A .wav is a head of chunks, RIFF, WAVE, fmt and data, then the samples
;as they are. This reads the 16 bit PCM ones, any rate, mono or stereo, and
;writes 16 bit mono. The head of one being written is put first with no
;length, and the length put in once the sound has ended

(import "./sound.inc")

;module
(env-push)

(defun wav-le (data at bytes)
	(code data bytes at))

(defun WAV-info (stream)
	; (WAV-info stream) -> (rate channels bits bytes) | (-1 -1 -1 -1)
	;the stream is left at the first sample
	(defq out '(-1 -1 -1 -1))
	(when (and stream (defq riff (read-blk stream 12)) (= (length riff) 12)
			(eql (slice riff 0 4) "RIFF") (eql (slice riff 8 12) "WAVE"))
		(defq fmt :nil done :nil)
		;the chunks till data, fmt among them, the rest stepped over
		(while (and (not done) (defq head (read-blk stream 8)) (= (length head) 8))
			(defq id (slice head 0 4) size (wav-le head 4 4))
			(cond
				((eql id "fmt ")
					(defq body (read-blk stream size))
					(if (= (length body) size) (setq fmt body) (setq done :t)))
				((eql id "data")
					(and fmt (= (wav-le fmt 0 2) 1)
						(setq out (list (wav-le fmt 4 4) (wav-le fmt 2 2) (wav-le fmt 14 2) size)))
					(setq done :t))
				((read-blk stream (+ size (logand size 1)))))))
	out)

(defun wav-head-text (rate bytes)
	(cat "RIFF" (char (+ 36 bytes) 4) "WAVE"
		"fmt " (char 16 4) (char 1 2) (char 1 2) (char rate 4)
		(char (* rate 2) 4) (char 2 2) (char 16 2)
		"data" (char bytes 4)))

(defun WAV-head (stream rate)
	; (WAV-head stream rate) -> stream
	;the head of a 16 bit mono file whose length is not known yet
	(write-blk stream (wav-head-text rate 0)))

(defun WAV-tail (stream rate bytes)
	; (WAV-tail stream rate bytes) -> stream
	;the length put in the head, once it is known
	(stream-seek stream 0 0)
	(write-blk stream (wav-head-text rate bytes))
	(stream-flush stream))

;the sound is told what this reads
(sound-loader ".wav" WAV-info WAV-head WAV-tail)

;module
(export-symbols '(WAV-info WAV-head WAV-tail))
(env-pop)

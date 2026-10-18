;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/audio/sound.inc - sound, the formats it is kept in, and its rate
;
; Sound goes through a pipeline as a stream of its samples, as an image
; goes through its stages, lib/streams/stages.inc, a task a stage, so no
; whole sound is held: what the microphone hears, through a filter, to a
; file; a file, through another, to the speaker. Each stage is given the
; rate as it is made. Today it is +sound_rate, 16 bit mono, the rate of a
; voice and of the microphone, service/audio/app.inc.
;
; A format is a module of lib/audio/, as the formats of an image are of
; lib/image/, that says what it is for as it is read, (sound-loader):
; (info stream) reads the head of a file, the stream left at its first
; sample, (head stream rate) writes one for a file whose length is not
; known yet, and (tail stream rate bytes) puts the length in once it is.
; Import the formats wanted, lib/audio/wav.inc, as an image's are.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +sound_rate 16000 *sound_loaders* (list))

(defun sound-loader (ext info head tail)
	; (sound-loader ext info head tail)
	;files whose names end ext are read and written by these
	(push *sound_loaders* (list ext info head tail)))

(defun sound-loader-of (file)
	; (sound-loader-of file) -> :nil | (ext info head tail)
	(some (# (if (ends-with (first %0) file) %0)) *sound_loaders*))

(defun sound-info (file)
	; (sound-info file) -> :nil | (rate channels bits bytes)
	;what a sound file is, and how many bytes of samples it has
	(and (defq kind (sound-loader-of file)) (defq stream (file-stream file))
		(defq info ((second kind) stream)) (> (first info) 0) info))

(defun sound-seconds (file)
	; (sound-seconds file) -> num
	;how long a sound file is, in whole seconds
	(if (defq info (sound-info file))
		(progn (bind '(rate channels bits bytes) info)
			(/ bytes (* rate channels (/ bits 8))))
		0))

(defun sound-sample (data i)
	; (sound-sample data index) -> num
	;the 16 bit sample at index of samples, signed
	(defq v (code data 2 (* i 2)))
	(if (> v 32767) (- v 65536) v))

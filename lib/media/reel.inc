;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a reel, a film as a player has it: packets in order, each a frame,
; and a codec that makes frames of them
;
; A kind of reel is by the end of a file's name, and says how to open
; one, give a packet of it, and make and use its codec. (reel-open)
; gives the reel, its :frames, :rate in frames a second, :width and
; :height. (reel-packet) gives packet n. (reel-codec) makes what puts
; frames on a canvas of its own, (reel-frame codec n) puts frame n there
; and gives the canvas, (reel-close codec) lets go of what it holds.
; (reel-wave canvas w h pcm) draws a frame of a reel of sound.
; Frames are counted in the order they are shown, packets in the order
; they are decoded, and with B-frames those differ: the codec reads the
; packets it needs, from a key where it must, so a player only says
; which frame. lib/media/flm.inc, mp4.inc, mp3.inc and flac.inc are kinds.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *reel_kinds* (list))

;module
(env-push)

(defun reel-kind (ext open packet codec frame &optional close)
	; (reel-kind ext open packet codec frame [close])
	;files whose names end ext are reels: (open stream) -> :nil | reel,
	;(packet reel n), (codec reel scale), (frame codec n), (close codec). The
	;kinds are of the shared root of a GUI's node, gui/canvas/lisp.inc
	;imports them, and one of a name already there is replaced: a kind
	;added by a task would hold that task's functions, and so the task,
	;after it had gone
	(defq kind (list ext open packet codec frame close))
	(if (defq i (some (# (if (eql (first %0) ext) (!))) *reel_kinds*))
		(elem-set *reel_kinds* i kind)
		(push *reel_kinds* kind)))

(defun reel-open (file &optional stream)
	; (reel-open file [stream]) -> :nil | reel
	;the reel of a file, its kind by its name, read from the stream if
	;given, as for an object of a store, lib/files/names.inc
	(and (defq kind (some (# (if (ends-with (first %0) file) %0)) *reel_kinds*))
		(setq stream (ifn stream (file-stream file)))
		(defq reel ((second kind) stream))
		(> (ifn (. reel :find :frames) 0) 0)
		(progn (. reel :insert :kind kind) reel)))

(defun reel-packet (reel n)
	; (reel-packet reel n) -> :nil | str
	((third (. reel :find :kind)) reel n))

(defun reel-codec (reel &optional scale)
	; (reel-codec reel [scale]) -> :nil | codec
	;:nil if there is none here for what the reel holds. With a scale, 2
	;3 or 4, the frames are shown that much smaller, the codec's canvas is
	;made with that scale and its :swap makes them so. :scale of the codec
	;is the one it took, a kind may take only some
	(when (defq codec ((elem-get (. reel :find :kind) 3) reel (ifn scale 1)))
		(. codec :insert :kind (. reel :find :kind))
		codec))

(defun reel-frame (codec n)
	; (reel-frame codec n) -> :nil | canvas
	;frame n, in the order they are shown, on the codec's canvas
	((elem-get (. codec :find :kind) 4) codec n))

(defun reel-wave (canvas w h pcm)
	; (reel-wave canvas w h pcm) -> canvas
	;the picture of a reel of sound, a frame's samples, pcm the left and the
	;right as reals at the scale of 16 bits, or :nil for none: the left above
	;the right, a column of two pixels a sample, joined to the one before
	(.-> canvas (:fill 0xff000000) (:set_color 0xff303030) (:fbox 0 (dec (/ h 2)) w 1))
	(when pcm
		(each (lambda (samples mid colour)
				(defq len (length samples) k (/ (n2r (/ h 4)) (n2r 32768)) y0 mid)
				(. canvas :set_color colour)
				(each (lambda (x)
						(defq y (- mid (n2i (* (elem-get samples (/ (* x len) w)) k))))
						(. canvas :fbox x (min y y0) 2 (inc (abs (- y y0))))
						(setq y0 y))
					(range 0 w 2)))
			pcm (list (/ h 4) (* 3 (/ h 4))) '(0xff40c060 0xff4080e0)))
	canvas)

(defun reel-close (codec)
	; (reel-close codec) -> :nil
	;let go of what the codec holds that is not in Lisp, a host decoder
	(if (defq close (elem-get (. codec :find :kind) 5)) (close codec)))

;module
(export-symbols '(reel-kind reel-open reel-packet reel-codec reel-frame reel-close reel-wave))
(env-pop)

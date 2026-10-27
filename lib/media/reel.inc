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
; (reel-whole str at bytes) is a whole number held as a float of 4 or 8
; bytes, high first, as the containers hold a rate of sound.
; (reel-pcm chans [scale]) is the channels of a frame of sound, nums, as
; reals times scale, the first two, the one of one on both.
; (reel-clock seconds) is a time as h:mm:ss.
; Frames are counted in the order they are shown, packets in the order
; they are decoded, and with B-frames those differ: the codec reads the
; packets it needs, from a key where it must, so a player only says
; which frame. lib/media/flm.inc, mp4.inc, mp3.inc and flac.inc are kinds.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *reel_kinds* (list))

;module
(env-push)

(defun reel-whole (s at bytes)
	; (reel-whole str at bytes) -> num
	(defq bits (code-be s bytes at))
	(cond
		((= bytes 4)
			(defq power (- (logand (>> bits 23) 0xff) 127))
			(if (< power 0) 0 (>> (logior (logand bits 0x7fffff) 0x800000) (- 23 power))))
		(:t (defq power (- (logand (>> bits 52) 0x7ff) 1023))
			(if (< power 0) 0 (>> (logior (logand bits 0xfffffffffffff) 0x10000000000000) (- 52 power))))))

(defun reel-pcm (chans &optional scale)
	; (reel-pcm chans [scale]) -> (left right)
	;the native taken as it is used: a system not yet built with it can
	;still import this, the kinds are of a desktop's shared root
	(defq ints (ffi "lib/media/media_flac_ints") scale (ifn scale (n2r 1))
		pcm (map! (lambda (ch) (ints (fill (reals) (length ch) (n2r 0)) ch scale)) (list chans) 0 (min 2 (length chans))))
	(if (= (length pcm) 1) (list (first pcm) (first pcm)) pcm))

(defun reel-clock (time)
	; (reel-clock seconds) -> str
	(defq s (n2i time))
	(cat (str (/ s 3600)) ":" (pad (str (% (/ s 60) 60)) 2 "0") ":" (pad (str (% s 60)) 2 "0")))

(defun reel-kind (ext open packet codec frame &optional close)
	; (reel-kind ext open packet codec frame [close])
	;files whose names end ext are reels: (open stream [few]) -> :nil | reel,
	;(packet reel n), (codec reel scale), (frame codec n), (close codec). The
	;kinds are of the shared root of a GUI's node, gui/canvas/lisp.inc
	;imports them, and one of a name already there is replaced: a kind
	;added by a task would hold that task's functions, and so the task,
	;after it had gone
	(defq kind (list ext open packet codec frame close))
	(if (defq i (some (# (if (eql (first %0) ext) (!))) *reel_kinds*))
		(elem-set *reel_kinds* i kind)
		(push *reel_kinds* kind)))

(defun reel-open (file &optional stream few)
	; (reel-open file [stream few]) -> :nil | reel
	;the reel of a file, its kind by its name, read from the stream if
	;given, as for an object of a store, lib/files/names.inc. With few,
	;only as much as shows its start, if its kind can: a preview, or a
	;look at what it is. Such a reel is :partial, its :frames those read
	(and (defq kind (some (# (if (ends-with (first %0) file) %0)) *reel_kinds*))
		(setq stream (ifn stream (file-stream file)))
		(defq reel ((second kind) stream few))
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
		(defq fbox (const (ffi "gui/canvas/lisp_fbox")))
		(each (lambda (samples mid colour)
				(defq len (length samples) y0 mid ys (nums-scale samples (/ (n2r (/ h 4)) (n2r 32768))))
				(. canvas :set_color colour)
				(each (lambda (x)
						(defq y (- mid (n2i (elem-get ys (/ (* x len) w)))))
						(fbox canvas x (min y y0) 2 (inc (abs (- y y0))))
						(setq y0 y))
					(range 0 w 2)))
			pcm (list (/ h 4) (* 3 (/ h 4))) '(0xff40c060 0xff4080e0)))
	canvas)

(defun reel-rescale (codec scale)
	; (reel-rescale codec scale) -> :nil | codec
	;the codec shown at another scale, if its kind can: its decoder as it
	;is, nothing decoded again, on a canvas of the scale with the picture it
	;had. :nil if it can not, and a codec of the scale is to be made
	(if (defq rescale (. codec :find :rescale)) (rescale codec scale)))

(defun reel-close (codec)
	; (reel-close codec) -> :nil
	;let go of what the codec holds that is not in Lisp, a host decoder
	(if (defq close (elem-get (. codec :find :kind) 5)) (close codec)))

;module
(export-symbols '(reel-kind reel-open reel-packet reel-codec reel-frame reel-rescale reel-close reel-wave reel-whole reel-pcm reel-clock))
(env-pop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a reel, a film as a player has it: packets in order, each a frame,
; and a codec that makes frames of them
;
; A kind of reel is by the end of a file's name, and says how to open
; one, give a packet of it, and make and use its codec. (reel-open)
; gives the reel, its :frames, :rate in frames a second, :width and
; :height. (reel-packet) gives packet n. (reel-codec) makes what puts
; frames on a canvas of its own, (reel-decode codec packet n) puts one
; there and gives the canvas. Packet 0 is a whole frame; a player that
; goes back, or round again, starts from it. lib/media/flm.inc is a kind.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *reel_kinds* (list))

(defun reel-kind (ext open packet codec decode)
	; (reel-kind ext open packet codec decode)
	;files whose names end ext are reels: (open stream) -> :nil | reel,
	;(packet reel n), (codec reel), (decode codec packet n)
	(push *reel_kinds* (list ext open packet codec decode)))

(defun reel-open (file)
	; (reel-open file) -> :nil | reel
	(and (defq kind (some (# (if (ends-with (first %0) file) %0)) *reel_kinds*))
		(defq stream (file-stream file))
		(defq reel ((second kind) stream))
		(> (ifn (. reel :find :frames) 0) 0)
		(progn (. reel :insert :kind kind) reel)))

(defun reel-packet (reel n)
	; (reel-packet reel n) -> :nil | str
	((third (. reel :find :kind)) reel n))

(defun reel-codec (reel)
	; (reel-codec reel) -> codec
	(defq codec ((elem-get (. reel :find :kind) 3) reel))
	(. codec :insert :kind (. reel :find :kind))
	codec)

(defun reel-decode (codec packet n)
	; (reel-decode codec packet n) -> :nil | canvas
	((elem-get (. codec :find :kind) 4) codec packet n))

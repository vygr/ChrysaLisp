;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; What the Molecule app and its children, ./child.lisp, share: the colors of
; the atoms, and a job, an atom's image, and its answer, the image's pixels.
; They start where the farm's end, lib/task/jobs.inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/task/jobs.inc")

(defq +palette (push `(,quote) (map (lambda (%0) (Vec3-q
				(/ (n2q (logand (>> %0 16) 0xff)) 255.0)
				(/ (n2q (logand (>> %0 8) 0xff)) 255.0)
				(/ (n2q (logand %0 0xff)) 255.0)))
		;the first was black, and a black ball shows only its highlight, a
		;dark grey shows it is a ball
		(list 0xff505050 +argb_white +argb_red +argb_green
			+argb_cyan +argb_blue +argb_yellow +argb_magenta))))

;an atom's image: its width and height in pixels, which of the palette,
;and how well lit, 0 to 5. The answer says which again, and its pixels
;follow it, 32 bits each, (pixmap-write). Not drawn on pixels shared: a
;block of shared memory is a host handle while it is kept, and the
;hundreds of images of a Molecule used up the node's, no file could be
;opened after
(structure +atom_job +job_size
	(uint pixels kind level))

(structure +atom_reply +job_reply_size
	(uint pixels kind level))

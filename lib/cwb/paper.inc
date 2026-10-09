;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/cwb/paper.inc - what is behind a document while it is worked on
;
; The background of the document, or paper if it has none, and lines by a
; style, :plain :grid :lines or :axis. It is there to work on and is not
; part of the picture, a .cwb shown as an image has none of it.
;
; The lines are where the grid is, every :grid of the document from its top
; left, by the zoom, so what snaps to the grid lands on a line.
;
; It can be drawn for some of the rows only, as one of the nodes that draw
; a board in stripes draws it, lib/cwb/stripes.inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +cwb_paper_col 0xffF8F8FF +cwb_paper_ink 0xffADD8E6)

(defun cwb-paper (canvas width height back style gap &optional y y1)
	; (cwb-paper canvas width height back style gap [y y1]) -> canvas
	;on a canvas that is width by height: back, a colour, 0 for plain
	;paper, and the lines of the style, gap apart. Only rows y up to y1 if
	;given. It is all boxes, so that a clip, if the canvas has one, holds
	(setd y 0 y1 height)
	(defq rows (- y1 y))
	(when (> rows 0)
		;paper, and a background that is not solid goes over it
		(.-> canvas (:set_color (if (= (logand back 0xff000000) 0xff000000) back +cwb_paper_col))
			(:fbox 0 y width rows))
		(if (and (/= back 0) (/= (logand back 0xff000000) 0xff000000))
			(.-> canvas (:set_color back) (:fbox 0 y width rows)))
		(. canvas :set_color +cwb_paper_ink)
		(defq gap (max 2 gap))
		(case style
			(:grid
				(each (# (. canvas :fbox %0 y 1 rows)) (range gap width gap))
				(each (# (if (and (>= %0 y) (< %0 y1)) (. canvas :fbox 0 %0 width 1))) (range gap height gap)))
			(:lines
				(each (# (if (and (>= %0 y) (< %0 y1)) (. canvas :fbox 0 %0 width 1))) (range gap height gap)))
			(:axis
				;the two lines through the middle, on the grid, and marks along them
				(defq cx (* (/ (/ width 2) gap) gap) cy (* (/ (/ height 2) gap) gap))
				(. canvas :fbox cx y 1 rows)
				(if (and (>= cy y) (< cy y1)) (. canvas :fbox 0 cy width 1))
				(each (# (. canvas :fbox %0 (max y (- cy 3)) 1 (max 0 (- (min y1 (+ cy 4)) (max y (- cy 3))))))
					(range gap width gap))
				(each (# (if (and (>= %0 y) (< %0 y1)) (. canvas :fbox (- cx 3) %0 7 1))) (range gap height gap)))))
	canvas)

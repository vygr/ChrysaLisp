(include "lib/asm/func.inc")
(include "class/lisp/class.inc")
(include "class/fixeds/class.inc")
(include "class/fixed/class.inc")

;a line or a pad against another, in the fixed point of layer.inc: the
;macros of hit_line of lisp.vp, and of each flood, flood.inc

;fixed point as the Lisp of layer.inc has it, (hit-line?): a product is
;shifted down after it is made, a dot is the sum of such, a quotient is of
;the dividend shifted up, so what this finds is what that finds
(defun pcb-fmul (a b)
	;a = a * b
	(vp-mul-rr b a)
	(vp-asr-cr +fp_shift a))

(defun pcb-fdot (x1 y1 x2 y2 t)
	;x1 = x1 * x2 + y1 * y2, t trashed
	(pcb-fmul x1 x2)
	(vp-cpy-rr y1 t)
	(pcb-fmul t y2)
	(vp-add-rr t x1))

(defun pcb-collide ()
	;the routines collide and sdist, into the function this is called in
(vp-label 'collide)
	;inputs
	;:r13 = line fixeds (ptr)
	;:r1 = record list (ptr)
	;outputs
	;:r0 = 0 if clear, else hit
	;trashes
	;:r0-:r13
	;info
	;(hit-line?) of layer.inc: the record is (id layer_line p1 p2 norm dist)

	(assign '((:r13 +array_begin) (:r1 +array_begin)) '(:r13 :r1))
	(assign `((:r1 ,(* 1 +ptr_size)) (:r1 ,(* 4 +ptr_size)) (:r1 ,(* 5 +ptr_size))) '(:r12 :r10 :r9))
	(assign '((:r12 +array_begin) (:r10 +array_begin) (:r9 +num_value)) '(:r12 :r10 :r9))
	;d, both radii and the larger gap
	(assign `((:r13 ,(* 4 +long_size)) (:r13 ,(* 5 +long_size))
			(:r12 ,(* 4 +long_size)) (:r12 ,(* 5 +long_size))) '(:r11 :r1 :r0 :r2))
	(vp-add-rr :r0 :r11)
	(vpif '(:r2 > :r1))
		(vp-cpy-rr :r2 :r1)
	(endif)
	(vp-add-rr :r1 :r11)
	;how far each end of the line is across the record's
	(load-fields :r10 '(0 +long_size) '(:r0 :r1))
	(load-fields :r13 '(0 +long_size) '(:r2 :r3))
	(vp-cpy-rr :r0 :r4) (vp-cpy-rr :r1 :r5)
	(pcb-fdot :r4 :r5 :r2 :r3 :r6)
	(vp-sub-rr :r9 :r4)
	(assign `((:r13 ,(* 2 +long_size)) (:r13 ,(* 3 +long_size))) '(:r2 :r3))
	(pcb-fdot :r0 :r1 :r2 :r3 :r6)
	(vp-sub-rr :r9 :r0)
	;both beyond one side, clear
	(vpif '(:r4 > :r11) '(:r0 > :r11))
		(vp-xor-rr :r0 :r0)
		(vp-ret)
	(endif)
	(vp-cpy-rr :r11 :r6)
	(vp-mul-cr -1 :r6)
	(vpif '(:r4 < :r6) '(:r0 < :r6))
		(vp-xor-rr :r0 :r0)
		(vp-ret)
	(endif)
	;the lines cross, (vector-collide-lines-2d)
	(assign `((:r13 0) (:r13 +long_size) (:r13 ,(* 2 +long_size)) (:r13 ,(* 3 +long_size))) '(:r6 :r7 :r0 :r1))
	(vp-sub-rr :r6 :r0) (vp-sub-rr :r7 :r1)
	(assign `((:r12 0) (:r12 +long_size) (:r12 ,(* 2 +long_size)) (:r12 ,(* 3 +long_size))) '(:r2 :r3 :r4 :r5))
	(vp-sub-rr :r4 :r2) (vp-sub-rr :r5 :r3)
	(vp-mul-cr -1 :r2) (vp-mul-cr -1 :r3)
	(vp-sub-rr :r6 :r4) (vp-sub-rr :r7 :r5)
	;av :r0 :r1, bv :r2 :r3, cv :r4 :r5
	(vp-cpy-rr :r0 :r6) (pcb-fmul :r6 :r3) (vp-cpy-rr :r1 :r9) (pcb-fmul :r9 :r2) (vp-sub-rr :r9 :r6)
	(vp-cpy-rr :r0 :r7) (pcb-fmul :r7 :r5) (vp-cpy-rr :r1 :r9) (pcb-fmul :r9 :r4) (vp-sub-rr :r9 :r7)
	(vp-cpy-rr :r4 :r8) (pcb-fmul :r8 :r3) (vp-cpy-rr :r5 :r9) (pcb-fmul :r9 :r2) (vp-sub-rr :r9 :r8)
	;axb :r6, axc :r7, cxb :r8
	(vpif '(:r6 > 0) '(:r7 >= 0) '(:r7 <= :r6) '(:r8 >= 0) '(:r8 <= :r6))
		(vp-cpy-cr 1 :r0)
		(vp-ret)
	(elseif '(:r6 < 0) '(:r7 <= 0) '(:r7 >= :r6) '(:r8 <= 0) '(:r8 >= :r6))
		(vp-cpy-cr 1 :r0)
		(vp-ret)
	(endif)
	;then any end within d of the other line, (vector-sdist-to-line)
	(pcb-fmul :r11 :r11)
	(each (lambda ((p s))
			(assign `((,p 0) (,p +long_size) (,s 0) (,s +long_size) (,s ,(* 2 +long_size)) (,s ,(* 3 +long_size)))
				'(:r0 :r1 :r2 :r3 :r4 :r5))
			(vp-call 'sdist)
			(vpif '(:r0 <= :r11))
				(vp-cpy-cr 1 :r0)
				(vp-ret)
			(endif)
			(assign `((,p ,(* 2 +long_size)) (,p ,(* 3 +long_size)) (,s 0) (,s +long_size) (,s ,(* 2 +long_size)) (,s ,(* 3 +long_size)))
				'(:r0 :r1 :r2 :r3 :r4 :r5))
			(vp-call 'sdist)
			(vpif '(:r0 <= :r11))
				(vp-cpy-cr 1 :r0)
				(vp-ret)
			(endif)) '((:r12 :r13) (:r13 :r12)))
	(vp-xor-rr :r0 :r0)
	(vp-ret)

(vp-label 'sdist)
	;inputs
	;:r0 :r1 = point
	;:r2 :r3 = line start, :r4 :r5 = line end
	;outputs
	;:r0 = the square of the distance from the point to the line, as a
	;fixed
	;trashes
	;:r0-:r10
	(vp-sub-rr :r2 :r4) (vp-sub-rr :r3 :r5)
	(vp-cpy-rr :r0 :r6) (vp-cpy-rr :r1 :r7)
	(vp-sub-rr :r2 :r6) (vp-sub-rr :r3 :r7)
	(pcb-fdot :r6 :r7 :r4 :r5 :r8)
	(vpif '(:r6 <= 0))
		(vp-sub-rr :r2 :r0) (vp-sub-rr :r3 :r1)
	(else)
		(vp-cpy-rr :r4 :r8) (vp-cpy-rr :r5 :r9)
		(pcb-fdot :r8 :r9 :r4 :r5 :r10)
		(vpif '(:r8 <= :r6))
			(vp-add-rr :r4 :r2) (vp-add-rr :r5 :r3)
			(vp-sub-rr :r2 :r0) (vp-sub-rr :r3 :r1)
		(else)
			(vp-shl-cr +fp_shift :r6)
			(vp-div-rr :r8 :r6)
			(pcb-fmul :r4 :r6) (pcb-fmul :r5 :r6)
			(vp-add-rr :r4 :r2) (vp-add-rr :r5 :r3)
			(vp-sub-rr :r2 :r0) (vp-sub-rr :r3 :r1)
		(endif)
	(endif)
	(vp-mul-cr -1 :r0) (vp-mul-cr -1 :r1)
	(vp-cpy-rr :r0 :r2) (vp-cpy-rr :r1 :r3)
	(pcb-fdot :r0 :r1 :r2 :r3 :r4)
	(vp-ret))

(import 'sys/class.inc)

;;;;;;;;;;;;;;;;;;;
; math static class
;;;;;;;;;;;;;;;;;;;

(def-class 'math)
(def-method 'random 'sys/math/random 'static '(r0) '(r0))
(def-method 'frac 'sys/math/frac 'static '(r0) '(r0))
(def-method 'floor 'sys/math/floor 'static '(r0) '(r0))
(def-method 'fmod 'sys/math/fmod 'static '(r0 r1) '(r0))
(def-method 'isqrt 'sys/math/isqrt 'static '(r0) '(r0))
(def-method 'fsqrt 'sys/math/fsqrt 'static '(r0) '(r0))
(def-method 'fsin 'sys/math/fsin 'static '(r0) '(r0))
(def-method 'fcos 'sys/math/fcos 'static '(r0) '(r0))
(def-method 'vec_intersect 'sys/math/vec_intersect 'static '(r0 r1 r2 r3 r5 r6 r7 r8) '(r0 r1))

(defcvar 'fp_shift 16 'fp_2pi 411774 'fp_pi 205887 'fp_hpi 102943 'fp_qpi 51471 'fp_rpi 20861
	'fp_int_mask (bit-shl -1 fp_shift) 'fp_frac_mask (bit-not fp_int_mask))

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun sys/math/fsqrt ()
	;inputs
	;r0 = 16.16 number
	;outputs
	;r0 = 16.16 sqrt
	;trashes
	;r1-r3
	(vp-shl-cr fp_shift r0)
	(f-call 'math 'isqrt '(r0) '(r0)))

(defcfun sys/math/frac ()
	;inputs
	;r0 = 16.16 number
	;outputs
	;r0 = 16.16 fraction
	;trashes
	;r1
	(vpif '(r0 >= 0))
		(vp-and-cr fp_frac_mask r0)
	(else)
		(vp-cpy-rr r0 r1)
		(vp-cpy-cr 1.0 r0)
		(vp-and-cr fp_frac_mask r1)
		(vp-sub-rr r1 r0)
	(endif))

(defcfun sys/math/floor ()
	;inputs
	;r0 = 16.16 number
	;outputs
	;r0 = 16.16 floor
	;trashes
	;r1
	(vpif '(r0 >= 0))
		(vp-and-cr fp_int_mask r0)
	(else)
		(vp-cpy-rr r0 r1)
		(vp-and-cr fp_frac_mask r1)
		(vp-sub-rr r1 r0)
	(endif))

;;;;;;;;;;;;;;;;;;;;;
; vector math codegen
;;;;;;;;;;;;;;;;;;;;;

(defcvar '*vsp* 0 '*vr* '(r0 r1 r2 r3 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15))
(defcfun vec-set (_) (setq *vsp* _))

(defcfun vec-push (n)
	(slice *vsp* (setq *vsp* (add *vsp* n)) *vr*))

(defcfun vec-pop (n)
	(slice (setq *vsp* (sub *vsp* n)) (add *vsp* n) *vr*))

(defcfun vec-top (n)
	(slice (sub *vsp* n) *vsp* *vr*))

(defcfun vec-tmp (n)
	(slice *vsp* (add *vsp* n) *vr*))

(defcfun vec-load (n &rest _)
	(each (lambda (_)
		(assign _ (vec-push n))) _))

(defcfun vec-store (n &rest _)
	(each (lambda (_)
		(assign (vec-pop n) _)) _))

(defcfun vec-clr (n)
	(each (lambda (v)
		(vp-xor-rr v v)) (vec-top n)))

(defcfun vec-dup (n)
	(each (lambda (v2 v1)
		(vp-cpy-rr v2 v1)) (vec-top n) (vec-push n)))

(defcfun vec-add (n)
	(each (lambda (v2 v1)
		(vp-add-rr v2 v1)) (vec-pop n) (vec-top n)))

(defcfun vec-sub (n)
	(each (lambda (v2 v1)
		(vp-sub-rr v2 v1)) (vec-pop n) (vec-top n)))

(defcfun vec-mul (n)
	(each (lambda (v2 v1)
		(vp-mul-rr v2 v1)) (vec-pop n) (vec-top n))
	(vec-asr n fp_shift))

(defcfun vec-div (n)
	(defq v3 (vec-tmp n) v2 (vec-pop n) v1 (vec-top n))
	(each (lambda (v)
		(vp-shl-cr fp_shift v)) v1)
	(each (lambda (v3 v1)
		(vp-ext-rr v1 v3)) v3 v1)
	(each (lambda (v3 v2 v1)
		(vp-div-rrr v2 v3 v1)) v3 v2 v1))

(defcfun vec-sum (n)
	(reduce (lambda (v1 v2)
		(vp-add-rr v2 v1) v1) (vec-pop n))
	(vec-push 1))

(defcfun vec-dif (n)
	(reduce (lambda (v1 v2)
		(vp-sub-rr v2 v1) v1) (vec-pop n))
	(vec-push 1))

(defcfun vec-shl (n s)
	(each (lambda (v)
		(if (reg s)
			(vp-shl-rr s v)
			(vp-shl-cr s v))) (vec-top n)))

(defcfun vec-shr (n s)
	(each (lambda (v)
		(if (reg s)
			(vp-shr-rr s v)
			(vp-shr-cr s v))) (vec-top n)))

(defcfun vec-asr (n s)
	(each (lambda (v)
		(if (reg s)
			(vp-asr-rr s v)
			(vp-asr-cr s v))) (vec-top n)))

(defcfun vec-abs (n)
	(each (lambda (v)
		(vpif `(,v < 0))
			(vp-mul-cr -1 v)
		(endif)) (vec-top n)))

(defcfun vec-min (n)
	(reduce (lambda (v1 v2)
		(vpif `(,v2 < ,v1))
			(vp-cpy-rr v2 v1)
		(endif) v1) (vec-pop n))
	(vec-push 1))

(defcfun vec-max (n)
	(reduce (lambda (v1 v2)
		(vpif `(,v2 > ,v1))
			(vp-cpy-rr v2 v1)
		(endif) v1) (vec-pop n))
	(vec-push 1))

(defcfun vec-square (n)
	(each (lambda (v)
		(vp-mul-rr v v)) (vec-top n))
	(vec-asr n fp_shift))

(defcfun vec-scale (n &optional s)
	(setd s (elem 0 (vec-pop 1)))
	(each (lambda (v)
		(if (reg s)
			(vp-mul-rr s v)
			(vp-mul-cr s v))) (vec-top n))
	(vec-asr n fp_shift))

(defcfun vec-perp ()
	(defq v (vec-top 2) x (elem 0 v) y (elem 1 v))
	(vp-xchg-rr x y)
	(vp-mul-cr -1 y))

(defcfun vec-dot (n)
	(vec-mul n)
	(vec-sum n))

(defcfun vec-det (n)
	(defq v2 (vec-pop n))
	(each (lambda (v)
		(vp-mul-rr (elem (sub n _ 1) v2) v)) (vec-top n))
	(vec-asr n fp_shift)
	(vec-dif n))

(defcfun vec-length-squared (n)
	(vec-square n)
	(vec-sum n))

(defcfun vec-length (n)
	(vec-length-squared n)
	(defq v (vec-top 1) r (slice 0 (dec (if (ge *vsp* 4) 4 *vsp*) *vr*)))
	(if (ne (length r) 0) (apply vp-push r))
	(f-call 'math 'fsqrt v v)
	(if (ne (length r) 0) (apply vp-pop r)))

(defcfun vec-norm (n)
	(vec-dup n)
	(vec-length n)
	(defq r (vec-tmp 2) l (elem 0 (vec-pop 1)))
	(vpif `(,l == 0))
		(vec-clr n)
	(else)
		(defq rl (elem 0 r) rh (elem 1 r))
		(vp-cpy-cr (bit-shl 1.0 fp_shift) rl)
		(vp-xor-rr rh rh)
		(vp-div-rrr l rh rl)
		(vec-scale n rl)
	(endif))

(defcfun vec-manhattan (n)
	(vec-sub n)
	(vec-abs n)
	(vec-sum n))

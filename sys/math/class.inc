(include 'sys/class.inc)

;;;;;;;;;;;;;;;;;;;
; math static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_math)
(dec-method 'random 'sys/math/random 'static '(r0) '(r0))
(dec-method 'isqrt 'sys/math/isqrt 'static '(r0) '(r0))
(dec-method 'fsqrt 'sys/math/fsqrt 'static '(r0) '(r0))
(dec-method 'fsin 'sys/math/fsin 'static '(r0) '(r0))
(dec-method 'fcos 'sys/math/fcos 'static '(r0) '(r0))
(dec-method 'intersect 'sys/math/intersect 'static '(r0 r1 r2 r3 r4 r5 r6 r7) '(r0 r1))
(dec-method 'distance_sqd 'sys/math/distance_sqd 'static '(r0 r1 r2 r3 r4 r5) '(r0))

(dec-method 'lisp_fsin 'sys/math/lisp_fsin 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_fcos 'sys/math/lisp_fcos 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_fsqrt 'sys/math/lisp_fsqrt 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_frac 'sys/math/lisp_frac 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_floor 'sys/math/lisp_floor 'static '(r0 r1) '(r0 r1))

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun sys/math/fsqrt ()
	;inputs
	;r0 = number (16.16)
	;outputs
	;r0 = sqrt (16.16)
	;trashes
	;r1-r3
	(vp-shl-cr fp_shift r0)
	(call 'sys_math 'isqrt '(r0) '(r0)))

;;;;;;;;;;;;;;;;;;;;;
; vector math codegen
;;;;;;;;;;;;;;;;;;;;;

(defcvar '*vsp* 0 '*vr* '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
(defcfun vec-set (_) (setq *vsp* _))

(defcfun vec-goto (l)
	(vec-set 0)
	(goto l))

(defcmacro vec-assign (_1 _2)
	`(assign ,_1 ,_2 (slice *vsp* -1 *vr*)))

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
		(vec-assign _ (vec-push n))) _))

(defcfun vec-store (n &rest _)
	(each (lambda (_)
		(vec-assign (vec-pop n) _)) _))

(defcfun vec-tee (n &rest _)
	(each (lambda (_)
		(vec-assign (vec-top n) _)) _))

(defcfun vec-load-int (n r &optional i)
	(setd i 0)
	(each (lambda (v)
		(vp-cpy-ir-i r (add i (mul _ int_size)) v)) (vec-push n)))

(defcfun vec-store-int (n r &optional i)
	(setd i 0)
	(each (lambda (v)
		(vp-cpy-ri-i v r (add i (mul _ int_size)))) (vec-pop n)))

(defcfun vec-load-long (n r &optional i)
	(setd i 0)
	(each (lambda (v)
		(vp-cpy-ir r (add i (mul _ long_size)) v)) (vec-push n)))

(defcfun vec-store-long (n r &optional i)
	(setd i 0)
	(each (lambda (v)
		(vp-cpy-ri v r (add i (mul _ long_size)))) (vec-pop n)))

(defcfun vec-clr (n &optional v)
	(setd v (vec-top n))
	(each (lambda (v)
		(vp-xor-rr v v)) v))

(defcfun vec-cpy (n &optional v2 v1)
	(setd v2 (vec-pop n) v1 (vec-top n))
	(each (lambda (v2 v1)
		(vp-cpy-rr v2 v1)) v2 v1))

(defcfun vec-dup (n &optional v2 v1)
	(setd v2 (vec-top n) v1 (vec-push n))
	(vec-cpy n v2 v1))

(defcfun vec-over (n h)
	(vec-pop h)
	(defq v2 (vec-top n))
	(vec-push h)
	(vec-cpy n v2 (vec-push n)))

(defcfun vec-frac (n &optional v)
	(setd v (vec-top n))
	(bind '(r) (vec-tmp 1))
	(each (lambda (v)
		(vpif (list v '>= 0))
			(vp-and-cr fp_frac_mask v)
		(else)
			(vp-cpy-rr v r)
			(vp-cpy-cr 1.0 v)
			(vp-and-cr fp_frac_mask r)
			(vp-sub-rr r v)
		(endif)) v))

(defcfun vec-floor (n &optional v)
	(setd v (vec-top n))
	(bind '(r) (vec-tmp 1))
	(each (lambda (v)
		(vpif (list v '>= 0))
			(vp-and-cr fp_int_mask v)
		(else)
			(vp-cpy-rr v r)
			(vp-and-cr fp_frac_mask r)
			(vp-sub-rr r v)
		(endif)) v))

(defcfun vec-add (n &optional v2 v1)
	(setd v2 (vec-pop n) v1 (vec-top n))
	(each (lambda (v2 v1)
		(vp-add-rr v2 v1)) v2 v1))

(defcfun vec-sub (n &optional v2 v1)
	(setd v2 (vec-pop n) v1 (vec-top n))
	(each (lambda (v2 v1)
		(vp-sub-rr v2 v1)) v2 v1))

(defcfun vec-mul (n &optional v2 v1)
	(setd v2 (vec-pop n) v1 (vec-top n))
	(each (lambda (v2 v1)
		(vp-mul-rr v2 v1)) v2 v1))

(defcfun vec-div (n &optional v2 v1)
	(defq v3 (vec-tmp n))
	(setd v2 (vec-pop n) v1 (vec-top n))
	(each (lambda (v3 v1)
		(vp-ext-rr v1 v3)) v3 v1)
	(each (lambda (v3 v2 v1)
		(vp-div-rrr v2 v3 v1)) v3 v2 v1))

(defcfun vec-mod (n &optional v2 v1)
	(defq v3 (vec-tmp n))
	(setd v2 (vec-pop n) v1 (vec-top n))
	(each (lambda (v3 v1)
		(vp-ext-rr v1 v3)) v3 v1)
	(each (lambda (v3 v2 v1)
		(vp-div-rrr v2 v3 v1)) v3 v2 v1)
	(each (lambda (v3 v1)
		(vp-cpy-rr v3 v1)) v3 v1))

(defcfun vec-fmul (n &optional v2 v1)
	(vec-mul n v2 v1)
	(vec-asr n fp_shift v1))

(defcfun vec-fdiv (n &optional v2 v1)
	(defq v3 (vec-tmp n))
	(setd v2 (vec-pop n) v1 (vec-top n))
	(each (lambda (v)
		(vp-shl-cr fp_shift v)) v1)
	(each (lambda (v3 v1)
		(vp-ext-rr v1 v3)) v3 v1)
	(each (lambda (v3 v2 v1)
		(vp-div-rrr v2 v3 v1)) v3 v2 v1))

(defcfun vec-fmod (n &optional v2 v1)
	(defq v3 (vec-push n) v4 (vec-tmp n))
	(vec-pop n)
	(setd v2 (vec-pop n) v1 (vec-top n))
	(each (lambda (v4 v1)
		(vp-cpy-rr v1 v4)) v4 v1)
	(each (lambda (v)
		(vp-shl-cr fp_shift v)) v1)
	(each (lambda (v3 v1)
		(vp-ext-rr v1 v3)) v3 v1)
	(each (lambda (v3 v2 v1)
		(vp-div-rrr v2 v3 v1)) v3 v2 v1)
	(each (lambda (v)
		(vp-asr-cr fp_shift v)) v1)
	(each (lambda (v2 v1)
		(vp-mul-rr v1 v2)) v2 v1)
	(each (lambda (v4 v1)
		(vp-cpy-rr v4 v1)) v4 v1)
	(each (lambda (v2 v1)
		(vp-sub-rr v2 v1)) v2 v1))

(defcfun vec-shl (n s &optional v)
	(setd v (vec-top n))
	(each (lambda (v)
		(if (reg s)
			(vp-shl-rr s v)
			(vp-shl-cr s v))) v))

(defcfun vec-shr (n s &optional v)
	(setd v (vec-top n))
	(each (lambda (v)
		(if (reg s)
			(vp-shr-rr s v)
			(vp-shr-cr s v))) v))

(defcfun vec-asr (n s &optional v)
	(setd v (vec-top n))
	(each (lambda (v)
		(if (reg s)
			(vp-asr-rr s v)
			(vp-asr-cr s v))) v))

(defcfun vec-abs (n &optional v)
	(setd v (vec-top n))
	(each (lambda (v)
		(vpif (list v '< 0))
			(vp-mul-cr -1 v)
		(endif)) v))

(defcfun vec-square (n &optional v)
	(setd v (vec-top n))
	(each (lambda (v)
		(vp-mul-rr v v)) v))

(defcfun vec-fsquare (n &optional v)
	(vec-square n v)
	(vec-asr n fp_shift v))

(defcfun vec-scale (n &optional s v)
	(setd s (elem 0 (vec-pop 1)) v (vec-top n))
	(each (lambda (v)
		(if (reg s)
			(vp-mul-rr s v)
			(vp-mul-cr s v))) v))

(defcfun vec-fscale (n &optional s v)
	(vec-scale n s v)
	(vec-asr n fp_shift v))

(defcfun vec-sum (n)
	(reduce (lambda (v1 v2)
		(vp-add-rr v2 v1) v1) (vec-pop n))
	(vec-push 1))

(defcfun vec-dif (n)
	(reduce (lambda (v1 v2)
		(vp-sub-rr v2 v1) v1) (vec-pop n))
	(vec-push 1))

(defcfun vec-min (n)
	(reduce (lambda (v1 v2)
		(vpif (list v2 '< v1))
			(vp-cpy-rr v2 v1)
		(endif) v1) (vec-pop n))
	(vec-push 1))

(defcfun vec-max (n)
	(reduce (lambda (v1 v2)
		(vpif (list v2 '> v1))
			(vp-cpy-rr v2 v1)
		(endif) v1) (vec-pop n))
	(vec-push 1))

(defcfun vec-perp ()
	(bind '(x y) (vec-top 2))
	(vp-swp-rr x y)
	(vp-mul-cr -1 y))

(defcfun vec-fdot (n &optional v2)
	(vec-fmul n v2)
	(vec-sum n))

(defcfun vec-fdet (n &optional v2)
	(setd v2 (vec-pop n))
	(each (lambda (v)
		(vp-mul-rr (elem (sub n _ 1) v2) v)) (vec-top n))
	(vec-asr n fp_shift)
	(vec-dif n))

(defcfun vec-flength-squared (n)
	(vec-fsquare n)
	(vec-sum n))

(defcfun vec-flength (n)
	(vec-flength-squared n)
	(defq r (slice 0 (dec (if (ge *vsp* 4) 5 *vsp*)) *vr*))
	(if (ne (length r) 0) (apply vp-push r))
	(call 'sys_math 'fsqrt (vec-top 1) (vec-top 1))
	(if (ne (length r) 0) (apply vp-pop r)))

(defcfun vec-fdistance-squared (n)
	(vec-sub n)
	(vec-flength-squared n))

(defcfun vec-fnorm (n)
	(vec-dup n)
	(vec-flength n)
	(bind '(rl rh) (vec-tmp 2))
	(bind '(l) (vec-pop 1))
	(vpif (list l '== 0))
		(vec-clr n)
	(else)
		(vp-cpy-cr (shl 1.0 fp_shift) rl)
		(vp-xor-rr rh rh)
		(vp-div-rrr l rh rl)
		(vec-fscale n rl)
	(endif))

(defcfun vec-manhattan (n)
	(vec-sub n)
	(vec-abs n)
	(vec-sum n))

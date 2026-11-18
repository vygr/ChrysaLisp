(include "././array/class.inc")
(include "./struct.inc")

(def-class :nums :array
	(dec-method :vtable class/nums/vtable)
	(dec-method :type class/nums/type :override)
	(dec-method :hash class/nums/hash :override)
	(dec-method :vcreate class/nums/create :override)
	(dec-method :add class/nums/add :virtual (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :sub class/nums/sub :virtual (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :mul class/nums/mul :virtual (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :div class/nums/div :virtual (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :mod class/nums/mod :virtual (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :abs class/nums/abs :virtual (:r0 :r1) (:r0))
	(dec-method :sum class/nums/sum :virtual (:r0) (:r0 :r1))
	(dec-method :scale class/nums/scale :virtual (:r0 :r1 :r2) (:r0))
	(dec-method :min class/nums/min :virtual (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :max class/nums/max :virtual (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :dot class/nums/dot :virtual (:r0 :r1) (:r0 :r1))
	(dec-method :add_scaled class/nums/add_scaled :virtual (:r0 :r1 :r2 :r3) (:r0))
	(dec-method :convert class/nums/convert :virtual (:r0 :r1) (:r0))
	(dec-method :kind class/nums/kind :static (:r0) (:r0 :r1))
	(dec-method :create class/nums/create)
	(dec-method :lisp_vecop1 class/nums/lisp_vecop1 :static (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :lisp_vecop2 class/nums/lisp_vecop2 :static (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :lisp_add class/nums/lisp_add :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_sub class/nums/lisp_sub :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_mul class/nums/lisp_mul :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_div class/nums/lisp_div :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_mod class/nums/lisp_mod :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_sum class/nums/lisp_sum :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_dot class/nums/lisp_dot :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_add_scaled class/nums/lisp_add_scaled :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_convert class/nums/lisp_convert :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_scale class/nums/lisp_scale :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_min class/nums/lisp_min :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_max class/nums/lisp_max :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_abs class/nums/lisp_abs :static (:r0 :r1) (:r0 :r1)))

;;;;;;;;;;;;;;;;;;;;;
; inline vector loops
;;;;;;;;;;;;;;;;;;;;;

(defun vec-loop-until (srcs elms end dst func)
	(loop-start)
		(each (# (assign (list (list %0 0)) (list %1))) srcs elms)
		(each (# (vp-add-cr +long_size %0)) srcs)
		(defq r (apply func elms))
		(when dst
			(assign (list r) (list (list dst 0)))
			(vp-add-cr +long_size dst))
	(loop-until `(,(first srcs) = ,end)))

(defun vec-loop-while (srcs elms end dst func)
	(vpif `(,(first srcs) /= ,end))
		(vec-loop-until srcs elms end dst func)
	(endif))

(defun vec-map (sobs dobs elms sitrs ditrs eitr func &optional fpre fpost)
	(assign `((,(first sobs) +array_length)) `(,eitr))
	(vp-simd vp-load-rcr (cat sobs dobs) (list +array_begin) (cat sitrs ditrs))
	(vp-shl-cr (log2 +ptr_size) eitr)
	(vp-add-rr (first sitrs) eitr)
	(if fpre (fpre))
	(vec-loop-until sitrs elms eitr (first ditrs) func)
	(if fpost (fpost)))

(defun vec-reduce (sobs elms sitrs eitr func &optional fpre fpost)
	(assign `((,(first sobs) +array_length)) `(,eitr))
	(vp-simd vp-load-rcr sobs (list +array_begin) sitrs)
	(vp-shl-cr (log2 +ptr_size) eitr)
	(vp-add-rr (first sitrs) eitr)
	(if fpre (fpre))
	(vec-loop-until sitrs elms eitr :nil func)
	(if fpost (fpost)))

(defun vec-loop-until-idx (srcs elms idxs dst func)
	(loop-start)
		(each (# (assign (list (list %0 (first idxs))) (list %1))) srcs elms)
		(defq r (apply func elms))
		(if dst (assign (list r) (list (list dst (first idxs)))))
		(vp-add-cr +long_size (first idxs))
	(loop-until `(,(first idxs) = ,(last idxs))))

(defun vec-map-idx (sobs dobs elms sitrs ditrs idxs func &optional fpre fpost)
	(assign `((,(first sobs) +array_length)) `(,(last idxs)))
	(vp-simd vp-load-rcr (cat sobs dobs) (list +array_begin) (cat sitrs ditrs))
	(vp-xor-rr (first idxs) (first idxs))
	(vp-shl-cr (log2 +ptr_size) (last idxs))
	(if fpre (fpre))
	(vec-loop-until-idx sitrs elms idxs (first ditrs) func)
	(if fpost (fpost)))

(defun vec-reduce-idx (sobs elms sitrs idxs func &optional fpre fpost)
	(assign `((,(first sobs) +array_length)) `(,(last idxs)))
	(vp-simd vp-load-rcr sobs (list +array_begin) sitrs)
	(vp-xor-rr (first idxs) (first idxs))
	(vp-shl-cr (log2 +ptr_size) (last idxs))
	(if fpre (fpre))
	(vec-loop-until-idx sitrs elms idxs :nil func)
	(if fpost (fpost)))

;;;;;;;;;;;;;;;;;;;;;
; vector conversions
;;;;;;;;;;;;;;;;;;;;;

;the kinds of vector, by the number :nums :kind gives
;quoted twice, as a + name is put in place where it is used
(defq +nums_kinds ''(:nums :fixeds :doubles :floats))

(defun nums-step (kind)
	;the bytes of an element of a kind
	(if (eql kind :floats) +int_size +long_size))

(defun nums-load (kind s tmp)
	;the element of a kind at s into :d0 as a double, :d1 the recip of a
	;fixed's one
	(case kind
		(:nums (assign `((,s 0)) `(,tmp)) (vp-cvt-rd tmp :d0))
		(:fixeds (assign `((,s 0)) `(,tmp)) (vp-cvt-rd tmp :d0) (vp-mul-dd :d1 :d0))
		(:doubles (assign `((,s 0)) '(:d0)))
		(:floats (assign `((,s 0 i)) '(:d0)))))

(defun nums-store (kind d tmp)
	;:d0, a double, as the element of a kind at d, :d2 a fixed's one, ints
	;and fixeds truncated as n2i and n2q do a double
	(case kind
		(:nums (vp-cvt-dr :d0 tmp) (assign `(,tmp) `((,d 0))))
		(:fixeds (vp-mul-dd :d2 :d0) (vp-cvt-dr :d0 tmp) (assign `(,tmp) `((,d 0))))
		(:doubles (assign '(:d0) `((,d 0))))
		(:floats (vp-cvt-df :d0 :f0) (assign '(:f0) `((,d 0 i))))))

(defun nums-convert-pair (from to s d len tmp)
	;len elements of the kind from at s, as the kind to at d: the same kind
	;its bits, nums and fixeds by a shift as n2q and n2i, the rest by a
	;double
	(vpif `(,len /= 0))
		(loop-start)
			(cond
				((eql from to)
					(if (eql to :floats)
						(assign `((,s 0 ui)) `(,tmp))
						(assign `((,s 0)) `(,tmp)))
					(if (eql to :floats)
						(assign `(,tmp) `((,d 0 i)))
						(assign `(,tmp) `((,d 0)))))
				((and (eql from :nums) (eql to :fixeds))
					(assign `((,s 0)) `(,tmp))
					(vp-shl-cr +fp_shift tmp)
					(assign `(,tmp) `((,d 0))))
				((and (eql from :fixeds) (eql to :nums))
					(assign `((,s 0)) `(,tmp))
					(vp-asr-cr +fp_shift tmp)
					(assign `(,tmp) `((,d 0))))
				(:t (nums-load from s tmp)
					(nums-store to d tmp)))
			(vp-add-cr (nums-step from) s)
			(vp-add-cr (nums-step to) d)
			(vp-sub-cr 1 len)
		(loop-until `(,len = 0))
	(endif))

(defun nums-convert-method (cls to)
	;the body of the :convert of a class whose elements are of the kind to:
	;made the length of the source, each of its elements converted from
	;whichever kind the source is, chosen once
	(vp-rdef (dst src s d len tmp kind))
	(entry cls :convert `(,dst ,src))
	(assign `((,src +array_length)) `(,len))
	(vp-push dst src len)
	(call :nums :set_cap `(,dst ,len) :nil :r2)
	(vp-pop dst src len)
	(assign `(,len) `((,dst +array_length)))
	(vp-push dst)
	(call :nums :kind `(,src) `(,src ,kind))
	(vp-pop dst)
	(assign `((,dst +array_begin) (,src +array_begin)) `(,d ,s))
	(vp-lea-pr 'fn_consts tmp)
	(load-fields tmp
		(fn-consts (/ (n2d 1) (n2d (<< 1 +fp_shift))) (n2d (<< 1 +fp_shift)))
		'(:d1 :d2))
	(switch)
	(each (lambda (from)
			(vpcase `(,kind = ,(!)))
				(nums-convert-pair from to s d len tmp)
				(break))
		+nums_kinds)
	(endswitch)
	(exit cls :convert `(,dst))
	(vp-ret))

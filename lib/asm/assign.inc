;;;;;;;;;;;;
; assignment
;;;;;;;;;;;;

;module
(env-push)

(defq +pkeys1 '(:sym :list :num :str)
	+pkeys2 '(:$ :&d :&i :@ :c :d :db :di :ds :dub :dui :dus
		:f :i :ib :ii :is :iub :iui :ius :k :r :s))

(defmacro assign-type-sym (%0) (static-qqp (sym (str ,%0 "_t"))))

(defun assign-type-t (%0)
	(elem-get '(b ub s us i ui "") (ifn (find (first %0) "bBsSiI") -2)))

(defun assign-src-type (o)
	(pcase (pop (type-of o)) +pkeys1
		(:sym
			(cond
				((vp-rreg? o) :r)
				((vp-freg? o) :f)
				(:c)))
		(:list
			(cond
				((vp-rreg? (defq i (first o)))
					(cond
						((vp-rreg? (defq i (second o)))
							(if (= 2 (length o)) :d
								(sym (cat :d (third o)))))
						((/= 2 (length o))
							(sym (cat :i (third o))))
						((num? i) :i)
						((setq i (get (assign-type-sym i)))
							(sym (cat :i (assign-type-t i))))
						(:t :i)))
				((eql i '@) :@)
				((eql i '$) :$)
				((eql i '&)
					(if (vp-rreg? (second o))
						(if (vp-rreg? (third o)) :&d :&i) :nil))
				(:t :c)))
		(:num :c)
		(:str :s)))

(defun assign-dst-type (o)
	(pcase (pop (type-of o)) +pkeys1
		(:sym
			(and (not (vp-reg? o)) (eql (first o) ":")
				(throw ":reg equate !" o))
			(cond
				((vp-rreg? o) :r)
				((vp-freg? o) :f)
				(:k)))
		(:list
			(if (vp-rreg? (defq i (first o)))
				(cond
					((vp-rreg? (defq i (second o)))
						(if (= 2 (length o)) :d
							(sym (cat :d (third o)))))
					((/= 2 (length o))
						(sym (cat :i (third o))))
					((num? i) :i)
					((setq i (get (assign-type-sym i)))
						(sym (cat :i (assign-type-t i))))
					(:t :i))))))

(defun assign-ignored-to-asm ()
	(defq s (list) d (list))
	(each (# (unless (find %1 (list %0 '_)) (push s %0) (push d %1))) src dst)
	(setq src s dst d))

(defun assign-topology-sort ()
	(defq s src d dst i 0 c 1000 l (dec (length d)))
	(while (and (< i l) (/= c 0))
		(if (setq j (if (list?? (defq j (elem-get d i))) :nil
				(some! (lambda (s d)
					(cond
						((and (list?? s) (find j s)) (!))
						((and (list?? d) (find j d)) (!))
						((eql j s) (!)))) (list s d) :nil -1 (inc i))))
			(setq s (rotate s i j (inc j)) d (rotate d i j (inc j)) c (dec c))
			(++ i)))
	(if (= c 0)
		(throw "Copy cycle detected !" (list s d))
		(setq src s dst d)))

(defun assign-single-in (op b o d)
	;a single from memory to a float register, to a double by its single
	(cond
		((vp-sreg? d) (op b o d))
		((op b o (defq f (elem-get +vp_sregs (vp-freg? d)))) (vp-cvt-fd f d))))

(defun assign-single-out (s)
	;a float register to 32 bits is a single
	(if (vp-sreg? s) s (throw "A double to 32 bits, (vp-cvt-df) it to a single first !" s)))

(defun assign-ff (s d)
	;a float register to another, of its width, or made the other's
	(defq fs (vp-sreg? s) fd (vp-sreg? d))
	(cond
		((and fs fd) (vp-cpy-ff s d))
		(fs (vp-cvt-fd s d))
		(fd (vp-cvt-df s d))
		((vp-cpy-dd s d))))

(defun assign-asm-to-asm (src dst)
	(when (/= (length src) (length dst))
		(throw "Mismatching number of src/dst parameters !" (list src dst)))
	(assign-ignored-to-asm)
	(when (> (length dst) 0)
		(if (> (length dst) 1) (assign-topology-sort))
		(each (lambda (%0 %1)
			(pcase (assign-src-type %0) +pkeys2
				(:r (pcase (assign-dst-type %1) +pkeys2
					(:r (vp-cpy-rr %0 %1))
					(:i (vp-store-rrc %0 (first %1) (second %1)))
					(:iui (vp-store-rrc-i %0 (first %1) (second %1)))
					(:k (def *func_env* %1 %0))
					(:ii (vp-store-rrc-i %0 (first %1) (second %1)))
					(:d (vp-store-rrr %0 (first %1) (second %1)))
					(:iub (vp-store-rrc-b %0 (first %1) (second %1)))
					(:ib (vp-store-rrc-b %0 (first %1) (second %1)))
					(:f (if (vp-sreg? %1) (vp-cpy-rf %0 %1) (vp-cpy-rd %0 %1)))
					(:ds (vp-store-rrr-s %0 (first %1) (second %1)))
					(:dub (vp-store-rrr-b %0 (first %1) (second %1)))
					(:db (vp-store-rrr-b %0 (first %1) (second %1)))
					(:di (vp-store-rrr-i %0 (first %1) (second %1)))
					(:is (vp-store-rrc-s %0 (first %1) (second %1)))
					(:ius (vp-store-rrc-s %0 (first %1) (second %1)))
					(:dui (vp-store-rrr-i %0 (first %1) (second %1)))
					(:dus (vp-store-rrr-s %0 (first %1) (second %1)))
					(:t (throw "Invalid dst parameter !" (list %0 %1)))))
				(:i (pcase (assign-dst-type %1) +pkeys2
					(:r (vp-load-rcr (first %0) (second %0) %1))
					(:f (vp-load-rcd (first %0) (second %0) %1))
					(:t (throw "Invalid dst parameter !" (list %0 %1)))))
				(:c (if (eql (assign-dst-type %1) :r)
					(vp-cpy-cr %0 %1)
					(throw "Invalid dst parameter !" (list %0 %1))))
				(:iui (vp-load-rcr-ui (first %0) (second %0) %1))
				(:s (fn-string %0 %1))
				(:$ (vp-lea-pr (second %0) %1))
				(:&i (vp-lea-rcr (second %0) (third %0) %1))
				(:@ (fn-bind (second %0) %1))
				(:d (pcase (assign-dst-type %1) +pkeys2
					(:r (vp-load-rrr (first %0) (second %0) %1))
					(:f (vp-load-rrd (first %0) (second %0) %1))
					(:t (throw "Invalid dst parameter !" (list %0 %1)))))
				;of 32 bits to a float register, a single, made a double for a double
				(:ii (pcase (assign-dst-type %1) +pkeys2
					(:f (assign-single-in (const vp-load-rcf) (first %0) (second %0) %1))
					(:t (vp-load-rcr-i (first %0) (second %0) %1))))
				(:f (pcase (assign-dst-type %1) +pkeys2
					(:r (if (vp-sreg? %0) (vp-cpy-fr %0 %1) (vp-cpy-dr %0 %1)))
					(:d (vp-store-drr %0 (first %1) (second %1)))
					(:f (assign-ff %0 %1))
					(:i (vp-store-drc %0 (first %1) (second %1)))
					(:ii (vp-store-frc (assign-single-out %0) (first %1) (second %1)))
					(:di (vp-store-frr (assign-single-out %0) (first %1) (second %1)))
					(:k (def *func_env* %1 %0))
					(:t (throw "Invalid dst parameter !" (list %0 %1)))))
				(:ius (vp-load-rcr-us (first %0) (second %0) %1))
				(:iub (vp-load-rcr-ub (first %0) (second %0) %1))
				(:&d (vp-lea-rrr (second %0) (third %0) %1))
				(:dub (vp-load-rrr-ub (first %0) (second %0) %1))
				(:dui (vp-load-rrr-ui (first %0) (second %0) %1))
				(:dus (vp-load-rrr-us (first %0) (second %0) %1))
				(:db (vp-load-rrr-b (first %0) (second %0) %1))
				(:ds (vp-load-rrr-s (first %0) (second %0) %1))
				(:di (pcase (assign-dst-type %1) +pkeys2
					(:f (assign-single-in (const vp-load-rrf) (first %0) (second %0) %1))
					(:t (vp-load-rrr-i (first %0) (second %0) %1))))
				(:is (vp-load-rcr-s (first %0) (second %0) %1))
				(:ib (vp-load-rcr-b (first %0) (second %0) %1))
				(:t (throw "Invalid src parameter !" (list %0 %1))))) src dst)))

(defun assign (&optional src dst)
	; (assign [src dst])
	;registers, memory and constants to registers, a register to memory
	(assign-asm-to-asm (ifn src '()) (ifn dst '())))

;module
(export-symbols '(assign assign-src-type))
(env-pop)

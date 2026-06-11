;;;;;;;;;;;;
; assignment
;;;;;;;;;;;;

;module
(env-push)

(defq +pkey1 ''(:sym :list :num :str)
	+pkeys2 ''(:$ :&d :&i :@ :c :d :db :di :ds :dub :dui :dus
		:f :i :ib :ii :is :iub :iui :ius :k :r :s))

(defmacro assign-type-sym (%0) (static-qqp (sym (str ,%0 "_t"))))

(defun assign-type-t (%0)
	(elem-get '(b ub s us i ui "") (ifn (find (first %0) "bBsSiI") -2)))

(defun assign-src-type (o)
	(pcase (pop (type-of o)) +pkey1
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
	(pcase (pop (type-of o)) +pkey1
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

(defun assign-ignored-to-script ()
	(defq s (list) d (list))
	(each (# (unless (eql %1 "_") (push s %0) (push d %1))) src dst)
	(setq src s dst d))

(defun assign-topology-sort ()
	(defq s src d dst i 0 c 1000 l (dec (length d)))
	(while (and (< i l) (/= c 0))
		(if (setq j (if (list? (defq j (elem-get d i))) :nil
				(some! (lambda (s d)
					(cond
						((and (list? s) (find j s)) (!))
						((and (list? d) (find j d)) (!))
						((eql j s) (!)))) (list s d) :nil -1 (inc i))))
			(setq s (rotate s i j (inc j)) d (rotate d i j (inc j)) c (dec c))
			(++ i)))
	(if (= c 0)
		(throw "Copy cycle detected !" (list s d))
		(setq src s dst d)))

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
					(:i (vp-cpy-ri %0 (first %1) (second %1)))
					(:iui (vp-cpy-ri-i %0 (first %1) (second %1)))
					(:k (def *func_env* %1 %0))
					(:ii (vp-cpy-ri-i %0 (first %1) (second %1)))
					(:d (vp-cpy-rd %0 (first %1) (second %1)))
					(:iub (vp-cpy-ri-b %0 (first %1) (second %1)))
					(:ib (vp-cpy-ri-b %0 (first %1) (second %1)))
					(:f (vp-cpy-rf %0 %1))
					(:ds (vp-cpy-rd-s %0 (first %1) (second %1)))
					(:dub (vp-cpy-rd-b %0 (first %1) (second %1)))
					(:db (vp-cpy-rd-b %0 (first %1) (second %1)))
					(:di (vp-cpy-rd-i %0 (first %1) (second %1)))
					(:is (vp-cpy-ri-s %0 (first %1) (second %1)))
					(:ius (vp-cpy-ri-s %0 (first %1) (second %1)))
					(:dui (vp-cpy-rd-i %0 (first %1) (second %1)))
					(:dus (vp-cpy-rd-s %0 (first %1) (second %1)))
					(:t (throw "Invalid dst parameter !" (list %0 %1)))))
				(:i (pcase (assign-dst-type %1) +pkeys2
					(:r (vp-cpy-ir (first %0) (second %0) %1))
					(:f (vp-cpy-if (first %0) (second %0) %1))))
				(:c (vp-cpy-cr %0 %1))
				(:iui (vp-cpy-ir-ui (first %0) (second %0) %1))
				(:s (fn-string %0 %1))
				(:$ (vp-lea-p (second %0) %1))
				(:&i (vp-lea-i (second %0) (third %0) %1))
				(:@ (fn-bind (second %0) %1))
				(:d (pcase (assign-dst-type %1) +pkeys2
					(:r (vp-cpy-dr (first %0) (second %0) %1))
					(:f (vp-cpy-df (first %0) (second %0) %1))))
				(:ii (vp-cpy-ir-i (first %0) (second %0) %1))
				(:f (pcase (assign-dst-type %1) +pkeys2
					(:r (vp-cpy-fr %0 %1))
					(:d (vp-cpy-fd %0 (first %1) (second %1)))
					(:f (vp-cpy-ff %0 %1))
					(:i (vp-cpy-fi %0 (first %1) (second %1)))
					(:k (def *func_env* %1 %0))
					(:t (throw "Invalid dst parameter !" (list %0 %1)))))
				(:ius (vp-cpy-ir-us (first %0) (second %0) %1))
				(:iub (vp-cpy-ir-ub (first %0) (second %0) %1))
				(:&d (vp-lea-d (second %0) (third %0) %1))
				(:dub (vp-cpy-dr-ub (first %0) (second %0) %1))
				(:dui (vp-cpy-dr-ui (first %0) (second %0) %1))
				(:dus (vp-cpy-dr-us (first %0) (second %0) %1))
				(:db (vp-cpy-dr-b (first %0) (second %0) %1))
				(:ds (vp-cpy-dr-s (first %0) (second %0) %1))
				(:di (vp-cpy-dr-i (first %0) (second %0) %1))
				(:is (vp-cpy-ir-s (first %0) (second %0) %1))
				(:ib (vp-cpy-ir-b (first %0) (second %0) %1))
				(:t (throw "Invalid src parameter !" (list %0 %1))))) src dst)))

(defun print-inst (%0)
	(and (list? %0) (not (lambda? (first %0))) (print (ascii-char 9) %0)))

(defun assign-asm-to-script (src dst %2)
	(unless (= (length src) (length (setq dst (map (const trim) (split dst ",")))))
		(throw "Mismatching number of src/dst parameters !" (list src dst)))
	(assign-ignored-to-script)
	(when (/= 0 (length dst))
		(reset-reg-stack (length src))
		(each (# (cscript %0) (compile-arrow)) dst)
		(when *build_inst*
			(print "pre opt:")
			(each (const print-inst) *inst*))
		(opt-inst-list *inst*)
		(when *build_inst*
			(print "post opt:")
			(each (const print-inst) *inst*))
		(def-reg-map (reverse src) %2)
		(eval *inst* *func_env*)))

(defun assign-script-to-asm (src dst %2)
	(unless (= (length (setq src (split src ","))) (length dst))
		(throw "Mismatching number of src/dst parameters !" (list src dst)))
	(when (/= 0 (length dst))
		(reset-reg-stack 0)
		(each (# (cscript %0) (compile-deref?)) src)
		(when *build_inst*
			(print "pre opt:")
			(each (const print-inst) *inst*))
		(opt-inst-list *inst*)
		(when *build_inst*
			(print "post opt:")
			(each (const print-inst) *inst*))
		(def-reg-map (cat dst) %2)
		(eval *inst* *func_env*)))

(defun assign-script-to-script (src dst %2)
	(setq src (split src ",") dst (map (const trim) (split dst ",")))
	(unless (= (length src) (length dst))
		(throw "Mismatching number of src/dst parameters !" (list src dst)))
	(assign-ignored-to-script)
	(when (/= 0 (length dst))
		(reset-reg-stack 0)
		(each cscript src)
		(reach (# (cscript %0) (compile-arrow)) dst)
		(when *build_inst*
			(print "pre opt:")
			(each (const print-inst) *inst*))
		(opt-inst-list *inst*)
		(when *build_inst*
			(print "post opt:")
			(each (const print-inst) *inst*))
		(def-reg-map :nil %2)
		(eval *inst* *func_env*)))

(defun assign (&optional src dst %2)
	;optional src, dst, compiler regs
	(if (str? (setq src (ifn src '())))
		(if (str? (setq dst (ifn dst '())))
			(assign-script-to-script src dst %2)
			(assign-script-to-asm src dst %2))
		(if (str? (setq dst (ifn dst '())))
			(assign-asm-to-script src dst %2)
			(assign-asm-to-asm src dst))))

;module
(export-symbols '(assign assign-src-type))
(env-pop)

;;;;;;;;;;;;;;;;;;;
; structured coding
;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defmacro beg-sym () (static-q (sym (str "b_" *switch*))))
(defmacro end-sym () (static-q (sym (str "e_" *switch*))))
(defmacro cnt-sym () (static-q (sym (str "_c_" *switch*))))
(defmacro loc-sym (%0) (static-qqp (sym (str "o_" *switch* "_" ,%0))))
(defmacro llb-sym (%0) (static-qqp (sym (cat "_b_" ,%0))))
(defmacro lle-sym (%0) (static-qqp (sym (cat "_e_" ,%0))))

(defun goto (l)
	(vp-jmp l))

(defun gotoif (e l)
	(bind '(d o s) (cond
		((str? e) (assign e '(:r0)) '(:r0 /= 0))
		(e)))
	(defq e (find o '(= /= <= >= > <)))
	(case (assign-src-type s)
		(:c (if e ((eval (elem-get '(vp-beq-cr vp-bne-cr vp-ble-cr vp-bge-cr vp-bgt-cr vp-blt-cr) e)) s d l)
			(throw "No such comparator" o)))
		(:r (if e ((eval (elem-get '(vp-beq-rr vp-bne-rr vp-ble-rr vp-bge-rr vp-bgt-rr vp-blt-rr) e)) s d l)
			(throw "No such comparator" o)))
		(:f (if e ((eval (elem-get '(vp-beq-ff vp-bne-ff vp-ble-ff vp-bge-ff vp-bgt-ff vp-blt-ff) e)) s d l)
			(throw "No such comparator" o)))
		(:t (throw "No such compare mode" (assign-src-type s)))))

(defun gotoifnot (e l)
	(bind '(d o s) (cond
		((str? e) (assign e '(:r0)) '(:r0 /= 0))
		(e)))
	(defq e (find o '(/= = > < <= >=)))
	(case (assign-src-type s)
		(:c (if e ((eval (elem-get '(vp-beq-cr vp-bne-cr vp-ble-cr vp-bge-cr vp-bgt-cr vp-blt-cr) e)) s d l)
			(throw "No such comparator" o)))
		(:r (if e ((eval (elem-get '(vp-beq-rr vp-bne-rr vp-ble-rr vp-bge-rr vp-bgt-rr vp-blt-rr) e)) s d l)
			(throw "No such comparator" o)))
		(:f (if e ((eval (elem-get '(vp-beq-ff vp-bne-ff vp-ble-ff vp-bge-ff vp-bgt-ff vp-blt-ff) e)) s d l)
			(throw "No such comparator" o)))
		(:t (throw "No such compare mode" (assign-src-type s)))))

(defun switch (&optional %0)
	(push *switch_stk* *switch*)
	(setq *switch* *switch_nxt* *switch_nxt* (inc *switch_nxt*))
	(when %0 (deffvar (llb-sym %0) (beg-sym) (lle-sym %0) (end-sym)))
	(deffvar (cnt-sym) 0))

(defun default ()
	(vp-label (loc-sym (defq s (cnt-sym) c (eval s))))
	(set (penv) s (inc c)))

(defun endswitch ()
	(vp-label (end-sym))
	(default)
	(setq *switch* (pop *switch_stk*)))

(defun nextcaseif (e)
	(gotoif e (loc-sym (eval (cnt-sym)))))

(defun nextcaseifnot (e)
	(gotoifnot e (loc-sym (eval (cnt-sym)))))

(defun exitif (e)
	(gotoif e (if l (eval (lle-sym l)) (end-sym))))

(defun exitifnot (e)
	(gotoifnot e (if l (eval (lle-sym l)) (end-sym))))

(defun repeatif (e)
	(gotoif e (if l (eval (llb-sym l)) (beg-sym))))

(defun repeatifnot (e)
	(gotoifnot e (if l (eval (llb-sym l)) (beg-sym))))

(defun break (&optional l)
	(goto (if l (eval (lle-sym l)) (end-sym))))

(defun continue (&optional l)
	(goto (if l (eval (llb-sym l)) (beg-sym))))

(defun breakif (&rest e)
	(unless (sym? (defq i -2 l (last e)))
		(setq i -1 l :nil))
	(each! (const exitif) (list e) 0 i))

(defun breakifnot (&rest e)
	(unless (sym? (defq i -2 l (last e)))
		(setq i -1 l :nil))
	(each! (const exitifnot) (list e) 0 i))

(defun continueif (&rest e)
	(unless (sym? (defq i -2 l (last e)))
		(setq i -1 l :nil))
	(each! (const repeatif) (list e) 0 i))

(defun continueifnot (&rest e)
	(unless (sym? (defq i -2 l (last e)))
		(setq i -1 l :nil))
	(each! (const repeatifnot) (list e) 0 i))

(defun vpcase (&rest e)
	(default)
	(each! (const nextcaseifnot) (list e)))

(defun vpcasenot (&rest e)
	(default)
	(each! (const nextcaseif) (list e)))

(defun loop-start (&optional l)
	(switch l)
	(vp-label (beg-sym)))

(defun loop-while (&rest e)
	(unless (sym? (defq i -2 l (last e)))
		(setq i -1 l :nil))
	(switch l)
	(vp-label (beg-sym))
	(each! (const exitifnot) (list e) 0 i))

(defun loop-whilenot (&rest e)
	(unless (sym? (defq i -2 l (last e)))
		(setq i -1 l :nil))
	(switch l)
	(vp-label (beg-sym))
	(each! (const exitif) (list e) 0 i))

(defun loop-end ()
	(continue)
	(endswitch))

(defun loop-until (&rest e)
	(unless (sym? (defq i -2 l (last e)))
		(setq i -1 l :nil))
	(each! (const repeatifnot) (list e) 0 i)
	(endswitch))

(defun loop-untilnot (&rest e)
	(unless (sym? (defq i -2 l (last e)))
		(setq i -1 l :nil))
	(each! (const repeatif) (list e) 0 i)
	(endswitch))

(defun vpif (&rest e)
	(unless (sym? (defq i -2 l (last e)))
		(setq i -1 l :nil))
	(switch l)
	(each! (const nextcaseifnot) (list e) 0 i))

(defun vpifnot (&rest e)
	(unless (sym? (defq i -2 l (last e)))
		(setq i -1 l :nil))
	(switch l)
	(each! (const nextcaseif) (list e) 0 i))

(defun else ()
	(break)
	(default))

(defun elseif (&rest e)
	(else)
	(each! (const nextcaseifnot) (list e)))

(defun elseifnot (&rest e)
	(else)
	(each! (const nextcaseif) (list e)))

(defun endif ()
	(endswitch))

;;;;;;;;;;;;
; errorcase
;;;;;;;;;;;;

(defmacro validatecase (&rest e) (if (>= *build_mode* 2) `(progn ~e)))
(defmacro errorcase (&rest e) (if (>= *build_mode* 1) `(progn ~e)))
(defmacro noterrorcase (&rest e) (if (<= *build_mode* 0) `(progn ~e)))
(defmacro errorif (e l) `(errorcase (gotoif ,e ,l)))
(defmacro errorifnot (e l) `(errorcase (gotoifnot ,e ,l)))

(defmacro errorif-lisp-args-len (l args cnd len)
	`(errorcase
		(assign '((,args +array_length)) '(:r2))
		(gotoif '(:r2 ,cnd ,len) ,l)))

(defmacro errorif-lisp-args-sig (l args len1 &optional len2)
	(setd len2 len1)
	`(errorcase
		(call :lisp :env_args_sig '(,args ($ sig) ,len1 ,len2) '(tmp))
		(gotoif `(,tmp = 0) ,l)))

(defmacro errorif-lisp-args-match (l args class len)
	`(errorcase
		(call :lisp :env_args_match '(,args (@ ,(f-path class :vtable)) ,len) '(tmp))
		(gotoif `(,tmp = 0) ,l)))

(defmacro errorif-lisp-args-type (l args class len)
	`(errorcase
		(call :lisp :env_args_type '(,args (@ ,(f-path class :vtable)) ,len) '(tmp))
		(gotoif `(,tmp = 0) ,l)))

;module
(export-symbols
	'(errorcase noterrorcase errorif errorifnot
	errorif-lisp-args-len errorif-lisp-args-sig errorif-lisp-args-sig-range
	errorif-lisp-args-match errorif-lisp-args-type
	switch endswitch vpcase vpcasenot default nextcaseif nextcaseifnot
	vpif vpifnot else elseif elseifnot endif validatecase
	break breakif breakifnot goto gotoif gotoifnot exitif exitifnot
	loop-start loop-end loop-while loop-whilenot loop-until loop-untilnot
	continue continueif continueifnot repeatif repeatifnot))
(env-pop)

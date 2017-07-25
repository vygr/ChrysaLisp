;;;;;;;;;;;;;
; Emit Buffer
;;;;;;;;;;;;;

(defcvar '*emit-buffer* (list) '*distance* (list) '*out-buffer* (cat "") '*old-out-buffer* (cat ""))

(defmacro emit (f)
	`(push *emit-buffer* ,f))

(defcfun emit-reset ()
	(push (clear *distance* *emit-buffer*) progn))

(defcfun emit-passes ()
	(defq *pass* 0 *pc* 0 *out-stream* (string-stream *out-buffer*) b nil)
	(eval *emit-buffer*)
	(setq *out-buffer* (str *out-stream*) *pass* (inc *pass*))
	(until (eql *out-buffer* *old-out-buffer*)
		(setq b *old-out-buffer* *old-out-buffer* *out-buffer* *out-stream* (string-stream b) *pc* 0)
		(eval *emit-buffer*)
		(setq *out-buffer* (str *out-stream*) *pass* (inc *pass*))))

(defcfun print-emit (_)
	(and (lst? _) (not (eql (elem 0 _) 'lambda)) (print _)))

(defcfun emit-label (_)
	(set _ *pc*))

(defcfun emit-byte (&rest _)
	(defq s *out-stream*)
	(each! nil nil nil (lambda (_)
		(write-char s _ 1)) (list _))
	(setq *pc* (add *pc* (length _))))

(defcfun emit-short (&rest _)
	(defq s *out-stream*)
	(each! nil nil nil (lambda (_)
		(write-char s _ 2)) (list _))
	(setq *pc* (add *pc* (mul (length _) 2))))

(defcfun emit-int (&rest _)
	(defq s *out-stream*)
	(each! nil nil nil (lambda (_)
		(write-char s _ 4)) (list _))
	(setq *pc* (add *pc* (mul (length _) 4))))

(defcfun emit-long (&rest _)
	(defq s *out-stream*)
	(each! nil nil nil (lambda (_)
		(write-char s _ 8)) (list _))
	(setq *pc* (add *pc* (mul (length _) 8))))

(defcfun emit-string (_)
	(write *out-stream* _)
	(setq *pc* (add *pc* (length _))))

(defcfun emit-align (a &optional b)
	(when (ne *pass* 0)
		(defq p *pc* b (opt b 0) s *out-stream*)
		(setq *pc* (align p a))
		(while (le (setq p (inc p)) *pc*)
			(write-char s b))))

(defmacro opt-emit-kill-list ()
	''(emit-label emit-pop emit-xchg-rr emit-div-rrr emit-div-rrr-u
		emit-syscall emit-call emit-call-i emit-call-r emit-call-p))

(defmacro opt-emit-ok-list ()
	''(emit-cmp-cr emit-cmp-rr))

(defcfun opt-emit-find-rw (_ r w b i)
	(while (and (gt (setq _ (dec _)) 0)
				(not (or (and (eql w (defq o (elem 0 (defq e (elem _ *emit-buffer*)))))
							(eql b (elem 2 e))
							(eq i (eval (elem 3 e)))
							(setq c 1))
						(and (eql r o)
							(eql b (elem 1 e))
							(not (eql b (elem 3 e)))
							(eq i (eval (elem 2 e)))
							(setq c 3)))))
		(and (or (eql b (elem -2 e))
				(find o (opt-emit-kill-list)))
			(setq _ 0)))
	(if (gt _ 0) _))

(defcfun opt-emit-find-use (_ o b)
	(defq l (length *emit-buffer*))
	(while (and (lt (setq _ (inc _)) l)
				(not (and (eql o (elem 0 (defq e (elem _ *emit-buffer*))))
						(eql b (elem 1 e)))))
		(and (or (find b e)
				(find (elem 0 e) (opt-emit-kill-list)))
			(setq _ l)))
	(if (lt _ l) _))

(defcfun opt-emit-buffer ()
	(each! 2 nil nil (lambda (e)
		(cond
			;read after read/write
			((defq c (find (defq o (elem 0 e)) '(emit-cpy-ir-ui emit-cpy-ir-us emit-cpy-ir-ub
						 				emit-cpy-ir-i emit-cpy-ir-s emit-cpy-ir-b emit-cpy-ir)))
				(when (defq w (opt-emit-find-rw _
								(elem c '(emit-cpy-ir-ui emit-cpy-ir-us emit-cpy-ir-ub
								 		emit-cpy-ir-i emit-cpy-ir-s emit-cpy-ir-b emit-cpy-ir))
								(elem c '(emit-cpy-ri-i emit-cpy-ri-s emit-cpy-ri-b
										emit-cpy-ri-i emit-cpy-ri-s emit-cpy-ri-b emit-cpy-ri))
								(elem 1 e) (eval (elem 2 e))))
						(defq r (elem c (elem w *emit-buffer*)))
						(when (not (some! (inc w) _ t (lambda (_)
								(and (not (find (elem 0 _) (opt-emit-ok-list)))
									(eql r (elem -2 _)))) (list *emit-buffer*)))
							(elem-set _ *emit-buffer*
								(if (eql r (elem 3 e))
									(lambda)
									(list 'emit-cpy-rr r (elem 3 e)))))))
			;compare constant forwarding
			((and (eql o 'emit-cpy-cr) (le -0x80000000 (eval (elem 1 e)) 0x7fffffff))
				(when (defq u (opt-emit-find-use _ 'emit-cmp-rr (elem 2 e)))
					(elem-set u *emit-buffer* (list 'emit-cmp-cr (elem 1 e) (elem 2 (elem u *emit-buffer*))))
					(elem-set _ *emit-buffer* (lambda))))))
	(list *emit-buffer*)))

;;;;;;;;;;;;;;;;;
; VP Instructions
;;;;;;;;;;;;;;;;;

(each (lambda (_) (defcvar (defq r (sym-cat "r" (str _))) r)) (range 0 16))

(defcfun type (o)
	(defq i (obj? o))
	(cond
		((not i) i)
		((eql i 'class/class_symbol)
			(cond
				((reg o) 'r)
				(t 'c)))
		((eql i 'class/class_vector)
			(setq i (elem 0 o))
			(cond
				((reg i)
					(if (reg (elem 1 o)) 'd
						(if (eq 2 (length o)) 'i
							(sym-cat "i" (elem 2 o)))))
				((eql i '@) i)
				((eql i '$) i)
				((eql i '&)
					(if (reg (elem 1 o))
						(if (reg (elem 2 o)) '&d '&i) nil))
				(t 'c)))
		((eql i 'class/class_string) 's)
		((eql i 'class/class_boxed_long) 'c)
		(t nil)))

(defcfun label-sym (s)
	(sym-cat "_lb_" s))

(defcfun vp-label (l) (setq l (label-sym l)) (emit `(emit-label ',l)) (defcvar l 0))
(defcfun vp-align (a &optional b) (emit `(emit-align ,a ,b)))
(defcfun vp-string (s) (emit `(emit-string ,s)))
(defcfun vp-byte (&rest b) (emit `(emit-byte ~b)))
(defcfun vp-short (&rest b) (emit `(emit-short ~b)))
(defcfun vp-int (&rest b) (emit `(emit-int ~b)))
(defcfun vp-long (&rest b) (emit `(emit-long ~b)))

(defcfun vp-jmpif ((d o s) l)
	(defq c (type s))
	(cond
		((eql c 'c) (vp-cmp-cr s d))
		((eql c 'r) (vp-cmp-rr s d))
		(t (throw "No such compare mode" c)))
	(cond
		((eql o '==) (vp-beq l))
		((eql o '!=) (vp-bne l))
		((eql o '<) (vp-blt l))
		((eql o '>) (vp-bgt l))
		((eql o '<=) (vp-ble l))
		((eql o '>=) (vp-bge l))
		(t (throw "No such comparator" o))))

(defcfun vp-jmpifnot ((d o s) l)
	(defq c (type s))
	(cond
		((eql c 'c) (vp-cmp-cr s d))
		((eql c 'r) (vp-cmp-rr s d))
		(t (throw "No such compare mode" c)))
	(cond
		((eql o '==) (vp-bne l))
		((eql o '!=) (vp-beq l))
		((eql o '<) (vp-bge l))
		((eql o '>) (vp-ble l))
		((eql o '<=) (vp-bgt l))
		((eql o '>=) (vp-blt l))
		(t (throw "No such comparator" o))))

(defcfun vp-beq (l) (emit `(emit-beq ,(label-sym l) ,(dec (length (push *distance* 0))))))
(defcfun vp-bne (l) (emit `(emit-bne ,(label-sym l) ,(dec (length (push *distance* 0))))))
(defcfun vp-blt (l) (emit `(emit-blt ,(label-sym l) ,(dec (length (push *distance* 0))))))
(defcfun vp-ble (l) (emit `(emit-ble ,(label-sym l) ,(dec (length (push *distance* 0))))))
(defcfun vp-bgt (l) (emit `(emit-bgt ,(label-sym l) ,(dec (length (push *distance* 0))))))
(defcfun vp-bge (l) (emit `(emit-bge ,(label-sym l) ,(dec (length (push *distance* 0))))))

(defcfun vp-push (&rest b) (emit `(emit-push ~b)))
(defcfun vp-pop (&rest b) (emit `(emit-pop ~b)))

(defcfun vp-call (l) (emit `(emit-call ,(label-sym l))))
(defcfun vp-call-r (d) (emit `(emit-call-r ,d)))
(defcfun vp-call-i (b i) (emit `(emit-call-i ,b ,i)))
(defcfun vp-call-p (l) (emit `(emit-call-p ,(label-sym l))))

(defcfun vp-jmp (l) (emit `(emit-jmp ,(label-sym l) ,(dec (length (push *distance* 0))))))
(defcfun vp-jmp-r (d) (emit `(emit-jmp-r ,d)))
(defcfun vp-jmp-i (b i) (emit `(emit-jmp-i ,b ,i)))
(defcfun vp-jmp-p (l) (emit `(emit-jmp-p ,(label-sym l))))

(defcfun vp-lea-i (b i d) (emit `(emit-lea-i ,b ,i ,d)))
(defcfun vp-lea-d (b i d) (emit `(emit-lea-d ,b ,i ,d)))
(defcfun vp-lea-p (l r) (emit `(emit-lea-p ,(label-sym l) ,r)))

(defcfun vp-cpy-cr (c d) (emit `(emit-cpy-cr ,c ,d)))
(defcfun vp-cpy-rr (s d) (emit `(emit-cpy-rr ,s ,d)))
(defcfun vp-cpy-ir (b i d) (emit `(emit-cpy-ir ,b ,i ,d)))
(defcfun vp-cpy-dr (b i d) (emit `(emit-cpy-dr ,b ,i ,d)))
(defcfun vp-cpy-pr (l d) (emit `(emit-cpy-pr ,(label-sym l) ,d)))
(defcfun vp-cpy-ri (s b i) (emit `(emit-cpy-ri ,s ,b ,i)))
(defcfun vp-cpy-rd (s b i) (emit `(emit-cpy-rd ,s ,b ,i)))
(defcfun vp-cpy-rp (s l) (emit `(emit-cpy-rp ,s ,(label-sym l))))

(defcfun vp-cpy-ir-b (b i d) (emit `(emit-cpy-ir-b ,b ,i ,d)))
(defcfun vp-cpy-dr-b (b i d) (emit `(emit-cpy-dr-b ,b ,i ,d)))
(defcfun vp-cpy-ir-ub (b i d) (emit `(emit-cpy-ir-ub ,b ,i ,d)))
(defcfun vp-cpy-dr-ub (b i d) (emit `(emit-cpy-dr-ub ,b ,i ,d)))
(defcfun vp-cpy-ir-s (b i d) (emit `(emit-cpy-ir-s ,b ,i ,d)))
(defcfun vp-cpy-dr-s (b i d) (emit `(emit-cpy-dr-s ,b ,i ,d)))
(defcfun vp-cpy-ir-us (b i d) (emit `(emit-cpy-ir-us ,b ,i ,d)))
(defcfun vp-cpy-dr-us (b i d) (emit `(emit-cpy-dr-us ,b ,i ,d)))
(defcfun vp-cpy-ir-i (b i d) (emit `(emit-cpy-ir-i ,b ,i ,d)))
(defcfun vp-cpy-dr-i (b i d) (emit `(emit-cpy-dr-i ,b ,i ,d)))
(defcfun vp-cpy-ir-ui (b i d) (emit `(emit-cpy-ir-ui ,b ,i ,d)))
(defcfun vp-cpy-dr-ui (b i d) (emit `(emit-cpy-dr-ui ,b ,i ,d)))

(defcfun vp-cpy-ri-b (s b i) (emit `(emit-cpy-ri-b ,s ,b ,i)))
(defcfun vp-cpy-rd-b (s b i) (emit `(emit-cpy-rd-b ,s ,b ,i)))
(defcfun vp-cpy-ri-s (s b i) (emit `(emit-cpy-ri-s ,s ,b ,i)))
(defcfun vp-cpy-rd-s (s b i) (emit `(emit-cpy-rd-s ,s ,b ,i)))
(defcfun vp-cpy-ri-i (s b i) (emit `(emit-cpy-ri-i ,s ,b ,i)))
(defcfun vp-cpy-rd-i (s b i) (emit `(emit-cpy-rd-i ,s ,b ,i)))

(defcfun vp-add-cr (c d) (emit `(emit-add-cr ,c ,d)))
(defcfun vp-add-rr (s d) (emit `(emit-add-rr ,s ,d)))
(defcfun vp-sub-cr (c d) (emit `(emit-sub-cr ,c ,d)))
(defcfun vp-sub-rr (s d) (emit `(emit-sub-rr ,s ,d)))
(defcfun vp-cmp-cr (c d) (emit `(emit-cmp-cr ,c ,d)))
(defcfun vp-cmp-rr (s d) (emit `(emit-cmp-rr ,s ,d)))
(defcfun vp-mul-cr (c d) (emit `(emit-mul-cr ,c ,d)))
(defcfun vp-mul-rr (s d) (emit `(emit-mul-rr ,s ,d)))
(defcfun vp-and-cr (c d) (emit `(emit-and-cr ,c ,d)))
(defcfun vp-and-rr (s d) (emit `(emit-and-rr ,s ,d)))
(defcfun vp-or-cr (c d) (emit `(emit-or-cr ,c ,d)))
(defcfun vp-or-rr (s d) (emit `(emit-or-rr ,s ,d)))
(defcfun vp-xor-cr (c d) (emit `(emit-xor-cr ,c ,d)))
(defcfun vp-xor-rr (s d) (emit `(emit-xor-rr ,s ,d)))

(defcfun vp-shl-cr (c d) (emit `(emit-shl-cr ,c ,d)))
(defcfun vp-shl-rr (s d) (emit `(emit-shl-rr ,s ,d)))
(defcfun vp-shr-cr (c d) (emit `(emit-shr-cr ,c ,d)))
(defcfun vp-shr-rr (s d) (emit `(emit-shr-rr ,s ,d)))
(defcfun vp-asr-cr (c d) (emit `(emit-asr-cr ,c ,d)))
(defcfun vp-asr-rr (s d) (emit `(emit-asr-rr ,s ,d)))

(defcfun vp-ret () (emit '(emit-ret)))
(defcfun vp-xchg-rr (s d) (emit `(emit-xchg-rr ,s ,d)))
(defcfun vp-ext-rr (s d) (emit `(emit-ext-rr ,(dec (length (push *distance* 0))) ,s ,d)))
(defcfun vp-div-rrr (d r q) (emit `(emit-div-rrr ,d ,r ,q)))
(defcfun vp-div-rrr-u (d r q) (emit `(emit-div-rrr-u ,d ,r ,q)))
(defcfun vp-syscall () (emit `(emit-syscall)))

;;;;;;;;;;;;;;;;;;;;;
; Arch Emit Functions
;;;;;;;;;;;;;;;;;;;;;

(cond
((eql *cpu* 'x86_64)
(import 'sys/x64.inc))
((eql *cpu* 'ARM)
(import 'sys/arm.inc))
(t (throw "No such CPU !" *cpu*)))

;;;;;;;;;;;;;;;;;;;
; C-Script compiler
;;;;;;;;;;;;;;;;;;;

;so we don't trash anything in the parent environment
;while we build up the functions...
(env-push)

(defq +symbol_chars "+ABCDEFGHIJKLMNOPQRSTUVWXYZ_/0123456789abcdefghijklmnopqrstuvwxyz"
	+path_chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ_/0123456789abcdefghijklmnopqrstuvwxyz"
	+label_chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_abcdefghijklmnopqrstuvwxyz"
	+number_chars "0123456789"
	+folow_on_number_chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.0123456789"
	+op_chars "+-*/%~^<=>!&|."
	+folow_on_op_chars "/%<=>&|"
	+bracket_chars "()[]")

(defmacro is_symbol_char (_)
	(list 'find-rev _ +symbol_chars))

(defmacro is_path_char (_)
	(list 'find-rev _ +path_chars))

(defmacro is_label_char (_)
	(list 'find-rev _ +label_chars))

(defmacro is_number_char (_)
	(list 'find-rev _ +number_chars))

(defmacro is_folow_on_number_char (_)
	(list 'find-rev _ +folow_on_number_chars))

(defmacro is_op_char (_)
	(list 'find-rev _ +op_chars))

(defmacro is_folow_on_op_char (_)
	(list 'find-rev _ +folow_on_op_chars))

(defmacro is_bracket_char (_)
	(list 'find-rev _ +bracket_chars))

(defun cscript-tokenize (line)
	(defq state :space unary t token (clear '()) out_types (clear '()) out_tokens (clear '()))
	(each (lambda (c)
		(while c
			(case state
				(:space
					(cond
						((eql c (ascii-char 34))
							(setq state :string))
						((eql c "@")
							(setq state :path))
						((eql c "$")
							(setq state :label))
						((and (eql c "+") unary)
							(push token c)
							(setq state :symbol))
						((is_op_char c)
							(push token c)
							(setq state :operator))
						((is_number_char c)
							(push token c)
							(setq state :number))
						((is_symbol_char c)
							(push token c)
							(setq state :symbol))
						((defq _ (is_bracket_char c))
							(push out_types (elem _ '(:lrb :rrb :lsb :rsb)))
							(push out_tokens c)
							(setq unary (elem _ '(t nil t nil)))))
					(setq c nil))
				(:symbol
					(cond
						((is_symbol_char c)
							(push token c)
							(setq c nil))
						(t	(cond
								((find-rev (elem -2 (push out_tokens (apply cat token)))
										'("pptr" "byte" "ubyte" "short" "ushort" "int" "uint" "long" "ulong" "ptr"))
									(push out_types :operator))
								(t	(push out_types state)))
							(clear token)
							(setq state :space unary nil))))
				(:number
					(cond
						((is_folow_on_number_char c)
							(push token c)
							(setq c nil))
						(t	(push out_types state)
							(push out_tokens (apply cat token))
							(clear token)
							(setq state :space unary nil))))
				(:string
					(cond
						((eql c (ascii-char 34))
							(push out_types state)
							(push out_tokens (apply cat token))
							(clear token)
							(setq state :space unary nil))
						(t 	(push token c)))
					(setq c nil))
				(:path
					(cond
						((is_path_char c)
							(push token c)
							(setq c nil))
						(t 	(push out_types state)
							(push out_tokens (apply cat token))
							(clear token)
							(setq state :space unary nil))))
				(:label
					(cond
						((is_label_char c)
							(push token c)
							(setq c nil))
						(t 	(push out_types state)
							(push out_tokens (apply cat token))
							(clear token)
							(setq state :space unary nil))))
				(:operator
					(cond
						((is_folow_on_op_char c)
							(push token c)
							(setq c nil))
						(t	(cond
								(unary
									(if (setq _ (find-rev (elem 0 token) "-*&~!"))
										(elem-set 0 token (elem _ "_#:~!"))
										(throw "Unknown unary operator !" (elem 0 token))))
								(t	(setq unary t)))
						 	(push out_types state)
							(push out_tokens (apply cat token))
							(clear token)
							(setq state :space))))))) line)
	(unless (eql state :space)
		(push out_types state)
		(push out_tokens (apply cat token)))
	(list out_tokens out_types))

;;;;;;;;;;;;;;;;
; reverse polish
;;;;;;;;;;;;;;;;

(defun cscript-reverse-polish (tokenize_output)
	(defq token_stack '() out_tokens (clear '()) out_types (clear '()))
	(each! 0 -1 (lambda (token type)
		(case type
			((:lrb :lsb)
				(push token_stack token))
			(:rrb
				(while (and (not (eql "(" (setq token (pop token_stack)))) token)
					(push out_tokens token)
					(push out_types :operator)))
			(:rsb
				(while (and (not (eql "[" (setq token (pop token_stack)))) token)
					(push out_tokens token)
					(push out_types :operator))
				(push out_tokens "[]")
				(push out_types :operator))
			(:operator
				;precidence
				(unless (defq s (get-sym token))
					(throw "Operator not defined !" token))
				(defq v (- (elem 2 s) (elem 3 s)) x t)
				(while (and x (/= 0 (length token_stack)))
					(setq s (get-sym (elem -2 token_stack)))
					(cond
						((>= v (elem 2 s))
							(push out_tokens (pop token_stack))
							(push out_types :operator))
						(t	(setq x nil))))
				(push token_stack token))
			(t	;number, symbol, string, symbol, path, label
				(push out_tokens token)
				(push out_types type)))) tokenize_output)
	(while (defq _ (pop token_stack))
		(push out_tokens _)
		(push out_types :operator))
	(list out_tokens out_types))

;;;;;;;;;;;;;
; compilation
;;;;;;;;;;;;;

(defcvar '*vreg* '(_v0 _v1 _v2 _v3 _v4 _v5 _v6 _v7 _v8 _v9 _v10 _v11 _v12 _v13 _v14 _v15))
(each (lambda (_) (defcvar _ nil)) *vreg*)

(defmacro vreg-sym (_)
	(list 'elem _ '*vreg*))

(defmacro add-inst (&rest b)
	(cat '(push *inst*) b))

(defmacro set-type (_)
	(list 'elem-set -2 '*vregt* _))

(defmacro get-type ()
	'(elem -2 *vregt*))

(defmacro top-reg ()
	'(vreg-sym (dec (length *vregt*))))

(defmacro tmp-reg ()
	'(vreg-sym (length *vregt*)))

(defmacro reset-reg-stack (_)
	`(defq *vreg* *vreg* *inst* (push (clear '()) progn)
		*vregt* (slice 0 ,_ '(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))))

(defun set-reg-map (l &optional _)
	(setd _ '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
	(each (lambda (_ r) (set (env) _ r)) *vreg* (if (/= 0 (length l)) (merge-obj l _) _)))

(defun push-reg (_)
	(vreg-sym (dec (length (push *vregt* _)))))

(defun pop-reg ()
	(list (vreg-sym (dec (length *vregt*))) (pop *vregt*)))

(defun compile-deref ()
	(if (defq x (top-reg) w (get-type))
		(defq z (slice 1 -1 w) z (if (eql z "") nil z) w (elem 0 w))
		(throw "No type info !" x))
	(set-type z)
	(setq w (elem (find-rev w "bBsSiIlLp") '(vp-cpy-ir-b vp-cpy-ir-ub vp-cpy-ir-s vp-cpy-ir-us
										vp-cpy-ir-i vp-cpy-ir-ui vp-cpy-ir vp-cpy-ir vp-cpy-ir)))
	(add-inst (list w x 0 x)))

(defun compile-deref? ()
	(if (get-type)
		(compile-deref)))

(defun pop-value ()
	(compile-deref?)
	(pop *vregt*)
	(vreg-sym (length *vregt*)))

(defun top-value ()
	(when (get-type)
		(compile-deref)
		(set-type nil))
	(top-reg))

(defun compile-null ()
	(throw "Null operator !" nil))

(defun compile-const (_)
	(add-inst (list 'vp-cpy-cr _ (push-reg nil))))

(defun compile-arrow (&optional _)
	(bind '((y w) x) (list (pop-reg) (pop-value)))
	(if _ (push-reg nil))
	(setq w (elem (find-rev w "bBsSiIlLp") '(vp-cpy-ri-b vp-cpy-ri-b vp-cpy-ri-s vp-cpy-ri-s
										vp-cpy-ri-i vp-cpy-ri-i vp-cpy-ri vp-cpy-ri vp-cpy-ri)))
	(add-inst (list w x y 0)))

(defun compile-ref (_)
	(cond
		((not (defq s (get-sym _)))
			;not in symbol table so figure out what it is
			(cond
				((get (sym (str _ "_t")))
					;field/member
					(add-inst (list 'vp-cpy-cr _ (push-reg (eval (sym (str _ "_t")))))))
				((get _)
					;equate
					(compile-const _))
				(t (throw "Symbol not defined !" _))))
		((eql 'var (elem 1 s))
			;variable
			(add-inst (list 'vp-lea-i rsp (+ (get-scope (elem 0 s)) (elem 2 s))
									(push-reg (elem 3 s)))))
		(t (throw "Symbol not a variable !" _))))

(defun compile-cast (_)
	(if (defq c (find-rev (sym _) '(pptr byte ubyte short ushort int uint long ulong ptr)))
		(set-type (elem c '("pp" "b" "B" "s" "S" "i" "I" "l" "L" "p")))
		(throw "Unknown type cast" _)))

(defun compile-member (_)
	(bind '(_ w) (pop-reg))
	(compile-deref)
	(set-type w)
	(add-inst (list 'vp-add-rr _ (top-reg))))

(defun compile-uaddrof (_)
	(set-type nil))

(defun compile-field (_)
	(bind '(_ w) (pop-reg))
	(set-type w)
	(add-inst (list 'vp-add-rr _ (top-reg))))

(defun compile-index (_)
	(defq _ (pop-value))
	(compile-deref)
	(add-inst (list 'vp-add-rr _ (top-reg))))

(defun compile-uminus (_)
	(add-inst (list 'vp-mul-cr -1 (top-value))))

(defun compile-uderef (_)
	(compile-deref))

(defun compile-unot (_)
	(add-inst (list 'vp-xor-cr -1 (top-value))))

(defun compile-mul (_)
	(add-inst (list 'vp-mul-rr (pop-value) (top-value))))

(defun compile-fmul (_)
	(add-inst
		(list 'vp-mul-rr (pop-value) (defq _ (top-value)))
		(list 'vp-asr-cr 16 _)))

(defun compile-divu (_)
	(defq _ (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x _)
		(list 'vp-div-rrr-u z _ x)))

(defun compile-remu (_)
	(defq _ (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x _)
		(list 'vp-div-rrr-u z _ x)
		(list 'vp-cpy-rr _ x)))

(defun compile-div (_)
	(defq _ (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x _)
		(list 'vp-div-rrr z _ x)))

(defun compile-fdiv (_)
	(defq _ (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-shl-cr 16 x)
		(list 'vp-ext-rr x _)
		(list 'vp-div-rrr z _ x)))

(defun compile-rem (_)
	(defq _ (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x _)
		(list 'vp-div-rrr z _ x)
		(list 'vp-cpy-rr _ x)))

(defun compile-plus (_)
	(add-inst (list 'vp-add-rr (pop-value) (top-value))))

(defun compile-minus (_)
	(add-inst (list 'vp-sub-rr (pop-value) (top-value))))

(defun compile-lshift (_)
	(add-inst (list 'vp-shl-rr (pop-value) (top-value))))

(defun compile-rshift (_)
	(add-inst (list 'vp-shr-rr (pop-value) (top-value))))

(defun compile-arshift (_)
	(add-inst (list 'vp-asr-rr (pop-value) (top-value))))

(defun compile-cmp (_)
	(add-inst (list (elem (find-rev _ '(= /= <= >= > <))
		'(vp-seq-rr vp-sne-rr vp-sle-rr vp-sge-rr vp-sgt-rr vp-slt-rr))
		(pop-value) (top-value))))

(defun compile-and (_)
	(add-inst (list 'vp-and-rr (pop-value) (top-value))))

(defun compile-xor (_)
	(add-inst (list 'vp-xor-rr (pop-value) (top-value))))

(defun compile-or (_)
	(add-inst (list 'vp-or-rr (pop-value) (top-value))))

(defun compile-ulnot (_)
	(add-inst (list 'vp-lnot-rr (tmp-reg) (top-value))))

(defun compile-land (_)
	(add-inst (list 'vp-land-rr (pop-value) (top-value))))

(defun compile-lor (_)
	(add-inst (list 'vp-or-rr (pop-value) (top-value))))

(defun compile-operator (_)
	(cond
		((not (defq s (get-sym _)))
			(throw "Operator not defined !" _))
		((not (eql 'op (elem 1 s)))
			(throw "Not an operator !" _))
		(t	((elem 4 s) _))))

(defun compile-string (_)
	(add-inst (list 'fn-string _ (push-reg nil))))

(defun compile-bind (_)
	(add-inst `(fn-bind ',_ ,(push-reg nil))))

(defun compile-label (_)
	(add-inst `(vp-lea-p ',_ ,(push-reg nil))))

(defun cscript-compile (rpn_output)
	(each! 0 -1 (lambda (token type)
		(case type
			(:operator (compile-operator (sym token)))
			(:number (compile-const (str-to-num token)))
			(:symbol (compile-ref (sym token)))
			(:path (compile-bind (sym token)))
			(:label (compile-label (sym token)))
			(:string (compile-string token)))) rpn_output))

;;;;;;;;;;;
; optimizer
;;;;;;;;;;;

(defmacro opt-sxx-cr-ops ()
	''(vp-seq-cr vp-sne-cr vp-sle-cr vp-sge-cr vp-sgt-cr vp-slt-cr))

(defmacro opt-sxx-rr-ops ()
	''(vp-seq-rr vp-sne-rr vp-sle-rr vp-sge-rr vp-sgt-rr vp-slt-rr))

(defmacro opt-read-ops ()
	''(vp-cpy-ir vp-cpy-ir-b vp-cpy-ir-s vp-cpy-ir-i vp-cpy-ir-ub vp-cpy-ir-us vp-cpy-ir-ui))

(defmacro opt-write-ops ()
	''(vp-cpy-ri vp-cpy-ri-b vp-cpy-ri-s vp-cpy-ri-i))

(defmacro opt-rr-ops ()
	''(vp-add-rr vp-sub-rr vp-mul-rr vp-and-rr vp-or-rr vp-xor-rr vp-shl-rr vp-shr-rr vp-asr-rr))

(defmacro opt-cr-ops ()
	''(vp-add-cr vp-sub-cr vp-mul-cr vp-and-cr vp-or-cr vp-xor-cr vp-shl-cr vp-shr-cr vp-asr-cr))

(defmacro opt-fold-cpy-ops ()
	''(+ - * logand logior logxor << >> >>>))

(defmacro opt-fold-cr-ops ()
	''(+ + * logand logior logxor + + +))

(defmacro opt-cr-fuse-ops ()
	''(vp-add-cr vp-sub-cr))

(defmacro opt-uses (r _)
	`(find-rev ,r ,_))

(defun opt-find-1 (_ r x)
	(setq _ (some! _ 0 nil (lambda (e)
				(cond
					((match? e x) _)
					((opt-uses r e) -1))) (list *inst*))
		p (and _ (/= _ -1) _)))

(defun opt-find-2 (_ r x y)
	(setq _ (some! _ 0 nil (lambda (e)
				(cond
					((match? e x) (setq m 0) _)
					((match? e y) (setq m 1) _)
					((opt-uses r e) -1))) (list *inst*))
		p (and _ (/= _ -1) _)))

(defun opt-inst-list ()
	(defq i 0 p nil m nil)
	(while (< (setq i (inc i)) (length *inst*))
		(cond
			;variable loading and offset folding
			((find-rev (defq e (elem i *inst*) o (elem 0 e)) (opt-read-ops))
				(when (opt-find-2 i (defq r (elem 1 e)) `(vp-lea-i rsp _ ,r) `(vp-add-cr _ ,r))
					(cond
						((= m 0)
							(elem-set 1 e rsp)
							(elem-set 2 e `(+ ,(elem 2 (elem p *inst*)) ,(elem 2 e))))
						((= m 1)
							(elem-set 2 e `(+ ,(elem 1 (elem p *inst*)) ,(elem 2 e)))
							(setq i (dec i))))
					(elem-set p *inst* (lambda))))
			;variable writing and offset folding
			((find-rev o (opt-write-ops))
				(when (opt-find-2 i (defq r (elem 2 e)) `(vp-lea-i rsp _ ,r) `(vp-add-cr _ ,r))
					(cond
						((= m 0)
							(elem-set 2 e rsp)
							(elem-set 3 e `(+ ,(elem 2 (elem p *inst*)) ,(elem 3 e))))
						((= m 1)
							(elem-set 3 e `(+ ,(elem 1 (elem p *inst*)) ,(elem 3 e)))
							(setq i (dec i))))
					(elem-set p *inst* (lambda))))
			;strength reduction
			((and (eql o 'vp-mul-cr) (defq s (log2 (eval (elem 1 e)))))
				(elem-set 0 e 'vp-shl-cr)
				(elem-set 1 e s)
				(setq i (dec i)))
			;constant propagation
			((defq c (find-rev o (opt-rr-ops)))
				(when (opt-find-1 i (defq r (elem 1 e)) `(vp-cpy-cr _ ,r))
					(elem-set 0 e (elem c (opt-cr-ops)))
					(elem-set 1 e (elem 1 (elem p *inst*)))
					(elem-set p *inst* (lambda))
					(setq i (dec i))))
			;arithmetic reassignment and constant folding
			((and (defq c (find-rev o (opt-cr-ops)))
				(opt-find-2 i (defq r (elem 2 e)) `(vp-cpy-cr _ ,r) (list o '_ r)))
					(cond
						((= m 0)
							(setq c (elem c (opt-fold-cpy-ops)))
							(elem-set 0 e 'vp-cpy-cr))
						((= m 1)
							(setq c (elem c (opt-fold-cr-ops)))))
					(elem-set 1 e (list c (elem 1 (elem p *inst*)) (elem 1 e)))
					(elem-set p *inst* (lambda)))
			;constant fusion
			((and (defq c (find-rev o (opt-cr-fuse-ops)))
				(opt-find-2 i (defq r (elem 2 e)) `(vp-add-cr _ ,r) `(vp-sub-cr _ ,r)))
					(cond
						((= m 0)
							(setq c (elem c '(+ -))))
						((= m 1)
							(setq c (elem c '(- +)))))
					(elem-set 1 e (list c (elem 1 e) (elem 1 (elem p *inst*))))
					(elem-set p *inst* (lambda)))
			;compare constant forwarding
			((and (defq c (find-rev o (opt-sxx-rr-ops)))
				(opt-find-1 i (defq r (elem 1 e)) `(vp-cpy-cr _ ,r))
				(<= -0x80000000 (defq v (eval (elem 1 (elem p *inst*)))) 0x7fffffff))
					(elem-set 0 e (elem c (opt-sxx-cr-ops)))
					(elem-set 1 e v)
					(elem-set p *inst* (lambda)))
			)))

;define C-Script operators for this files functions...
(new-scope)
(operator "ptr" 0 1 compile-cast)
(operator "byte" 0 1 compile-cast)
(operator "ubyte" 0 1 compile-cast)
(operator "short" 0 1 compile-cast)
(operator "ushort" 0 1 compile-cast)
(operator "int" 0 1 compile-cast)
(operator "uint" 0 1 compile-cast)
(operator "long" 0 1 compile-cast)
(operator "ulong" 0 1 compile-cast)
(operator "pptr" 0 1 compile-cast)
(operator "." 1 0 compile-field)
(operator "->" 1 0 compile-member)
(operator "[]" 1 0 compile-index)
(operator ":" 2 1 compile-uaddrof)
(operator "_" 2 1 compile-uminus)
(operator "#" 2 1 compile-uderef)
(operator "~" 2 1 compile-unot)
(operator "!" 2 1 compile-ulnot)
(operator "*>" 3 0 compile-fmul)
(operator "</" 3 0 compile-fdiv)
(operator "*" 3 0 compile-mul)
(operator "/" 3 0 compile-divu)
(operator "%" 3 0 compile-remu)
(operator "//" 3 0 compile-div)
(operator "%%" 3 0 compile-rem)
(operator "+" 4 0 compile-plus)
(operator "-" 4 0 compile-minus)
(operator "<<" 5 0 compile-lshift)
(operator ">>" 5 0 compile-rshift)
(operator ">>>" 5 0 compile-arshift)
(operator "<" 6 0 compile-cmp)
(operator ">" 6 0 compile-cmp)
(operator "<=" 6 0 compile-cmp)
(operator ">=" 6 0 compile-cmp)
(operator "=" 7 0 compile-cmp)
(operator "/=" 7 0 compile-cmp)
(operator "&" 8 0 compile-and)
(operator "^" 9 0 compile-xor)
(operator "|" 10 0 compile-or)
(operator "&&" 11 0 compile-land)
(operator "||" 12 0 compile-lor)
(operator "=>" 13 0 compile-arrow)
(operator "(" 14)
(operator ")" 14)
(operator "[" 14)
(operator "]" 14)
(new-scope)

;export to the outside environment and wipe the temp environment.
(def (penv)
	'cscript-tokenize cscript-tokenize
	'cscript-reverse-polish cscript-reverse-polish
	'cscript-compile cscript-compile
	'reset-reg-stack reset-reg-stack
	'set-reg-map set-reg-map
	'opt-inst-list opt-inst-list
	'compile-deref? compile-deref?
	'compile-arrow compile-arrow)
(env-pop)

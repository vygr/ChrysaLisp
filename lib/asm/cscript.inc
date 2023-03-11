;;;;;;;;;;;;;;;;;;;
; C-Script compiler
;;;;;;;;;;;;;;;;;;;

(include "lib/asm/code.inc")

;module
(env-push)

;;;;;;;;;;;
; tokenizer
;;;;;;;;;;;

(defq +symbol_chars "+:ABCDEFGHIJKLMNOPQRSTUVWXYZ_/0123456789abcdefghijklmnopqrstuvwxyz"
	+path_chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ_/0123456789abcdefghijklmnopqrstuvwxyz"
	+label_chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_abcdefghijklmnopqrstuvwxyz"
	+number_chars "0123456789"
	+folow_on_number_chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.0123456789"
	+op_chars "+-*/%~^<=>!&|."
	+folow_on_op_chars "/%<=>&|"
	+bracket_chars "()[]"
	+stringy_chars (cat (ascii-char 34) "$@"))

(defmacro is-symbol-char? (_)
	(list 'find-rev _ +symbol_chars))

(defmacro is-path-char? (_)
	(list 'find-rev _ +path_chars))

(defmacro is-label-char? (_)
	(list 'find-rev _ +label_chars))

(defmacro is-number-char? (_)
	(list 'find-rev _ +number_chars))

(defmacro is-folow-on-number-char? (_)
	(list 'find-rev _ +folow_on_number_chars))

(defmacro is-op-char? (_)
	(list 'find-rev _ +op_chars))

(defmacro is-folow-on-op-char? (_)
	(list 'find-rev _ +folow_on_op_chars))

(defmacro is-bracket-char? (_)
	(list 'find-rev _ +bracket_chars))

(defmacro is-stringy-char? (_)
	(list 'find-rev _ +stringy_chars))

(defun cscript-tokenize (line)
	(defq state :space unary :t token (clear '()) out_types (clear '()) out_tokens (clear '()))
	(each (lambda (c)
		(while c
			(case state
				(:space
					(cond
						((defq _ (is-stringy-char? c))
							(setq state (elem-get _ '(:string :label :path))))
						((and (eql c "+") unary)
							(push token c)
							(setq state :symbol))
						((is-op-char? c)
							(push token c)
							(setq state :operator))
						((is-number-char? c)
							(push token c)
							(setq state :number))
						((is-symbol-char? c)
							(push token c)
							(setq state :symbol))
						((defq _ (is-bracket-char? c))
							(push out_types (elem-get _ '(:lrb :rrb :lsb :rsb)))
							(push out_tokens c)
							(setq unary (elem-get _ '(:t :nil :t :nil)))))
					(setq c :nil))
				(:symbol
					(cond
						((is-symbol-char? c)
							(push token c)
							(setq c :nil))
						(:t	(cond
								((find-rev (elem-get -2 (push out_tokens (apply cat token)))
										'("pptr" "byte" "ubyte" "short" "ushort" "int" "uint" "long" "ulong" "ptr"))
									(push out_types :operator))
								(:t	(push out_types state)))
							(clear token)
							(setq state :space unary :nil))))
				(:number
					(cond
						((is-folow-on-number-char? c)
							(push token c)
							(setq c :nil))
						(:t	(push out_types state)
							(push out_tokens (apply cat token))
							(clear token)
							(setq state :space unary :nil))))
				(:string
					(cond
						((eql c (ascii-char 34))
							(push out_types state)
							(push out_tokens (apply cat token))
							(clear token)
							(setq state :space unary :nil))
						(:t (push token c)))
					(setq c :nil))
				(:path
					(cond
						((is-path-char? c)
							(push token c)
							(setq c :nil))
						(:t (push out_types state)
							(push out_tokens (apply cat token))
							(clear token)
							(setq state :space unary :nil))))
				(:label
					(cond
						((is-label-char? c)
							(push token c)
							(setq c :nil))
						(:t (push out_types state)
							(push out_tokens (apply cat token))
							(clear token)
							(setq state :space unary :nil))))
				(:operator
					(cond
						((is-folow-on-op-char? c)
							(push token c)
							(setq c :nil))
						(:t	(cond
								(unary
									(if (setq _ (find-rev (elem-get 0 token) "-*&~!"))
										(push out_tokens (elem-get _ "_#:~!"))
										(throw "Unknown unary operator !" (elem-get 0 token))))
								(:t	(push out_tokens (apply cat token))
									(setq unary :t)))
						 	(push out_types state)
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
				(defq v (- (elem-get 2 s) (elem-get 3 s)) x :t)
				(while (and x (/= 0 (length token_stack)))
					(setq s (get-sym (elem-get -2 token_stack)))
					(cond
						((>= v (elem-get 2 s))
							(push out_tokens (pop token_stack))
							(push out_types :operator))
						(:t	(setq x :nil))))
				(push token_stack token))
			(:t	;number, symbol, string, path, label
				(push out_tokens token)
				(push out_types type)))) tokenize_output)
	(while (defq _ (pop token_stack))
		(push out_tokens _)
		(push out_types :operator))
	(list out_tokens out_types))

;;;;;;;;;;;;;
; compilation
;;;;;;;;;;;;;

(defq +vreg ''(_v0 _v1 _v2 _v3 _v4 _v5 _v6 _v7 _v8 _v9 _v10 _v11 _v12 _v13 _v14))
(each (lambda (_) (defcvar _ :nil)) +vreg)

(defmacro vreg-sym (_)
	`(elem-get ,_ +vreg))

(defmacro add-inst (&rest b)
	`(push *inst* ~b))

(defmacro set-type (_)
	(list 'elem-set -2 '*vregt* _))

(defmacro get-type ()
	'(elem-get -2 *vregt*))

(defmacro top-reg ()
	'(vreg-sym (dec (length *vregt*))))

(defmacro tmp-reg ()
	'(vreg-sym (length *vregt*)))

(defmacro reset-reg-stack (_)
	`(defq *inst* (push (clear '()) progn)
		*vregt* (slice 0 ,_ '(:nil :nil :nil :nil :nil :nil :nil :nil :nil :nil :nil :nil :nil :nil :nil))))

(defun set-reg-map (l _)
	(setd _ '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14))
	(each (lambda (_ r) (set (penv) _ r)) +vreg (if l (merge-obj l _) _)))

(defun push-reg (_)
	(vreg-sym (dec (length (push *vregt* _)))))

(defun pop-reg ()
	(list (vreg-sym (dec (length *vregt*))) (pop *vregt*)))

(defun compile-deref ()
	(if (defq x (top-reg) w (get-type))
		(defq z (slice 1 -1 w) z (if (eql z "") :nil z) w (elem-get 0 w))
		(throw "No type info !" x))
	(set-type z)
	(setq w (elem-get (find-rev w "bBsSiIlLp") '(vp-cpy-ir-b vp-cpy-ir-ub vp-cpy-ir-s vp-cpy-ir-us
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
		(set-type :nil))
	(top-reg))

(defun compile-null ()
	(throw "Null operator !" :nil))

(defun compile-const (_)
	(add-inst (list 'vp-cpy-cr _ (push-reg :nil))))

(defun compile-arrow (&optional _)
	(bind '((y w) x) (list (pop-reg) (pop-value)))
	(if _ (push-reg :nil))
	(setq w (elem-get (find-rev w "bBsSiIlLp") '(vp-cpy-ri-b vp-cpy-ri-b vp-cpy-ri-s vp-cpy-ri-s
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
				(:t (throw "Symbol not defined !" _))))
		((eql 'var (elem-get 1 s))
			;variable
			(add-inst (list 'vp-lea-i :rsp (+ (get-scope (elem-get 0 s)) (elem-get 2 s))
									(push-reg (elem-get 3 s)))))
		(:t (throw "Symbol not a variable !" _))))

(defun compile-cast (_)
	(if (defq c (find-rev (sym _) '(pptr byte ubyte short ushort int uint long ulong ptr)))
		(set-type (elem-get c '("pp" "b" "B" "s" "S" "i" "I" "l" "L" "p")))
		(throw "Unknown type cast" _)))

(defun compile-member (_)
	(bind '(_ w) (pop-reg))
	(compile-deref)
	(set-type w)
	(add-inst (list 'vp-add-rr _ (top-reg))))

(defun compile-uaddrof (_)
	(set-type :nil))

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
	(add-inst (list (elem-get (find-rev _ '(= /= <= >= > <))
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
			(throw "Compile op not defined !" _))
		((not (eql 'op (elem-get 1 s)))
			(throw "Not an operator !" _))
		(:t	((elem-get 4 s) _))))

(defun compile-string (_)
	(add-inst (list 'fn-string _ (push-reg :nil))))

(defun compile-bind (_)
	(add-inst `(fn-bind ',_ ,(push-reg :nil))))

(defun compile-label (_)
	(add-inst `(vp-lea-p ',_ ,(push-reg :nil))))

(defun cscript-compile (rpn_output)
	(each! 0 -1 (lambda (token type)
		(case type
			(:operator (compile-operator (sym token)))
			(:number (compile-const (str-as-num token)))
			(:symbol (compile-ref (sym token)))
			(:path (compile-bind (sym token)))
			(:label (compile-label (sym token)))
			(:string (compile-string token)))) rpn_output))

(defun cscript (_)
	(cscript-compile (cscript-reverse-polish (cscript-tokenize _))))

;define C-Script operators...
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

;module
(export-symbols
	cscript
	reset-reg-stack set-reg-map
	compile-deref? compile-arrow)
(env-pop)

;;;;;;;;;;;;;;;;;;;
; C-Script compiler
;;;;;;;;;;;;;;;;;;;

;module
(env-push)

;;;;;;;;;;;
; tokenizer
;;;;;;;;;;;

(defq +symbol_chars (char-class "A-Za-z0-9+:_/")
	+op_chars (char-class "-+*/%~^<=>!&|.")
	+folow_on_op_chars (char-class "/%<=>&|")
	+bracket_chars (char-class "()[]")
	+stringy_chars (char-class (cat (ascii-char 34) "$@")))

(defmacro is-symbol-char? (_) `(bfind ,_ +symbol_chars))
(defmacro is-path-char? (_) `(bfind ,_ +char_class_path))
(defmacro is-label-char? (_) `(bfind ,_ +char_class_word))
(defmacro is-digit-char? (_) `(bfind ,_ +char_class_digit))
(defmacro is-op-char? (_) `(bfind ,_ +op_chars))
(defmacro is-bracket-char? (_) `(bfind ,_ +bracket_chars))
(defmacro is-stringy-char? (_) `(bfind ,_ +stringy_chars))

(defun cscript-tokenize (line)
	(defq i 0 j 0 c 0 l (length line) state :space unary :t
		out_types (clear '()) out_tokens (clear '()))
	(while (< i l)
		(case state
			(:space
				(when (< (setq i (bskip +char_class_white_space line i)) l)
					(setq c (elem-get line i))
					(cond
						((setq j (is-stringy-char? c))
							(setq state (elem-get '(:string :label :path) j) i (inc i)))
						((and (eql c "+") unary) (setq state :symbol))
						((is-op-char? c) (setq state :operator))
						((is-digit-char? c) (setq state :number))
						((is-symbol-char? c) (setq state :symbol))
						((setq j (is-bracket-char? c))
							(push out_types (elem-get '(:lrb :rrb :lsb :rsb) j))
							(push out_tokens c)
							(setq i (inc i) unary (elem-get '(:t :nil :t :nil) j))))))
			(:number
				(push out_types :number)
				(push out_tokens (slice line i (setq state :space unary :nil i
					(bskip +char_class_digit_base line (inc i))))))
			(:path
				(push out_types :path)
				(push out_tokens (slice line i (setq state :space unary :nil i
					(bskip +char_class_path line i)))))
			(:label
				(push out_types :label)
				(push out_tokens (slice line i (setq state :space unary :nil i
					(bskip +char_class_word line i)))))
			(:string
				(push out_types :string)
				(push out_tokens (slice line i (setq state :space j
					(bskipn (ascii-char 34) line i))))
				(setq unary :nil i (inc j)))
			(:symbol
				(push out_types
					(if (find (last (push out_tokens (slice line i (setq state :space unary :nil i
							(bskip +symbol_chars line (inc i))))))
						'("pptr" "byte" "ubyte" "short" "ushort"
						"int" "uint" "long" "ulong" "ptr"))
					:operator :symbol)))
			(:operator
				(push out_types :operator)
				(setq c (slice line i (setq state :space j
					(bskip +folow_on_op_chars line (inc i)) i j)))
				(cond
					(unary
						(if (setq j (find-rev (first c) "-*&~!"))
							(push out_tokens (elem-get "_#:~!" j))
							(throw "Unknown unary operator !" (first c))))
					(:t (push out_tokens c)
						(setq unary :t))))))
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
				(unless (defq s (scope-get-sym token))
					(throw "Operator not defined !" token))
				(defq v (- (third s) (elem-get s 3)) x :t)
				(while (and x (/= 0 (length token_stack)))
					(setq s (scope-get-sym (last token_stack)))
					(cond
						((>= v (third s))
							(push out_tokens (pop token_stack))
							(push out_types :operator))
						(:t (setq x :nil))))
				(push token_stack token))
			(:t ;number, symbol, string, path, label
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

(defmacro vreg-sym (_)
	`(elem-get ,+vreg ,_))

(defmacro add-inst (&rest b)
	`(push *inst* ~b))

(defmacro set-type (_)
	(list 'elem-set '*vregt* -2 _))

(defmacro get-type ()
	'(last *vregt*))

(defmacro top-reg ()
	'(vreg-sym (dec (length *vregt*))))

(defmacro tmp-reg ()
	'(vreg-sym (length *vregt*)))

(defun push-reg (_)
	(vreg-sym (dec (length (push *vregt* _)))))

(defun pop-reg ()
	(list (vreg-sym (dec (length *vregt*))) (pop *vregt*)))

(defmacro reset-reg-stack (_)
	`(defq *inst* (push (clear '()) progn)
		*vregt* (slice '(:nil :nil :nil :nil :nil :nil :nil
			:nil :nil :nil :nil :nil :nil :nil :nil) 0 ,_)))

(defun def-reg-map (pre spill)
	(setd spill '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7
		:r8 :r9 :r10 :r11 :r12 :r13 :r14))
	(each (# (deffvar %0 %1)) +vreg (if pre (merge-obj pre spill) spill)))

(defun compile-deref ()
	(if (defq x (top-reg) w (get-type))
		(defq z (rest w) z (if (eql z "") :nil z) w (first w))
		(throw "No type info !" x))
	(set-type z)
	(setq w (elem-get
		'(vp-cpy-ir-b vp-cpy-ir-ub vp-cpy-ir-s vp-cpy-ir-us
		vp-cpy-ir-i vp-cpy-ir-ui vp-cpy-ir vp-cpy-ir vp-cpy-ir)
		(find-rev w "bBsSiIlLp")))
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
	(setq w (elem-get
		'(vp-cpy-ri-b vp-cpy-ri-b vp-cpy-ri-s vp-cpy-ri-s
		vp-cpy-ri-i vp-cpy-ri-i vp-cpy-ri vp-cpy-ri vp-cpy-ri)
		 (find-rev w "bBsSiIlLp")))
	(add-inst (list w x y 0)))

(defun compile-ref (_)
	(cond
		((not (defq s (scope-get-sym _)))
			;not in symbol table so figure out what it is
			(cond
				((get (sym (str _ "_t")))
					;field/member
					(add-inst (list 'vp-cpy-cr
						_ (push-reg (eval (sym (str _ "_t")))))))
				((get _)
					;equate
					(compile-const _))
				(:t (throw "Symbol not defined !" _))))
		((eql 'var (second s))
			;variable
			(add-inst (list 'vp-lea-i :rsp
				(+ (scope-get (first s)) (third s))
				(push-reg (elem-get s 3)))))
		(:t (throw "Symbol not a variable !" _))))

(defun compile-cast (_)
	(if (defq c (find-rev (sym _) '(pptr byte ubyte short ushort int uint long ulong ptr)))
		(set-type (elem-get '("pp" "b" "B" "s" "S" "i" "I" "l" "L" "p") c))
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
	(add-inst (list (elem-get
		'(vp-seq-rr vp-sne-rr vp-sle-rr vp-sge-rr vp-sgt-rr vp-slt-rr)
			(find-rev _ '(= /= <= >= > <)))
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
		((not (defq s (scope-get-sym _)))
			(throw "Compile op not defined !" _))
		((not (eql 'op (second s)))
			(throw "Not an operator !" _))
		(:t ((elem-get s 4) _))))

(defun compile-string (_)
	(add-inst (list 'fn-string _ (push-reg :nil))))

(defun compile-bind (_)
	(add-inst (list 'fn-bind (list quote _) (push-reg :nil))))

(defun compile-label (_)
	(add-inst (list 'vp-lea-p (list quote _) (push-reg :nil))))

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
(scope-new)
(scope-operator "ptr" 0 1 compile-cast)
(scope-operator "byte" 0 1 compile-cast)
(scope-operator "ubyte" 0 1 compile-cast)
(scope-operator "short" 0 1 compile-cast)
(scope-operator "ushort" 0 1 compile-cast)
(scope-operator "int" 0 1 compile-cast)
(scope-operator "uint" 0 1 compile-cast)
(scope-operator "long" 0 1 compile-cast)
(scope-operator "ulong" 0 1 compile-cast)
(scope-operator "pptr" 0 1 compile-cast)
(scope-operator "." 1 0 compile-field)
(scope-operator "->" 1 0 compile-member)
(scope-operator "[]" 1 0 compile-index)
(scope-operator ":" 2 1 compile-uaddrof)
(scope-operator "_" 2 1 compile-uminus)
(scope-operator "#" 2 1 compile-uderef)
(scope-operator "~" 2 1 compile-unot)
(scope-operator "!" 2 1 compile-ulnot)
(scope-operator "*>" 3 0 compile-fmul)
(scope-operator "</" 3 0 compile-fdiv)
(scope-operator "*" 3 0 compile-mul)
(scope-operator "/" 3 0 compile-divu)
(scope-operator "%" 3 0 compile-remu)
(scope-operator "//" 3 0 compile-div)
(scope-operator "%%" 3 0 compile-rem)
(scope-operator "+" 4 0 compile-plus)
(scope-operator "-" 4 0 compile-minus)
(scope-operator "<<" 5 0 compile-lshift)
(scope-operator ">>" 5 0 compile-rshift)
(scope-operator ">>>" 5 0 compile-arshift)
(scope-operator "<" 6 0 compile-cmp)
(scope-operator ">" 6 0 compile-cmp)
(scope-operator "<=" 6 0 compile-cmp)
(scope-operator ">=" 6 0 compile-cmp)
(scope-operator "=" 7 0 compile-cmp)
(scope-operator "/=" 7 0 compile-cmp)
(scope-operator "&" 8 0 compile-and)
(scope-operator "^" 9 0 compile-xor)
(scope-operator "|" 10 0 compile-or)
(scope-operator "&&" 11 0 compile-land)
(scope-operator "||" 12 0 compile-lor)
(scope-operator "=>" 13 0 compile-arrow)
(scope-operator "(" 14)
(scope-operator ")" 14)
(scope-operator "[" 14)
(scope-operator "]" 14)
(scope-new)

;module
(export-symbols
	'(cscript
	reset-reg-stack def-reg-map
	compile-deref? compile-arrow))
(env-pop)

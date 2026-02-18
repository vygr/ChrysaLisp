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
	+follow_on_op_chars (char-class "/%<=>&|")
	+bracket_chars (char-class "()[]")
	+stringy_chars (char-class "\q$@"))

(defmacro is-symbol-char? (%0) (static-qqp (bfind ,%0 +symbol_chars)))
(defmacro is-path-char? (%0) (static-qqp (bfind ,%0 +char_class_path)))
(defmacro is-label-char? (%0) (static-qqp (bfind ,%0 +char_class_word)))
(defmacro is-digit-char? (%0) (static-qqp (bfind ,%0 +char_class_digit)))
(defmacro is-op-char? (%0) (static-qqp (bfind ,%0 +op_chars)))
(defmacro is-bracket-char? (%0) (static-qqp (bfind ,%0 +bracket_chars)))
(defmacro is-stringy-char? (%0) (static-qqp (bfind ,%0 +stringy_chars)))

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
						"int" "uint" "long" "ptr" "real" "fixed"))
					:operator :symbol)))
			(:operator
				(push out_types :operator)
				(setq c (slice line i (setq state :space j
					(bskip +follow_on_op_chars line (inc i)) i j)))
				(cond
					(unary
						(if (setq j (find (first c) "-*&~!"))
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
	(each! (lambda (token type)
		(case type
			((:lrb :lsb)
				(push token_stack token))
			(:rrb
				(while (and (nql "(" (setq token (pop token_stack))) token)
					(push out_tokens token)
					(push out_types :operator)))
			(:rsb
				(while (and (nql "[" (setq token (pop token_stack))) token)
					(push out_tokens token)
					(push out_types :operator))
				(push out_tokens "[]")
				(push out_types :operator))
			(:operator
				;precedence
				(unless (defq s (scope-get token))
					(throw "Operator not defined !" token))
				(defq v (- (third s) (elem-get s 3)) x :t)
				(while (and x (/= 0 (length token_stack)))
					(setq s (scope-get (last token_stack)))
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

(defmacro vreg-sym (%0)
	`(elem-get ,+vreg ,%0))

(defmacro add-inst (&rest b)
	`(push *inst* ~b))

(defmacro set-type (%0)
	(list 'elem-set '*vregt* -2 %0))

(defmacro get-type ()
	'(last *vregt*))

(defmacro top-reg ()
	'(vreg-sym (dec (length *vregt*))))

(defmacro tmp-reg ()
	'(vreg-sym (length *vregt*)))

(defun push-reg (%0)
	(vreg-sym (dec (length (push *vregt* %0)))))

(defun pop-reg ()
	(list (vreg-sym (dec (length *vregt*))) (pop *vregt*)))

(defmacro reset-reg-stack (%0)
	`(defq *inst* (push (clear '()) progn)
		*vregt* (slice '(:nil :nil :nil :nil :nil :nil :nil
			:nil :nil :nil :nil :nil :nil :nil :nil) 0 ,%0)))

(defun def-reg-map (pre spill)
	(setd spill '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7
		:r8 :r9 :r10 :r11 :r12 :r13 :r14))
	(each (# (deffvar %0 %1)) +vreg (if pre (merge pre spill) spill)))

(defun compile-deref ()
	(if (defq x (top-reg) w (get-type))
		(defq z (rest w) z (if (eql z "") :nil z) w (first w))
		(throw "No type info !" x))
	(set-type z)
	(setq w (elem-get
		'(vp-cpy-ir-b vp-cpy-ir-ub vp-cpy-ir-s vp-cpy-ir-us
		vp-cpy-ir-i vp-cpy-ir-ui vp-cpy-ir vp-cpy-ir
		vp-cpy-if vp-cpy-ir)
		(find w "bBsSiIlpfx")))
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

(defun compile-const (%0)
	(add-inst (list 'vp-cpy-cr %0 (push-reg :nil))))

(defun compile-arrow (&optional %0)
	(bind '((y w) x) (list (pop-reg) (pop-value)))
	(if %0 (push-reg :nil))
	(setq w (elem-get
		'(vp-cpy-ri-b vp-cpy-ri-b vp-cpy-ri-s vp-cpy-ri-s
		vp-cpy-ri-i vp-cpy-ri-i vp-cpy-ri vp-cpy-ri
		vp-cpy-fi vp-cpy-ri)
		 (find w "bBsSiIlpfx")))
	(add-inst (list w x y 0)))

(defun compile-ref (%0)
	(cond
		((not (defq s (scope-get %0)))
			;not in symbol table so figure out what it is
			(defq type_sym (sym (str %0 "_t")) type_str (get type_sym))
			(cond
				((find (first type_str) "0123456789")
					;struct/member
					(compile-const %0))
				(type_str
					;field/member
					(add-inst (list 'vp-cpy-cr %0 (push-reg type_str))))
				((get %0)
					;equate
					(compile-const %0))
				(:t (throw "Symbol not defined !" %0))))
		((eql 'var (second s))
			;variable
			(add-inst (list 'vp-lea-i :rsp
				(+ (scope-offset (first s)) (third s))
				(push-reg (elem-get s 3)))))
		(:t (throw "Symbol not a variable !" %0))))

(defun compile-cast (%0)
	(if (defq c (find (sym %0) '(ptr byte ubyte short ushort int uint long pptr real fixed)))
		(set-type (elem-get '("p" "b" "B" "s" "S" "i" "I" "l" "pp" "f" "x") c))
		(throw "Unknown type cast" %0)))

(defun compile-member (%0)
	(bind '(%0 w) (pop-reg))
	(compile-deref)
	(set-type w)
	(add-inst (list 'vp-add-rr %0 (top-reg))))

(defun compile-uaddrof (&)
	(set-type :nil))

(defun compile-field (%0)
	(bind '(%0 w) (pop-reg))
	(set-type w)
	(add-inst (list 'vp-add-rr %0 (top-reg))))

(defun compile-index (%0)
	(defq %0 (pop-value))
	(compile-deref)
	(add-inst (list 'vp-add-rr %0 (top-reg))))

(defun compile-uminus (&)
	(add-inst (list 'vp-mul-cr -1 (top-value))))

(defun compile-uderef (&)
	(compile-deref))

(defun compile-unot (&)
	(add-inst (list 'vp-xor-cr -1 (top-value))))

(defun compile-mul (&)
	(add-inst (list 'vp-mul-rr (pop-value) (top-value))))

(defun compile-fmul (&)
	(add-inst
		(list 'vp-mul-rr (pop-value) (defq r (top-value)))
		(list 'vp-asr-cr 16 r)))

(defun compile-divu (&)
	(defq r (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x r)
		(list 'vp-div-rrr-u z r x)))

(defun compile-remu (&)
	(defq r (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x r)
		(list 'vp-div-rrr-u z r x)
		(list 'vp-cpy-rr r x)))

(defun compile-div (&)
	(defq r (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x r)
		(list 'vp-div-rrr z r x)))

(defun compile-fdiv (&)
	(defq r (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-shl-cr 16 x)
		(list 'vp-ext-rr x r)
		(list 'vp-div-rrr z r x)))

(defun compile-rem (&)
	(defq r (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x r)
		(list 'vp-div-rrr z r x)
		(list 'vp-cpy-rr r x)))

(defun compile-plus (&)
	(add-inst (list 'vp-add-rr (pop-value) (top-value))))

(defun compile-minus (&)
	(add-inst (list 'vp-sub-rr (pop-value) (top-value))))

(defun compile-lshift (&)
	(add-inst (list 'vp-shl-rr (pop-value) (top-value))))

(defun compile-rshift (&)
	(add-inst (list 'vp-shr-rr (pop-value) (top-value))))

(defun compile-arshift (&)
	(add-inst (list 'vp-asr-rr (pop-value) (top-value))))

(defun compile-cmp (%0)
	(add-inst (list (elem-get
		'(vp-seq-rr vp-sne-rr vp-sle-rr vp-sge-rr vp-sgt-rr vp-slt-rr)
			(find %0 '(= /= <= >= > <)))
		(pop-value) (top-value))))

(defun compile-and (&)
	(add-inst (list 'vp-and-rr (pop-value) (top-value))))

(defun compile-xor (&)
	(add-inst (list 'vp-xor-rr (pop-value) (top-value))))

(defun compile-or (&)
	(add-inst (list 'vp-or-rr (pop-value) (top-value))))

(defun compile-ulnot (&)
	(add-inst (list 'vp-lnot-rr (tmp-reg) (top-value))))

(defun compile-land (&)
	(add-inst (list 'vp-land-rr (pop-value) (top-value))))

(defun compile-lor (&)
	(add-inst (list 'vp-or-rr (pop-value) (top-value))))

(defun compile-operator (%0)
	(cond
		((not (defq s (scope-get %0)))
			(throw "Compile op not defined !" %0))
		((nql 'op (second s))
			(throw "Not an operator !" %0))
		(:t ((elem-get s 4) %0))))

(defun compile-string (%0)
	(add-inst (list 'fn-string %0 (push-reg :nil))))

(defun compile-bind (%0)
	(add-inst (list 'fn-bind (list quote %0) (push-reg :nil))))

(defun compile-label (%0)
	(add-inst (list 'vp-lea-p (list quote %0) (push-reg :nil))))

(defun cscript-compile (rpn_output)
	(each! (lambda (token type)
		(case type
			(:operator (compile-operator (sym token)))
			(:number (compile-const (str-as-num token)))
			(:symbol (compile-ref (sym token)))
			(:path (compile-bind (sym token)))
			(:label (compile-label (sym token)))
			(:string (compile-string token)))) rpn_output))

(defun cscript (%0)
	(cscript-compile (cscript-reverse-polish (cscript-tokenize %0))))

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
(scope-operator "pptr" 0 1 compile-cast)
(scope-operator "real" 0 1 compile-cast)
(scope-operator "fixed" 0 1 compile-cast)
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

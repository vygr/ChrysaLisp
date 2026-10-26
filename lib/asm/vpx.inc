;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; expressions over named locals, compiled to VP
;
; What was C-Script. An expression was text, {this->+field + 1}, with a
; tokenizer and a reverse polish of its own here. It is a form now,
; (vpx (+ (.> this +field) 1)), read by the reader and seen by whatever
; reads the source, and made by a Lisp program as any list is. What it
; compiles to is what it was: every function assembled the same to the
; byte, on six targets, the day the text went, 2026-10-12.
;
; It is of the VP, not of Lisp: named locals, (def-vars), fields, pointers
; and numbers. Each (assign) is compiled by itself, a value is loaded from
; the stack and stored back to it. That is the next thing to change.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

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
		 ;the type's first char, of what is stored to
		 (find (first w) "bBsSiIlpfx")))
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
	;(* a), what a points at: a has to say what that is, a pptr, a pubyte,
	;a (pptr a). Of a plain ptr nothing would be read, it would be a itself,
	;a link's own record compared with the slot of the list it is in
	(and (defq w (get-type)) (eql (rest w) "")
		(throw "(* a) of a ptr, what it points at has no type, use a pptr or a cast !" (top-reg)))
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

(defun compile-min (&)
	(add-inst (list 'vp-min-rr (pop-value) (top-value))))

(defun compile-max (&)
	(add-inst (list 'vp-max-rr (pop-value) (top-value))))

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
	(add-inst (list 'vp-lnot-rr (top-value))))

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

(defun vpx-compile (rpn_output)
	(each! (lambda (token type)
		(case type
			(:operator (compile-operator (sym token)))
			(:const (compile-const token))
			(:symbol (compile-ref (sym token)))
			(:path (compile-bind (sym token)))
			(:label (compile-label (sym token)))
			(:string (compile-string token)))) rpn_output))

;;;;;;;
; forms
;;;;;;;

;(vpx expr ...) is a list of expressions, (vpx (.> this +stream_bufp) len),
;what (assign), (call), (entry), (exit), (vpif) and the rest take where
;they take registers. It is quasi quoted, so a part of one can be worked
;out as it is assembled, (vpx (= (.> obj +obj_vtable) (@ ,(f-path :str :vtable)))).
;
;	(.> a b ...)	what a points at, its b, and of what that points at...
;	(. a b ...)		a, which is a struct, its b
;	(elem a i)		a[i]
;	(& a)			where a is
;	(* a)			what a points at
;	(- a)			a negated
;	(lognot a) (! a)	a with every bit the other way, and 1 if a is 0
;	(ptr a) (long a) (int a) (uint a) (short a) (ushort a) (byte a)
;	(ubyte a) (pptr a) (real a) (fixed a)	a as that
;	(=> a b)		b is set to a, and it is a
;	(@ path) ($ label) "str" number name
;	(+ a b ...) (- a b ...) (* a b ...) (/ a b) (% a b) (// a b) (%% a b)
;	(*> a b) (</ a b) (<< a b) (>> a b) (>>> a b)
;	(< a b) (> a b) (<= a b) (>= a b) (= a b) (/= a b)
;	(& a b ...) (^ a b ...) (| a b ...) (&& a b ...) (|| a b ...)
;	(min a b ...) (max a b ...)	the least, the most, signed
;
;More than two is from the left, (+ a b c) is (+ (+ a b) c). A number with
;a point in it is its 16.16 bits. A form is made into reverse polish, and
;that is compiled

(defmacro vpx (&rest %0) (list 'quasi-quote (cat '(:vpx) %0)))

(defun vpx? (%0)
	; (vpx? form) -> :nil | :t
	(and (list? %0) (nempty? %0) (eql (first %0) :vpx)))

(defq +form_unary (scatter (Fmap 11) '& ":" '- "_" '* "#" 'lognot "~" '! "!"
		'ptr "ptr" 'byte "byte" 'ubyte "ubyte" 'short "short" 'ushort "ushort" 'int "int" 'uint "uint"
		'long "long" 'pptr "pptr" 'real "real" 'fixed "fixed")
	+form_binary (scatter (Fmap 11) '.> "->" '. "." 'elem "[]" '=> "=>"
		'+ "+" '- "-" '* "*" '/ "/" '% "%" '// "//" '%% "%%" '*> "*>" '</ "</"
		'<< "<<" '>> ">>" '>>> ">>>" '< "<" '> ">" '<= "<=" '>= ">=" '= "=" '/= "/="
		'& "&" '^ "^" '| "|" '&& "&&" '|| "||" 'min "min" 'max "max"))

(defun vpx-polish (form)
	; (vpx-polish form) -> (tokens types)
	;a form as reverse polish. No function here calls itself: what is
	;still to do is a list, the last of it next, each a form or (:op token)
	(defq out_tokens (list) out_types (list) todo (list form))
	(while (defq item (pop todo))
		(cond
			;a number with a point in it is a fixed to the reader. Here it
			;is what the text gave, its 16.16 bits as a whole number
			((num? item)
				(push out_tokens (if (find :fixed (type-of item)) (n2i (* item 65536.0)) item))
				(push out_types :const))
			;a name first: a name is a str too, to (str?)
			((sym? item) (push out_tokens item) (push out_types :symbol))
			((str? item) (push out_tokens item) (push out_types :string))
			((not (list? item)) (throw "Not a form !" item))
			((eql (first item) :op) (push out_tokens (second item)) (push out_types :operator))
			((empty? item) (throw "An empty form !" form))
			((eql (defq head (first item)) '@) (push out_tokens (second item)) (push out_types :path))
			((eql head '$) (push out_tokens (second item)) (push out_types :label))
			((= (length item) 2)
				(unless (defq token (. +form_unary :find head)) (throw "Not an operator of one !" item))
				(push todo (list :op token) (second item)))
			((defq token (. +form_binary :find head))
				;a b op c op ..., so the last of them is put on first
				(reach (# (push todo (list :op token) %0)) (slice item 2 -1))
				(push todo (second item)))
			((throw "Not an operator !" item))))
	(list out_tokens out_types))

(defun vpx-form (%0)
	; (vpx-form form)
	(vpx-compile (vpx-polish %0)))

(defun script? (%0)
	; (script? x) -> :nil | :t
	;is it expressions to compile, and not registers
	(vpx? %0))

(defun script-items (%0)
	; (script-items x) -> (form ...)
	(rest %0))

(defun script-ignored? (%0)
	; (script-ignored? form) -> :nil | :t
	;is it the _ that says a result is not wanted
	(eql %0 '_))

(defun script-compile (%0)
	; (script-compile form)
	(vpx-form %0))

(defun script-value (%0)
	; (script-value form)
	;a source compiled and read: a variable is left as its address till it
	;is read, and a destination before it is assigned may be that variable
	(vpx-form %0)
	(top-value))

;the operators, by the token each is in the reverse polish. The two numbers
;were its precedence and which side it bound to, for text
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
(scope-operator "min" 4 0 compile-min)
(scope-operator "max" 4 0 compile-max)
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
	'(vpx-form vpx vpx? script? script-items script-ignored? script-compile script-value
	reset-reg-stack def-reg-map
	compile-deref? compile-arrow))
(env-pop)

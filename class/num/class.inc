(include "class/obj/class.inc")

(def-class 'num 'obj)
(dec-method :vtable 'class/num/vtable)
(dec-method :create 'class/num/create :static '(r0) '(r0))
(dec-method :init 'class/num/init :static '(r0 r1 r2))
(dec-method :get_value 'class/num/get_value :static '(r0) '(r0 r1))
(dec-method :set_value 'class/num/set_value :static '(r0 r1) '(r0 r1))

(dec-method :print 'class/num/print :override)
(dec-method :vcreate 'class/num/create :virtual '(r0) '(r0))

(dec-method :add 'class/num/add :virtual '(r0 r1) '(r0 r1))
(dec-method :sub 'class/num/sub :virtual '(r0 r1) '(r0 r1))
(dec-method :mul 'class/num/mul :virtual '(r0 r1) '(r0 r1))
(dec-method :min 'class/num/min :virtual '(r0 r1) '(r0 r1))
(dec-method :max 'class/num/max :virtual '(r0 r1) '(r0 r1))
(dec-method :div 'class/num/div :virtual '(r0 r1) '(r0 r1))
(dec-method :mod 'class/num/mod :virtual '(r0 r1) '(r0 r1))
(dec-method :sqrt 'class/num/sqrt :virtual '(r0) '(r0 r1))
(dec-method :abs 'class/num/abs :virtual '(r0) '(r0 r1))
(dec-method :sign 'class/num/sign :virtual '(r0) '(r0 r1))
(dec-method :neg 'class/num/neg :virtual '(r0) '(r0 r1))
(dec-method :random 'class/num/random :virtual '(r0) '(r0 r1))

(dec-method :eq 'class/num/eq :virtual '(r0 r1) '(r0 r1))
(dec-method :ne 'class/num/ne :virtual '(r0 r1) '(r0 r1))
(dec-method :lt 'class/num/lt :virtual '(r0 r1) '(r0 r1))
(dec-method :gt 'class/num/gt :virtual '(r0 r1) '(r0 r1))
(dec-method :le 'class/num/le :virtual '(r0 r1) '(r0 r1))
(dec-method :ge 'class/num/ge :virtual '(r0 r1) '(r0 r1))

(dec-method :lisp_add 'class/num/lisp_add :static '(r0 r1) '(r0 r1))
(dec-method :lisp_sub 'class/num/lisp_sub :static '(r0 r1) '(r0 r1))
(dec-method :lisp_mul 'class/num/lisp_mul :static '(r0 r1) '(r0 r1))
(dec-method :lisp_div 'class/num/lisp_div :static '(r0 r1) '(r0 r1))
(dec-method :lisp_mod 'class/num/lisp_mod :static '(r0 r1) '(r0 r1))
(dec-method :lisp_min 'class/num/lisp_min :static '(r0 r1) '(r0 r1))
(dec-method :lisp_max 'class/num/lisp_max :static '(r0 r1) '(r0 r1))
(dec-method :lisp_abs 'class/num/lisp_abs :static '(r0 r1) '(r0 r1))
(dec-method :lisp_neg 'class/num/lisp_neg :static '(r0 r1) '(r0 r1))
(dec-method :lisp_sign 'class/num/lisp_sign :static '(r0 r1) '(r0 r1))
(dec-method :lisp_sqrt 'class/num/lisp_sqrt :static '(r0 r1) '(r0 r1))
(dec-method :lisp_random 'class/num/lisp_random :static '(r0 r1) '(r0 r1))

(dec-method :lisp_eq 'class/num/lisp_eq :static '(r0 r1) '(r0 r1))
(dec-method :lisp_ne 'class/num/lisp_ne :static '(r0 r1) '(r0 r1))
(dec-method :lisp_lt 'class/num/lisp_lt :static '(r0 r1) '(r0 r1))
(dec-method :lisp_gt 'class/num/lisp_gt :static '(r0 r1) '(r0 r1))
(dec-method :lisp_le 'class/num/lisp_le :static '(r0 r1) '(r0 r1))
(dec-method :lisp_ge 'class/num/lisp_ge :static '(r0 r1) '(r0 r1))

(dec-method :lisp_and 'class/num/lisp_and :static '(r0 r1) '(r0 r1))
(dec-method :lisp_or 'class/num/lisp_or :static '(r0 r1) '(r0 r1))
(dec-method :lisp_xor 'class/num/lisp_xor :static '(r0 r1) '(r0 r1))
(dec-method :lisp_shr 'class/num/lisp_shr :static '(r0 r1) '(r0 r1))
(dec-method :lisp_asr 'class/num/lisp_asr :static '(r0 r1) '(r0 r1))
(dec-method :lisp_shl 'class/num/lisp_shl :static '(r0 r1) '(r0 r1))

(dec-method :lisp_i2f 'class/num/lisp_i2f :static '(r0 r1) '(r0 r1))
(dec-method :lisp_i2r 'class/num/lisp_i2r :static '(r0 r1) '(r0 r1))
(dec-method :lisp_f2i 'class/num/lisp_f2i :static '(r0 r1) '(r0 r1))
(dec-method :lisp_f2r 'class/num/lisp_f2r :static '(r0 r1) '(r0 r1))
(dec-method :lisp_r2i 'class/num/lisp_r2i :static '(r0 r1) '(r0 r1))
(dec-method :lisp_r2f 'class/num/lisp_r2f :static '(r0 r1) '(r0 r1))

(dec-method :hash 'class/num/hash :override '(r0) '(r0 r1))

(def-struct 'num 'obj)
	(long 'value)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/num/init ()
	;inputs
	;r0 = num object (ptr)
	;r1 = vtable (pptr)
	;r2 = initial value (long)
	;outputs
	;r0 = num object (ptr)
	;r1 = 0 if error, else ok
	;trashes
	;r1
	(assign '(r2) '((r0 num_value)))
	(s-call 'num :init '(r0 r1) '(r0 r1)))

(defcfun class/num/get_value ()
	;inputs
	;r0 = num object (ptr)
	;outputs
	;r0 = num object (ptr)
	;r1 = value (long)
	;trashes
	;r1
	(assign '((r0 num_value)) '(r1)))

(defcfun class/num/set_value ()
	;inputs
	;r0 = num object (ptr)
	;r1 = value (long)
	;outputs
	;r0 = num object (ptr)
	;r1 = value (long)
	;trashes
	;none
	(assign '(r1) '((r0 num_value))))

;;;;;;;;;;;;;;;;;;
; paramater macros
;;;;;;;;;;;;;;;;;;

(defcfun-bind class/num/get_values (src &optional dst)
	(assign-asm-to-asm (map (lambda (_) (list _ num_value)) src) (opt dst src)))

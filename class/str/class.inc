(include "././seq/class.inc")

(def-class str seq
	(dec-method :vtable class/str/vtable)
	(dec-method :create_from_buffer class/str/create_from_buffer :static (:r0 :r1) (:r0))
	(dec-method :create_from_cstr class/str/create_from_cstr :static (:r0) (:r0))
	(dec-method :create_from_file class/str/create_from_file :static (:r0) (:r0))
	(dec-method :create_from_long class/str/create_from_long :static (:r0 :r1) (:r0))
	(dec-method :append class/str/append :static (:r0 :r1) (:r0))
	(dec-method :init class/str/init :static (:r0 :r1 :r2 :r3))
	(dec-method :init1 class/str/init1 :static (:r0 :r1 :r2 :r3) (:r0 :r1))
	(dec-method :init2 class/str/init2 :static (:r0 :r1 :r2 :r3) (:r0 :r1))
	(dec-method :init3 class/str/init3 :static (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :split class/str/split :static (:r0 :r1) (:r0 :r1))
	(dec-method :hash_pstr class/str/hash_pstr :static (:r3 :r4) (:r1))
	(dec-method :same_pstr class/str/same_pstr :static (:r1 :r2 :r3) (:r4))
	(dec-method :compare class/str/compare :static (:r0 :r1) (:r0 :r1))
	(dec-method :starts_with class/str/starts_with :static (:r0 :r1) (:r0 :r1))
	(dec-method :same class/str/same :static (:r0 :r1) (:r0 :r1))
	(dec-method :bfind class/str/bfind :static (:r0 :r1) (:r0 :r1 :r2))
	(dec-method :type class/str/type :override (:r0) (:r0 :r1))
	(dec-method :print class/str/print :override)
	(dec-method :hash class/str/hash :override)
	(dec-method :get_length class/str/get_length :final)
	(dec-method :ref_elem class/str/ref_elem :final)
	(dec-method :slice class/str/slice :final)
	(dec-method :rslice class/str/rslice :final)
	(dec-method :cat class/str/cat :final)
	(dec-method :find class/str/find :final)
	(dec-method :rfind class/str/rfind :final)
	(dec-method :lisp_str class/str/lisp_str :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_to_num class/str/lisp_tonum :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_code class/str/lisp_code :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_char class/str/lisp_char :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_cmp class/str/lisp_cmp :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_bfind class/str/lisp_bfind :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_bskip class/str/lisp_bskip :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_bskipn class/str/lisp_bskipn :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_rbskip class/str/lisp_rbskip :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_rbskipn class/str/lisp_rbskipn :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_save class/str/lisp_save :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_load class/str/lisp_load :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_expand class/str/lisp_expand :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_create class/str/lisp_create :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_encode class/str/lisp_encode :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_decode class/str/lisp_decode :static (:r0 :r1) (:r0 :r1)))

(def-struct str seq_size
	(int hashslot)
	(uint hashcode length)
	(align)
	(offset data))

(defcvar 'str_gap (- 64 (+ str_data +byte_size)))

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defun class/str/get_length ()
	;inputs
	;:r0 = str object (ptr)
	;outputs
	;:r0 = str object (ptr)
	;:r1 = string length (bytes)
	;trashes
	;:r1
	(assign '((:r0 str_length)) '(:r1)))

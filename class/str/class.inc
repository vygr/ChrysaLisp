(include "class/seq/class.inc")

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
	(dec-method :split_char class/str/split_char :static (:r0 :r1) (:r0 :r1))
	(dec-method :compare class/str/compare :static (:r0 :r1) (:r0 :r1))
	(dec-method :starts_with class/str/starts_with :static (:r0 :r1) (:r0 :r1))
	(dec-method :same class/str/same :static (:r0 :r1) (:r0 :r1))
	(dec-method :type class/str/type :override (:r0) (:r0 :r1))
	(dec-method :print class/str/print :override)
	(dec-method :hash class/str/hash :override)
	(dec-method :get_length class/str/get_length :final)
	(dec-method :ref_element class/str/ref_element :final)
	(dec-method :slice class/str/slice :final)
	(dec-method :cat class/str/cat :final)
	(dec-method :find class/str/find :final)
	(dec-method :rfind class/str/rfind :final)
	(dec-method :lisp_str class/str/lisp_str :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_to_num class/str/lisp_to_num :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_split class/str/lisp_split :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_code class/str/lisp_code :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_char class/str/lisp_char :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_cmp class/str/lisp_cmp :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_save class/str/lisp_save :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_load class/str/lisp_load :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_expand class/str/lisp_expand :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_create class/str/lisp_create :static (:r0 :r1) (:r0 :r1)))

(def-struct str seq_size
	(uint length hashcode)
	(align)
	(offset data))

(defun roundup (_)
	(setq _ (dec _))
	(setq _ (logior _ (>> _ 32)))
	(setq _ (logior _ (>> _ 16)))
	(setq _ (logior _ (>> _ 8)))
	(setq _ (logior _ (>> _ 4)))
	(setq _ (logior _ (>> _ 2)))
	(setq _ (logior _ (>> _ 1)))
	(inc _))

(defcvar 'str_gap (- (* 2 (roundup (+ str_data +byte_size))) (+ str_data +byte_size)))

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

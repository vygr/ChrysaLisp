(include "class/str/class.inc")

(def-class sym str
	(dec-method :vtable class/sym/vtable)
	(dec-method :statics_init class/sym/statics_init)
	(dec-method :get_static_sym class/sym/get_static_sym :static (:r1) (:r1))
	(dec-method :ref_static_sym class/sym/ref_static_sym :static (:r1) (:r1))
	(dec-method :intern class/sym/intern :static (:r0) (:r0))
	(dec-method :intern_str class/sym/intern_str :static (:r0) (:r0))
	(dec-method :intern_cstr class/sym/intern_cstr :static (:r0) (:r0))
	(dec-method :intern_strs class/sym/intern_strs :static (:r1) (:r1))
	(dec-method :type class/sym/type :override (:r0) (:r0 :r1))
	(dec-method :print class/sym/print :override)
	(dec-method :lisp_sym class/sym/lisp_sym :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_gensym class/sym/lisp_gensym :static (:r0 :r1) (:r0 :r1)))

(def-struct sym str_size)

(defcvar 'num_sym_buckets 101)

;;;;;;;;;;;;;;;;
; static symbols
;;;;;;;;;;;;;;;;

(defcvar '*static_syms* (list))
(defun def-static-sym (_)
	(defcvar (sym (cat "static_sym_" (if (starts-with ":" _) (slice 1 -1 _) _))) (length *static_syms*))
	(push *static_syms* _))

(def-static-sym ":vtable")
(def-static-sym ":num")
(def-static-sym ":func")
(def-static-sym ":seq")
(def-static-sym ":str")
(def-static-sym ":sym")
(def-static-sym ":array")
(def-static-sym ":list")
(def-static-sym ":hmap")
(def-static-sym ":font")
(def-static-sym ":text")
(def-static-sym ":ink_color")
(def-static-sym ":vdu_width")
(def-static-sym ":vdu_height")
(def-static-sym ":draw")
(def-static-sym "main")

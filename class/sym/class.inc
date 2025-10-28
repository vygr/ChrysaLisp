(include "././str/class.inc")

(def-class sym str
	(dec-method :vtable class/sym/vtable)
	(dec-method :statics_init class/sym/statics_init)
	(dec-method :get_static_sym class/sym/get_static_sym :static (:r1) (:r1))
	(dec-method :ref_static_sym class/sym/ref_static_sym :static (:r1) (:r1))
	(dec-method :intern_str class/sym/intern_str :static (:r0) (:r0))
	(dec-method :intern_cstr class/sym/intern_cstr :static (:r0) (:r0))
	(dec-method :intern_pstr class/sym/intern_pstr :static (:r0 :r1) (:r0))
	(dec-method :type class/sym/type :final (:r0) (:r0 :r1))
	(dec-method :print class/sym/print :final)
	(dec-method :lisp_sym class/sym/lisp_sym :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_gensym class/sym/lisp_gensym :static (:r0 :r1) (:r0 :r1)))

(def-struct sym str_size)

(defcvar 'num_sym_buckets 307)

;;;;;;;;;;;;;;;;
; static symbols
;;;;;;;;;;;;;;;;

(defcvar '*static_syms* (list))

(each (# (defcvar (sym (cat "static_sym_"
		(if (starts-with ":" %0) (rest %0) %0))) (length *static_syms*))
		(push *static_syms* (str %0)))
	'(:vtable :num :fixed :real :nums :fixeds :reals :func :seq :str :sym :array
	:list :path :hmap :font :text :ink_color :vdu_width :vdu_height :draw main))

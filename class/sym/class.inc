(include "././str/class.inc")
(include "./struct.inc")

(def-class :sym :str
	(dec-method :vtable class/sym/vtable)
	(dec-method :type class/sym/type :override :nil :nil (:r1-:r3 :f0-:f15) (:r1-:r3 :f0-:f15))
	(dec-method :eql class/obj/eql :final :nil :nil (:r1) (:r1))
	(dec-method :print class/sym/print :final :nil :nil (:r1-:r5 :f0-:f15) (:r1-:r5 :f0-:f15))
	(dec-method :statics_init class/sym/statics_init :static :nil :nil (:r0-:r11 :r14 :f0-:f15))
	(dec-method :intern_str class/sym/intern_str :static (:r0) (:r0) (:r0-:r12 :f0-:f15))
	(dec-method :intern_cstr class/sym/intern_cstr :static (:r0) (:r0) (:r0-:r11 :f0-:f15))
	(dec-method :intern_pstr class/sym/intern_pstr :static (:r0 :r1) (:r0) (:r0-:r11 :f0-:f15))
	(dec-method :eql_str class/sym/eql_str :static (:r0 :r1) (:r0 :r1) (:r1-:r6))
	(dec-method :lisp_sym class/sym/lisp_sym :static (:r0 :r1) (:r0 :r1) (:r1-:r12 :f0-:f15))
	(dec-method :lisp_gensym class/sym/lisp_gensym :static (:r0 :r1) (:r0 :r1) (:r1-:r11 :f0-:f15)))

(defcvar 'num_sym_buckets 499)

;;;;;;;;;;;;;;;;
; static symbols
;;;;;;;;;;;;;;;;

(defcvar '*static_syms* (list))

(each (# (defcvar (sym (cat "static_sym_"
		(if (starts-with ":" %0) (rest %0) %0))) (* (length *static_syms*) +ptr_size))
		(push *static_syms* (str %0)))
	'(:vtable :num :fixed :real :nums :fixeds :reals :func :seq :str :sym :array
	:list :pmap :pset :path :hmap :font :text :ink_color :vdu_width :vdu_height :draw
	:stream :sstream :fstream :mstream :netid :nodeid :in :out :stdio :lisp :error
	:hset :dim :lstream main))

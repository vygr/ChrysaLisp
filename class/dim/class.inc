(include "././obj/class.inc")
(include "./struct.inc")

(def-class :dim :obj
	(dec-method :vtable class/dim/vtable)
	(dec-method :type class/dim/type :override :nil :nil (:r1-:r5 :f0-:f15) (:r1-:r5 :f0-:f15))
	(dec-method :create class/dim/create :static (:r0 :r1) (:r0) (:r0-:r3 :r14 :f0-:f15))
	(dec-method :init class/dim/init :static (:r0 :r1 :r2 :r3) :nil (:r1))
	(dec-method :deinit class/dim/deinit :final :nil :nil (:r1-:r14 :f0-:f15) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_dim class/dim/lisp_dim :static (:r0 :r1) (:r0 :r1) (:r1-:r6 :r14 :f0-:f15))
	(dec-method :lisp_get class/dim/lisp_dget :static (:r0 :r1) (:r0 :r1) (:r1-:r11 :r14 :f0-:f15))
	(dec-method :lisp_set class/dim/lisp_dset :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15)))

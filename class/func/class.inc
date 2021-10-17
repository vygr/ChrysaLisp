(include "class/num/class.inc")

(def-class func obj
	(dec-method :vtable class/func/vtable)
	(dec-method :create class/func/create :static (:r0) (:r0))
	(dec-method :init class/num/init :static (:r0 :r1 :r2))
	(dec-method :type class/func/type :override (:r0) (:r0 :r1))
	(dec-method :print class/func/print :override))

(def-enum func_type 0
	(enum args_eval args_raw))

(def-struct func num_size)

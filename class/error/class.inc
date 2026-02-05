(include "././obj/class.inc")
(include "./struct.inc")

(def-class :error :obj
	(dec-method :vtable class/error/vtable)
	(dec-method :create class/error/create :static (:r0 :r1 :r2 :r3 :r4 :r5 :r6) (:r0))
	(dec-method :init class/error/init :static (:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8))
	(dec-method :get_msg class/error/get_msg :static (:r0) (:r0 :r1))
	(dec-method :print class/error/print :final)
	(dec-method :deinit class/error/deinit :final))

;;;;;;;;;;;;
; error msgs
;;;;;;;;;;;;

(defcvar '*error_msgs* (list))
(defun deferror (%0)
	(defcvar (sym (cat "+error_msg_" %0)) (inc (length *error_msgs*)))
	(push *error_msgs* (cat %0 " !")))

(deferror "not_a_function")
(deferror "not_a_list")
(deferror "not_a_number")
(deferror "not_a_sequence")
(deferror "not_a_stream")
(deferror "not_a_string")
(deferror "not_a_symbol")
(deferror "not_an_environment")
(deferror "not_valid_index")
(deferror "open_error")
(deferror "symbol_not_bound")
(deferror "wrong_num_of_args")
(deferror "wrong_types")
(deferror "rebind_constant")

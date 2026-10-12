(include "././mail/class.inc")
(include "./struct.inc")

(def-class :sys_kernel :nil
	(dec-method :id sys/kernel/id :static :nil (:r0 :r1) (:r0-:r1))
	(dec-method :system_id sys/kernel/system_id :static :nil (:r0 :r1) (:r0-:r1))
	(dec-method :kernel sys/kernel/kernel :static (:r0) :nil (:r0-:r14 :f0-:f15))
	(dec-method :ping sys/kernel/ping :static :nil :nil ())
	(dec-method :lisp_stats sys/kernel/lisp_stats :static (:r0 :r1) (:r0 :r1) (:r1-:r11 :f0-:f15))
	(dec-method :lisp_sysid sys/kernel/lisp_sysid :static (:r0 :r1) (:r0 :r1) (:r1-:r5 :f0-:f15)))

(defcvar 'ping_period 5000000)

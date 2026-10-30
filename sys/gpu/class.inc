(include "lib/asm/class.inc")

;;;;;;;;;;;;;;;;;;
; gpu static class
;;;;;;;;;;;;;;;;;;

;the host's GPU, src/host/gpu_sdl3.cpp, its table in the order of the host's,
;0 on a host that has none

(def-class :host_gpu :nil
	(dec-method :format :nil :virtual () (:r0))
	(dec-method :buffer_create :nil :virtual (:r0) (:r0))
	(dec-method :buffer_destroy :nil :virtual (:r0) (:r0))
	(dec-method :buffer_write :nil :virtual (:r0 :r1 :r2 :r3) (:r0))
	(dec-method :buffer_read :nil :virtual (:r0 :r1 :r2) (:r0))
	(dec-method :done :nil :virtual (:r0) (:r0))
	(dec-method :take :nil :virtual (:r0 :r1 :r2) (:r0))
	(dec-method :kernel_create :nil :virtual (:r0 :r1 :r2) (:r0))
	(dec-method :kernel_destroy :nil :virtual (:r0) (:r0))
	(dec-method :dispatch :nil :virtual (:r0 :r1 :r2 :r3 :r4) (:r0))
	(dec-method :lisp_call sys/gpu/lisp_call :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15)))

(defun host-gpu-call (%0 %1 &optional %2 %3 %4 %5)
	;%0 = class name
	;%1 = member name
	;%2 = in parameters
	;%3 = out parameters
	;%4 = dispatch reg
	;%5 = temp reg
	(setd %4 :r14 %5 :r13)
	(fn-bind 'sys/statics/statics %4)
	(vp-cpy-ir %4 statics_sys_load_host_gpu_funcs %4)
	(abi-call-table %0 %1 %4 %2 %3 %5))

; Override print and prin to suppress console output during benchmarking.
; This must precede all imports to ensure the prebinder resolves downstream
; library calls to these silent definitions.
(redefun print (&rest args) :nil)
(redefun prin (&rest args) :nil)

(import "usr/env.inc")
(import "lib/asm/asm.inc")

(enums +select 0
	(enum main timeout))

(defq +timeout 5000000)

(defun main ()
	(defq select (task-mboxes +select_size)
		*working* :t files (all-vp-files)
		*abi* (abi) *cpu* (cpu))
	(while *working*
		(mail-timeout (elem-get select +select_timeout) +timeout 0)
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((or (= idx +select_timeout) (eql msg ""))
				(setq *working* :nil))
			((= idx +select_main)
				(mail-timeout (elem-get select +select_timeout) 0 0)
				(defq reply_mbox (slice msg 0 +net_id_size) start (pii-time))
				(make-all files)
				(defq duration (- (pii-time) start))
				(mail-send reply_mbox (str duration))))))
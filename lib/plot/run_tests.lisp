;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Plot Library Test Runner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/plot/test.inc")

; Exit with appropriate code
(cond
	((= *fail_count* 0)
		(exit 0))
	(:t
		(exit 1)))

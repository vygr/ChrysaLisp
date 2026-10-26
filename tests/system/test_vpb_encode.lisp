(report-header "VP binary: the encoder in VP gives the bytes the Lisp one does")

;every function of the system, its VP binary by (vpb-encode), which is the
;encoder in VP, lib/trans/vpb_encode.vp, once it is built, and by
;(vpb-encode-lisp). A build translates what the VP one gives, so a byte
;wrong there is a native wrong, and a node that loads it can die
(import "lib/asm/asm.inc")
(defq ve_built (> (age (cat (load-path) "lib/trans/vpb_encode")) 0) ve_counts '(0 0))
(defun ve-run ()
	(bind '(& *cpu* *abi*) (map (const sym) (split (load-path) "/")))
	(within-compile-env (lambda ()
		(defq same 0 differ 0)
		(each (lambda (file)
				(each (lambda (entry)
						(if (eql (vpb-encode entry) (vpb-encode-lisp entry))
							(setq same (inc same)) (setq differ (inc differ))))
					(vp-compile file)))
			(all-vp-files))
		(setq ve_counts (list same differ)))))

(ifn ve_built (test-skip "VP encoder" "lib/trans/vpb_encode not built here")
	(ve-run)
	(assert-true "every function encoded" (> (first ve_counts) 1000))
	(assert-eq "none differ" 0 (second ve_counts)))

(undef (env) 've_built 've_counts 've-run)

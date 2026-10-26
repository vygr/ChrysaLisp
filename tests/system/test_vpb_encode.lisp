(report-header "VP binary: what the encoder makes decodes to the VP it was made of")

;every function of the system, its VP binary by (vpb-encode), the encoder
;in VP, lib/trans/vpb_encode.vp, decoded again by (vpb-decode): the same
;instructions and operands, its labels named as the decoder names them, in
;the order they are first seen, and the same bytes when encoded again. A
;build translates what the encoder gives, so a byte wrong there is a native
;wrong, and a node that loads it can die
(import "lib/asm/asm.inc")
(defq ve_counts '(0 0))
(defun ve-run ()
	(bind '(& *cpu* *abi*) (map (const sym) (split (load-path) "/")))
	(within-compile-env (lambda ()
		(defq same 0 differ 0)
		(each (lambda (file)
				(each (lambda (entry)
						(defq bin (vpb-encode entry) back (vpb-decode bin) labels (Fmap 31) nlabels 0
							named (lambda (l)
								(cond
									((eql l '*pc*) l)
									((. labels :find l))
									(:t (. labels :insert l (defq n (sym (cat "_" (str nlabels)))))
										(setq nlabels (inc nlabels))
										n)))
							operand (lambda (o)
								(cond
									((and (sym? o) (not (starts-with ":" o))) (named o))
									((and (list? o) (eql (first o) 'quote)) (list 'quote (named (second o))))
									((and (list? o) (eql (first o) '-)) (list '- (named (second o)) (named (third o))))
									(o)))
							code (reduce (lambda (out inst) (push out (cat (list (first inst)) (map operand (rest inst)))))
								(rest (second entry)) (list 'progn)))
						(if (and (eql (first back) (first entry)) (eql (str (second back)) (str code))
								(eql (vpb-encode back) bin))
							(setq same (inc same)) (setq differ (inc differ))))
					(vp-compile file)))
			(all-vp-files))
		(setq ve_counts (list same differ)))))

(ve-run)
(assert-true "every function encoded" (> (first ve_counts) 1000))
(assert-eq "none differ" 0 (second ve_counts))

(undef (env) 've_counts 've-run)

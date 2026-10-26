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
							code (reduce! (lambda (out inst) (push out (map! operand (list inst) 1 -1 (list (first inst)))))
								(list (second entry)) (list 'progn) 1))
						(if (and (eql (first back) (first entry)) (eql (str (second back)) (str code))
								(eql (vpb-encode back) bin))
							(setq same (inc same)) (setq differ (inc differ))))
					(vp-compile file)))
			(all-vp-files))
		(setq ve_counts (list same differ)))))

(ve-run)
(assert-true "every function encoded" (> (first ve_counts) 1000))
(assert-eq "none differ" 0 (second ve_counts))


;a bundle, or a binary, from another node may be cut short, or say a length
;past its end: what is whole is had, and no more is read
(defq ve_got (within-compile-env (lambda ()
	(include "lib/trans/vpb.inc")
	(defq b (vpb-bundle '("abc" "defgh")))
	(list (vpb-unbundle b) (vpb-unbundle (slice b 0 -2)) (vpb-unbundle (cat (char 0x80) (char 0x80)))
		(vpb-name (cat (char 3) "foobar")) (vpb-name (cat (char 9) "foo")) (vpb-name "")))))
(assert-list-eq "a bundle unbundled" '("abc" "defgh") (first ve_got))
(assert-list-eq "one cut short, what is whole" '("abc") (second ve_got))
(assert-eq "a length that does not end" '() (third ve_got))
(assert-eq "a name" "foo" (elem-get ve_got 3))
(assert-eq "a name past the end" "" (elem-get ve_got 4))
(assert-eq "no name" "" (elem-get ve_got 5))

(undef (env) 've_counts 've-run 've_got)

(report-header "Bind: a function with no native is translated from its VP by the loader, and kept")

;two functions there is only VP binary of, <name>.vpb, one calling the
;other: a bind of the first has the loader translate both, closure first,
;keep their natives where a build would, the same to the byte, and run
(bind '(& bt_cpu bt_abi) (map (const sym) (split (load-path) "/")))
(defq bt_lf (ascii-char 10) bt_id (str (pii-time))
	bt_a (cat "tests/bt_" bt_id "_a") bt_b (cat "tests/bt_" bt_id "_b")
	bt_obj (cat "obj/" bt_cpu "/" bt_abi "/") bt_src (cat bt_obj "tests/bt_" bt_id ".vp")
	bt_bins (list))

;b adds 3 to :r0, a is called from Lisp with a num and gives a num, of b of it
(save (cat "(include {lib/asm/func.inc})" bt_lf "(include {class/num/class.inc})" bt_lf
	"(include {class/lisp/class.inc})" bt_lf
	"(def-func {" bt_b "})" bt_lf "(vp-add-cr 3 :r0)" bt_lf "(vp-ret)" bt_lf "(def-func-end)" bt_lf
	"(def-func {" bt_a "})" bt_lf "(vp-push :r0)" bt_lf "(list-bind-args :r1 '(:r0) '(:num))" bt_lf
	"(fn-call '" bt_b ")" bt_lf "(call :num :create '(:r0) '(:r1))" bt_lf
	"(vp-pop :r0)" bt_lf "(vp-ret)" bt_lf "(def-func-end)" bt_lf) bt_src)
(defun bt-make ()
	;the VP binary of each, and what a build makes of it
	(bind '(& *cpu* *abi*) (map (const sym) (split (load-path) "/")))
	(within-compile-env (lambda ()
		(each (lambda (entry)
				(defq bin (vpb-encode entry))
				(push bt_bins (list (first entry) (vp-translate-bin bin)))
				(save bin (cat bt_obj (first entry) ".vpb")))
			(vp-compile bt_src)))))
(bt-make)
(pii-remove bt_src)

(assert-true "no native of either before" (every (# (= 0 (age (cat bt_obj %0)))) (list bt_a bt_b)))
(ffi bt_a bt_f)
(assert-eq "bound, and b called from a" 45 (bt_f 42))
(each (lambda ((name native))
		(assert-eq (cat "the native kept is what a build makes, " name) native (load (cat bt_obj name))))
	bt_bins)

(each (# (pii-remove (cat bt_obj %0)) (pii-remove (cat bt_obj %0 ".vpb"))) (list bt_a bt_b))
(undef (env) 'bt_cpu 'bt_abi 'bt_lf 'bt_id 'bt_a 'bt_b 'bt_obj 'bt_src 'bt_bins 'bt_f 'bt-make)

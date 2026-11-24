(report-header "Bind args: a native's arguments into registers, & an argument passed over")

;(list-bind-args) is what a native takes its arguments out of their list
;with, each to a register by its type. In its types & is an argument passed
;over, as in a (bind): the VP it makes is held to the same written by hand,
;the list's start, the first and the third of it, and each num's value. A
;register that is not to be loaded is left out of the list given: && was
;that once, and is refused
(defq ba_lf "\n" ba_id (str (pii-time)) ba_src (cat "tests/scratch/ba_" ba_id ".vp")
	ba_head (cat "(include {lib/asm/func.inc})" ba_lf "(include {class/num/class.inc})" ba_lf
		"(include {class/lisp/class.inc})" ba_lf))

(defun ba-vp (body)
	; (ba-vp body) -> text
	;the VP of a function of that body, as it prints
	(defq out "")
	(save (cat ba_head "(def-func {tests/ba_" ba_id "})" ba_lf body ba_lf "(vp-ret)" ba_lf "(def-func-end)" ba_lf) ba_src)
	(bind '(& *cpu* *abi*) (map (const sym) (split (load-path) "/")))
	(catch (within-compile-env (lambda ()
			(each (lambda (entry) (setq out (str entry))) (vp-compile ba_src))))
		(progn (pii-remove ba_src) :nil))
	(pii-remove ba_src)
	out)

(defq ba_macro (ba-vp "(list-bind-args :r1 '(:r0 :r2) '(:num & :num))")
	ba_hand (ba-vp (cat "(assign '((:r1 +array_begin)) '(:r2))" ba_lf
		"(assign '((:r2 0) (:r2 (* 2 +ptr_size))) '(:r0 :r2))" ba_lf
		"(assign '((:r0 +num_value) (:r2 +num_value)) '(:r0 :r2))")))
(assert-true "there is VP to compare" (> (length ba_macro) 40))
(assert-eq "& passes an argument over: the first and the third, as by hand" ba_hand ba_macro)
(assert-error "&& is refused, a register not loaded is left out of the list"
	(ba-vp "(list-bind-args :r1 '(:r3 :r0) '(&& :num))") {A register not loaded is left out of the list, there is no && !})
(assert-error "and registers that are not as many as the arguments wanted"
	(ba-vp "(list-bind-args :r1 '(:r0 :r2) '(:num & :num :num))") {Mismatched bind specs !})

(report-header "Control Flow: if/cond/case/while/until")

; Definitions and Binding
(defq global_var 100)
(assert-eq "Defq global" 100 global_var)

(defq setd_var :nil)
(setd setd_var 42)
(assert-eq "setd initial" 42 setd_var)
(setd setd_var 99)
(assert-eq "setd keep" 42 setd_var)

(defq setd_a :nil setd_b 15 setd_c :nil)
(setd setd_a 10 setd_b 20 setd_c 30)
(assert-eq "setd multi a" 10 setd_a)
(assert-eq "setd multi b" 15 setd_b) ; should keep original
(assert-eq "setd multi c" 30 setd_c)

; Scoping with lambda instead of let
((lambda (local_var)
	(assert-eq "Lambda scoping" 50 local_var)) 50)

; Conditionals
(assert-eq "If true"  "yes" (if :t "yes" "no"))
(assert-eq "If false" "no"  (if :nil "yes" "no"))

(defq cond_res (cond
	((= 1 2) "A")
	((= 1 1) "B")
	(:t "C")))
(assert-eq "Cond" "B" cond_res)

(assert-eq "cond passthrough" 123 (cond (:nil) (123)))
(assert-eq "cond passthrough 2" 45 (cond (45) (123)))

(assert-eq "Case" "two" (case 2 (1 "one") (2 "two") (3 "three")))
(assert-eq "Case list keys" "even" (case 2 ((1 3) "odd") ((2 4) "even") (:t "unknown")))
(assert-eq "Case default" "many" (case 5 (1 "one") (:t "many")))
(assert-eq "Case string" 2 (case "apple" ("orange" 1) ("apple" 2) (:t 0)))
(assert-eq "Case symbol" 2 (case 'foo (bar 1) (foo 2) (:t 0)))
(assert-eq "Case mixed" "one-half" (case 1.5 (1.0 "one") (1.5 "one-half") (:t "none")))

; Loops
(defq i 0 sum 0)
(while (< i 5)
	(setq sum (+ sum i))
	(++ i))
; 0+1+2+3+4 = 10
(assert-eq "While loop" 10 sum)

(defq i 0)
(until (= i 5)
	(++ i))
(assert-eq "Until loop" 5 i)

(defq sum_times 0)
(times 5
	(setq sum_times (+ sum_times (!))))
(assert-eq "times macro index sum" 10 sum_times)

(defq sum_for 0)
(for 3 7
	(setq sum_for (+ sum_for (!))))
(assert-eq "for macro index sum" 18 sum_for)

; Progn
(assert-eq "Progn last" 3 (progn 1 2 3))

; --- if: Multi-form Else & Passthrough ---
(defq if_m 0)
(assert-eq "if multi-form else" 12 (if :nil "then" (setq if_m 1) (++ if_m) (+ if_m 10)))
(assert-eq "if multi-form else side effect check" 2 if_m)
(assert-eq "if multi-form else skipped on true" "then" (if :t "then" (setq if_m 999)))
(assert-eq "if multi-form else skipped check" 2 if_m)
(assert-eq "if no else passthrough" :nil (if :nil "then"))

; --- ifn: Single-form, Multi-form Else & Passthrough ---
(assert-eq "ifn true"  "no"  (ifn :t "yes" "no"))
(assert-eq "ifn false" "yes" (ifn :nil "yes" "no"))
(assert-eq "ifn passthrough" 123 (ifn 123 45)) ; test form is true, so it passes it through!
(assert-eq "ifn default" 45 (ifn :nil 45))       ; test form is false, evaluates the default

(defq ifn_m 0)
(assert-eq "ifn multi-form else" 12 (ifn :t "then" (setq ifn_m 1) (++ ifn_m) (+ ifn_m 10)))
(assert-eq "ifn multi-form else side effect check" 2 ifn_m)
(assert-eq "ifn multi-form else skipped on false" "then" (ifn :nil "then" (setq ifn_m 999)))
(assert-eq "ifn multi-form else skipped check" 2 ifn_m)
(assert-eq "ifn truthy multi-form else overrides passthrough" 30 (ifn 123 "then" 10 20 30))

(defq condn_res (condn
	((= 1 1) "A")
	((= 1 2) "B")
	(:t "C")))
(assert-eq "condn" "B" condn_res)

(assert-eq "condn passthrough" 456 (condn (456)))
(assert-eq "condn false passthrough" :nil (condn (123) (:nil)))

; --- Logical Macros ---
(assert-true "AND logic" (and :t :t))
(assert-eq   "AND short" :nil (and :nil (throw "Should not eval" :nil)))
(assert-true "OR logic"  (or :nil :t))
(assert-eq	"or short 2" 5 (or :nil 5))
(assert-eq	"and short 2" 10 (and :t 10))

; --- when: 1-form and Multi-form ---
(assert-eq "when 1-form true" "exec" (when :t "exec"))
(assert-eq "when 1-form false returns nil" :nil (when :nil "exec"))

(defq when_m 0)
(assert-eq "when multi-form true" 3 (when :t (++ when_m) (++ when_m) (++ when_m)))
(assert-eq "when multi-form val" 3 when_m)
(assert-eq "when multi-form false returns nil" :nil (when :nil (++ when_m)))
(assert-eq "when multi-form false skipped check" 3 when_m)

; --- unless: 1-form and Multi-form ---
(assert-eq "unless 1-form false" "exec" (unless :nil "exec"))
(assert-eq "unless 1-form true returns nil" :nil (unless :t "exec"))
(assert-eq "unless 1-form truthy returns nil" :nil (unless 123 "exec"))

(defq unless_m 0)
(assert-eq "unless multi-form false" 3 (unless :nil (++ unless_m) (++ unless_m) (++ unless_m)))
(assert-eq "unless multi-form val" 3 unless_m)
(assert-eq "unless multi-form true returns nil" :nil (unless :t (++ unless_m)))
(assert-eq "unless multi-form truthy returns nil" :nil (unless "truthy" (++ unless_m)))
(assert-eq "unless multi-form true skipped check" 3 unless_m)

; --- Proof: when and unless always return :nil when body does not execute ---
(report-header "when / unless: guaranteed :nil on non-execution")

; when with false condition (single- and multi-form)
(assert-eq "when false literal 1-form" :nil (when :nil 100))
(assert-eq "when false literal multi-form" :nil (when :nil 10 20 30))
(assert-eq "when false expr 1-form" :nil (when (= 1 2) "executed"))
(assert-eq "when false expr multi-form" :nil (when (find 9 '(1 2 3)) "a" "b" "c"))
(assert-true "when false returns nil?" (nil? (when :nil "never")))

; unless with true and diverse truthy conditions (single- and multi-form)
; Proves that truthy test results (numbers, strings, symbols, lists) never leak
(assert-eq "unless :t 1-form" :nil (unless :t 100))
(assert-eq "unless :t multi-form" :nil (unless :t 10 20 30))

(assert-eq "unless number 42 1-form" :nil (unless 42 "executed"))
(assert-eq "unless number 42 multi-form" :nil (unless 42 1 2 3))
(assert-eq "unless number 0 1-form" :nil (unless 0 "executed"))
(assert-eq "unless number 0 multi-form" :nil (unless 0 1 2 3))

(assert-eq "unless string 1-form" :nil (unless "truthy string" "executed"))
(assert-eq "unless string multi-form" :nil (unless "truthy string" 1 2 3))

(assert-eq "unless symbol 1-form" :nil (unless 'some_sym "executed"))
(assert-eq "unless symbol multi-form" :nil (unless 'some_sym 1 2 3))

(assert-eq "unless list 1-form" :nil (unless '(1 2 3) "executed"))
(assert-eq "unless list multi-form" :nil (unless '(1 2 3) 1 2 3))

(assert-eq "unless dynamic expr 1-form" :nil (unless (+ 10 20) "executed"))
(assert-eq "unless dynamic expr multi-form" :nil (unless (cat "foo" "bar") 1 2 3))

(assert-true "unless truthy returns nil?" (nil? (unless 123 "never")))
(assert-true "unless :t returns nil?" (nil? (unless :t "never" "ever")))

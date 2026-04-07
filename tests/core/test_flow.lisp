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
(assert-eq "Case symbol" 2 (case 'foo ('bar 1) ('foo 2) (:t 0)))
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

; Progn
(assert-eq "Progn last" 3 (progn 1 2 3))

(assert-eq "ifn true"  "no"  (ifn :t "yes" "no"))
(assert-eq "ifn false" "yes" (ifn :nil "yes" "no"))
(assert-eq "ifn passthrough" 123 (ifn 123 45)) ; test form is true, so it passes it through!
(assert-eq "ifn default" 45 (ifn :nil 45))       ; test form is false, evaluates the default

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
(assert-true  "when"   (when :t :t))
(assert-true  "unless" (unless :nil :t))
(assert-eq	"or short 2" 5 (or :nil 5))
(assert-eq	"and short 2" 10 (and :t 10))

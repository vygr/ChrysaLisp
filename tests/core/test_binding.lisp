(report-header "Binding: bind, apply, eval, catch/throw")

; --- catch & throw ---
(defq catch_res (catch
	(throw "error message" 42)
	(progn
		; In catch handler, _ is the error string containing the object
		(assert-true "catch object in msg" (found? _ "42"))
		"recovered")))
(assert-eq "catch recovery" "recovered" catch_res)

; --- bind options ---
(defq b_val :nil)
(bind '(& b_val) '(10 20))
(assert-eq "bind & skip" 20 b_val)

(bind '(& &rest b_val) '(1 2 3))
(assert-list-eq "bind &rest" '(2 3) b_val)

(bind '(&most b_val &) '(1 2 3 4))
(assert-list-eq "bind &most" '(1 2 3) b_val)

(defq bo1 0 bo2 0)
(bind '(bo1 &optional bo2) '(100))
(assert-eq "bind &optional 1" 100 bo1)
(assert-eq "bind &optional 2" :nil bo2)

(defq bi1 0 bi2 0)
(bind '(bi1 &ignore bi2) '(1 2 3))
(assert-eq "bind &ignore 1" 1 bi1)
(assert-eq "bind &ignore 2" 0 bi2) ; bi2 remains 0, skipped

; --- bind destructuring ---
(defq dx 0 dy 0)
(bind '((dx dy)) (list (list 12 13)))
(assert-eq "bind destructuring dx" 12 dx)
(assert-eq "bind destructuring dy" 13 dy)

; --- apply ---
(assert-eq "apply +" 6 (apply (const +) '(1 2 3)))

; --- identity ---
(assert-eq "identity" 100 (identity 100))

; --- eval ---
(assert-eq "eval" 5 (eval '(+ 2 3)))

; --- macroexpand ---
; (inc x) expands to (#add x 1) in this environment
(defq exp (macroexpand '(inc x)))
(assert-eq "macroexpand first" "#add" (str (first exp)))
(assert-eq "macroexpand second" 'x (second exp))
(assert-eq "macroexpand third" 1 (third exp))

; --- eval-list ---
(assert-list-eq "eval-list" '(3 7) (eval-list '((+ 1 2) (+ 3 4))))

; --- prebind ---
(defq px 10)
(assert-list-eq "prebind" '(px 20) (prebind '(px 20)))

; --- read from sstream ---
(defq ss (string-stream "(a b c)"))
(defq r_res (read ss))
(assert-list-eq "read sstream" '(a b c) (first r_res))

(defq ri (repl-info))
(assert-true "repl-info" (and (list? ri) (>= (length ri) 2)))

; --- Core Environment Utils ---
(defq e_parent (env))
(defq e_child (env-push e_parent))
((lambda ()
	(defq local_sym 555)
	(export-symbols '(local_sym))
))
(assert-eq "export-symbols" 555 (get 'local_sym e_parent))

(defclass TestClassExtra () :nil (defmethod :test (this) 123))
(export-classes '(TestClassExtra))
(assert-true "export-classes" (not (nil? (get 'TestClassExtra))))

; --- exec ---
(defq exec_res :nil)
(exec '(setq exec_res 123))
(assert-eq "exec" 123 exec_res)

; --- export / export-symbols ---
(defq e_dst (env))
((lambda ()
	(defq exported_val 999)
	(export e_dst '(exported_val))))
(assert-eq "export root" 999 (get 'exported_val e_dst))

(defq *test_export_sym* 123)
(defq e_dst2 (env))
(export e_dst2 '(*test_export_sym*))
(assert-eq "export direct root" 123 (get '*test_export_sym* e_dst2))

; --- Quasi-Quotation (static-qq) ---
(defq sq_a 10 sq_b 20)
(assert-list-eq "static-qq" '(10 20 30) (static-qq (,sq_a ,sq_b 30)))
(assert-list-eq "static-qq splice" '(10 20 30) (static-qq (~(list 10 20) 30)))
(assert-list-eq "static-qqp" '(1 2 3) (static-qqp (1 2 3)))

; --- nested #
(defq out_a (list) out_b (list))
(map (# (push out_a %0) (map (# (bind '(%%0 %%1) %0) (push out_b %0 %1)) %0))
	'(((1 a) (2 b) (3 c)) ((4 d) (5 e) (6 f)) ((7 x) (8 y) (9 z))))
(assert-list-eq "nested # a" '(((1 a) (2 b) (3 c)) ((4 d) (5 e) (6 f)) ((7 x) (8 y) (9 z))) out_a)
(assert-list-eq "nested # b" '(1 a 2 b 3 c 4 d 5 e 6 f 7 x 8 y 9 z) out_b)

;;;;;;;;;;;;;;;;;
; tests/utils.inc
;;;;;;;;;;;;;;;;;
; Global test counters
(defq *test_passes* 0 *test_failures* 0)

(defun report-header (section-name)
    (print)
    (print "=== Testing: " section-name " ==="))

(defmacro assert-eq (name expected form)
    ; (assert-eq "name" expected actual_expression)
    (defq res (gensym))
    `(progn
        (defq ,res ,form)
        (if (eql ,expected ,res)
            (progn 
                (++ *test_passes*) 
                (print "[PASS] " ,name))
            (progn 
                (++ *test_failures*) 
                (print "[FAIL] " ,name " | Expected: " ,expected " | Got: " ,res)))))

(defmacro assert-true (name form)
    `(assert-eq ,name :t (if ,form :t :nil)))

; Simple deep equality using string comparison
(defun equal? (a b)
    (eql (str a) (str b)))

(defmacro assert-list-eq (name expected form)
    (defq res (gensym) exp (gensym))
    `(progn
        (defq ,res ,form ,exp ,expected)
        (if (equal? ,exp ,res)
            (progn 
                (++ *test_passes*) 
                (print "[PASS] " ,name))
            (progn 
                (++ *test_failures*) 
                (print "[FAIL] " ,name " | Expected: " ,exp " | Got: " ,res)))))

(defun print-summary ()
    (print)
    (print "=== Test Summary ===")
    (print "Passed: " *test_passes*)
    (print "Failed: " *test_failures*)
    (if (> *test_failures* 0)
        (print "RESULT: FAILURE")
        (print "RESULT: SUCCESS")))
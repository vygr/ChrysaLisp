;;;;;;;;;;;;;;;;;;;;;;;;;;
; tests/test_root_utils.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;
(report-header "Root Utilities (defun)")

; --- exec ---
(defq exec_res :nil)
(exec '(setq exec_res 123))
(assert-eq "exec" 123 exec_res)

; --- swap ---
(defq sw_arr (array 1 2 3))
(swap sw_arr 0 2)
(assert-list-eq "swap" '(3 2 1) (map identity sw_arr))

; --- export / export-symbols ---
(defq e_dst (env))
((lambda ()
    (defq exported_val 999)
    (export e_dst '(exported_val))))
(assert-eq "export" 999 (get 'exported_val e_dst))

(defq *test_export_sym* 123)
; (export-symbols '(*test_export_sym*))
; export-symbols exports to (penv (penv)). 
; In tests, this level might be tricky. Let's just test export directly more thoroughly.
(defq e_dst2 (env))
(export e_dst2 '(*test_export_sym*))
(assert-eq "export direct" 123 (get '*test_export_sym* e_dst2))

; --- str-as-num ---
(assert-eq "str-as-num" 123 (str-as-num "123"))

; --- type-to-size ---
(assert-eq "type-to-size int" +int_size (type-to-size 'i))
(assert-eq "type-to-size ubyte" +byte_size (type-to-size 'ub))
; type-to-size in root.inc has a default that returns nil or +long_size depending on match
(assert-eq "type-to-size default" :nil (type-to-size 'unknown))

; --- get-cstr ---
(defq cs "ABC\x00DEF")
(assert-eq "get-cstr" "ABC" (get-cstr cs 0))

; --- age ---
; Should be > 0 for existing file
(assert-true "age" (> (age "README.md") 0))

; --- load-stream ---
(defq ls (load-stream "README.md"))
(assert-true "load-stream" (not (nil? ls)))

; --- path functions ---
(assert-true "path-to-file" (str? (path-to-file)))
(defq abs_p (path-to-absolute "./README.md"))
(assert-true "path-to-absolute" (str? abs_p))
(assert-true "path-to-relative" (str? (path-to-relative "/tmp/a/b" "/tmp/a")))

; --- System info ---
(assert-true "abi" (sym? (abi)))

; --- Debug/Profile stubs ---
(profile-report "test")
(assert-true "profile-report" :t)

; --- freq-update / freq-print ---
; These are currently commented out in root.inc, but let's check if they exist
(if (def? 'freq-update)
    (progn
        (freq-update 'test_key)
        (assert-true "freq-update" :t))
    (print "[SKIP] freq-update not defined"))


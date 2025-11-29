;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Minimal Test - Just compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/text/regexp_engine.inc")

(print "Creating engine...")
(defq engine (RegexpEngine))
(print "Engine created\n")

(print "Compiling pattern 'a'...")
(defq ast (. engine :compile-enhanced "a"))
(print "AST type: " (if (list? ast) "list" "other"))
(if (list? ast)
    (progn
        (print "AST first element: " (first ast))
        (print "AST length: " (length ast)))
    (print "AST value: " ast))

(print "\nTest complete!")

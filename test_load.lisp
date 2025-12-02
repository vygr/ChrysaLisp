;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Quick Load Test for RegexpEngine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "\n=== RegexpEngine Load Test ===\n")

(print "1. Loading library...")
(catch
    (progn
        (import "lib/text/regexp_engine.inc")
        (print "   ✓ Library loaded successfully\n"))
    (progn
        (print "   ✗ Load failed: " _)
        (exit)))

(print "2. Creating engine instance...")
(catch
    (progn
        (defq engine (RegexpEngine))
        (print "   ✓ Engine created: " engine "\n"))
    (progn
        (print "   ✗ Creation failed: " _)
        (exit)))

(print "3. Testing simple literal compilation...")
(catch
    (progn
        (defq ast (. engine :compile-enhanced "hello"))
        (print "   ✓ Compiled 'hello'")
        (print "   AST: " ast "\n"))
    (progn
        (print "   ✗ Compilation failed: " _)
        (exit)))

(print "4. Testing digit class compilation...")
(catch
    (progn
        (defq ast (. engine :compile-enhanced "\\d+"))
        (print "   ✓ Compiled '\\d+'")
        (print "   AST: " ast "\n"))
    (progn
        (print "   ✗ Compilation failed: " _)
        (exit)))

(print "5. Testing character class compilation...")
(catch
    (progn
        (defq ast (. engine :compile-enhanced "[a-z]+"))
        (print "   ✓ Compiled '[a-z]+'")
        (print "   AST: " ast "\n"))
    (progn
        (print "   ✗ Compilation failed: " _)
        (exit)))

(print "6. Testing group compilation...")
(catch
    (progn
        (defq ast (. engine :compile-enhanced "(abc)"))
        (print "   ✓ Compiled '(abc)'")
        (print "   AST: " ast "\n"))
    (progn
        (print "   ✗ Compilation failed: " _)
        (exit)))

(print "=== All Load Tests Passed! ===\n")

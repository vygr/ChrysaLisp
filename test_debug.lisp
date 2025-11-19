;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Debug Test for RegexpEngine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/text/regexp_engine.inc")

(print "\n=== Debug Test ===\n")

(defq engine (RegexpEngine))

(print "Test 1: Compile 'hello'")
(defq ast (. engine :compile-enhanced "hello"))
(print "AST structure:")
(print ast)
(print "")

(print "Test 2: Execute AST on 'hello world' at position 0")
(defq result (. engine :exec-ast "hello world" ast 0))
(print "Result: " result)
(print "")

(print "Test 3: Execute AST on 'hello world' at position 0 with sequence")
(if (and (list? ast) (eql (first ast) :sequence))
    (progn
        (print "AST is a sequence with " (length (second ast)) " nodes")
        (defq nodes (second ast))
        (print "First node: " (first nodes))
        (print "First node type: " (first (first nodes)))
        (if (eql (first (first nodes)) :literal-string)
            (print "Literal string value: '" (second (first nodes)) "'")))
    (print "AST is not a sequence"))
(print "")

(print "Test 4: Try basic Regexp for comparison")
(defq basic (Regexp))
(print "Basic Regexp match 'hello' in 'hello world': ")
(print (. basic :match? "hello world" "hello"))

(print "\n=== End Debug ===\n")

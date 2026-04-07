(report-header "Tree Load/Save")

(import "lib/collections/tree.inc")

; 1. Create a complex tree
(defq original_tree (Emap 11))
(. original_tree :insert 'name "ChrysaLisp")
(. original_tree :insert 'version 0.25)
(. original_tree :insert 'features (list "parallel" "distributed" "lisp"))

(defq sub_map (Fmap 5))
(. sub_map :insert 'cpu 'x86_64)
(. sub_map :insert 'os 'darwin)
(. original_tree :insert 'env sub_map)

; 2. Save to memory stream
(defq ms (memory-stream))
(tree-save ms original_tree)

; 3. Rewind and verify format (at least the first line)
(stream-seek ms 0 0)
(defq line1 (trim (read-line ms)))
; The first line should be the root node's type and buckets: ((:Emap 11)
(assert-eq "Tree Format: Root" "((:Emap 11)" line1)

; 4. Seek to beginning and load
(stream-seek ms 0 0)
(defq loaded_tree (tree-load ms))

; 5. Verify contents
(assert-eq "Tree Load: String" "ChrysaLisp" (. loaded_tree :find 'name))
(assert-eq "Tree Load: Number" 0.25 (. loaded_tree :find 'version))
(assert-list-eq "Tree Load: List" (list "parallel" "distributed" "lisp") (. loaded_tree :find 'features))

(defq loaded_env (. loaded_tree :find 'env))
(assert-eq "Tree Load: Nested Key 1" 'x86_64 (. loaded_env :find 'cpu))
(assert-eq "Tree Load: Nested Key 2" 'darwin (. loaded_env :find 'os))

; 6. Test with Fset
(defq s (Fset 5))
(. s :insert "A")
(. s :insert "B")
(defq ms2 (memory-stream))
(tree-save ms2 s)
(stream-seek ms2 0 0)
(defq loaded_s (tree-load ms2))
(assert-eq "Tree Load: Fset A" "A" (. loaded_s :find "A"))
(assert-eq "Tree Load: Fset B" "B" (. loaded_s :find "B"))

; 7. Test with Path/Array
(defq p (path 1.0 2.0 3.0 4.0))
(defq ms3 (memory-stream))
(tree-save ms3 p)
(stream-seek ms3 0 0)
(defq loaded_p (tree-load ms3))
; path is like a fixeds/nums array. equal? handles seq comparison.
(assert-true "Tree Load: Path" (equal? p loaded_p))

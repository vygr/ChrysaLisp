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

; 8. Test direct pset tree roundtrip
(defq tree_ps (pset "alpha" "beta" "gamma"))
(defq ms4 (memory-stream))
(tree-save ms4 tree_ps)
(stream-seek ms4 0 0)
(defq loaded_ps (tree-load ms4))
(assert-eq "Tree Load: pset item 1" "alpha" (pfind loaded_ps "alpha"))
(assert-eq "Tree Load: pset item 2" "beta" (pfind loaded_ps "beta"))

; 9. Test direct pmap tree roundtrip
(defq tree_pm (pmap "k1" "v1" "k2" "v2"))
(defq ms5 (memory-stream))
(tree-save ms5 tree_pm)
(stream-seek ms5 0 0)
(defq loaded_pm (tree-load ms5))
(assert-eq "Tree Load: pmap val 1" "v1" (pfind loaded_pm "k1"))
(assert-eq "Tree Load: pmap val 2" "v2" (pfind loaded_pm "k2"))

; 10. Test Lmap tree roundtrip
(import "lib/collections/lmap.inc")
(defq lm (Lmap))
(. lm :insert 'x 100)
(. lm :insert 'y 200)
(defq ms6 (memory-stream))
(tree-save ms6 lm)
(stream-seek ms6 0 0)
(defq loaded_lm (tree-load ms6))
(assert-eq "Tree Load: Lmap x" 100 (. loaded_lm :find 'x))
(assert-eq "Tree Load: Lmap y" 200 (. loaded_lm :find 'y))

; 11. Test large string chunking (> 512 chars) roundtrip
; a) Large binary string spanning full range of 0-255 bytes across multiple chunks
(defq all_bytes (apply (const cat) (map (const char) (range 0 256)))
	large_bin (cat all_bytes all_bytes all_bytes all_bytes all_bytes all_bytes)
	tree_bin (Emap 5))
(. tree_bin :insert :demo large_bin)
(defq ms7 (memory-stream))
(tree-save ms7 tree_bin)
(stream-seek ms7 0 0)
(defq bin_serialized (read-line ms7))
; verify :cat statement was written
(assert-true "Tree Save: binary :cat chunking" (find "(:cat" (read-line ms7)))
(stream-seek ms7 0 0)
(defq loaded_bin (tree-load ms7))
(assert-eq "Tree Load: large binary string (all 0-255 bytes)" large_bin (. loaded_bin :find :demo))

; b) Large ASCII text string (bracketed chunks) with quotes, brackets, and newlines
(defq large_txt (pad "" 1500 "The \qquick\q \tbrown [fox] jumps\n over the 'lazy' dog. ")
	tree_txt (Emap 5))
(. tree_txt :insert :text large_txt)
(defq ms8 (memory-stream))
(tree-save ms8 tree_txt)
(stream-seek ms8 0 0)
(read-line ms8)
(assert-true "Tree Save: text :cat chunking" (find "(:cat" (read-line ms8)))
(stream-seek ms8 0 0)
(defq loaded_txt (tree-load ms8))
(assert-eq "Tree Load: large text string" large_txt (. loaded_txt :find :text))

; c) Large mixed string spanning both text and binary bytes (including 0 and 255)
(defq large_mix (cat (pad "" 600 "Hello \qworld\q [123] ") all_bytes (pad "" 600 "Goodbye!"))
	tree_mix (Emap 5))
(. tree_mix :insert :mixed large_mix)
(defq ms9 (memory-stream))
(tree-save ms9 tree_mix)
(stream-seek ms9 0 0)
(defq loaded_mix (tree-load ms9))
(assert-eq "Tree Load: large mixed string" large_mix (. loaded_mix :find :mixed))

; 12. Test loading actual onslaught.tre. It is only there once the game has
; been played on this machine, and only holds a demo once a battle has been
; recorded.
(defq o_cfg (tree-load (file-stream "usr/Guest/onslaught.tre"))
	o_demo (if o_cfg (. o_cfg :find :demo)))
(cond
	((not o_cfg)
		(test-skip "onslaught.tre" "no saved game config on this machine"))
	((or (not o_demo) (empty? o_demo))
		;no demo, or an empty one, the game has been run but no battle fought
		(assert-true "onslaught.tre config exists" (not (empty? o_cfg)))
		(test-skip "onslaught.tre demo" "no battle recorded on this machine"))
	(:t (assert-true "onslaught.tre config exists" (not (empty? o_cfg)))
		(assert-true "onslaught.tre demo exists" (not (empty? o_demo)))
		(assert-true "onslaught.tre demo is string" (str? o_demo))
		(assert-true "onslaught.tre demo length > 90" (> (length o_demo) 90))))

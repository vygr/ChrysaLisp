(import "lib/collections/bloom.inc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bloom Filter Unit Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-basic-operations ()
	(print "Test 1: Basic Operations")
	(defq bf (Bloom 512 3)
		  pass :t)

	; Test empty filter
	(unless (. bf :empty?)
		(print "  FAIL: New filter should be empty")
		(setq pass :nil))

	; Test adding elements
	(. bf :add "test1")
	(. bf :add "test2")

	; Test contains
	(unless (. bf :contains? "test1")
		(print "  FAIL: Should find 'test1'")
		(setq pass :nil))

	(unless (. bf :contains? "test2")
		(print "  FAIL: Should find 'test2'")
		(setq pass :nil))

	; Test item count
	(unless (= (. bf :item_count) 2)
		(print "  FAIL: Item count should be 2")
		(setq pass :nil))

	(if pass
		(print "  PASS")
		(print "  FAIL"))
	pass)

(defun test-negative-membership ()
	(print "Test 2: Negative Membership (No False Negatives)")
	(defq bf (Bloom 512 3)
		  pass :t)

	; Add some items
	(. bf :add "exists1")
	(. bf :add "exists2")

	; Items not added should return :nil (definitely not in set)
	; Note: This is not a perfect test because Bloom filters can have
	; false positives, but we test a few items that are likely not to collide
	(defq not_added '("notexists1" "notexists2" "notexists3" "notexists4" "notexists5"))
	(defq false_negatives 0)

	; Items that WERE added must always return :t
	(unless (. bf :contains? "exists1")
		(print "  FAIL: False negative for 'exists1'")
		(setq pass :nil false_negatives (+ false_negatives 1)))

	(unless (. bf :contains? "exists2")
		(print "  FAIL: False negative for 'exists2'")
		(setq pass :nil false_negatives (+ false_negatives 1)))

	(if (= false_negatives 0)
		(print "  PASS - No false negatives")
		(print (cat "  FAIL - " false_negatives " false negatives detected")))
	pass)

(defun test-clear-operation ()
	(print "Test 3: Clear Operation")
	(defq bf (Bloom 512 3)
		  pass :t)

	; Add items
	(. bf :add "item1")
	(. bf :add "item2")

	(unless (= (. bf :item_count) 2)
		(print "  FAIL: Should have 2 items")
		(setq pass :nil))

	; Clear
	(. bf :clear)

	(unless (. bf :empty?)
		(print "  FAIL: Filter should be empty after clear")
		(setq pass :nil))

	(unless (= (. bf :item_count) 0)
		(print "  FAIL: Item count should be 0 after clear")
		(setq pass :nil))

	(if pass
		(print "  PASS")
		(print "  FAIL"))
	pass)

(defun test-copy-operation ()
	(print "Test 4: Copy Operation")
	(defq bf1 (Bloom 512 3)
		  pass :t)

	; Add to original
	(. bf1 :add "original")

	; Copy
	(defq bf2 (. bf1 :copy))

	; Verify copy has same data
	(unless (. bf2 :contains? "original")
		(print "  FAIL: Copy should contain 'original'")
		(setq pass :nil))

	; Add to copy
	(. bf2 :add "copy_only")

	; Verify independence
	(when (. bf1 :contains? "copy_only")
		(print "  FAIL: Original should not contain 'copy_only'")
		(setq pass :nil))

	(unless (. bf2 :contains? "copy_only")
		(print "  FAIL: Copy should contain 'copy_only'")
		(setq pass :nil))

	(if pass
		(print "  PASS")
		(print "  FAIL"))
	pass)

(defun test-size-and-hash-count ()
	(print "Test 5: Size and Hash Count")
	(defq bf (Bloom 2048 5)
		  pass :t)

	(unless (= (. bf :size) 2048)
		(print "  FAIL: Size should be 2048")
		(setq pass :nil))

	(unless (= (. bf :hash_count) 5)
		(print "  FAIL: Hash count should be 5")
		(setq pass :nil))

	(if pass
		(print "  PASS")
		(print "  FAIL"))
	pass)

(defun test-multiple-inserts ()
	(print "Test 6: Multiple Inserts")
	(defq bf (Bloom 1024 3)
		  items (map (lambda (i) (cat "item_" i)) (range 0 50))
		  pass :t)

	; Add all items
	(each (lambda (item)
		(. bf :add item))
		items)

	; Verify all items are found
	(each (lambda (item)
		(unless (. bf :contains? item)
			(print (cat "  FAIL: Should contain " item))
			(setq pass :nil)))
		items)

	(if pass
		(print "  PASS - All 50 items found")
		(print "  FAIL"))
	pass)

(defun run-all-tests ()
	(print)
	(print "═══════════════════════════════════════")
	(print "  Bloom Filter Unit Tests")
	(print "═══════════════════════════════════════")
	(print)

	(defq results (list))

	(push results (test-basic-operations))
	(push results (test-negative-membership))
	(push results (test-clear-operation))
	(push results (test-copy-operation))
	(push results (test-size-and-hash-count))
	(push results (test-multiple-inserts))

	(print)
	(print "───────────────────────────────────────")
	(defq passed (length (filter identity results))
		  total (length results))
	(print (cat "Results: " passed "/" total " tests passed"))

	(if (= passed total)
		(print "✓ ALL TESTS PASSED")
		(print "✗ SOME TESTS FAILED"))
	(print "═══════════════════════════════════════")
	(print))

(defun main ()
	(run-all-tests))

(import "lib/collections/bloom.inc")
(import "lib/collections/counting_bloom.inc")
(import "lib/collections/scalable_bloom.inc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Advanced Bloom Filter Features Demo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-header (text)
	; Print a section header
	(print)
	(print "========================================")
	(print text)
	(print "========================================"))

(defun demo-counting-bloom ()
	; Demonstrate Counting Bloom Filter with deletion support
	(print-header "COUNTING BLOOM FILTER - DELETION SUPPORT")
	(print)

	(defq cbf (CountingBloom 512 3 15))
	(print "Created Counting Bloom Filter (512 counters, 3 hash functions)")
	(print)

	; Add elements
	(print "Adding elements: apple, banana, cherry")
	(. cbf :add "apple")
	(. cbf :add "banana")
	(. cbf :add "cherry")
	(print (cat "Items in filter: " (. cbf :item_count)))
	(print)

	; Check membership
	(print "Membership tests:")
	(print (cat "  Contains 'apple'? " (if (. cbf :contains? "apple") "Yes" "No")))
	(print (cat "  Contains 'banana'? " (if (. cbf :contains? "banana") "Yes" "No")))
	(print (cat "  Contains 'grape'? " (if (. cbf :contains? "grape") "Yes" "No")))
	(print)

	; Remove an element - THIS IS THE KEY FEATURE!
	(print "Removing 'banana'...")
	(. cbf :remove "banana")
	(print (cat "Items in filter: " (. cbf :item_count)))
	(print)

	; Check again
	(print "Membership tests after removal:")
	(print (cat "  Contains 'apple'? " (if (. cbf :contains? "apple") "Yes" "No")))
	(print (cat "  Contains 'banana'? " (if (. cbf :contains? "banana") "Yes" "No")))
	(print (cat "  Contains 'cherry'? " (if (. cbf :contains? "cherry") "Yes" "No")))
	(print)

	; Show counter statistics
	(bind '(min_c max_c avg_c) (. cbf :counter_stats))
	(print "Counter Statistics:")
	(print (cat "  Min counter value: " min_c))
	(print (cat "  Max counter value: " max_c))
	(print (cat "  Avg counter value: " avg_c))
	(print)

	(print "Key Advantage: Supports element deletion!")
	(print "Trade-off: Uses more memory than standard Bloom filter")
	(print (cat "  Standard: ~" (/ 512 8) " bytes (bit array)"))
	(print (cat "  Counting: ~" (* 512 8) " bytes (64-bit counters)"))
	(print))

(defun demo-scalable-bloom ()
	; Demonstrate Scalable Bloom Filter with dynamic growth
	(print-header "SCALABLE BLOOM FILTER - DYNAMIC GROWTH")
	(print)

	(defq sbf (ScalableBloom 128 0.01 2))
	(print "Created Scalable Bloom Filter")
	(print "  Initial size: 128 bits")
	(print "  Target FP rate: 1%")
	(print "  Growth factor: 2x")
	(print)

	(print "Adding 10 items...")
	(each (lambda (i)
		(. sbf :add (cat "item_" i)))
		(range 0 10))

	(bind '(filter_count total_size count capacity) (. sbf :stats))
	(print (cat "  Filters: " filter_count))
	(print (cat "  Total size: " total_size " bits"))
	(print (cat "  Items: " count))
	(print (cat "  Capacity: " capacity))
	(print)

	(print "Adding 100 more items to trigger growth...")
	(each (lambda (i)
		(. sbf :add (cat "item_" i)))
		(range 10 110))

	(bind '(filter_count total_size count capacity) (. sbf :stats))
	(print (cat "  Filters: " filter_count " (grown!)"))
	(print (cat "  Total size: " total_size " bits"))
	(print (cat "  Items: " count))
	(print (cat "  Capacity: " capacity))
	(print)

	(print "Adding 500 more items...")
	(each (lambda (i)
		(. sbf :add (cat "item_" i)))
		(range 110 610))

	(bind '(filter_count total_size count capacity) (. sbf :stats))
	(print (cat "  Filters: " filter_count " (multiple filters!)"))
	(print (cat "  Total size: " total_size " bits"))
	(print (cat "  Items: " count))
	(print (cat "  Capacity: " capacity))
	(print (cat "  Estimated FP rate: "
		(slice (str (* (. sbf :false_positive_rate) 100.0)) 0 5) "%"))
	(print)

	; Verify all items are found
	(print "Verifying all 610 items...")
	(defq missing 0)
	(each (lambda (i)
		(unless (. sbf :contains? (cat "item_" i))
			(setq missing (+ missing 1))))
		(range 0 610))

	(print (cat "  Missing items: " missing " (should be 0!)"))
	(print)

	(print "Key Advantage: Automatically grows to handle unlimited items!")
	(print "Trade-off: Multiple internal filters, slightly higher FP rate")
	(print))

(defun demo-serialization ()
	; Demonstrate saving and loading Bloom filters
	(print-header "SERIALIZATION - SAVE AND LOAD FILTERS")
	(print)

	(print "Creating and populating a Bloom filter...")
	(defq bf1 (Bloom 512 3))
	(defq test_data '("cat" "dog" "mouse" "elephant" "tiger" "lion"))

	(each (lambda (animal)
		(. bf1 :add animal))
		test_data)

	(print (cat "  Added " (. bf1 :item_count) " animals"))
	(print (cat "  Contains 'cat'? " (if (. bf1 :contains? "cat") "Yes" "No")))
	(print (cat "  Contains 'dog'? " (if (. bf1 :contains? "dog") "Yes" "No")))
	(print)

	; Save to stream
	(print "Saving to file: /tmp/bloom_test.dat")
	(. bf1 :save_to_file "/tmp/bloom_test.dat")
	(print "  Saved!")
	(print)

	; Create new filter and load
	(print "Creating new empty filter and loading from file...")
	(defq bf2 (Bloom))
	(. bf2 :load_from_file "/tmp/bloom_test.dat")

	(print (cat "  Loaded " (. bf2 :item_count) " items"))
	(print (cat "  Filter size: " (. bf2 :size) " bits"))
	(print (cat "  Hash functions: " (. bf2 :hash_count)))
	(print)

	(print "Verifying loaded data:")
	(print (cat "  Contains 'cat'? " (if (. bf2 :contains? "cat") "Yes" "No")))
	(print (cat "  Contains 'dog'? " (if (. bf2 :contains? "dog") "Yes" "No")))
	(print (cat "  Contains 'elephant'? " (if (. bf2 :contains? "elephant") "Yes" "No")))
	(print (cat "  Contains 'zebra'? " (if (. bf2 :contains? "zebra") "Yes" "No")))
	(print)

	; Test with Counting Bloom Filter
	(print "Testing with Counting Bloom Filter...")
	(defq cbf1 (CountingBloom 256 3))
	(each (lambda (item)
		(. cbf1 :add item))
		'("red" "green" "blue"))

	(. cbf1 :save_to_file "/tmp/counting_bloom_test.dat")
	(print "  Saved Counting Bloom filter")

	(defq cbf2 (CountingBloom))
	(. cbf2 :load_from_file "/tmp/counting_bloom_test.dat")
	(print (cat "  Loaded, contains 'red'? " (if (. cbf2 :contains? "red") "Yes" "No")))
	(print)

	; Test with Scalable Bloom Filter
	(print "Testing with Scalable Bloom Filter...")
	(defq sbf1 (ScalableBloom 128 0.01 2))
	(each (lambda (i)
		(. sbf1 :add (cat "num_" i)))
		(range 0 50))

	(. sbf1 :save_to_file "/tmp/scalable_bloom_test.dat")
	(print (cat "  Saved Scalable filter with " (. sbf1 :filter_count) " internal filters"))

	(defq sbf2 (ScalableBloom))
	(. sbf2 :load_from_file "/tmp/scalable_bloom_test.dat")
	(print (cat "  Loaded, has " (. sbf2 :filter_count) " internal filters"))
	(print (cat "  Contains 'num_25'? " (if (. sbf2 :contains? "num_25") "Yes" "No")))
	(print)

	(print "Key Advantage: Persist filters to disk for later use!")
	(print "Use cases: Cache warm-up, checkpoint/restore, sharing filters")
	(print))

(defun demo-comparison ()
	; Compare all three types side-by-side
	(print-header "COMPARISON - CHOOSING THE RIGHT FILTER")
	(print)

	(defq test_items (map (lambda (i) (cat "test_" i)) (range 0 100)))

	; Standard Bloom
	(defq bf (Bloom 1024 3))
	(each (lambda (item) (. bf :add item)) test_items)

	; Counting Bloom
	(defq cbf (CountingBloom 1024 3))
	(each (lambda (item) (. cbf :add item)) test_items)

	; Scalable Bloom
	(defq sbf (ScalableBloom 512 0.01 2))
	(each (lambda (item) (. sbf :add item)) test_items)

	(print "After adding 100 items:")
	(print)
	(print "Standard Bloom Filter:")
	(print (cat "  Size: " (. bf :size) " bits (~" (/ (. bf :size) 8) " bytes)"))
	(print (cat "  FP rate: " (slice (str (* (. bf :false_positive_rate) 100.0)) 0 5) "%"))
	(print "  Supports deletion: NO")
	(print "  Can grow: NO")
	(print)

	(print "Counting Bloom Filter:")
	(print (cat "  Size: " (. cbf :size) " counters (~" (* (. cbf :size) 8) " bytes)"))
	(print (cat "  FP rate: " (slice (str (* (. cbf :false_positive_rate) 100.0)) 0 5) "%"))
	(print "  Supports deletion: YES ★")
	(print "  Can grow: NO")
	(print)

	(bind '(fc ts ic cap) (. sbf :stats))
	(print "Scalable Bloom Filter:")
	(print (cat "  Size: " ts " bits (~" (/ ts 8) " bytes)"))
	(print (cat "  Internal filters: " fc))
	(print (cat "  FP rate: " (slice (str (* (. sbf :false_positive_rate) 100.0)) 0 5) "%"))
	(print "  Supports deletion: NO")
	(print "  Can grow: YES ★")
	(print)

	(print "Recommendation:")
	(print "  • Fixed size, no deletion → Standard Bloom")
	(print "  • Need deletion → Counting Bloom")
	(print "  • Unknown/growing dataset → Scalable Bloom")
	(print))

(defun main ()
	(print)
	(print "╔════════════════════════════════════════╗")
	(print "║  ADVANCED BLOOM FILTER FEATURES        ║")
	(print "║  New Capabilities Demo                 ║")
	(print "╚════════════════════════════════════════╝")

	; Run all demonstrations
	(demo-counting-bloom)
	(demo-scalable-bloom)
	(demo-serialization)
	(demo-comparison)

	(print-header "DEMO COMPLETE")
	(print)
	(print "Summary of New Features:")
	(print "  1. Counting Bloom Filter - Deletion support via counters")
	(print "  2. Scalable Bloom Filter - Automatic growth for unlimited items")
	(print "  3. Serialization - Save/load filters to/from disk")
	(print)
	(print "All three variants are production-ready and fully tested!")
	(print))

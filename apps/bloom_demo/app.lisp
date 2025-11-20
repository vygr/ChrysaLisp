(import "lib/collections/bloom.inc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bloom Filter Demo Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-header (text)
	; Print a section header
	(print)
	(print "========================================")
	(print text)
	(print "========================================"))

(defun print-divider ()
	; Print a simple divider
	(print "----------------------------------------"))

(defun demo-basic-operations ()
	; Demonstrate basic Bloom filter operations
	(print-header "BASIC OPERATIONS")
	(print)

	; Create a Bloom filter with 512 bits and 3 hash functions
	(defq bf (Bloom 512 3))
	(print "Created Bloom filter with:")
	(print (cat "  - Bit size: " (. bf :size)))
	(print (cat "  - Hash functions: " (. bf :hash_count)))
	(print)

	; Add some elements
	(print "Adding elements: apple, banana, cherry, date")
	(each (lambda (item)
		(. bf :add item))
		'("apple" "banana" "cherry" "date"))
	(print (cat "Items added: " (. bf :item_count)))
	(print)

	; Test membership
	(print "Testing membership:")
	(each (lambda (item)
		(defq result (if (. bf :contains? item) "POSSIBLY in set" "DEFINITELY NOT in set"))
		(print (cat "  '" item "' -> " result)))
		'("apple" "banana" "grape" "date" "elderberry"))
	(print))

(defun demo-false-positives ()
	; Demonstrate false positive behavior
	(print-header "FALSE POSITIVE DEMONSTRATION")
	(print)

	(defq bf (Bloom 256 2))
	(print "Created small Bloom filter (256 bits, 2 hash functions)")
	(print "Adding 20 fruit names...")
	(print)

	(defq fruits '("apple" "banana" "cherry" "date" "elderberry"
				   "fig" "grape" "honeydew" "kiwi" "lemon"
				   "mango" "nectarine" "orange" "papaya" "quince"
				   "raspberry" "strawberry" "tangerine" "ugli" "watermelon"))

	(each (lambda (fruit)
		(. bf :add fruit))
		fruits)

	(print (cat "Items added: " (. bf :item_count)))
	(print (cat "Estimated false positive rate: "
		(slice (str (* (. bf :false_positive_rate) 100.0)) 0 5) "%"))
	(print)

	; Test with items NOT in the set
	(print "Testing items NOT added to the filter:")
	(defq test_items '("avocado" "blueberry" "coconut" "dragonfruit" "guava"))
	(defq false_positives 0)

	(each (lambda (item)
		(defq is_possibly_in (. bf :contains? item))
		(print (cat "  '" item "' -> "
			(if is_possibly_in "POSSIBLY in set (FALSE POSITIVE!)" "DEFINITELY NOT in set")))
		(when is_possibly_in
			(setq false_positives (+ false_positives 1))))
		test_items)

	(print)
	(print (cat "False positives encountered: " false_positives " out of " (length test_items)))
	(print))

(defun demo-performance-comparison ()
	; Compare Bloom filter with regular set for membership testing
	(print-header "SPACE EFFICIENCY COMPARISON")
	(print)

	; Create a Bloom filter
	(defq bf (Bloom 2048 3)
		  regular_set (Fset 16))

	(print "Adding 100 strings to both Bloom filter and regular Fset...")
	(defq test_data (map (lambda (i) (cat "item_" i)) (range 0 100)))

	(each (lambda (item)
		(. bf :add item)
		(. regular_set :insert item))
		test_data)

	(print)
	(print "Bloom Filter:")
	(print (cat "  - Bit array size: " (. bf :size) " bits"))
	(print (cat "  - Hash functions: " (. bf :hash_count)))
	(print (cat "  - Items tracked: " (. bf :item_count)))
	(print (cat "  - Estimated false positive rate: "
		(slice (str (* (. bf :false_positive_rate) 100.0)) 0 5) "%"))
	(print)

	(print "Regular Fset:")
	(print "  - Stores actual objects in memory")
	(print "  - No false positives")
	(print "  - Higher memory usage for large datasets")
	(print))

(defun demo-optimal-parameters ()
	; Demonstrate optimal parameter calculation
	(print-header "OPTIMAL PARAMETER CALCULATION")
	(print)

	(defq expected_items 1000
		  bf (Bloom 8192 5))

	(print (cat "Bloom filter size: " (. bf :size) " bits"))
	(print (cat "Expected items to store: " expected_items))
	(print)

	(defq optimal_k (. bf :optimal_hash_count expected_items))
	(print (cat "Optimal number of hash functions: " optimal_k))
	(print (cat "Current number of hash functions: " (. bf :hash_count)))
	(print)

	(print "Note: Optimal hash count minimizes false positive rate")
	(print "      Formula: k = (m/n) * ln(2)"))
	(print))

(defun demo-clear-and-reuse ()
	; Demonstrate clearing and reusing a Bloom filter
	(print-header "CLEAR AND REUSE")
	(print)

	(defq bf (Bloom 512 3))

	(print "Adding 5 cities...")
	(each (lambda (city)
		(. bf :add city))
		'("Paris" "London" "Tokyo" "NewYork" "Sydney"))

	(print (cat "Items in filter: " (. bf :item_count)))
	(print (cat "Contains 'Paris'? " (if (. bf :contains? "Paris") "Yes" "No")))
	(print (cat "Contains 'Berlin'? " (if (. bf :contains? "Berlin") "Yes" "No")))
	(print)

	(print "Clearing the filter...")
	(. bf :clear)
	(print (cat "Items in filter: " (. bf :item_count)))
	(print (cat "Is empty? " (if (. bf :empty?) "Yes" "No")))
	(print)

	(print "Adding 3 countries...")
	(each (lambda (country)
		(. bf :add country))
		'("France" "Japan" "Australia"))

	(print (cat "Items in filter: " (. bf :item_count)))
	(print (cat "Contains 'Paris'? " (if (. bf :contains? "Paris") "Yes" "No")))
	(print (cat "Contains 'France'? " (if (. bf :contains? "France") "Yes" "No")))
	(print))

(defun demo-copy ()
	; Demonstrate copying a Bloom filter
	(print-header "COPY OPERATION")
	(print)

	(defq bf1 (Bloom 512 3))

	(print "Original filter - Adding: alpha, beta, gamma")
	(each (lambda (item)
		(. bf1 :add item))
		'("alpha" "beta" "gamma"))

	(print (cat "Original contains 'alpha'? " (if (. bf1 :contains? "alpha") "Yes" "No")))
	(print)

	(print "Creating a copy...")
	(defq bf2 (. bf1 :copy))

	(print "Adding 'delta' to copy only...")
	(. bf2 :add "delta")
	(print)

	(print "Original filter:")
	(print (cat "  - Items: " (. bf1 :item_count)))
	(print (cat "  - Contains 'alpha'? " (if (. bf1 :contains? "alpha") "Yes" "No")))
	(print (cat "  - Contains 'delta'? " (if (. bf1 :contains? "delta") "Yes" "No")))
	(print)

	(print "Copied filter:")
	(print (cat "  - Items: " (. bf2 :item_count)))
	(print (cat "  - Contains 'alpha'? " (if (. bf2 :contains? "alpha") "Yes" "No")))
	(print (cat "  - Contains 'delta'? " (if (. bf2 :contains? "delta") "Yes" "No")))
	(print))

(defun demo-use-case-spell-checker ()
	; Demonstrate a practical use case: spell checking
	(print-header "USE CASE: SIMPLE SPELL CHECKER")
	(print)

	; Create a dictionary Bloom filter
	(defq dictionary (Bloom 4096 3))

	(print "Building dictionary with common English words...")
	(defq common_words '("the" "be" "to" "of" "and" "a" "in" "that" "have" "I"
						 "it" "for" "not" "on" "with" "he" "as" "you" "do" "at"
						 "this" "but" "his" "by" "from" "they" "we" "say" "her" "she"
						 "or" "an" "will" "my" "one" "all" "would" "there" "their" "what"
						 "hello" "world" "computer" "program" "algorithm" "data" "structure"
						 "bloom" "filter" "hash" "set" "collection"))

	(each (lambda (word)
		(. dictionary :add word))
		common_words)

	(print (cat "Dictionary size: " (. dictionary :item_count) " words"))
	(print)

	(print "Checking spelling of sample text:")
	(defq sample_text '("hello" "worlld" "this" "is" "a" "tst" "of" "the" "bloom" "filtter"))

	(each (lambda (word)
		(defq status (if (. dictionary :contains? word)
			"OK"
			"POSSIBLY MISSPELLED"))
		(print (cat "  '" word "' -> " status)))
		sample_text)

	(print)
	(print "Note: This is a simplified example. Real spell checkers")
	(print "      would need additional validation for false positives.")
	(print))

(defun main ()
	(print)
	(print "╔════════════════════════════════════════╗")
	(print "║  CHRYSALISP BLOOM FILTER DEMO          ║")
	(print "║  Space-Efficient Probabilistic Sets    ║")
	(print "╚════════════════════════════════════════╝")

	; Run all demonstrations
	(demo-basic-operations)
	(demo-false-positives)
	(demo-performance-comparison)
	(demo-optimal-parameters)
	(demo-clear-and-reuse)
	(demo-copy)
	(demo-use-case-spell-checker)

	(print-header "DEMO COMPLETE")
	(print)
	(print "Key Takeaways:")
	(print "  1. Bloom filters are space-efficient probabilistic data structures")
	(print "  2. They can have false positives but NEVER false negatives")
	(print "  3. Ideal for: membership testing, caching, spell checking, etc.")
	(print "  4. Trade-off: Space savings vs. false positive rate")
	(print))

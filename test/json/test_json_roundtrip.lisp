;; JSON Roundtrip Tests
;; Inspired by XStream's TreeMapAndTreeSetTest patterns
;; Tests JSON stringify → parse roundtrip preservation

(import "lib/test/unittest.inc")
(import "lib/json/parse.inc")
(import "lib/json/stringify.inc")

(defun roundtrip-test (desc data)
	; Helper: stringify then parse, verify equality
	(deftest desc
		(defq json (json-stringify data))
		(defq parsed (json-parse json))
		(assert-eq data parsed)))

(defsuite "JSON Roundtrip Tests"
	(lambda ()
		; Test 1: Simple string
		(deftest "Roundtrip String"
			(defq original "hello")
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq original parsed))

		; Test 2: Simple number
		(deftest "Roundtrip Number"
			(defq original 42)
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq original parsed))

		; Test 3: Boolean true
		(deftest "Roundtrip Boolean True"
			(defq original :t)
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq original parsed))

		; Test 4: Boolean false
		(deftest "Roundtrip Boolean False"
			(defq original :nil)
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq original parsed))

		; Test 5: Null value
		(deftest "Roundtrip Null"
			(defq original nil)
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq original parsed))

		; Test 6: Empty object
		(deftest "Roundtrip Empty Object"
			(defq original (env))
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq 0 (length (env-keys parsed))))

		; Test 7: Simple object with one property
		(deftest "Roundtrip Simple Object"
			(defq original (env))
			(set-insert original :name "test")
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq "test" (get parsed :name)))

		; Test 8: Object with multiple properties
		(deftest "Roundtrip Object Multiple Properties"
			(defq original (env))
			(set-insert original :name "Alice")
			(set-insert original :age 30)
			(set-insert original :active :t)
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq "Alice" (get parsed :name))
			(assert-eq 30 (get parsed :age))
			(assert-eq :t (get parsed :active)))

		; Test 9: Empty array
		(deftest "Roundtrip Empty Array"
			(defq original (list))
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq 0 (length parsed)))

		; Test 10: Simple array
		(deftest "Roundtrip Simple Array"
			(defq original (list 1 2 3))
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq 3 (length parsed))
			(assert-eq 1 (first parsed))
			(assert-eq 2 (elem 1 parsed))
			(assert-eq 3 (elem 2 parsed)))

		; Test 11: Array of strings
		(deftest "Roundtrip String Array"
			(defq original (list "a" "b" "c"))
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq "a" (first parsed))
			(assert-eq "b" (elem 1 parsed))
			(assert-eq "c" (elem 2 parsed)))

		; Test 12: Nested object
		(deftest "Roundtrip Nested Object"
			(defq original (env))
			(defq inner (env))
			(set-insert inner :city "NYC")
			(set-insert original :name "Bob")
			(set-insert original :address inner)
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq "Bob" (get parsed :name))
			(defq addr (get parsed :address))
			(assert-not-nil addr)
			(assert-eq "NYC" (get addr :city)))

		; Test 13: Array of objects
		(deftest "Roundtrip Array of Objects"
			(defq obj1 (env))
			(set-insert obj1 :id 1)
			(defq obj2 (env))
			(set-insert obj2 :id 2)
			(defq original (list obj1 obj2))
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq 2 (length parsed))
			(assert-eq 1 (get (first parsed) :id))
			(assert-eq 2 (get (elem 1 parsed) :id)))

		; Test 14: Object with array property
		(deftest "Roundtrip Object With Array"
			(defq original (env))
			(set-insert original :name "test")
			(set-insert original :items (list 1 2 3))
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq "test" (get parsed :name))
			(defq items (get parsed :items))
			(assert-eq 3 (length items))
			(assert-eq 1 (first items)))

		; Test 15: Deeply nested structure
		(deftest "Roundtrip Deep Nesting"
			(defq level3 (env))
			(set-insert level3 :value "deep")
			(defq level2 (env))
			(set-insert level2 :nested level3)
			(defq level1 (env))
			(set-insert level1 :child level2)
			(defq original (env))
			(set-insert original :root level1)

			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(defq root (get parsed :root))
			(defq child (get root :child))
			(defq nested (get child :nested))
			(assert-eq "deep" (get nested :value)))

		; Test 16: Special characters in strings
		(deftest "Roundtrip String With Quotes"
			(defq original (env))
			(set-insert original :text "Say \"hello\"")
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq "Say \"hello\"" (get parsed :text)))

		; Test 17: Numbers (int and float)
		(deftest "Roundtrip Number Types"
			(defq original (env))
			(set-insert original :int 42)
			(set-insert original :negative -17)
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq 42 (get parsed :int))
			(assert-eq -17 (get parsed :negative)))

		; Test 18: Mixed array types
		(deftest "Roundtrip Mixed Array"
			(defq original (list "text" 123 :t :nil))
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq 4 (length parsed))
			(assert-eq "text" (first parsed))
			(assert-eq 123 (elem 1 parsed))
			(assert-eq :t (elem 2 parsed))
			(assert-eq :nil (elem 3 parsed)))

		; Test 19: Object with null property
		(deftest "Roundtrip Object With Null"
			(defq original (env))
			(set-insert original :name "test")
			(set-insert original :value nil)
			(defq json (json-stringify original))
			(defq parsed (json-parse json))
			(assert-eq "test" (get parsed :name))
			(assert-eq nil (get parsed :value)))

		; Test 20: Complex real-world structure
		(deftest "Roundtrip Complex Structure"
			(defq user (env))
			(set-insert user :id 123)
			(set-insert user :name "Alice")
			(set-insert user :active :t)
			(set-insert user :tags (list "admin" "user"))
			(defq meta (env))
			(set-insert meta :created "2025-01-01")
			(set-insert user :metadata meta)

			(defq original (env))
			(set-insert original :user user)
			(set-insert original :count 1)

			(defq json (json-stringify original))
			(defq parsed (json-parse json))

			(assert-eq 1 (get parsed :count))
			(defq u (get parsed :user))
			(assert-eq 123 (get u :id))
			(assert-eq "Alice" (get u :name))
			(assert-eq :t (get u :active))
			(defq tags (get u :tags))
			(assert-eq 2 (length tags))
			(assert-eq "admin" (first tags))
			(defq m (get u :metadata))
			(assert-eq "2025-01-01" (get m :created)))
	))

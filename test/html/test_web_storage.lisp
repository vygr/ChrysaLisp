
;; Web Storage API Tests (localStorage and sessionStorage)
;; TDD approach - tests first!

(import "lib/test/unittest.inc")
(import "lib/html/web_storage.inc")

(deftest-suite "Web Storage API Tests")

; LocalStorage Tests

; Test 1: Create localStorage
(deftest "Create LocalStorage"
	(defq storage (create-local-storage))
	(assert-not-nil storage))

; Test 2: Set and get item
(deftest "LocalStorage Set And Get"
	(defq storage (create-local-storage))

	(. storage :set-item "key1" "value1")
	(assert-eq "value1" (. storage :get-item "key1")))

; Test 3: Get non-existent item
(deftest "LocalStorage Get Non Existent"
	(defq storage (create-local-storage))
	(assert-eq :nil (. storage :get-item "nonexistent")))

; Test 4: Remove item
(deftest "LocalStorage Remove Item"
	(defq storage (create-local-storage))

	(. storage :set-item "temp" "value")
	(assert-eq "value" (. storage :get-item "temp"))

	(. storage :remove-item "temp")
	(assert-eq :nil (. storage :get-item "temp")))

; Test 5: Clear all items
(deftest "LocalStorage Clear"
	(defq storage (create-local-storage))

	(. storage :set-item "a" "1")
	(. storage :set-item "b" "2")
	(. storage :set-item "c" "3")

	(. storage :clear)

	(assert-eq :nil (. storage :get-item "a"))
	(assert-eq :nil (. storage :get-item "b"))
	(assert-eq :nil (. storage :get-item "c")))

; Test 6: Get length
(deftest "LocalStorage Length"
	(defq storage (create-local-storage))

	(assert-eq 0 (. storage :length))

	(. storage :set-item "a" "1")
	(assert-eq 1 (. storage :length))

	(. storage :set-item "b" "2")
	(assert-eq 2 (. storage :length))

	(. storage :remove-item "a")
	(assert-eq 1 (. storage :length)))

; Test 7: Key access by index
(deftest "LocalStorage Key By Index"
	(defq storage (create-local-storage))

	(. storage :set-item "first" "1")
	(. storage :set-item "second" "2")

	(defq key0 (. storage :key 0))
	(assert-not-nil key0)

	(defq key1 (. storage :key 1))
	(assert-not-nil key1))

; Test 8: Update existing item
(deftest "LocalStorage Update Item"
	(defq storage (create-local-storage))

	(. storage :set-item "counter" "1")
	(assert-eq "1" (. storage :get-item "counter"))

	(. storage :set-item "counter" "2")
	(assert-eq "2" (. storage :get-item "counter")))

; Test 9: Persistence to file
(deftest "LocalStorage Persistence"
	(defq storage (create-local-storage "test_localstorage.dat"))

	(. storage :set-item "persistent" "data123")
	(. storage :save)

	; Create new storage from same file
	(defq storage2 (create-local-storage "test_localstorage.dat"))
	(. storage2 :load)

	(assert-eq "data123" (. storage2 :get-item "persistent")))

; SessionStorage Tests

; Test 10: Create sessionStorage
(deftest "Create SessionStorage"
	(defq storage (create-session-storage))
	(assert-not-nil storage))

; Test 11: SessionStorage set and get
(deftest "SessionStorage Set And Get"
	(defq storage (create-session-storage))

	(. storage :set-item "session_key" "session_value")
	(assert-eq "session_value" (. storage :get-item "session_key")))

; Test 12: SessionStorage is separate from localStorage
(deftest "Session And Local Are Separate"
	(defq local (create-local-storage))
	(defq session (create-session-storage))

	(. local :set-item "key" "local_value")
	(. session :set-item "key" "session_value")

	(assert-eq "local_value" (. local :get-item "key"))
	(assert-eq "session_value" (. session :get-item "key")))

; Test 13: Store JSON data
(deftest "LocalStorage Store JSON"
	(defq storage (create-local-storage))

	(defq data (env))
	(set-insert data :name "Alice")
	(set-insert data :age 30)

	; Would use JSON.stringify in real code
	(. storage :set-item "user" (print data))

	(defq retrieved (. storage :get-item "user"))
	(assert-not-nil retrieved))

; Test 14: Script access to localStorage
(deftest "Script LocalStorage Access"
	(defq html "
		<script>
			(. localStorage :set-item \"script_key\" \"script_value\")
			(defq value (. localStorage :get-item \"script_key\"))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq "script_value" (. ctx :get-global "value")))

; Test 15: Script access to sessionStorage
(deftest "Script SessionStorage Access"
	(defq html "
		<script>
			(. sessionStorage :set-item \"temp_key\" \"temp_value\")
			(defq value (. sessionStorage :get-item \"temp_key\"))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq "temp_value" (. ctx :get-global "value")))

; Test 16: Script remove from localStorage
(deftest "Script LocalStorage Remove"
	(defq html "
		<script>
			(. localStorage :set-item \"remove_me\" \"value\")
			(. localStorage :remove-item \"remove_me\")
			(defq value (. localStorage :get-item \"remove_me\"))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq :nil (. ctx :get-global "value")))

; Test 17: Script clear localStorage
(deftest "Script LocalStorage Clear"
	(defq html "
		<script>
			(. localStorage :set-item \"a\" \"1\")
			(. localStorage :set-item \"b\" \"2\")
			(. localStorage :clear)
			(defq count (. localStorage :length))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq 0 (. ctx :get-global "count")))

; Test 18: Store numbers as strings
(deftest "LocalStorage Numbers As Strings"
	(defq storage (create-local-storage))

	(. storage :set-item "number" "42")
	(assert-eq "42" (. storage :get-item "number")))

; Test 19: Store complex strings
(deftest "LocalStorage Complex Strings"
	(defq storage (create-local-storage))

	(defq complex "Line 1\nLine 2\tTabbed")
	(. storage :set-item "multiline" complex)

	(assert-eq complex (. storage :get-item "multiline")))

; Test 20: Multiple script operations
(deftest "Script Multiple Storage Operations"
	(defq html "
		<script>
			(. localStorage :set-item \"count\" \"0\")
			(. localStorage :set-item \"count\" \"1\")
			(. localStorage :set-item \"count\" \"2\")
			(. localStorage :set-item \"count\" \"3\")
			(defq final (. localStorage :get-item \"count\"))
			(defq len (. localStorage :length))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq "3" (. ctx :get-global "final"))
	(assert-eq 1 (. ctx :get-global "len")))  ; Only one key

; Report test results
(test-report)

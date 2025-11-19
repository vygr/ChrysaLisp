
;; Cookie Storage Tests
;; TDD approach - tests first!

(import "lib/test/unittest.inc")
(import "lib/html/cookies.inc")

(deftest-suite "Cookie Storage Tests")

; Test 1: Create cookie store
(deftest "Create Cookie Store"
	(defq store (create-cookie-store))
	(assert-not-nil store))

; Test 2: Set and get cookie
(deftest "Set And Get Cookie"
	(defq store (create-cookie-store))

	(. store :set-cookie "username" "alice")
	(defq value (. store :get-cookie "username"))

	(assert-eq "alice" value))

; Test 3: Multiple cookies
(deftest "Multiple Cookies"
	(defq store (create-cookie-store))

	(. store :set-cookie "user" "bob")
	(. store :set-cookie "session" "xyz123")
	(. store :set-cookie "theme" "dark")

	(assert-eq "bob" (. store :get-cookie "user"))
	(assert-eq "xyz123" (. store :get-cookie "session"))
	(assert-eq "dark" (. store :get-cookie "theme")))

; Test 4: Get non-existent cookie
(deftest "Get Non Existent Cookie"
	(defq store (create-cookie-store))

	(defq value (. store :get-cookie "nonexistent"))
	(assert-eq :nil value))

; Test 5: Update cookie value
(deftest "Update Cookie Value"
	(defq store (create-cookie-store))

	(. store :set-cookie "counter" "1")
	(assert-eq "1" (. store :get-cookie "counter"))

	(. store :set-cookie "counter" "2")
	(assert-eq "2" (. store :get-cookie "counter")))

; Test 6: Delete cookie
(deftest "Delete Cookie"
	(defq store (create-cookie-store))

	(. store :set-cookie "temp" "value")
	(assert-eq "value" (. store :get-cookie "temp"))

	(. store :delete-cookie "temp")
	(assert-eq :nil (. store :get-cookie "temp")))

; Test 7: Get all cookies
(deftest "Get All Cookies"
	(defq store (create-cookie-store))

	(. store :set-cookie "a" "1")
	(. store :set-cookie "b" "2")
	(. store :set-cookie "c" "3")

	(defq all (. store :get-all-cookies))
	(assert-eq 3 (length all)))

; Test 8: Clear all cookies
(deftest "Clear All Cookies"
	(defq store (create-cookie-store))

	(. store :set-cookie "a" "1")
	(. store :set-cookie "b" "2")

	(. store :clear-all)

	(assert-eq :nil (. store :get-cookie "a"))
	(assert-eq :nil (. store :get-cookie "b"))
	(assert-eq 0 (length (. store :get-all-cookies))))

; Test 9: Cookie with special characters
(deftest "Cookie With Special Characters"
	(defq store (create-cookie-store))

	(. store :set-cookie "message" "Hello, World!")
	(assert-eq "Hello, World!" (. store :get-cookie "message")))

; Test 10: Cookie persistence to file
(deftest "Cookie Persistence"
	(defq store (create-cookie-store "test_cookies.dat"))

	(. store :set-cookie "persistent" "value123")
	(. store :save)

	; Create new store from same file
	(defq store2 (create-cookie-store "test_cookies.dat"))
	(. store2 :load)

	(assert-eq "value123" (. store2 :get-cookie "persistent")))

; Test 11: Cookie with path
(deftest "Cookie With Path"
	(defq store (create-cookie-store))

	(. store :set-cookie "pathcookie" "value" :path "/admin")
	(defq cookie (. store :get-cookie-full "pathcookie"))

	(assert-eq "value" (get cookie :value))
	(assert-eq "/admin" (get cookie :path)))

; Test 12: Cookie with expiry (future)
(deftest "Cookie With Expiry"
	(defq store (create-cookie-store))

	; Set cookie that expires in future
	(defq future-time (+ (time) 3600000))  ; 1 hour from now
	(. store :set-cookie "future" "value" :expires future-time)

	; Should still be valid
	(assert-eq "value" (. store :get-cookie "future")))

; Test 13: Cookie with expiry (past)
(deftest "Expired Cookie"
	(defq store (create-cookie-store))

	; Set cookie that already expired
	(defq past-time (- (time) 3600000))  ; 1 hour ago
	(. store :set-cookie "expired" "value" :expires past-time)

	; Should return nil (expired)
	(assert-eq :nil (. store :get-cookie "expired")))

; Test 14: Cookie from script
(deftest "Cookie From Script"
	(defq html "
		<script>
			(. document :set-cookie \"script_cookie\" \"from_script\")
			(defq value (. document :get-cookie \"script_cookie\"))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq "from_script" (. ctx :get-global "value")))

; Test 15: Multiple cookie operations from script
(deftest "Script Cookie Operations"
	(defq html "
		<script>
			(. document :set-cookie \"count\" \"1\")
			(. document :set-cookie \"count\" \"2\")
			(. document :set-cookie \"count\" \"3\")
			(defq final (. document :get-cookie \"count\"))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	(assert-eq "3" (. ctx :get-global "final")))

; Report test results
(test-report)

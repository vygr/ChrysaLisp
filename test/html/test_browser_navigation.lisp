
;; Browser Navigation Tests
;; Tests for Back/Forward navigation and history tracking

(import "lib/test/unittest.inc")

(deftest-suite "Browser Navigation Tests")

;; Mock HTML-Canvas class for testing navigation
(defclass Test-Browser nil
	(defq
		history (list)
		history_position -1
		can_go_back :nil
		can_go_forward :nil
		current_file ""))

(defmethod :load_file Test-Browser (filepath &optional from_history)
	; Simulate loading a file
	(set this :current_file filepath)

	; Add to history if not navigating from history
	(unless from_history
		(. this :add_to_history filepath))
	this)

(defmethod :add_to_history Test-Browser (url)
	; Add URL to history
	(defq history (get :history this))
	(defq pos (get :history_position this))

	; If we're in the middle of history, truncate forward entries
	(when (< pos (dec (length history)))
		(set this :history (slice history 0 (inc pos))))

	; Add new URL
	(push (get :history this) url)
	(set this :history_position (dec (length (get :history this))))

	; Update navigation state
	(. this :update_history_state)
	this)

(defmethod :update_history_state Test-Browser ()
	; Update can_go_back and can_go_forward flags
	(defq pos (get :history_position this))
	(defq hist_len (length (get :history this)))

	(set this :can_go_back (> pos 0))
	(set this :can_go_forward (< pos (dec hist_len)))
	this)

(defmethod :go_back Test-Browser ()
	; Navigate back in history
	(when (get :can_go_back this)
		(defq pos (get :history_position this))
		(defq new_pos (dec pos))
		(set this :history_position new_pos)
		(defq url (elem new_pos (get :history this)))
		(. this :load_file url :t)
		(. this :update_history_state)
		:t))

(defmethod :go_forward Test-Browser ()
	; Navigate forward in history
	(when (get :can_go_forward this)
		(defq pos (get :history_position this))
		(defq new_pos (inc pos))
		(set this :history_position new_pos)
		(defq url (elem new_pos (get :history this)))
		(. this :load_file url :t)
		(. this :update_history_state)
		:t))

(defmethod :get_current_url Test-Browser ()
	; Get current URL
	(get :current_file this))

(defun main ()
	(run-test-suite
		; Test initial state
		(deftest "Browser Starts with Empty History"
			(defq browser (Test-Browser))
			(assert-eq 0 (length (get :history browser)))
			(assert-eq -1 (get :history_position browser))
			(assert-false (get :can_go_back browser))
			(assert-false (get :can_go_forward browser)))

		; Test loading first page
		(deftest "Loading First Page Adds to History"
			(defq browser (Test-Browser))
			(. browser :load_file "/test/page1.html")

			(assert-eq 1 (length (get :history browser)))
			(assert-eq 0 (get :history_position browser))
			(assert-false (get :can_go_back browser))
			(assert-false (get :can_go_forward browser)))

		; Test loading second page
		(deftest "Loading Second Page Adds to History"
			(defq browser (Test-Browser))
			(. browser :load_file "/test/page1.html")
			(. browser :load_file "/test/page2.html")

			(assert-eq 2 (length (get :history browser)))
			(assert-eq 1 (get :history_position browser))
			(assert-true (get :can_go_back browser))
			(assert-false (get :can_go_forward browser)))

		; Test going back
		(deftest "Go Back Changes Position"
			(defq browser (Test-Browser))
			(. browser :load_file "/test/page1.html")
			(. browser :load_file "/test/page2.html")

			(. browser :go_back)

			(assert-eq 0 (get :history_position browser))
			(assert-eq "/test/page1.html" (. browser :get_current_url))
			(assert-false (get :can_go_back browser))
			(assert-true (get :can_go_forward browser)))

		; Test going forward
		(deftest "Go Forward Changes Position"
			(defq browser (Test-Browser))
			(. browser :load_file "/test/page1.html")
			(. browser :load_file "/test/page2.html")
			(. browser :go_back)

			(. browser :go_forward)

			(assert-eq 1 (get :history_position browser))
			(assert-eq "/test/page2.html" (. browser :get_current_url))
			(assert-true (get :can_go_back browser))
			(assert-false (get :can_go_forward browser)))

		; Test multiple pages
		(deftest "Navigate Through Multiple Pages"
			(defq browser (Test-Browser))
			(. browser :load_file "/test/page1.html")
			(. browser :load_file "/test/page2.html")
			(. browser :load_file "/test/page3.html")

			(assert-eq 3 (length (get :history browser)))
			(assert-eq 2 (get :history_position browser))

			; Go back twice
			(. browser :go_back)
			(assert-eq "/test/page2.html" (. browser :get_current_url))

			(. browser :go_back)
			(assert-eq "/test/page1.html" (. browser :get_current_url))
			(assert-false (get :can_go_back browser))

			; Go forward twice
			(. browser :go_forward)
			(assert-eq "/test/page2.html" (. browser :get_current_url))

			(. browser :go_forward)
			(assert-eq "/test/page3.html" (. browser :get_current_url))
			(assert-false (get :can_go_forward browser)))

		; Test history truncation
		(deftest "Navigating from Middle Truncates Forward History"
			(defq browser (Test-Browser))
			(. browser :load_file "/test/page1.html")
			(. browser :load_file "/test/page2.html")
			(. browser :load_file "/test/page3.html")

			; Go back to page 2
			(. browser :go_back)
			(assert-eq "/test/page2.html" (. browser :get_current_url))

			; Load new page - should truncate page3
			(. browser :load_file "/test/page4.html")

			(assert-eq 3 (length (get :history browser)))
			(assert-eq 2 (get :history_position browser))
			(assert-eq "/test/page4.html" (. browser :get_current_url))
			(assert-false (get :can_go_forward browser)))

		; Test cannot go back when at start
		(deftest "Cannot Go Back at Start of History"
			(defq browser (Test-Browser))
			(. browser :load_file "/test/page1.html")

			(defq result (. browser :go_back))
			(assert-nil result)
			(assert-eq "/test/page1.html" (. browser :get_current_url)))

		; Test cannot go forward when at end
		(deftest "Cannot Go Forward at End of History"
			(defq browser (Test-Browser))
			(. browser :load_file "/test/page1.html")
			(. browser :load_file "/test/page2.html")

			(defq result (. browser :go_forward))
			(assert-nil result)
			(assert-eq "/test/page2.html" (. browser :get_current_url)))

		; Test history contains correct URLs
		(deftest "History Contains Correct URLs"
			(defq browser (Test-Browser))
			(. browser :load_file "/test/page1.html")
			(. browser :load_file "/test/page2.html")
			(. browser :load_file "/test/page3.html")

			(defq hist (get :history browser))
			(assert-eq 3 (length hist))
			(assert-eq "/test/page1.html" (elem 0 hist))
			(assert-eq "/test/page2.html" (elem 1 hist))
			(assert-eq "/test/page3.html" (elem 2 hist)))

		; Test current URL tracking
		(deftest "Current URL Tracks Active Page"
			(defq browser (Test-Browser))
			(. browser :load_file "/test/page1.html")
			(assert-eq "/test/page1.html" (. browser :get_current_url))

			(. browser :load_file "/test/page2.html")
			(assert-eq "/test/page2.html" (. browser :get_current_url))

			(. browser :go_back)
			(assert-eq "/test/page1.html" (. browser :get_current_url)))

		; Test complex navigation sequence
		(deftest "Complex Navigation Sequence"
			(defq browser (Test-Browser))
			; Load pages: 1 -> 2 -> 3 -> 4
			(. browser :load_file "/test/page1.html")
			(. browser :load_file "/test/page2.html")
			(. browser :load_file "/test/page3.html")
			(. browser :load_file "/test/page4.html")

			; Go back: 4 -> 3 -> 2
			(. browser :go_back)
			(. browser :go_back)
			(assert-eq "/test/page2.html" (. browser :get_current_url))

			; Load new page: should truncate 3 and 4, add 5
			(. browser :load_file "/test/page5.html")
			(assert-eq 3 (length (get :history browser)))

			; Go back: 5 -> 2
			(. browser :go_back)
			(assert-eq "/test/page2.html" (. browser :get_current_url))

			; Go forward: 2 -> 5
			(. browser :go_forward)
			(assert-eq "/test/page5.html" (. browser :get_current_url)))

		; Test file:// URL support
		(deftest "Support file:// URLs"
			(defq browser (Test-Browser))
			(. browser :load_file "file:///test/page1.html")

			; Note: Current implementation strips file:// in navigate_to_url
			; This test verifies history works with file paths
			(assert-greater (length (get :history browser)) 0))
	))


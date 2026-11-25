(report-header "Raw kinds: bytes, shorts and ints, storage of raw bits at their width")

;an element is its bits: read as a num, never sign extended; written, its
;low bits. They are arrays and not nums, no maths
(test-cases
	(str (bytes 1 255 256 -1)) "(1 255 0 255)"
	(str (shorts 1 -1 65536 70000)) "(1 65535 0 4464)"
	(str (ints 1 -1 4294967296)) "(1 4294967295 0)"
	(type-of (shorts 1)) '(:seq :array :shorts)
	(if (array? (bytes)) :t) :t
	(if (nums? (ints)) :t) :nil
	(if (shorts? (shorts)) :t) :t
	(if (ints? (shorts)) :t) :nil)

;each of the storage operations at each width
(each (lambda (make name)
		(defq a (make 10 20 30 40))
		(assert-eq (cat name " elem-get") 30 (elem-get a 2))
		(assert-eq (cat name " elem-get from the end") 40 (elem-get a -2))
		(assert-eq (cat name " length") 4 (length a))
		(assert-eq (cat name " slice") "(20 30)" (str (slice a 1 3)))
		(assert-eq (cat name " slice reversed") "(40 30 20 10)" (str (reverse a)))
		(assert-eq (cat name " cat") "(10 20 30 40 10 20 30 40)" (str (cat a a)))
		(assert-eq (cat name " find") 2 (find 30 a))
		(assert-eq (cat name " rfind, past the match as a nums has it") 3 (rfind 30 a))
		(assert-eq (cat name " find not there") :nil (find 99 a))
		(assert-eq (cat name " eql") :t (eql (make 1 2) (make 1 2)))
		(assert-eq (cat name " not eql another kind") :nil (eql (make 1 2) (nums 1 2)))
		(assert-eq (cat name " push") "(10 20 30 40 50 60)" (str (push (cat a) 50 60)))
		(assert-eq (cat name " pop") "(40 (10 20 30))" (str (list (pop (defq b (cat a))) b)))
		(assert-eq (cat name " elem-set") "(10 99 30 40)" (str (elem-set (cat a) 1 99)))
		(assert-eq (cat name " fill") "(7 7 7)" (str (fill (make) 3 7)))
		(assert-eq (cat name " splice") "(10 20 30)" (str (splice a a (nums 0 2 2 3))))
		(assert-eq (cat name " erase a range") "(10 30 40)" (str (erase (cat a) 1 2)))
		(assert-eq (cat name " insert") "(10 5 20 30 40)" (str (insert (cat a) 1 (make 5))))
		(assert-eq (cat name " copy") "(10 20 30 40)" (str (copy a)))
		(assert-eq (cat name " clear") "()" (str (clear (cat a))))
		;past the inline slots, a hundred of them
		(defq many (apply make (map! (const !) '() 0 100)))
		(assert-eq (cat name " many") "(100 99 0)" (str (list (length many) (elem-get many -2) (first many))))
		(assert-eq (cat name " many slice") "(50 51 52)" (str (slice many 50 53))))
	(list bytes shorts ints) '("bytes" "shorts" "ints"))

;a push takes another array's bytes at its own width, raw
(test-cases
	(str (push (ints) (bytes 1 2 3 4))) "(67305985)"
	(str (push (bytes) (ints 0x04030201))) "(1 2 3 4)"
	(str (push (shorts) (bytes 1 2 3 4))) "(513 1027)"
	(str (push (shorts) "AB")) "(16961)"
	(str (push (shorts) (shorts 1 2))) "(1 2)")

;floats share the ints' storage, and are what they were
(test-cases
	(length (floats (n2f 1.5) (n2f 2.5))) 2
	(str (slice (floats (n2f 1.0) (n2f 2.0) (n2f 3.0)) 1 3)) (str (floats (n2f 2.0) (n2f 3.0)))
	(elem-get (floats (n2f 1.0) (n2f 2.0)) 1) (n2f 2.0))

;for maths, nums-convert: of a raw kind into numbers, read signed if asked;
;of numbers into a raw kind, each an integer, its low bits
(test-cases
	(str (nums-convert (shorts 1 -1 32767 32768) (nums))) "(1 65535 32767 32768)"
	(str (nums-convert (shorts 1 -1 32767 32768) (nums) :t)) "(1 -1 32767 -32768)"
	(str (nums-convert (shorts 65535) (nums) :nil)) "(65535)"
	(str (nums-convert (bytes 1 255 128 127) (nums) :t)) "(1 -1 -128 127)"
	(str (nums-convert (ints 1 -1) (nums) :t)) "(1 -1)"
	(str (nums-convert (shorts 3 -2) (fixeds) :t)) "(3.00000 -2.00000)"
	(str (nums-convert (shorts 3 -2) (floats) :t)) (str (floats (n2f 3.0) (n2f -2.0)))
	(str (nums-convert (shorts 3 -2) (doubles) :t)) (str (doubles (n2d 3) (n2d -2)))
	(str (nums-convert (nums 1 -1 70000) (shorts))) "(1 65535 4464)"
	(str (nums-convert (fixeds 1.5 300.0) (bytes))) "(1 44)"
	(str (nums-convert (floats (n2f 1.5) (n2f -2.0)) (ints))) "(1 4294967294)"
	(str (nums-convert (doubles (n2d 3) (n2d -4)) (shorts))) "(3 65532)")
(assert-error "raw to raw is refused" (nums-convert (shorts 1) (bytes)) {(nums-convert src dst [signed]) wrong_types})
(assert-error "and a list" (nums-convert (list 1 2) (nums)) {(nums-convert src dst [signed]) wrong_types})

;what a push takes: a number, or what has raw data, a str, an array of
;numbers or a raw array, its bytes in whole elements of what is pushed
;to; into an array of numbers too, and of any width into any
(test-cases
	(str (push (nums) (ints 1 2 3 4))) "(8589934593 17179869187)"
	(str (push (nums) (bytes 1 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0))) "(1 2)"
	(str (push (nums 9) (nums 1 2 3))) "(9 1 2 3)"
	(length (push (nums) (pad "" 17 "\x00"))) 2
	(length (push (doubles) (nums-convert (nums 1 2 3 4) (floats)))) 2
	(str (push (nums) (fixeds 1.0 2.0))) "(65536 131072)"
	(str (push (ints) (nums 1 2))) "(1 0 2 0)"
	(str (push (ints) (shorts 1 2 3 4))) "(131073 262147)"
	(str (push (ints 9) (ints 1 2))) "(9 1 2)"
	(length (push (bytes) (ints 1 2 3 4))) 16
	(str (push (bytes) (shorts 1) 2 "A")) "(1 0 2 65)"
	(str (push (nums-convert (nums 1) (floats)) (nums-convert (nums 2 3) (floats)))) (str (nums-convert (nums 1 2 3) (floats)))
	(str (push (path 1.0 2.0) (path 3.0 4.0) 5.0)) (str (path 1.0 2.0 3.0 4.0 5.0))
	(str (push (list 1) 2 (list 3) (nums 4))) "(1 2 (3) (4))")

;a list has no raw data, its elements are objects: no array of numbers and
;no raw array takes one, nor what is no number and no sequence
(assert-error "a list into an array of numbers is refused" (push (nums) (list 1 2 3)) {(push array elem ...) wrong_types})
(assert-error "and into a raw array" (push (ints) (list 1)) {(push array elem ...) wrong_types})
(assert-error "and after what it does take" (push (bytes) 1 "A" (list 1)) {(push array elem ...) wrong_types})
(assert-error "and what is neither" (push (nums) (string-stream "")) {(push array elem ...) wrong_types})
(assert-error "and a push to what is no array" (push "abc" 1) {(push array elem ...) wrong_types})
(assert-error "and a push of nothing" (push (nums)) {(push array elem ...) wrong_types})

(report-header "String Edges: empty strings, conversions, trimming, splitting")

; --- str and sym ---
(test-cases
	(str) ""
	(str 5) "5"				(str -5) "-5"
	(str :nil) ":nil"		(str :t) ":t"
	(str "") ""
	(str (list)) "()"
	(str "a" 1 :b (list 1 "x")) {a1:b(1 "x")}
	(str (nums 1 2)) "(1 2)"
	(str 'sym) "sym"
	(str 1.5) "1.50000"		(str -0.25) "-0.25000"
	(eql (sym "abc") 'abc) :t
	(length :nil) 4)

; --- char and code ---
(test-cases
	(char 65) "A"
	(char 0x4142 2) "BA"
	(code "A") 65
	(code "AB") 65
	(code "AB" 2) 16961
	(code "ABC" 1 1) 66
	(ascii-code "A") 65
	(ascii-char 97) "a"
	(ascii-upper 97) 65		(ascii-lower 65) 97
	;not a letter, left alone
	(ascii-upper 33) 33)

; --- cmp, 0 when equal, else the sign gives the order ---
(test-cases
	(cmp "a" "a") 0
	(sign (cmp "a" "b")) -1		(sign (cmp "b" "a")) 1
	(sign (cmp "" "a")) -1		(sign (cmp "a" "")) 1
	(sign (cmp "ab" "a")) 1		(sign (cmp "A" "a")) -1)

; --- split, runs of separators give no empty strings ---
(test-cases
	(split "a,b,c" ",") '("a" "b" "c")
	(split "" ",") '()
	(split ",,," ",") '()
	(split "abc" ",") '("abc")
	(split ",a,,b," ",") '("a" "b")
	(split "abc" "") '("abc")
	;more than one separator must be a sorted char class
	(split "a b,c" (char-class " ,")) '("a" "b" "c"))

; --- trim ---
(test-cases
	(trim "  a b  ") "a b"
	(trim "") ""
	(trim "   ") ""
	(trim "a") "a"
	(trim "xxaxx" "x") "a"
	(trim "xxxx" "x") ""
	(trim-start "  a  ") "a  "
	(trim-end "  a  ") "  a"
	(trim-start "   ") ""
	(trim-end "   ") "")

; --- starts-with and ends-with, (starts-with prefix str) ---
(test-cases
	(starts-with "" "abc") :t
	(starts-with "abc" "abc") :t
	(starts-with "abcd" "abc") :nil
	(starts-with "a" "") :nil
	(ends-with "" "abc") :t
	(ends-with "bc" "abc") :t
	(ends-with "abcd" "abc") :nil)

; --- searching, (found? text substr) ---
(test-cases
	(found? "abc" "b") :t
	(found? "" "a") :nil
	(substr "b" "abc") '()
	(substr "abc" "abc") '(((0 3)))
	(substr "abcd" "abc") '(((0 3)))
	(replace-str "aXbXc" "X" "--") "a--b--c"
	(replace-str "abc" "z" "--") "abc"
	(replace-str "aaa" "a" "") "")

; --- padding and case ---
(test-cases
	(pad "a" 3) "  a"
	(pad "abc" 2) "abc"
	(pad "" 3) "   "
	(pad "a" 3 "-") "--a"
	(pad 5 3 "0") "005"
	(rpad "a" 3) "a  "
	(rpad "abcd" 2) "abcd"
	(to-upper "aBc1") "ABC1"
	(to-lower "aBc1") "abc1"
	(to-upper "") "")

; --- char class scanning ---
(test-cases
	(char-class "a-c") "abc"
	(bskip "ab" "abba c" 0) 4
	(bskip "ab" "" 0) 0
	(bskipn "ab" "ccab" 0) 2
	(bfind "b" "abc") 1
	(bfind "z" "abc") :nil
	(bfind "a" "") :nil)

(report-header "Regexp & Search Edges: empty text and patterns, escapes, captures")

; --- match? with nothing to match, or nothing to match with ---
(test-cases
	(match? "" "") :t
	(match? "abc" "") :t
	(match? "" "a") :nil
	(match? "" "a*") :t
	(match? "" "a?") :t
	(match? "" ".") :nil
	(match? "a" ".") :t
	(match? "abc" "^abc$") :t
	(match? "abc" "^$") :nil)

; --- case, groups, alternation, any char ---
(test-cases
	;matching is case sensitive
	(match? "AbC" "abc") :nil
	(match? "abcabc" "(abc)+") :t
	(match? "ac" "a(b)?c") :t
	(match? "x" "a|x|b") :t
	(match? "ab" "[ab][ab]") :t
	(match? "aXc" "a.c") :t
	;. matches a line end too
	(match? "a\nc" "a.c") :t)

; --- escaped special characters match themselves only ---
(test-cases
	(match? "a.b" "a\\.b") :t
	(match? "axb" "a\\.b") :nil
	(match? "a+b" "a\\+b") :t
	(match? "a-b" "a\\-b") :t
	(match? "(a)" "\\(a\\)") :t
	(match? "a b" "a\\sb") :t
	(match? "ab" "a\\sb") :nil
	(match? "tab\there" "\\t") :t
	(escape-regexp "a.b*c") "a\\.b\\*c"
	(escape-regexp "") ""
	(match? "a.b*c" (escape-regexp "a.b*c")) :t
	(match? "axbbc" (escape-regexp "a.b*c")) :nil)

; --- matches, each is ((start end) capture ...) ---
(test-cases
	(matches "" "a") '()
	(matches "xyz" "a") '()
	(matches "aaa" "a") '(((0 1)) ((1 2)) ((2 3)))
	;greedy, one match not three
	(matches "aaa" "a+") '(((0 3)))
	(matches "aaa" "a*") '(((0 3)))
	(matches "abab" "ab") '(((0 2)) ((2 4)))
	(matches "abab" "(a)(b)") '(((0 2) (0 1) (1 2)) ((2 4) (2 3) (3 4)))
	(matches "aXbXc" "X") '(((1 2)) ((3 4)))
	(matches "aa" "^a") '(((0 1)))
	(matches "aa" "a$") '(((1 2)))
	;^ is the start of the text, not of each line
	(matches "ab\nab" "^ab") '(((0 2))))

; --- replace-regex ---
(test-cases
	(replace-regex "aaa" "a" "b") "bbb"
	(replace-regex "aaa" "a+" "b") "b"
	(replace-regex "abc" "x" "y") "abc"
	(replace-regex "abc" "b" "") "ac"
	(replace-regex "abc" "(b)" "[$1]") "a[b]c"
	(replace-regex "abc" "(b)" "$1$1") "abbc"
	(replace-regex "abc" "b" "$0$0") "abbc"
	(replace-regex "a.b" "\\." "-") "a-b"
	(replace-regex "abc" "^" ">") ">abc"
	;a $ not followed by a group number is itself
	(replace-regex "price 10" "(\\d+)" "$$1") "price $10"
	;a group that took no part in the match is empty
	(replace-regex "abc" "(x)?b" "[$1]") "a[]c"
	(replace-regex "abc" "b(x)?" "[$1]") "a[]c"
	(replace-regex "abc" "(x)?(b)" "$1-$2") "a-bc")

; --- substr and replace-str take the pattern as plain text ---
(test-cases
	(substr "aaa" "a") '(((0 1)) ((1 2)) ((2 3)))
	;matches do not overlap
	(substr "aaa" "aa") '(((0 2)))
	(substr "abc" "abc") '(((0 3)))
	(substr "abc" "abcd") '()
	(substr "a.b" ".") '(((1 2)))
	(substr "abab" "ab") '(((0 2)) ((2 4)))
	(replace-str "aaa" "aa" "b") "ba"
	(replace-str "a.b" "." "-") "a-b"
	(replace-str "abc" "abc" "") ""
	(found? "abc" "abc") :t
	(found? "abc" "abcd") :nil
	(found? "abc" "c") :t
	(found? "a.c" ".") :t)

; --- char-class gives a sorted set of the characters ---
(test-cases
	(char-class "a") "a"
	(char-class "a-a") "a"
	(char-class "a-cx") "abcx"
	(char-class "0-9a-f") "0123456789abcdef"
	(char-class "ba") "ab"
	(char-class "aab") "ab")

; --- scanning with a char class, forwards and backwards ---
(defq re_cls (char-class "a-c"))
(test-cases
	(bfind "a" re_cls) 0
	(bfind "d" re_cls) :nil
	(bskip re_cls "abcd" 0) 3
	(bskip re_cls "abcd" 4) 4
	(bskip re_cls "dabc" 0) 0
	(bskipn re_cls "xyzab" 0) 3
	(bskipn re_cls "xyz" 0) 3
	;the reverse scans give the index after the last character they stop at
	(rbskip re_cls "xabc" -1) 1
	(rbskip re_cls "abc" -1) 0
	(rbskip re_cls "abcx" -1) 4
	(rbskipn re_cls "abxy" -1) 2
	(rbskipn re_cls "xy" -1) 0)

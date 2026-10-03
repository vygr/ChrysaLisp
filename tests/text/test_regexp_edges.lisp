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

; --- a pattern that needs no character can match at the very end of the text ---
(test-cases
	(match? "abc" "$") :t
	(match? "abc" "\\s*$") :t
	(match? "abc" "x$") :nil
	(match? "abc" "^$") :nil
	(matches "abc" "$") '(((3 3)))
	(matches "abc" "^") '(((0 0)))
	(matches "abc" "^$") '()
	(matches "" "$") '(((0 0)))
	(matches "abc" "\\s*$") '(((3 3)))
	(matches "abc  " "\\s*$") '(((3 5)))
	;an empty match is found at every position, the end included
	(matches "abc" "") '(((0 0)) ((1 1)) ((2 2)) ((3 3)))
	(matches "abc" "x*") '(((0 0)) ((1 1)) ((2 2)) ((3 3)))
	(matches "abc" "b*") '(((0 0)) ((1 2)) ((2 2)) ((3 3)))
	;but not again after a match that took the text right up to the end
	(matches "aaa" "a*") '(((0 3)))
	;$ is also the end of a line
	(matches "a\nb" "$") '(((1 1)) ((3 3)))
	(replace-regex "abc" "$" "<") "abc<"
	(replace-regex "" "$" "<") "<"
	(replace-regex "abc" "\\s*$" "!") "abc!"
	(replace-regex "abc  " "\\s*$" "") "abc"
	(replace-regex "abc" "x*" "-") "-a-b-c-"
	(replace-regex "aaa" "a*" "-") "-")

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

; --- query, a search built from the find options ---
(defun re-find (text pattern whole_words regexp)
	; the spans a query finds
	(bind '(engine meta &ignore) (query pattern whole_words regexp :nil))
	(map (const first) (. engine :search text meta)))

(defun re-swap (text pattern rep whole_words regexp)
	(bind '(engine meta &ignore) (query pattern whole_words regexp :nil))
	(replace-matches text (. engine :search text meta) rep))

;whole words, a plain pattern
(test-cases
	(re-find "the fox jumps" "fox" :t :nil) '((4 7))
	(re-find "the foxes jump" "fox" :t :nil) '()
	(re-find "firefox jumps" "fox" :t :nil) '()
	(re-find "fox" "fox" :t :nil) '((0 3))
	(re-find "fox,fox;fox" "fox" :t :nil) '((0 3) (4 7) (8 11))
	(re-find "ab abc ab" "ab" :t :nil) '((0 2) (7 9))
	;a digit or underscore is part of a word
	(re-find "a fox_b" "fox" :t :nil) '()
	(re-find "fox1 fox" "fox" :t :nil) '((5 8))
	;regexp characters in a plain pattern are just text
	(re-find "x a.b y" "a.b" :t :nil) '((2 5))
	(re-find "axb" "a.b" :t :nil) '()
	(re-find "c++ rocks" "c++" :t :nil) '((0 3))
	(re-find "x (y) z" "(y)" :t :nil) '((2 5))
	;a pattern can be several words
	(re-find "foo bar" "foo bar" :t :nil) '((0 7))
	(re-find "foo bar" "o b" :t :nil) '()
	(re-swap "cat concat cat" "cat" "X" :t :nil) "X concat X"
	(re-swap "cat concat cat" "cat" "X" :nil :nil) "X conX X")

;whole words, a regexp pattern, the word breaks apply to every alternative
(test-cases
	(re-find "the cat and dog" "cat|dog" :t :t) '((4 7) (12 15))
	(re-find "the cats and dogs" "cat|dog" :t :t) '()
	(re-find "concat dogma" "cat|dog" :t :t) '()
	(re-find "a dog" "cat|dog" :t :t) '((2 5))
	(re-find "the fox" "f.x" :t :t) '((4 7))
	(re-find "the foxy" "f.x" :t :t) '()
	;capture groups keep their numbers
	(re-swap "a cat or dog" "(c)at|(d)og" "[$0:$1$2]" :t :t) "a [cat:c] or [dog:d]"
	(re-swap "a cat or dogs" "(c)at|(d)og" "[$0:$1$2]" :t :t) "a [cat:c] or dogs"
	(re-swap "a cat or dogs" "(c)at|(d)og" "[$0:$1$2]" :nil :t) "a [cat:c] or [dog:d]s")

;an empty pattern stays empty, whole words or not, and ignore case lowers the pattern
(test-cases
	(third (query "" :t :nil :nil)) ""
	(third (query "" :t :t :nil)) ""
	(third (query "" :nil :nil :nil)) ""
	(third (query "FOX" :nil :nil :t)) "fox"
	(re-find "The FOX" "fox" :t :nil) '())

(bind '(re_engine re_meta &ignore) (query "FOX" :t :nil :t))
(assert-list-eq "ignore case, on lowered text" '(((4 7))) (. re_engine :search (to-lower "The FOX") re_meta))

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

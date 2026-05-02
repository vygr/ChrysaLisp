(report-header "Regexp: Lazy Quantifiers")

(defmacro assert-match-range (name pattern text start end)
	`(progn
		(defq res (matches ,text ,pattern))
		(defq first_match (first res))
		(if first_match
			(progn
				(defq range (first first_match))
				(assert-eq (cat "Regexp Match Range (start): " ,name) ,start (first range))
				(assert-eq (cat "Regexp Match Range (end): " ,name) ,end (second range)))
			(assert-true (cat "Regexp Match Failed: " ,name) :nil))))

; --- Lazy zero or one ?? ---
(assert-match-range "lazy ?? empty" "a??b" "ab" 0 2)
(assert-match-range "lazy ?? match" "a??b" "b" 0 1)

; --- Lazy zero or more *? ---
; Greedy: "a.*b" on "abab" matches "abab" (0 to 4)
; Lazy:   "a.*?b" on "abab" matches "ab" (0 to 2)
(assert-match-range "lazy *? short" "a.*?b" "abab" 0 2)
(assert-match-range "greedy *" "a.*b" "abab" 0 4)

; --- Lazy one or more +? ---
; Greedy: "a.+b" on "ababab" matches "ababab" (0 to 6)
; Lazy:   "a.+?b" on "ababab" matches "abab" (0 to 4)
(assert-match-range "lazy +? short" "a.+?b" "ababab" 0 4)
(assert-match-range "greedy +" "a.+b" "ababab" 0 6)

; --- Complex Lazy ---
(assert-match-range "lazy html" "<.*?>" "<div>content</div>" 0 5)
(assert-match-range "greedy html" "<.*>" "<div>content</div>" 0 18)

; --- Mixed Lazy and Greedy ---
(assert-match-range "multiple lazy +" "a.+?b.+?c" "axbyczc" 0 5)

; "a.*?b.*c" on "axbyczc"
; a.*?b matches "axb" (lazy)
; .*c matches "yczc" (greedy)
(assert-match-range "mixed lazy-greedy" "a.*?b.*c" "axbyczc" 0 7)

; "a.*b.*?c" on "axbyczc"
; a.*b matches "axby" (greedy)
; .*?c matches "c" (lazy) -> ends at index 5
(assert-match-range "mixed greedy-lazy" "a.*b.*?c" "axbyczc" 0 5)

; "a.*?b.*?c" on "axbyczc"
; a.*?b matches "axb"
; .*?c matches "yc"
(assert-match-range "mixed lazy-lazy" "a.*?b.*?c" "axbyczc" 0 5)

(report-header "Text Search: found, match, matches, replace")

; --- Pattern Searching ---
(defq txt "The quick brown fox jumps over the lazy dog")

; found? uses Substr search
(assert-true "found? true"  (found? txt "quick"))
(assert-true "found? false" (not (found? txt "slow")))

; match? uses Regexp search
(assert-true "match? true"  (match? txt "f.x"))
(assert-true "match? anchor" (match? txt "^The"))
(assert-true "match? false" (not (match? txt "^quick")))

; substr (returns list of matches, each match is ((start end)))
(defq sub_m (substr txt "brown"))
(assert-eq "substr count" 1 (length sub_m))
(assert-list-eq "substr range" '((10 15)) (first sub_m))

; matches (regexp)
(defq reg_m (matches txt "the"))
(assert-eq "matches count" 1 (length reg_m))
(assert-list-eq "matches range" '((31 34)) (first reg_m))

; --- Replacement ---
(defq rep_res (replace-str "A B A" "A" "X"))
(assert-eq "replace-str" "X B X" rep_res)

(defq reg_rep (replace-regex "123 456" "\\d+" "NUM"))
(assert-eq "replace-regex" "NUM NUM" reg_rep)

; Capture groups $0, $1 ...
(defq cap_rep (replace-regex "John Doe" "(\\w+) (\\w+)" "$2, $1"))
(assert-eq "replace-regex capture" "Doe, John" cap_rep)

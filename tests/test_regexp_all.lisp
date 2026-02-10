;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tests/test_regexp_all.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(report-header "Comprehensive Regexp Test")

(defmacro assert-match (name pattern text)
    `(assert-true (cat "Regexp Match: " ,name) (match? ,text ,pattern)))

(defmacro assert-no-match (name pattern text)
    `(assert-true (cat "Regexp No Match: " ,name) (not (match? ,text ,pattern))))

; --- Anchors ---
(report-header "Regexp: Anchors")
(assert-match "start ^" "^abc" "abcde")
(assert-no-match "start ^ fail" "^abc" "xabc")
(assert-match "end $" "abc$" "xyzabc")
(assert-no-match "end $ fail" "abc$" "abcxyz")
(assert-match "word !" "!fox!" "the fox jumps")
(assert-match "word ! start" "!the" "the fox")
(assert-match "word ! end" "fox!" "the fox")
(assert-no-match "word ! middle" "!ox!" "the fox")

; --- Wildcards & Quantifiers ---
(report-header "Regexp: Wildcards & Quantifiers")
(assert-match "any ." "a.c" "abc")
(assert-match "zero or one ?" "ab?c" "ac")
(assert-match "zero or one ? 2" "ab?c" "abc")
(assert-match "one or more +" "ab+c" "abc")
(assert-match "one or more + 2" "ab+c" "abbbc")
(assert-no-match "one or more + fail" "ab+c" "ac")
(assert-match "zero or more *" "ab*c" "ac")
(assert-match "zero or more * 2" "ab*c" "abbbc")

; --- OR & Groups ---
(report-header "Regexp: OR & Groups")
(assert-match "OR |" "cat|dog" "I have a dog")
(assert-match "OR | 2" "cat|dog" "I have a cat")
(assert-match "grouped OR" "a(b|c)d" "abd")
(assert-match "grouped OR 2" "a(b|c)d" "acd")

; --- Character Classes ---
(report-header "Regexp: Character Classes")
; Ranges MUST be 3-char aligned and first if possible.
(assert-match "range [0-9]" "[0-9]+" "abc123def")
(assert-match "explicit [abc]" "[abc]+" "bbbaaa")
(assert-no-match "negated [^abc]" "[^abc]+" "abc")
(assert-match "negated [^abc] 2" "[^abc]+" "def")
; Mixed range and literal: "0-9" is 3 chars, then "_" is aligned to next chunk.
(assert-match "range mixed [0-9_]" "[0-9_]+" "123_456")

; --- Escaped Classes ---
(report-header "Regexp: Escaped Classes")
(assert-match "\\d digit" "\\d+" "123")
(assert-match "\\D non-digit" "\\D+" "abc")
(assert-match "\\s space" "a\\sb" "a b")
(assert-match "\\S non-space" "\\S+" "abc")
(assert-match "\\w word" "\\w+" "word_123")
(assert-match "\\W non-word" "\\W+" "!!!")
(assert-match "\\l lower" "\\l+" "abc")
(assert-match "\\u upper" "\\u+" "ABC")
(assert-match "\\a alpha" "\\a+" "abcABC")
(assert-match "\\p alnum" "\\p+" "abc123")
(assert-match "\\x hex" "\\x+" "ABC123def")

; --- Capture Groups & Replacement ---
(report-header "Regexp: Capture Groups")
(defq text "2026-02-10")
(defq pattern "(\\d+)-(\\d+)-(\\d+)")
(assert-eq "capture replacement" "10/02/2026" (replace-regex text pattern "$3/$2/$1"))
(assert-eq "capture $0" "Full: 2026-02-10" (replace-regex text pattern "Full: $0"))

; Nested groups
(defq nested_text "abcde")
(defq nested_pattern "a(b(c)d)e")
; $0 = abcde
; $1 = bcd
; $2 = c
(assert-eq "nested capture" "c-bcd-abcde" (replace-regex nested_text nested_pattern "$2-$1-$0"))

; --- Literal Escapes ---
(report-header "Regexp: Literal Escapes")
(assert-match "newline \\n" "a\\nb" "a\nb")
(assert-match "tab \\t" "a\\tb" "a\tb")
; Fixed quote test: removed extra space in text
(assert-match "quote \\q" "a\\qb" "a\qb")
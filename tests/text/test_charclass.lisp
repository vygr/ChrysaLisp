(report-header "Char Classes & Escaping")

(import "lib/text/charclass.inc")

; --- Character Classes (bfind) ---
(defq cls (char-class "0-9A-F"))
(assert-true "bfind match digit" (bfind "5" cls))
(assert-true "bfind match hex"   (bfind "C" cls))
(assert-true "bfind miss"		(not (bfind "G" cls)))

; --- Escaping ---
(assert-eq "escape" "Hello\\nWorld" (escape "Hello\nWorld"))
(assert-eq "escape-regexp" "Hello\\." (escape-regexp "Hello."))
(assert-eq "unescape" "a\nb" (unescape "a\\nb"))

; --- bskip family tests ---

; bskip: skip characters IN class
(assert-eq "bskip space" 2 (bskip +char_class_space "  abc" 0))
(assert-eq "bskip alpha" 3 (bskip +char_class_alpha "abc123" 0))
(assert-eq "bskip digit" 3 (bskip +char_class_digit "123abc" 0))
(assert-eq "bskip hex" 5 (bskip +char_class_hex "decaf_coffee" 0))

; bskipn: skip characters NOT IN class
(assert-eq "bskipn alpha" 3 (bskipn +char_class_alpha "123abc" 0))
(assert-eq "bskipn digit" 3 (bskipn +char_class_digit "abc123" 0))
(assert-eq "bskipn space" 3 (bskipn +char_class_space "abc  " 0))

; rbskip: skip backward characters IN class
(assert-eq "rbskip space" 3 (rbskip +char_class_space "abc  " 5))
(assert-eq "rbskip alpha" 3 (rbskip +char_class_alpha "123abc" 6))
(assert-eq "rbskip digit" 0 (rbskip +char_class_digit "123abc" 3))

; rbskipn: skip backward characters NOT IN class
(assert-eq "rbskipn alpha" 3 (rbskipn +char_class_alpha "abc123" 6))
(assert-eq "rbskipn digit" 3 (rbskipn +char_class_digit "123abc" 6))
(assert-eq "rbskipn space" 2 (rbskipn +char_class_space "  abc" 5))

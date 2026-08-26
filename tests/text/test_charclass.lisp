(report-header "Char Classes & Escaping")

(import "lib/text/charclass.inc")

; --- Character Classes (bfind) ---
(defq cls (char-class "0-9A-F"))
(assert-true "bfind match digit" (bfind "5" cls))
(assert-true "bfind match hex"   (bfind "C" cls))
(assert-true "bfind miss"		(not (bfind "G" cls)))

; --- Escaping: All standard character types ---
(assert-eq "escape empty" "" (escape ""))
(assert-eq "escape plain text" "abc XYZ 123" (escape "abc XYZ 123"))
(assert-eq "escape newline" "a\\nb" (escape "a\nb"))
(assert-eq "escape tab" "a\\tb" (escape "a\tb"))
(assert-eq "escape carriage return" "a\\rb" (escape "a\rb"))
(assert-eq "escape form feed" "a\\fb" (escape "a\fb"))
(assert-eq "escape vertical tab" "a\\vb" (escape "a\vb"))
(assert-eq "escape double quote" "a\\qb" (escape "a\qb"))
(assert-eq "escape backslash" "a\\\\b" (escape "a\\b"))
(assert-eq "escape all control chars" "\\r\\f\\v\\n\\t\\q\\\\" (escape "\r\f\v\n\t\q\\"))
(assert-eq "escape consecutive" "\\n\\n\\t\\t" (escape "\n\n\t\t"))
(assert-eq "escape interleaved" "a\\rb\\fc\\vd\\ne\\tf\\qg\\\\h" (escape "a\rb\fc\vd\ne\tf\qg\\h"))
(assert-eq "escape-regexp" "Hello\\." (escape-regexp "Hello."))

; --- Unescaping: All standard character types ---
(assert-eq "unescape empty" "" (unescape ""))
(assert-eq "unescape plain text" "abc XYZ 123" (unescape "abc XYZ 123"))
(assert-eq "unescape newline" "a\nb" (unescape "a\\nb"))
(assert-eq "unescape tab" "a\tb" (unescape "a\\tb"))
(assert-eq "unescape carriage return" "a\rb" (unescape "a\\rb"))
(assert-eq "unescape form feed" "a\fb" (unescape "a\\fb"))
(assert-eq "unescape vertical tab" "a\vb" (unescape "a\\vb"))
(assert-eq "unescape double quote" "a\qb" (unescape "a\\qb"))
(assert-eq "unescape backslash" "a\\b" (unescape "a\\\\b"))
(assert-eq "unescape all control chars" "\r\f\v\n\t\q\\" (unescape "\\r\\f\\v\\n\\t\\q\\\\"))
(assert-eq "unescape consecutive" "\n\n\t\t" (unescape "\\n\\n\\t\\t"))
(assert-eq "unescape interleaved" "a\rb\fc\vd\ne\tf\qg\\h" (unescape "a\\rb\\fc\\vd\\ne\\tf\\qg\\\\h"))

; --- Unescaping: Hex escapes (\xNN) ---
(assert-eq "unescape hex space" " " (unescape "\\x20"))
(assert-eq "unescape hex uppercase" "A" (unescape "\\x41"))
(assert-eq "unescape hex lowercase" "z" (unescape "\\x7a"))
(assert-eq "unescape hex upper digits" "Z" (unescape "\\x5A"))
(assert-eq "unescape hex tilde" "~" (unescape "\\x7E"))
(assert-eq "unescape hex null byte" (char 0) (unescape "\\x00"))
(assert-eq "unescape hex 0xFF" (char 255) (unescape "\\xff"))
(assert-eq "unescape hex mixed string" "Hello, World!" (unescape "Hello\\x2C\\x20World\\x21"))

; --- Unescaping: Edge cases & Fallbacks ---
(assert-eq "unescape unhandled escape char" "z" (unescape "\\z"))
(assert-eq "unescape trailing backslash" "test\\" (unescape "test\\"))
(assert-eq "unescape incomplete hex 1" "x" (unescape "\\x"))
(assert-eq "unescape incomplete hex 2" "x4" (unescape "\\x4"))

; --- Escape / Unescape Roundtrip ---
(defq rt_sample "Line 1\t\qquoted\q\r\nLine 2\v\fEnd\\Path")
(assert-eq "escape-unescape roundtrip" rt_sample (unescape (escape rt_sample)))

; --- escape-regexp Tests ---
(assert-eq "escape-regexp empty" "" (escape-regexp ""))
(assert-eq "escape-regexp plain text" "Hello World 123" (escape-regexp "Hello World 123"))
(assert-eq "escape-regexp caret" "\\^" (escape-regexp "^"))
(assert-eq "escape-regexp dollar" "\\$" (escape-regexp "$"))
(assert-eq "escape-regexp exclam" "\\!" (escape-regexp "!"))
(assert-eq "escape-regexp dot" "\\." (escape-regexp "."))
(assert-eq "escape-regexp star" "\\*" (escape-regexp "*"))
(assert-eq "escape-regexp plus" "\\+" (escape-regexp "+"))
(assert-eq "escape-regexp question" "\\?" (escape-regexp "?"))
(assert-eq "escape-regexp pipe" "\\|" (escape-regexp "|"))
(assert-eq "escape-regexp left bracket" "\\[" (escape-regexp "["))
(assert-eq "escape-regexp right bracket" "\\]" (escape-regexp "]"))
(assert-eq "escape-regexp left paren" "\\(" (escape-regexp "("))
(assert-eq "escape-regexp right paren" "\\)" (escape-regexp ")"))
(assert-eq "escape-regexp backslash" "\\\\" (escape-regexp "\\"))
(assert-eq "escape-regexp all metachars"
	"\\^\\$\\!\\.\\*\\+\\?\\|\\[\\]\\(\\)\\\\"
	(escape-regexp "^$!.*+?|[]()\\"))
(assert-eq "escape-regexp arithmetic" "1 \\+ 1 = 2" (escape-regexp "1 + 1 = 2"))
(assert-eq "escape-regexp filename" "file\\.tar\\.gz" (escape-regexp "file.tar.gz"))
(assert-eq "escape-regexp pattern string" "\\(a\\|b\\)\\*\\?" (escape-regexp "(a|b)*?"))
(assert-eq "escape-regexp windows path" "C:\\\\path\\\\file" (escape-regexp "C:\\path\\file"))

; Functional match? verification using escaped regexp patterns
(assert-true "escape-regexp match literal plus" (match? "1 + 1 = 2" (escape-regexp "1 + 1 = 2")))
(assert-true "escape-regexp no match wildcard" (not (match? "1x2" (escape-regexp "1.2"))))
(assert-true "escape-regexp match literal dot" (match? "1.2" (escape-regexp "1.2")))
(assert-true "escape-regexp match literal brackets" (match? "[a-z]" (escape-regexp "[a-z]")))
(assert-true "escape-regexp no match char class range" (not (match? "m" (escape-regexp "[a-z]"))))

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

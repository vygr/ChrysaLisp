(import "lib/text/document.inc")

(report-header "Document Edges: selecting, breaking, tabs, line operations, reflow, undo")

(defun de-sel (text cx cy ax ay &rest ops)
	; a document of text, with a selection from the anchor ax ay to the
	; cursor cx cy, the ops done, gives (text cursors)
	(defq d (Document (+ +buffer_flag_undo +buffer_flag_syntax)))
	(. d :insert text)
	(. d :set_cursor cx cy ax ay)
	(each (# (if (list? %0) (apply . (cat (list d) %0)) (. d %0))) ops)
	(list (apply (const cat) (. d :get_buffer_lines))
		(map (# (slice %0 0 4)) (. d :get_cursors_sorted))))

(defun de-at (text cx cy &rest ops)
	; as de-sel, with just a cursor
	(apply de-sel (cat (list text cx cy cx cy) ops)))

(defun de-csr (&rest args)
	; just the first cursor that results
	(first (second (apply de-at args))))

(defun de-text (&rest args)
	; just the text that results, from a selection
	(first (apply de-sel args)))

; --- select_word, a cursor is (cx cy ax ay), the selection runs from the anchor ---
(test-cases
	(de-csr "one two three" 5 0 :select_word) '(7 0 4 0)
	;at either end of the word
	(de-csr "one two three" 4 0 :select_word) '(7 0 4 0)
	(de-csr "one two three" 7 0 :select_word) '(7 0 4 0)
	(de-csr "one two" 0 0 :select_word) '(3 0 0 0)
	(de-csr "one two" 7 0 :select_word) '(7 0 4 0)
	;between two spaces there is no word
	(de-csr "one  two" 4 0 :select_word) '(4 0 4 0)
	;a hyphen is part of a word, a dot and a bracket are not
	(de-csr "foo-bar baz" 1 0 :select_word) '(7 0 0 0)
	(de-csr "foo.bar" 1 0 :select_word) '(3 0 0 0)
	(de-csr "(foo)" 2 0 :select_word) '(4 0 1 0)
	;on an empty line
	(de-csr "" 0 0 :select_word) '(0 0 0 0)
	(de-csr "a\n\nb" 0 1 :select_word) '(0 1 0 1))

; --- select_line and select_all ---
(test-cases
	(de-csr "ab\ncd\nef" 1 1 :select_line) '(0 2 0 1)
	(de-csr "ab\ncd\nef" 1 2 :select_line) '(0 3 0 2)
	;the end of file line
	(de-csr "ab\ncd\nef" 0 3 :select_line) '(0 4 0 3)
	(de-csr "" 0 0 :select_line) '(0 1 0 0)
	(de-csr "abc" 1 0 :select_all) '(0 1 0 0)
	(de-csr "a\nb" 0 0 :select_all) '(0 2 0 0)
	(de-csr "" 0 0 :select_all) '(0 0 0 0))

; --- select_paragraph, paragraphs are split by blank lines ---
(test-cases
	(de-csr "a\nb\n\nc\nd\n\ne" 0 0 :select_paragraph) '(0 2 0 0)
	(de-csr "a\nb\n\nc\nd\n\ne" 0 3 :select_paragraph) '(0 5 0 3)
	(de-csr "a\nb\n\nc\nd\n\ne" 0 4 :select_paragraph) '(0 5 0 3)
	(de-csr "a\nb\n\nc\nd\n\ne" 0 6 :select_paragraph) '(0 7 0 6)
	;on the blank line, the paragraph above
	(de-csr "a\nb\n\nc\nd\n\ne" 0 2 :select_paragraph) '(0 2 0 0)
	;with no blank lines it is all one
	(de-csr "a\nb" 0 0 :select_paragraph) '(0 2 0 0)
	;a line of just spaces is blank
	(de-csr "a\n \nb" 0 0 :select_paragraph) '(0 1 0 0)
	(de-csr "" 0 0 :select_paragraph) '(0 0 0 0))

; --- select_block and select_form, these need the syntax flag ---
(test-cases
	;the innermost brackets around the cursor
	(de-csr "(a (b c) d)" 5 0 :select_block) '(8 0 3 0)
	(de-csr "(a (b c) d)" 4 0 :select_block) '(8 0 3 0)
	(de-csr "(a (b c) d)" 1 0 :select_block) '(11 0 0 0)
	(de-csr "(a (b c) d)" 0 0 :select_block) '(11 0 0 0)
	(de-csr "((a))" 2 0 :select_block) '(4 0 1 0)
	(de-csr "()" 1 0 :select_block) '(2 0 0 0)
	;over lines
	(de-csr "(a\n(b)\nc)" 0 1 :select_block) '(3 1 0 1)
	;a bracket in a comment is not a bracket
	(de-csr "(a ; c)\n b)" 1 1 :select_block) '(3 1 0 0)
	;with no brackets around it, or unbalanced ones, nothing is selected
	(de-csr "a b c" 2 0 :select_block) '(2 0 2 0)
	(de-csr "(a b" 2 0 :select_block) '(2 0 2 0)
	(de-csr "a b)" 2 0 :select_block) '(2 0 2 0)
	(de-csr "" 0 0 :select_block) '(0 0 0 0)
	;select_form takes the brackets when on one, else the word
	(de-csr "(a (b c) d)" 3 0 :select_form) '(8 0 3 0)
	(de-csr "(a (b c) d)" 7 0 :select_form) '(8 0 3 0)
	(de-csr "(a (b c) d)" 0 0 :select_form) '(11 0 0 0)
	(de-csr "(a (b c) d)" 10 0 :select_form) '(11 0 0 0)
	(de-csr "(a (b c) d)" 4 0 :select_form) '(5 0 4 0)
	(de-csr "(abc def)" 2 0 :select_form) '(4 0 1 0)
	(de-csr "" 0 0 :select_form) '(0 0 0 0))

; --- break, the new line takes the indent of the old ---
(test-cases
	(de-at "abcd" 2 0 :break) '("ab\ncd\n\n" ((0 1 0 1)))
	(de-at "    abcd" 6 0 :break) '("    ab\n    cd\n\n" ((4 1 4 1)))
	(de-at "    abcd" 8 0 :break) '("    abcd\n    \n\n" ((4 1 4 1)))
	(de-at "    abcd" 0 0 :break) '("\nabcd\n\n" ((0 1 0 1)))
	(de-at "\tab cd" 3 0 :break) '("\tab\n\tcd\n\n" ((1 1 1 1)))
	;spaces around the break are dropped, wherever the cursor is in them
	(de-at "ab   cd" 2 0 :break) '("ab\ncd\n\n" ((0 1 0 1)))
	(de-at "ab   cd" 3 0 :break) '("ab\ncd\n\n" ((0 1 0 1)))
	(de-at "ab   cd" 5 0 :break) '("ab\ncd\n\n" ((0 1 0 1)))
	;a selection is removed first
	(de-sel "  abcdef" 6 0 4 0 :break) '("  ab\n  ef\n\n" ((2 1 2 1)))
	(de-sel "  ab\n  cd" 3 1 3 0 :break) '("  a\n  d\n\n" ((2 1 2 1)))
	(de-at "" 0 0 :break) '("\n\n" ((0 1 0 1))))

; --- tab, spaces up to the next tab stop ---
(test-cases
	(de-at "ab" 0 0 :tab) '("    ab\n\n" ((4 0 4 0)))
	(de-at "ab" 1 0 :tab) '("a   b\n\n" ((4 0 4 0)))
	(de-at "ab" 2 0 :tab) '("ab  \n\n" ((4 0 4 0)))
	;on a tab stop it is a whole tab
	(de-at "abcd" 4 0 :tab) '("abcd    \n\n" ((8 0 8 0)))
	(de-at "abcde" 5 0 :tab) '("abcde   \n\n" ((8 0 8 0)))
	(de-at "" 0 0 :tab) '("    \n\n" ((4 0 4 0)))
	;a selection is replaced
	(de-sel "abcd" 3 0 1 0 :tab) '("a   d\n\n" ((4 0 4 0)))
	(de-at "ab" 1 0 '(:set_tab_width 8) :tab) '("a       b\n\n" ((8 0 8 0)))
	(de-at "ab" 1 0 '(:set_tab_width 2) :tab) '("a b\n\n" ((2 0 2 0)))
	(. (Document) :get_tab_width) 4)

; --- right_tab and left_tab, on every line of the selection ---
(test-cases
	(de-text "a\nb\nc" 0 2 0 0 :right_tab) "    a\n    b\nc\n\n"
	;a line the selection only partly covers is taken whole
	(de-text "a\nb\nc" 1 1 0 0 :right_tab) "    a\n    b\nc\n\n"
	;with no selection, the line of the cursor
	(de-text "a\nb" 0 0 0 0 :right_tab) "    a\nb\n\n"
	(de-text "    a\n  b\nc\n      d" 0 4 0 0 :left_tab) "a\nb\nc\n  d\n\n"
	(de-text "    a" 2 0 2 0 :left_tab) "a\n\n"
	;only spaces are taken, not tabs
	(de-text "\ta\n \tb" 0 2 0 0 :left_tab) "\ta\n\tb\n\n"
	(de-text "    a\n    b" 0 2 0 0 :left_tab :left_tab) "a\nb\n\n"
	(de-text "a\nb" 0 2 0 0 :right_tab :left_tab) "a\nb\n\n"
	(de-text "" 0 0 0 0 :left_tab) "\n")

; --- to_upper and to_lower, on the selection only ---
(test-cases
	(de-sel "AbC dEf" 5 0 1 0 :to_lower) '("Abc dEf\n\n" ((5 0 1 0)))
	(de-sel "AbC dEf" 5 0 1 0 :to_upper) '("ABC DEf\n\n" ((5 0 1 0)))
	(de-sel "Ab\nCd" 1 1 1 0 :to_upper) '("AB\nCd\n\n" ((1 1 1 0)))
	;with no selection there is nothing to change
	(de-at "AbC" 1 0 :to_lower) '("AbC\n\n" ((1 0 1 0)))
	(de-at "" 0 0 :to_upper) '("\n" ((0 0 0 0))))

; --- sort, invert and unique, on the lines of the selection ---
(test-cases
	(de-text "c\nb\na" 0 3 0 0 :sort) "a\nb\nc\n\n"
	(de-text "c\nb\na" 0 2 0 0 :sort) "b\nc\na\n\n"
	(de-text "c\nb\na" 1 2 1 0 :sort) "a\nb\nc\n\n"
	(de-text "b\na\nb\na" 0 4 0 0 :sort) "a\na\nb\nb\n\n"
	;it is a text sort, capitals come first, and 10 comes before 9
	(de-text "B\na\nA\nb" 0 4 0 0 :sort) "A\nB\na\nb\n\n"
	(de-text "10\n9\n1" 0 3 0 0 :sort) "1\n10\n9\n\n"
	(de-text "c\n\na" 0 3 0 0 :sort) "\na\nc\n\n"
	;one line, or none, is left as it is
	(de-text "c\nb\na" 0 0 0 0 :sort) "c\nb\na\n\n"
	(de-text "" 0 0 0 0 :sort) "\n"
	(de-text "a\nb\nc" 0 3 0 0 :invert) "c\nb\na\n\n"
	(de-text "a\nb\nc" 0 2 0 0 :invert) "b\na\nc\n\n"
	(de-text "a\nb" 0 0 0 0 :invert) "a\nb\n\n"
	(de-text "" 0 0 0 0 :invert) "\n")

;unique drops lines that repeat the one before, and the selection shrinks to fit
(test-cases
	(de-sel "a\na\nb\nb\na" 0 5 0 0 :unique) '("a\nb\na\n\n" ((0 3 0 0)))
	(de-sel "a\nb\nc" 0 3 0 0 :unique) '("a\nb\nc\n\n" ((0 3 0 0)))
	(de-sel "a\na\na" 0 3 0 0 :unique) '("a\n\n" ((0 1 0 0)))
	(de-sel "a\na\nb" 0 2 0 0 :unique) '("a\nb\n\n" ((0 1 0 0)))
	(de-text "a\na" 0 0 0 0 :unique) "a\na\n\n"
	(de-text "" 0 0 0 0 :unique) "\n")

; --- comment, toggles each line, and leaves blank lines alone ---
(test-cases
	(de-text "a\nb" 0 2 0 0 :comment) ";; a\n;; b\n\n"
	(de-text "a\nb" 0 2 0 0 :comment :comment) "a\nb\n\n"
	(de-text ";; a\nb" 0 2 0 0 :comment) "a\n;; b\n\n"
	(de-text "a\n\nb" 0 3 0 0 :comment) ";; a\n\n;; b\n\n"
	(de-text "a\nb" 0 0 0 0 :comment) ";; a\nb\n\n"
	(de-text "  a" 0 1 0 0 :comment) ";;   a\n\n"
	(de-text "" 0 0 0 0 :comment) "\n")

; --- trim, blank lines at each end, and white space at each line end ---
(test-cases
	(de-text "\n\na  \nb\t\n\n" 0 0 0 0 :trim) "a\nb\n\n"
	(de-text "a  \n\n  b  " 0 0 0 0 :trim) "a\n\n  b\n\n"
	(de-text "a" 0 0 0 0 :trim) "a\n\n"
	(de-text "  a" 0 0 0 0 :trim) "  a\n\n"
	(de-text "\n\n\n" 0 0 0 0 :trim) "\n"
	(de-text "  \n\t\n" 0 0 0 0 :trim) "\n"
	(de-text "" 0 0 0 0 :trim) "\n")

; --- reflow and split, on the paragraph of the cursor ---
(test-cases
	(. (Document) :get_wrap_width) 80
	(de-text "one two three four five" 0 0 0 0 '(:set_wrap_width 10) :reflow) "one two\nthree\nfour five\n\n"
	(de-text "one two three four five" 0 0 0 0 '(:set_wrap_width 100) :reflow) "one two three four five\n\n"
	;lines are joined, and runs of spaces become one
	(de-text "one\ntwo\nthree" 0 0 0 0 '(:set_wrap_width 100) :reflow) "one two three\n\n"
	(de-text "a  b   c" 0 0 0 0 '(:set_wrap_width 100) :reflow) "a b c\n\n"
	;only the one paragraph
	(de-text "one two\n\nthree four" 0 0 0 0 '(:set_wrap_width 5) :reflow) "one\ntwo\n\nthree four\n\n"
	(de-text "one two\n\nthree four" 0 2 0 2 '(:set_wrap_width 5) :reflow) "one two\n\nthree\nfour\n\n"
	;the indent of the first line is kept on every line
	(de-text "  one two three" 0 0 0 0 '(:set_wrap_width 9) :reflow) "  one\n  two\n  three\n\n"
	;a word longer than the width is not broken
	(de-text "supercalifragilistic word" 0 0 0 0 '(:set_wrap_width 5) :reflow) "supercalifragilistic\nword\n\n"
	(de-text "one" 0 0 0 0 '(:set_wrap_width 10) :reflow) "one\n\n"
	(de-text "" 0 0 0 0 '(:set_wrap_width 10) :reflow) "\n"
	(de-text "one two three" 0 0 0 0 :split) "one\ntwo\nthree\n\n"
	(de-text "one\ntwo three" 0 0 0 0 :split) "one\ntwo\nthree\n\n"
	(de-text "  one   two" 0 0 0 0 :split) "one\ntwo\n\n"
	(de-text "one two\n\nthree four" 0 2 0 2 :split) "one two\n\nthree\nfour\n\n"
	(de-text "one" 0 0 0 0 :split) "one\n\n"
	(de-text "" 0 0 0 0 :split) "\n")

; --- each operation is one undo step, and gives back the text and the cursor ---
(test-cases
	(de-at "ab" 1 0 :tab :undo) '("ab\n\n" ((1 0 1 0)))
	(de-at "abcd" 2 0 :break :undo) '("abcd\n\n" ((2 0 2 0)))
	(de-at "one two" 0 0 :reflow :undo) '("one two\n\n" ((0 0 0 0)))
	(de-sel "a\nb" 0 2 0 0 :comment :undo) '("a\nb\n\n" ((0 2 0 0)))
	(de-sel "a\na\nb" 0 3 0 0 :unique :undo) '("a\na\nb\n\n" ((0 3 0 0)))
	(de-sel "c\nb\na" 0 3 0 0 :sort :undo) '("c\nb\na\n\n" ((0 3 0 0)))
	(de-text "\n\na  \n" 0 0 0 0 :trim :undo) "\n\na  \n\n")

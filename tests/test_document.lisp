; Run with: ./run_tui.sh -e -n 1 -f -s tests/run_all.lisp

(import "lib/text/document.inc")

(report-header "Text Document: High-level Editing")

; --- Word Selection ---
(defq d (Document))
(. d :insert "Hello World")
(. d :set_cursor 2 0) ; middle of "Hello"
(. d :select_word)
(assert-eq "select_word" (nums 5 0 0 0) (first (. d :get_selected)))

; --- Line Selection ---
(defq d (Document))
(. d :insert "Line 1\nLine 2")
(. d :set_cursor 2 0)
(. d :select_line)
(assert-eq "select_line" (nums 0 1 0 0) (first (. d :get_selected)))

; --- Paragraph Selection ---
(defq d (Document))
(. d :insert "P1\n\nP2")
(. d :set_cursor 0 0)
(. d :select_paragraph)
(assert-eq "select_paragraph P1" (nums 0 1 0 0) (first (. d :get_selected)))

; --- All Selection ---
(defq d (Document))
(. d :insert "ABC\nDEF")
(. d :select_all)
(assert-eq "select_all" (nums 0 2 0 0) (first (. d :get_selected)))

; --- Tabbing (Indent/Outdent) ---
(defq d (Document))
(. d :insert "Line 1\nLine 2")
(. d :set_cursor 0 0)
(. d :add_cursor 0 1) ; two cursors at starts of lines
(. d :right_tab)
(assert-eq "right_tab line 0" "    Line 1\n" (. d :get_text_line 0))
(assert-eq "right_tab line 1" "    Line 2\n" (. d :get_text_line 1))

(defq d (Document))
(. d :insert "    Line 1\n    Line 2")
(. d :set_cursor 0 0)
(. d :add_cursor 0 1)
(. d :left_tab)
(assert-eq "left_tab line 0" "Line 1\n" (. d :get_text_line 0))
(assert-eq "left_tab line 1" "Line 2\n" (. d :get_text_line 1))

; --- Case Conversion ---
(defq d (Document))
(. d :insert "Hello")
(. d :set_cursor 0 0 5 0)
(. d :to_upper)
(assert-eq "to_upper" "HELLO\n" (. d :get_text_line 0))
(. d :set_cursor 0 0 5 0)
(. d :to_lower)
(assert-eq "to_lower" "hello\n" (. d :get_text_line 0))

; --- Commenting ---
(defq d (Document))
(. d :insert "Code")
(. d :set_cursor 0 0)
(. d :comment)
(assert-eq "comment" ";; Code\n" (. d :get_text_line 0))
(. d :comment)
(assert-eq "uncomment" "Code\n" (. d :get_text_line 0))

; --- Sorting & Uniq ---
(defq d (Document))
(. d :insert "B\nC\nA")
(. d :select_all)
(. d :sort)
(assert-eq "sort L0" "A\n" (. d :get_text_line 0))
(assert-eq "sort L1" "B\n" (. d :get_text_line 1))
(assert-eq "sort L2" "C\n" (. d :get_text_line 2))

(defq d (Document))
(. d :insert "A\nA\nB")
(. d :select_all)
(. d :unique)
(assert-eq "unique L0" "A\n" (. d :get_text_line 0))
(assert-eq "unique L1" "B\n" (. d :get_text_line 1))

; --- Block & Form Selection ---
(defq d (Document +buffer_flag_syntax))
(. d :insert "(outer (inner))")
(. d :set_cursor 8 0) ; middle of "inner"
(. d :select_block)
(assert-list-eq "select_block" (nums 14 0 7 0) (first (. d :get_selected)))
(. d :set_cursor 7 0) ; on the "(" of "(inner"
(. d :select_form)
(assert-list-eq "select_form" (nums 14 0 7 0) (first (. d :get_selected)))

; --- Tabbing (Single Cursor) ---
(defq d (Document))
(. d :insert "Text")
(. d :set_cursor 0 0)
(. d :tab)
(assert-eq "tab indent" "    Text\n" (. d :get_text_line 0))

; --- Invert ---
(defq d (Document))
(. d :insert "Line 1\nLine 2\nLine 3")
(. d :select_all)
(. d :invert)
(assert-eq "invert L0" "Line 3\n" (. d :get_text_line 0))
(assert-eq "invert L1" "Line 2\n" (. d :get_text_line 1))
(assert-eq "invert L2" "Line 1\n" (. d :get_text_line 2))

; --- Reflow & Split ---
(defq d (Document))
(. d :set_wrap_width 10)
(. d :insert "This is a long line that should be reflowed.")
(. d :reflow)
(assert-true "reflow line count" (> (length (. d :get_buffer_lines)) 2))

(defq d (Document))
(. d :set_tab_width 2)
(. d :insert "Line")
(. d :set_cursor 0 0)
(. d :tab)
(assert-eq "tab_width 2 indent" "  Line\n" (. d :get_text_line 0))

(defq d (Document))
(. d :insert "Word1 Word2 Word3")
(. d :split)
(assert-eq "split word 1" "Word1\n" (. d :get_text_line 0))
(assert-eq "split word 2" "Word2\n" (. d :get_text_line 1))

; --- Trim ---
(defq d (Document))
(. d :insert "\n\n  Spaced Line  \n\n")
(. d :trim)
(assert-eq "trim content" "  Spaced Line\n" (. d :get_text_line 0))
(assert-eq "trim height" 1 (second (. d :get_size)))

(import "lib/text/document.inc")
(import "lib/text/edit.inc")

(report-header "Text Edit Library (edit.inc)")

; Setup environment
(defq *file* "test.txt")
(defq *edit* (Document +buffer_flag_syntax))

(defq line123 (join '("Line 1" "Line 2" "Line 3") "\n"))
(defq LINE123 (join '("LINE 1" "LINE 2" "LINE 3") "\n"))
(defq WORD123 (join '("WORD 1" "WORD 2" "WORD 3") "\n"))

; --- Basic Mutation ---
(edit-insert "Line 1\nLine 2\nLine 3")
(edit-select-all)
(assert-list-eq "edit-get-text" line123 (edit-get-text))
(assert-eq "edit-get-filename" "test.txt" (edit-get-filename))

; --- Navigation ---
(edit-top)
(assert-eq "edit-cx top" 0 (edit-cx))
(assert-eq "edit-cy top" 0 (edit-cy))

(edit-down 2)
(assert-eq "edit-cy down 2" 2 (edit-cy))

(edit-home)
(assert-eq "edit-cx home" 0 (edit-cx))

(edit-end)
(assert-eq "edit-cx end" 6 (edit-cx))

(assert-eq "edit-eof? false" :nil (edit-eof?))
(edit-bottom)
(assert-eq "edit-eof? true" :t (edit-eof?))

; --- Selection ---
(edit-top)
(edit-select-down 1)
(assert-eq "edit-copy selection" "Line 1\n" (edit-copy))

(edit-select-all)
(assert-eq "edit-copy all" (cat line123 "\n") (edit-copy))

; --- Search & Find ---
(edit-top)
(edit-find "Line")
(edit-find-next)
(assert-eq "edit-find-next selection" "Line" (edit-copy))

(edit-find-add-next)
(assert-eq "edit-find-add-next count" 2 (length (. *edit* :get_cursors)))

; --- Mutation Utilities ---
(edit-select-all)
(edit-upper)
(edit-select-all)
(assert-list-eq "edit-upper" LINE123 (edit-get-text))

(edit-lower)
(edit-select-all)
(assert-list-eq "edit-lower" (to-lower line123) (edit-get-text))

; --- Complex Replace ---
(edit-top)
(edit-find "line")
(edit-cursors) ; set cursors to all "line"
(edit-replace "WORD")
(edit-select-all)
(assert-list-eq "edit-replace" WORD123 (edit-get-text))

; --- Split/Join ---
(defq parts (edit-split-text "A\nB\fC"))
(assert-list-eq "edit-split-text" '("A" "B" "C") parts)
(assert-eq "edit-join-text" "A\nB\nC" (edit-join-text parts))

; --- Primary ---
(edit-top)
(edit-find "WORD")
(edit-add-cursors)
(assert-eq "Cursors count before primary" 3 (length (. *edit* :get_cursors)))
(edit-primary)
(assert-eq "Cursors count after primary" 1 (length (. *edit* :get_cursors)))

; --- White Space & Brackets ---
(edit-select-all)
(edit-delete)
(edit-insert "  (bracket)  ")
(edit-top)
(edit-select-ws-right)
(assert-eq "edit-select-ws-right" "  " (edit-copy))

(edit-top)
(edit-right 2) ; Move to '('
(edit-select-bracket-right)
(assert-eq "edit-select-bracket-right" "(bracket" (edit-copy))

(edit-end)
(edit-select-ws-left)
(assert-eq "edit-select-ws-left" "  " (edit-copy))

(edit-end)
(edit-left 2) ; Move to ')'
(edit-select-bracket-left)
(assert-eq "edit-select-bracket-left" "(bracket)" (edit-copy))

; --- Additional Tests ---
(edit-select-all)
(edit-delete)
(edit-insert "word1 word2 word3")
(edit-top)
(edit-select-word)
(assert-eq "edit-select-word" "word1" (edit-copy))

(edit-select-all)
(edit-indent)
(edit-select-all)
(assert-list-eq "edit-indent" "    word1 word2 word3" (edit-get-text))

(edit-outdent)
(edit-select-all)
(assert-list-eq "edit-outdent" "word1 word2 word3" (edit-get-text))

(edit-select-all)
(edit-delete)
(edit-insert "ABC")
(edit-backspace 1)
(edit-select-all)
(assert-list-eq "edit-backspace ABC" "AB" (edit-get-text))

(edit-top)
(edit-delete 1)
(edit-select-all)
(assert-list-eq "edit-delete AB" "B" (edit-get-text))

(import "lib/text/document.inc")
(import "lib/text/edit.inc")

(report-header "Text Edit Library (edit.inc)")

; Setup environment
(defq *file* "test.txt")
(defq *edit* (Document +buffer_flag_syntax))

; --- Basic Mutation ---
(edit-insert "Line 1\nLine 2\nLine 3")
(edit-select-all)
; We expect 3 lines. edit-get-text joins with \n and trims final \n.
(assert-eq "edit-get-text" "Line 1\nLine 2\nLine 3" (edit-get-text))
(assert-eq "edit-get-filename" "test.txt" (edit-get-filename))

; --- Navigation ---
;(edit-top)
;(assert-eq "edit-cx top" 0 (edit-cx))
;(assert-eq "edit-cy top" 0 (edit-cy))

;(edit-down 2)
;(assert-eq "edit-cy down 2" 2 (edit-cy))

;(edit-home)
;(assert-eq "edit-cx home" 0 (edit-cx))

;(edit-end)
;(assert-eq "edit-cx end" 6 (edit-cx))

;(assert-eq "edit-eof? false" :nil (edit-eof?))
;(edit-bottom)
;(assert-eq "edit-eof? true" :t (edit-eof?))

; --- Selection ---
;(edit-top)
;(edit-select-down 1)
; Selecting down from (0,0) to (0,1) includes the newline of Line 1
;(assert-eq "edit-copy selection" "Line 1\n" (edit-copy))

;(edit-select-all)
;(assert-eq "edit-copy all" "Line 1\nLine 2\nLine 3\n" (edit-copy))

; --- Search & Find ---
;(edit-top)
;(edit-find "Line")
;(edit-find-next)
;(assert-eq "edit-find-next selection" "Line" (edit-copy))

;(edit-find-add-next)
;(assert-eq "edit-find-add-next count" 2 (length (. *edit* :get_cursors)))
;(assert-eq "edit-copy multi-cursor formfeed" "Line\fLine" (edit-copy))

; --- Mutation Utilities ---
;(edit-select-all)
;(edit-upper)
;(edit-select-all)
;(assert-eq "edit-upper" "LINE 1\nLINE 2\nLINE 3" (edit-get-text))

;(edit-lower)
;(edit-select-all)
;(assert-eq "edit-lower" "line 1\nline 2\nline 3" (edit-get-text))

; --- Complex Replace ---
;(edit-top)
;(edit-find "line")
;(edit-cursors) ; set cursors to all "line"
;(edit-replace "WORD")
;(edit-select-all)
;(assert-eq "edit-replace" "WORD 1\nWORD 2\nWORD 3" (edit-get-text))

; --- Split/Join ---
;(defq parts (edit-split-text "A\nB\fC"))
;(assert-list-eq "edit-split-text" '("A" "B" "C") parts)
;(assert-eq "edit-join-text" "A\nB\nC" (edit-join-text parts))

; --- Primary ---
;(edit-top)
;(edit-find "WORD")
;(edit-add-cursors)
;(assert-eq "Cursors count before primary" 3 (length (. *edit* :get_cursors)))
;(edit-primary)
;(assert-eq "Cursors count after primary" 1 (length (. *edit* :get_cursors)))

; --- White Space & Brackets ---
;(edit-select-all)
;(edit-delete)
;(edit-insert "  (bracket)  ")
;(edit-top)
;(edit-select-ws-right)
;(assert-eq "edit-select-ws-right" "  " (edit-copy))

;(edit-top)
;(edit-right 2)
;(edit-select-bracket-right)
;(assert-eq "edit-select-bracket-right" "(bracket" (edit-copy))

;(edit-end)
;(edit-select-ws-left)
;(assert-eq "edit-select-ws-left" "  " (edit-copy))

;(edit-end)
;(edit-left 2)
;(edit-select-bracket-left)
;(assert-eq "edit-select-bracket-left" "(bracket)" (edit-copy))

; --- Multi-line Multi-cursor ---
;(edit-select-all) (edit-delete)
;(edit-insert "A1\nA2\nSEP\nB1\nB2")
;(edit-top)
; Selection 1: A1\nA2\n (lines 0,1)
;(. *edit* :set_cursor 0 0 0 2)
; Selection 2: B1\nB2\n (lines 3,4)
;(. *edit* :add_cursor 0 3 0 5) 
;(assert-eq "Multi-line: Two cursors" 2 (length (. *edit* :get_cursors)))

; Verify copy: Expect "A1\nA2\n\fB1\nB2\n"
;(assert-eq "Multi-line: Copy result" "A1\nA2\n\fB1\nB2\n" (edit-copy))

; Transform and Paste (Include trailing newlines in each part to match selections)
;(edit-paste "X1\nX2\n\fY1\nY2\n")
;(edit-select-all)
;(assert-eq "Multi-line: Paste result" "X1\nX2\nSEP\nY1\nY2" (edit-get-text))

; --- Paste Scenarios ---
;(edit-select-all)
;(edit-delete)
;(edit-insert "A\nB")
;(edit-top)
;(edit-find "A") (edit-find-next)
;(edit-find "B") (edit-find-add-next)
;(assert-eq "Paste: Two cursors" 2 (length (. *edit* :get_cursors)))

; Scenario 1: Matching parts
;(edit-paste "1\f2")
;(edit-select-all)
;(assert-eq "Paste: Matching parts content" "1\n2" (edit-get-text))

; Scenario 2: Non-matching parts
;(edit-select-all) (edit-delete)
;(edit-insert "1\n2\n3")
;(edit-top)
;(edit-find "1") (edit-find-next)
;(edit-find "2") (edit-find-add-next)
;(edit-find "3") (edit-find-add-next)
;(assert-eq "Paste: Three cursors" 3 (length (. *edit* :get_cursors)))
;(edit-paste "X\fY") ; 2 parts, 3 cursors -> joins parts with \n -> "X\nY"
;(edit-select-all)
;(assert-eq "Paste: Non-matching parts content" "X\nY\nX\nY\nX\nY" (edit-get-text))

; --- Paragraph Selection, Split, and Reflow ---
;(edit-select-all) (edit-delete)
;(edit-insert "P1 L1\n\nSEP\n\nP2 L1")
;(edit-top)
;(edit-select-paragraph)
; select_paragraph on line 0 selects up to the next non-blank line delimiter
;(assert-eq "edit-select-paragraph single" "P1 L1\n" (edit-copy))

;(edit-select-all) (edit-delete)
;(edit-insert "P1 L1\n\nSEP\n\nP2 L1")
;(edit-top)
;(edit-find "P1") (edit-find-next)
;(edit-find "P2") (edit-find-add-next)

; Test split on both paragraphs
;(edit-split)
;(edit-select-all)
;(assert-eq "edit-split multi-paragraph" "P1\nL1\n\nSEP\n\nP2\nL1" (edit-get-text))

; Test reflow on both paragraphs
;(edit-select-all) (edit-delete)
;(edit-insert "a b c\n\nSEP\n\ne f g")
;(edit-top)
;(edit-find "a") (edit-find-next)
;(edit-find "e") (edit-find-add-next)
;(defq old_wrap (. *edit* :get_wrap_width))
;(. *edit* :set_wrap_width 4)
;(edit-reflow)
;(edit-select-all)
; "a b", "c" and "e f", "g" joined by "\n\n"
;(assert-eq "edit-reflow multi-paragraph" "a b\nc\n\nSEP\n\ne f\ng" (edit-get-text))
;(. *edit* :set_wrap_width old_wrap)

; --- Additional Utility Tests ---
;(edit-select-all)
;(edit-delete)
;(edit-insert "word1 word2 word3")
;(edit-top)
;(edit-select-word)
;(assert-eq "edit-select-word" "word1" (edit-copy))

;(edit-select-all)
;(edit-indent)
;(edit-select-all)
;(assert-eq "edit-indent" "    word1 word2 word3" (edit-get-text))

;(edit-outdent)
;(edit-select-all)
;(assert-eq "edit-outdent" "word1 word2 word3" (edit-get-text))

;(edit-select-all)
;(edit-delete)
;(edit-insert "ABC")
;(edit-backspace 1)
;(edit-select-all)
;(assert-eq "edit-backspace ABC" "AB" (edit-get-text))

;(edit-top)
;(edit-delete 1)
;(edit-select-all)
;(assert-eq "edit-delete AB" "B" (edit-get-text))

; --- Commenting ---
;(edit-select-all)
;(edit-delete)
;(edit-insert "code")
;(edit-select-all)
;(edit-comment)
;(edit-select-all)
;(assert-eq "edit-comment" ";; code" (edit-get-text))
;(edit-comment) ; uncomment
;(edit-select-all)
;(assert-eq "edit-uncomment" "code" (edit-get-text))

; --- Cut & Paste ---
;(edit-select-all)
;(defq cut_text (edit-cut))
;(assert-eq "edit-cut result" "code\n" cut_text)
;(edit-paste "pasted")
;(edit-select-all)
;(assert-eq "edit-paste" "pasted" (edit-get-text))

; --- Sort & Unique ---
;(edit-select-all)
;(edit-delete)
;(edit-insert "C\nA\nB\nB")
;(edit-select-all)
;(edit-sort)
;(edit-select-all)
;(assert-eq "edit-sort" "A\nB\nB\nC" (edit-get-text))
;(edit-unique)
;(edit-select-all)
;(assert-eq "edit-unique" "A\nB\nC" (edit-get-text))

; --- Trim & Split ---
;(edit-select-all)
;(edit-delete)
;(edit-insert "  text  ")
;(edit-trim)
;(edit-select-all)
;(assert-eq "edit-trim" "  text" (edit-get-text))

;(edit-select-all)
;(edit-delete)
;(edit-insert "one two three")
;(edit-select-all)
;(edit-split)
;(edit-select-all)
;(assert-eq "edit-split" "one\ntwo\nthree" (edit-get-text))

; --- More Selection & Navigation ---
;(edit-top)
;(edit-insert "Line 1\nLine 2")
;(edit-top)
;(edit-right 2)
;(edit-select-left 2)
;(assert-eq "edit-select-left" "Li" (edit-copy))

;(edit-top)
;(edit-select-right 2)
;(assert-eq "edit-select-right" "Li" (edit-copy))

;(edit-bottom)
;(edit-select-up 1)
;(assert-true "edit-select-up" (nempty? (edit-copy)))

;(edit-top)
;(edit-select-down 1)
;(assert-true "edit-select-down" (nempty? (edit-copy)))

;(edit-top)
;(edit-select-end)
;(assert-eq "edit-select-end" "Line 1" (edit-copy))

;(edit-select-home)
;(assert-eq "edit-select-home" "" (edit-copy)) ; collapsed to start

;(edit-bottom)
;(edit-select-top)
;(assert-true "edit-select-top" (nempty? (edit-copy)))

;(edit-top)
;(edit-select-bottom)
;(assert-true "edit-select-bottom" (nempty? (edit-copy)))

;(edit-top)
;(edit-insert "  word")
;(edit-top)
;(edit-ws-right)
;(assert-eq "edit-ws-right" 2 (edit-cx))
;(edit-ws-left)
;(assert-eq "edit-ws-left" 0 (edit-cx))

; --- Structural Selection ---
;(edit-select-all)
;(edit-delete)
;(edit-insert "(form (block))")
;(edit-top)
;(edit-right 7) ; Inside (block)
;(edit-select-block)
;(assert-eq "edit-select-block" "(block)" (edit-copy))

;(edit-top)
;(edit-right 1) ; on 'f' of form
;(edit-select-form)
;(assert-eq "edit-select-form" "form" (edit-copy))

;(edit-select-all)
;(edit-delete)
;(edit-insert "Line 1\nLine 2")
;(edit-top)
;(edit-select-line)
;(assert-eq "edit-select-line" "Line 1\n" (edit-copy))

;(edit-select-all)
;(edit-delete)
;(edit-insert "(A) (B) (C)")
;(edit-find "B")
;(edit-find-next)
;(edit-find-prev)
;(assert-eq "edit-find-prev" "B" (edit-copy))

;(edit-bottom)
;(. *edit* :set_cursor 8 0) ; Move to '(' of (C)
;(edit-bracket-right)
;(assert-eq "edit-bracket-right nav" 10 (edit-cx)) ; should be at ')' of (C)

;(edit-bracket-left)
;(assert-eq "edit-bracket-left nav" 8 (edit-cx)) ; should be back at '(' of (C)

; --- Focus Region ---
;(edit-select-all) (edit-delete)
;(edit-insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5")
; Set focus to lines 1-3 (Line 2, Line 3, Line 4)
;(. *edit* :set_cursor 0 1 0 4)
;(edit-set-focus)
; csr-floor might produce (0 4 0 1) or (0 1 0 4) but both represent lines 1-3.
; Let's extract and check lines to be sure.
;(bind '(fx1 fy1 fx2 fy2) (edit-get-focus))
;(assert-true "Focus region start" (or (= fy1 1) (= fy2 1)))
;(assert-true "Focus region end" (or (= fy1 4) (= fy2 4)))

; Find "Line" should wrap within focus (Line 2, 3, 4) for navigation methods
;(edit-top)
;(edit-find "Line")
;(edit-find-next) (assert-eq "Focus find 1" 1 (edit-cy))
;(edit-find-next) (assert-eq "Focus find 2" 2 (edit-cy))
;(edit-find-next) (assert-eq "Focus find 3" 3 (edit-cy))
;(edit-find-next) (assert-eq "Focus find 4 (wrap)" 1 (edit-cy))

; Test focus adjustment on insert
;(edit-top)
;(edit-insert "NEW\n") ; Insert at line 0
;(bind '(fx1 fy1 fx2 fy2) (edit-get-focus))
;(assert-true "Focus after insert start" (or (= fy1 2) (= fy2 2)))
;(assert-true "Focus after insert end" (or (= fy1 5) (= fy2 5)))

; Test focus adjustment on delete
;(edit-top)
;(edit-delete 4) ; Delete the "NEW\n"
;(bind '(fx1 fy1 fx2 fy2) (edit-get-focus))
;(assert-true "Focus after delete start" (or (= fy1 1) (= fy2 1)))
;(assert-true "Focus after delete end" (or (= fy1 4) (= fy2 4)))

; Test edit-focus (bridge find to cursors in focus)
;(edit-find "Line")
;(edit-focus)
;(assert-eq "edit-focus count" 3 (length (. *edit* :get_cursors)))
;(each (lambda (csr) (assert-true "Cursor in focus (edit-focus)" (csr-within (edit-get-focus) csr)))
;	(. *edit* :get_cursors))

; Test complex scenario: find, select paragraph, set focus, refined find, replace
;(edit-select-all) (edit-delete)
;(edit-insert "Para 1: text text\n\nPara 2: word word\n\nPara 3: text text")
;(edit-top)
;(edit-find "Para 2")
;(edit-find-next)
;(edit-select-paragraph)
;(edit-set-focus) ; Focus on Para 2
;(edit-find "word") ; Global find
;(edit-cursors) ; Sets cursors to ALL "word" matches (Para 2 has some)
;(edit-filter-cursors) ; Explicit filter to Para 2
;(assert-eq "Complex focus scenario count" 2 (length (. *edit* :get_cursors)))
;(edit-replace "WORD")
;(edit-select-all)
;(assert-eq "Complex focus scenario result" 
;	"Para 1: text text\n\nPara 2: WORD WORD\n\nPara 3: text text" 
;	(edit-get-text))

; Clear focus
;(edit-set-focus (nums 0 0 0 0 -1))
;(assert-eq "Clear focus" (nums 0 0 0 0 -1) (edit-get-focus))

; --- Smoke Test ---
(edit-print "Smoke test edit-print")

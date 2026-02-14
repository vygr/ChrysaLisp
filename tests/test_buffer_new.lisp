(import "lib/text/buffer.inc")

(report-header "Text Buffer: New Selection & Search Methods")

; --- White Space Selection ---
(defq b (Buffer))
(. b :insert "  ABC  ")
(. b :set_cursor 0 0)
(. b :right_white_space_select)
; Should select from (0,0) to (2,0)
(assert-eq "Right white space select" (nums 2 0 0 0) (first (. b :get_selected)))

(. b :set_cursor 7 0)
(. b :left_white_space_select)
; Should select from (7,0) back to (5,0)
(assert-eq "Left white space select" (nums 5 0 7 0) (first (. b :get_selected)))

; --- Bracket Selection ---
(defq b (Buffer +buffer_flag_syntax))
(. b :insert "(ABC)")
(. b :set_cursor 0 0)
(. b :right_bracket_select)
; Should select from (0,0) to (4,0)
(assert-eq "Right bracket select" (nums 4 0 0 0) (first (. b :get_selected)))

(. b :set_cursor 4 0)
(. b :left_bracket_select)
; Should select from (4,0) to (0,0)
(assert-eq "Left bracket select" (nums 0 0 4 0) (first (. b :get_selected)))

; --- Primary Cursor ---
(defq b (Buffer))
(. b :insert "A\nB\nC") ; initial cursor at (1,2)
(. b :add_cursor 0 0)
(. b :add_cursor 0 1)
(assert-eq "Three cursors before collapse" 3 (length (. b :get_cursors)))
(. b :primary_cursor)
(assert-eq "One cursor after collapse" 1 (length (. b :get_cursors)))
; Should be the one that was sorted last: (1,2)
(assert-eq "Primary cursor is last" (nums 1 2 1 2) (. b :get_cursor))

; Primary cursor on empty cursors should set to top
(def b :cursors '() :tcursors '())
(. b :primary_cursor)
(assert-eq "Primary cursor default" (nums 0 0 0 0) (. b :get_cursor))

; --- Search Navigation ---
(defq b (Buffer))
(. b :insert "ABC ABC ABC")
(. b :find "ABC" :nil :nil)

; find_next
(. b :set_cursor 0 0)
(. b :find_next)
; First match is 0..3. next = (nums 3 0 0 0 -1). cx=3, ax=0.
(assert-eq "Find next 1" (nums 3 0 0 0) (first (. b :get_selected)))
(. b :find_next)
; Second match is 4..7. next = (nums 7 0 4 0 -1). cx=7, ax=4.
(assert-eq "Find next 2" (nums 7 0 4 0) (first (. b :get_selected)))

; find_prev
(. b :find_prev)
; From (7,0), find_prev finds Match 1 (4..7) again, but at its START.
; next = (nums 4 0 7 0 -1). cx=4, ax=7.
(assert-eq "Find prev 1" (nums 4 0 7 0) (first (. b :get_selected)))
(. b :find_prev)
; From (4,0), find_prev finds Match 0 (0..3).
; next = (nums 0 0 3 0 -1). cx=0, ax=3.
(assert-eq "Find prev 2" (nums 0 0 3 0) (first (. b :get_selected)))

; find_add_next
(. b :set_cursor 0 0)
(. b :find_next) ; select first "ABC" (3,0)-(0,0)
(. b :find_add_next) ; add second "ABC" (7,0)-(4,0)
(assert-eq "Find add next count" 2 (length (. b :get_cursors)))
(assert-eq "Find add next 1" (nums 3 0 0 0) (first (. b :get_selected)))
(assert-eq "Find add next 2" (nums 7 0 4 0) (second (. b :get_selected)))

; Wrap around test
(. b :set_cursor 10 0)
(. b :find_next)
; Should wrap to first match
(assert-eq "Find next wrap" (nums 3 0 0 0) (first (. b :get_selected)))

(. b :set_cursor 0 0)
(. b :find_prev)
; Should wrap to last match (8,0) to (11,0).
; cx=8, ax=11.
(assert-eq "Find prev wrap" (nums 8 0 11 0) (first (. b :get_selected)))

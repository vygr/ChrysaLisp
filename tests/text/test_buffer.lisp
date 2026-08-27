(import "lib/text/buffer.inc")

(report-header "Text Buffer: Cursors & Mutation")

; Helper to create a buffer with some text
(defun create-test-buffer (text)
	(defq b (Buffer))
	(undoable b (. b :insert text))
	b)

; --- Basic Creation & Insertion ---
(defq b (Buffer))
(. b :insert "Hello World")
(assert-eq "Buffer insert" "Hello World\n" (elem-get (. b :get_buffer_lines) 0))

; --- Multiple Cursors ---
(defq b (Buffer))
(. b :insert "ABC")
; Add another cursor at (0,0) - now we have two cursors at (3,0) and (0,0)
(. b :add_cursor 0 0)
(assert-eq "Two cursors" 2 (length (. b :get_cursors)))

; Typing with two cursors
(. b :insert "!")
; Should result in "!ABC!"
(assert-eq "Multi-cursor insert" "!ABC!\n" (elem-get (. b :get_buffer_lines) 0))

; Check cursor positions after insert
; Each "!" insertion moves its cursor
(defq cs (. b :get_cursors_sorted))
(assert-eq "Cursor 1 pos" (nums 1 0 1 0 -1) (first cs))
(assert-eq "Cursor 2 pos" (nums 5 0 5 0 -1) (second cs))

; --- Selection & Deletion ---
(defq b (Buffer))
(. b :insert "0123456789")
(. b :set_cursor 2 0 5 0) ; select "234"
(. b :delete)
(assert-eq "Delete selection" "0156789\n" (elem-get (. b :get_buffer_lines) 0))

; --- Backspace ---
(defq b (Buffer))
(. b :insert "Hello")
(. b :backspace)
(assert-eq "Backspace" "Hell\n" (elem-get (. b :get_buffer_lines) 0))

; Let's test icopy / iinsert which are the core of save/load
(defq b (Buffer))
(. b :insert "Line 1\nLine 2")
(defq content (. b :icopy 0 0 6 1)) ; Copy everything
(assert-eq "icopy multi-line" "Line 1\nLine 2" content)

; --- Copy / Cut / Paste ---
(defq b (Buffer))
(. b :insert "Hello World")
(. b :set_cursor 0 0 5 0) ; Select "Hello"
(assert-eq "Copy" "Hello" (. b :copy))
(assert-eq "Buffer after copy" "Hello World\n" (elem-get (. b :get_buffer_lines) 0))

(. b :set_cursor 6 0 11 0) ; Select "World"
(assert-eq "Cut" "World" (. b :cut))
(assert-eq "Buffer after cut" "Hello \n" (elem-get (. b :get_buffer_lines) 0))

(. b :set_cursor 6 0) ; End of "Hello "
(. b :paste "Lisp")
(assert-eq "Paste" "Hello Lisp\n" (elem-get (. b :get_buffer_lines) 0))

; Multi-cursor Paste
(defq b (Buffer))
(. b :insert "A\nB")
(. b :set_cursor 1 0)
(. b :add_cursor 1 1)
(. b :paste "1\f2")
(assert-eq "Multi-cursor paste L1" "A1\n" (elem-get (. b :get_buffer_lines) 0))
(assert-eq "Multi-cursor paste L2" "B2\n" (elem-get (. b :get_buffer_lines) 1))

; --- Undo / Redo ---
; Need to set +buffer_flag_undo
(defq b (Buffer +buffer_flag_undo))
(. b :insert "A")
(. b :insert "B")
(assert-eq "Before undo" "AB\n" (elem-get (. b :get_buffer_lines) 0))
(. b :undo)
(assert-eq "After undo" "A\n" (elem-get (. b :get_buffer_lines) 0))
(. b :redo)
(assert-eq "After redo" "AB\n" (elem-get (. b :get_buffer_lines) 0))

; --- Cursor Merging ---
(defq b (Buffer))
(. b :set_cursor 0 0)
(. b :add_cursor 0 0) ; identical point
(assert-eq "Merged identical" 1 (length (. b :get_cursors)))

; --- Basic Navigation ---
(defq b (Buffer))
(. b :insert "ABC\nDEF")
; Result: ["ABC\n", "DEF\n", "\n"] (3 lines total, because Buffer adds final \n)
(. b :set_cursor 1 0)
(. b :right)
(assert-eq "Move right" (nums 2 0 2 0) (. b :get_cursor))
(. b :left)
(assert-eq "Move left" (nums 1 0 1 0) (. b :get_cursor))
(. b :down)
(assert-eq "Move down" (nums 1 1 1 1) (. b :get_cursor))
(. b :up)
(assert-eq "Move up" (nums 1 0 1 0) (. b :get_cursor))
(. b :end)
(assert-eq "Move end" (nums 3 0 3 0) (. b :get_cursor))
(. b :home)
(assert-eq "Move home" (nums 0 0 0 0) (. b :get_cursor))
(. b :bottom)
(assert-eq "Move bottom" (nums 0 2 0 2) (. b :get_cursor))
(. b :top)
(assert-eq "Move top" (nums 0 0 0 0) (. b :get_cursor))

; --- Selection Navigation ---
(defq b (Buffer))
(. b :insert "ABC")
(. b :set_cursor 0 0)
(. b :right_select)
(assert-eq "Right select" (nums 1 0 0 0) (. b :get_cursor))
(. b :left_select)
(assert-eq "Left select" (nums 0 0 0 0) (. b :get_cursor))
(. b :end_select)
(assert-eq "End select" (nums 3 0 0 0) (. b :get_cursor))
(. b :home_select)
(assert-eq "Home select" (nums 0 0 0 0) (. b :get_cursor))

; --- Utility Methods ---
(defq b (Buffer))
(. b :insert "ABC\nDEF")
(assert-eq "get_height" 2 (second (. b :get_size)))
(assert-eq "get_text_line" "ABC\n" (. b :get_text_line 0))
(assert-eq "clip_cursor" (nums 3 0 3 0 -1) (. b :clip_cursor 10 0))

; --- Getter/Setter Methods ---
(defq b (Buffer))
(. b :set_tab_width 8)
(assert-eq "get_tab_width" 8 (. b :get_tab_width))
(. b :set_wrap_width 120)
(assert-eq "get_wrap_width" 120 (. b :get_wrap_width))

; --- White Space Navigation ---
(defq b (Buffer))
(. b :insert "  ABC  ")
(. b :set_cursor 0 0)
(. b :right_white_space)
(assert-eq "Right white space" (nums 2 0 2 0) (. b :get_cursor))
(. b :set_cursor 7 0)
(. b :left_white_space)
(assert-eq "Left white space" (nums 5 0 5 0) (. b :get_cursor))

; --- Pattern Search & Found Cursors ---
(defq b (Buffer))
(. b :insert "Hello World\nHello Lisp")
(defq found (. b :find "Hello" :nil :nil :nil))
(assert-eq "Find results length" 3 (length found)) ; 2 matches + terminal newline empty list
(assert-eq "Find match line 0" 1 (length (first found)))
(assert-eq "Find match line 1" 1 (length (second found)))

(. b :set_found_cursors found)
(assert-eq "Found cursors count" 2 (length (. b :get_cursors)))
; Matches are "Hello" at (0,0) to (5,0) and (0,1) to (5,1)
; Cursors are stored as (ax ay cx cy sx), :set_found_cursors sets them as (x y x1 y 0)
; where x1 is start, x is end.
(assert-eq "Found cursor 1" (nums 5 0 0 0 0) (first (. b :get_cursors_sorted)))
(assert-eq "Found cursor 2" (nums 5 1 0 1 0) (second (. b :get_cursors_sorted)))

(. b :add_found_cursors found) ; Should merge since they are identical
(assert-eq "Add found cursors (merge)" 2 (length (. b :get_cursors)))

; --- Pattern Search & Found Cursors ---
(defq b (Buffer))
(. b :insert "Hello World\nHello Lisp")
(defq found (. b :find "Hello" :nil :nil :nil))
(assert-eq "Find results length" 3 (length found)) ; 2 matches + terminal newline empty list
(assert-eq "Find match line 0" 1 (length (first found)))
(assert-eq "Find match line 1" 1 (length (second found)))

(. b :set_found_cursors found)
(assert-eq "Found cursors count" 2 (length (. b :get_cursors)))
; Matches are "Hello" at (0,0) to (5,0) and (0,1) to (5,1)
; Cursors are stored as (ax ay cx cy sx), :set_found_cursors sets them as (x y x1 y 0)
; where x1 is start, x is end.
(assert-eq "Found cursor 1" (nums 5 0 0 0 0) (first (. b :get_cursors_sorted)))
(assert-eq "Found cursor 2" (nums 5 1 0 1 0) (second (. b :get_cursors_sorted)))

(. b :add_found_cursors found) ; Should merge since they are identical
(assert-eq "Add found cursors (merge)" 2 (length (. b :get_cursors)))

; --- Pattern Search Modes (wmode, xmode, imode) ---
(report-header "Buffer Find Modes: Case, Regex, Whole-Word")

; 1. Case-Insensitive Search (imode = :t)
(defq b_mode (Buffer))
(. b_mode :insert "Apple pie\nAPPLE tart\napple cider")
(defq f_case_sens (. b_mode :find "apple" :nil :nil :nil))
(assert-eq "Case-sensitive line 0 miss" 0 (length (elem-get f_case_sens 0)))
(assert-eq "Case-sensitive line 1 miss" 0 (length (elem-get f_case_sens 1)))
(assert-eq "Case-sensitive line 2 match" 1 (length (elem-get f_case_sens 2)))

(defq f_case_insens (. b_mode :find "apple" :nil :nil :t))
(assert-eq "Case-insensitive line 0 match" 1 (length (elem-get f_case_insens 0)))
(assert-eq "Case-insensitive line 1 match" 1 (length (elem-get f_case_insens 1)))
(assert-eq "Case-insensitive line 2 match" 1 (length (elem-get f_case_insens 2)))

; 2. Whole Word Search (wmode = :t)
(defq b_word (Buffer))
(. b_word :insert "cat concatenate cat. (cat) category")
(defq f_sub (. b_word :find "cat" :nil :nil :nil))
(assert-eq "Substring find matches all 5 occurrences" 5 (length (first f_sub)))

(defq f_word (. b_word :find "cat" :t :nil :nil))
; Matches: standalone "cat", "cat." (boundary at dot), and "(cat)" (boundary at parens)
; Excludes: "concatenate" and "category"
(assert-eq "Whole-word find matches 3 boundary occurrences" 3 (length (first f_word)))
(assert-list-eq "Whole-word match 1 bounds" '((0 3)) (elem-get (first f_word) 0))
(assert-list-eq "Whole-word match 2 bounds" '((16 19)) (elem-get (first f_word) 1))
(assert-list-eq "Whole-word match 3 bounds" '((22 25)) (elem-get (first f_word) 2))

; 3. Whole Word with Metacharacter Escaping (wmode = :t, xmode = :nil)
(defq b_esc (Buffer))
(. b_esc :insert "foo.bar fooXbar foo.bar/baz")
(defq f_esc (. b_esc :find "foo.bar" :t :nil :nil))
; Literal dot is escaped so it does not match "fooXbar"
(assert-eq "Whole-word with dot match count" 2 (length (first f_esc)))
(assert-list-eq "Whole-word literal dot match 1" '((0 7)) (elem-get (first f_esc) 0))
(assert-list-eq "Whole-word literal dot match 2" '((16 23)) (elem-get (first f_esc) 1))

; 4. Regular Expression Search (xmode = :t)
(defq b_rx (Buffer))
(. b_rx :insert "item_01: $100\nitem_02: $250\nsummary: none")
(defq f_rx (. b_rx :find "\\$(\\d+)" :nil :t :nil))
(assert-eq "Regex find line 0 match count" 1 (length (elem-get f_rx 0)))
(assert-eq "Regex find line 1 match count" 1 (length (elem-get f_rx 1)))
(assert-eq "Regex find line 2 miss" 0 (length (elem-get f_rx 2)))
; Verify full match bounds and capture group bounds: ((full_s full_e) (cap_s cap_e))
(assert-list-eq "Regex line 0 match with capture group" '((9 13) (10 13)) (first (elem-get f_rx 0)))
(assert-list-eq "Regex line 1 match with capture group" '((9 13) (10 13)) (first (elem-get f_rx 1)))

; 5. Whole Word + Case-Insensitive (wmode = :t, imode = :t)
(defq b_wi (Buffer))
(. b_wi :insert "The FOX jumps over the superfox and Fox.")
(defq f_wi (. b_wi :find "fox" :t :nil :t))
(assert-eq "Whole-word + ignore-case match count" 2 (length (first f_wi)))
(assert-list-eq "Whole-word + ignore-case match 1 (FOX)" '((4 7)) (elem-get (first f_wi) 0))
(assert-list-eq "Whole-word + ignore-case match 2 (Fox)" '((36 39)) (elem-get (first f_wi) 1))

; 6. Regex + Case-Insensitive (xmode = :t, imode = :t)
(defq b_xi (Buffer))
(. b_xi :insert "Status: ACTIVE\nstatus: pending\nSTATUS: Active")
(defq f_xi (. b_xi :find "status: a[a-z]+" :nil :t :t))
(assert-eq "Regex + ignore-case line 0 match" 1 (length (elem-get f_xi 0)))
(assert-eq "Regex + ignore-case line 1 miss" 0 (length (elem-get f_xi 1)))
(assert-eq "Regex + ignore-case line 2 match" 1 (length (elem-get f_xi 2)))

; 7. Regex + Whole-Word + Case-Insensitive (wmode = :t, xmode = :t, imode = :t)
(defq b_wxi (Buffer))
(. b_wxi :insert "run running RUN runner")
(defq f_wxi (. b_wxi :find "r[a-z]+" :t :t :t))
; !r[a-z]+! matches full words: "run", "running", "RUN", "runner"
(assert-eq "Regex + whole-word + ignore-case match count" 4 (length (first f_wxi)))

; 8. Cache Invalidation across mode changes on same Buffer instance
(defq b_cache (Buffer))
(. b_cache :insert "Test test TEST")
(defq f_c1 (. b_cache :find "test" :nil :nil :nil))
(assert-eq "Initial case-sensitive cache count" 1 (length (first f_c1)))
; Switching flags clears and recalculates cache
(defq f_c2 (. b_cache :find "test" :nil :nil :t))
(assert-eq "Switched to ignore-case cache count" 3 (length (first f_c2)))
(assert-list-eq "Last find state preserved" '("test" :nil :nil :t) (. b_cache :get_last_find))

; 9. Edge Cases: Empty pattern and no-match return :nil
(defq b_edge (Buffer))
(. b_edge :insert "Sample content")
(assert-eq "Empty pattern find returns nil" :nil (. b_edge :find "" :nil :nil :nil))
(assert-eq "Non-matching find returns nil" :nil (. b_edge :find "missing_term" :nil :nil :nil))
(assert-eq "Non-matching regex returns nil" :nil (. b_edge :find "\\d+" :nil :t :nil))

; --- Selection Info & Extent ---
(defq b (Buffer))
(. b :insert "ABC\nDEF")
(. b :set_cursor 0 1 2 1) ; "DE" at (0,1)-(2,1)
(. b :add_cursor 0 0 2 0) ; "AB" at (0,0)-(2,0)
(assert-eq "get_selected count" 2 (length (. b :get_cursors_sorted)))
; get_selected returns (cx cy ax ay) because sx is stripped by (most %0)
; sorted should be (0 0 2 0) then (0 1 2 1)
(assert-eq "get_selected (sorted)" (nums 0 0 2 0 -1) (first (. b :get_cursors_sorted)))
(assert-list-eq "get_selected_extent" (nums 0 0 0 2) (. b :get_cursors_extent))

; --- Floor Selection ---
(defq b (Buffer))
(. b :insert "ABC\nDEF")
(. b :set_cursor 1 0 2 0) ; point in middle of line
(. b :floor_selection)
(assert-list-eq "floor_selection" (nums 0 1 0 0 -1) (first (. b :get_cursors_sorted)))

; --- Bracket Matching ---
; Needs +buffer_flag_syntax
(defq b (Buffer +buffer_flag_syntax))
(. b :insert "(ABC)")
; We can't call build-syntax, but find_right_bracket calls it internally
(. b :set_cursor 0 0)
(bind '(x y) (. b :find_right_bracket 0 0))
(assert-list-eq "Find right bracket" (list 4 0) (list x y))
(bind '(x y) (. b :find_left_bracket 4 0))
(assert-list-eq "Find left bracket" (list 0 0) (list x y))

; --- Undo Management ---
(defq b (Buffer +buffer_flag_undo))
(assert-eq "next_mark" 0 (. b :next_mark))
(. b :clear_undo)
(assert-eq "undo after clear" b (. b :undo)) ; should do nothing

(defq b (Buffer +buffer_flag_undo))
(undoable b (. b :insert "X"))
(undoable b (. b :insert "Y"))
(. b :rewind)
(assert-eq "rewind" "\n" (elem-get (. b :get_buffer_lines) 0))

; --- Stream Save/Load ---
(defq b (Buffer))
(. b :insert "File Content")
(defq ms (memory-stream))
(. b :stream_save ms)
(stream-seek ms 0 0)
(defq b2 (Buffer))
(. b2 :stream_load ms)
(assert-eq "stream load content" "File Content\n" (elem-get (. b2 :get_buffer_lines) 0))

(stream-seek ms 0 0)
(defq b3 (Buffer))
(. b3 :stream_load_hex ms 8)
(assert-eq "stream load hex line 0" :t (nempty? (substr (elem-get (. b3 :get_buffer_lines) 0) "46 69 6C 65 20 43 6F 6E"))) ; "File Con"

; --- mstream smoke test ---
(defq ms (memory-stream))
(write-blk ms "Hello Memory")
(stream-seek ms 0 0)
(assert-eq "mstream read" "Hello Memory" (read-blk ms 12))
(import "lib/text/document.inc")

(report-header "Text Buffer: New Selection & Search Methods")

; --- White Space Selection ---
(defq b (Buffer))
(. b :insert "  ABC  ")
(. b :set_cursor 0 0)
(. b :right_white_space_select)
; Should select from (0,0) to (2,0)
(assert-eq "Right white space select" (nums 2 0 0 0 2) (first (. b :get_cursors_sorted)))

(. b :set_cursor 7 0)
(. b :left_white_space_select)
; Should select from (7,0) back to (5,0)
(assert-eq "Left white space select" (nums 5 0 7 0 5) (first (. b :get_cursors_sorted)))

; --- Bracket Selection ---
(defq b (Buffer +buffer_flag_syntax))
(. b :insert "(ABC)")
(. b :set_cursor 0 0)
(. b :right_bracket_select)
; Should select from (0,0) to (4,0)
(assert-eq "Right bracket select" (nums 4 0 0 0 4) (first (. b :get_cursors_sorted)))

(. b :set_cursor 4 0)
(. b :left_bracket_select)
; Should select from (4,0) to (0,0)
(assert-eq "Left bracket select" (nums 0 0 4 0 0) (first (. b :get_cursors_sorted)))

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
(. b :find "ABC" :nil :nil :nil)

; find_next
(. b :set_cursor 0 0)
(. b :find_next)
; First match is 0..3. next = (nums 3 0 0 0 -1). cx=3, ax=0.
(assert-eq "Find next 1" (nums 3 0 0 0 -1) (first (. b :get_cursors_sorted)))
(. b :find_next)
; Second match is 4..7. next = (nums 7 0 4 0 -1). cx=7, ax=4.
(assert-eq "Find next 2" (nums 7 0 4 0 -1) (first (. b :get_cursors_sorted)))

; find_prev
(. b :right)
(. b :find_prev)
; From (7,0), find_prev finds Match 1 (4..7) again, but at its START.
; next = (nums 4 0 7 0 -1). cx=4, ax=7.
(assert-eq "Find prev 1" (nums 4 0 7 0 -1) (first (. b :get_cursors_sorted)))
(. b :find_prev)
; From (4,0), find_prev finds Match 0 (0..3).
; next = (nums 0 0 3 0 -1). cx=0, ax=3.
(assert-eq "Find prev 2" (nums 0 0 3 0 -1) (first (. b :get_cursors_sorted)))

; find_add_next
(. b :set_cursor 0 0)
(. b :find_next) ; select first "ABC" (3,0)-(0,0)
(. b :find_add_next) ; add second "ABC" (7,0)-(4,0)
(assert-eq "Find add next count" 2 (length (. b :get_cursors)))
(assert-eq "Find add next 1" (nums 3 0 0 0 -1) (first (. b :get_cursors_sorted)))
(assert-eq "Find add next 2" (nums 7 0 4 0 -1) (second (. b :get_cursors_sorted)))

; --- Search & Mutation ---
(defq b (Document))
(. b :insert "a b a c a")
(. b :find "a" :nil :nil :nil)

; Select all "a"s
(. b :set_cursor 0 0)
(. b :find_next)     ; select first "a"
(. b :find_add_next) ; add second "a"
(. b :find_add_next) ; add third "a"
(assert-eq "Mutation: Three cursors" 3 (length (. b :get_cursors)))

; Copy selected text
(defq copied (. b :copy))
(assert-eq "Mutation: Copied text" "a\fa\fa" copied)

; Transform copied text: a -> X, Y, Z
(defq transformed "X\fY\fZ")

; Paste back
(. b :paste transformed)

; Verify buffer content
(assert-eq "Mutation: Buffer final content" "X b Y c Z\n" (elem-get (. b :get_buffer_lines) 0))

; Re-establish selection to verify copy works
(. b :select_word)

; Verify we can copy it again correctly
(assert-eq "Mutation: Copy again" "X\fY\fZ" (. b :copy))

; --- Start of File & End of File (sof / eof) ---
(defq b (Buffer))
(. b :insert "012\n456\n89") 
; Buffer state contains:
; Line 0: "012\n" (4 chars)
; Line 1: "456\n" (4 chars)
; Line 2: "89\n"  (3 chars)
; Line 3: "\n"    (1 char) - Terminal empty line added by last-line
; Total: 12 chars

; 1. At start of file (0, 0)
(assert-eq "sof at start" 0 (. b :sof (nums 0 0)))
(assert-eq "eof at start" 12 (. b :eof (nums 0 0)))

; 2. Inside line 0 (2, 0)
(assert-eq "sof inside L0" 2 (. b :sof (nums 2 0)))
(assert-eq "eof inside L0" 10 (. b :eof (nums 2 0)))

; 3. At start of line 1 (0, 1)
(assert-eq "sof start of L1" 4 (. b :sof (nums 0 1)))
(assert-eq "eof start of L1" 8 (. b :eof (nums 0 1)))

; 4. Inside line 1 (2, 1)
(assert-eq "sof inside L1" 6 (. b :sof (nums 2 1)))
(assert-eq "eof inside L1" 6 (. b :eof (nums 2 1)))

; 5. At start of terminal line (0, 3)
(assert-eq "sof start of terminal L3" 11 (. b :sof (nums 0 3)))
(assert-eq "eof start of terminal L3" 1 (. b :eof (nums 0 3)))

; 6. At absolute end of file (1, 3) - clips to (0, 3)
(assert-eq "sof at end of file" 12 (. b :sof (nums 1 3)))
(assert-eq "eof at end of file" 0 (. b :eof (nums 1 3)))
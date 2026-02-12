;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tests/test_buffer.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(defq cs (. b :get_selected))
(assert-eq "Cursor 1 pos" (nums 1 0 1 0) (first cs))
(assert-eq "Cursor 2 pos" (nums 5 0 5 0) (second cs))

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
(defq found (. b :find "Hello" :nil :nil))
(assert-eq "Find results length" 3 (length found)) ; 2 matches + terminal newline empty list
(assert-eq "Find match line 0" 1 (length (first found)))
(assert-eq "Find match line 1" 1 (length (second found)))

(. b :set_found_cursors found)
(assert-eq "Found cursors count" 2 (length (. b :get_cursors)))
; Matches are "Hello" at (0,0) to (5,0) and (0,1) to (5,1)
; Cursors are stored as (ax ay cx cy sx), :set_found_cursors sets them as (x y x1 y 0) 
; where x1 is start, x is end.
(assert-eq "Found cursor 1" (nums 5 0 0 0) (first (. b :get_selected)))
(assert-eq "Found cursor 2" (nums 5 1 0 1) (second (. b :get_selected)))

(. b :add_found_cursors found) ; Should merge since they are identical
(assert-eq "Add found cursors (merge)" 2 (length (. b :get_cursors)))

; --- Selection Info & Extent ---
(defq b (Buffer))
(. b :insert "ABC\nDEF")
(. b :set_cursor 0 1 2 1) ; "DE" at (0,1)-(2,1)
(. b :add_cursor 0 0 2 0) ; "AB" at (0,0)-(2,0)
(assert-eq "get_selected count" 2 (length (. b :get_selected)))
; get_selected returns (cx cy ax ay) because sx is stripped by (most %0)
; sorted should be (0 0 2 0) then (0 1 2 1)
(assert-eq "get_selected (sorted)" (nums 0 0 2 0) (first (. b :get_selected)))
(assert-list-eq "get_selected_extent" (nums 0 0 0 2) (. b :get_selected_extent))

; --- Floor Selection ---
(defq b (Buffer))
(. b :insert "ABC\nDEF")
(. b :set_cursor 1 0 2 0) ; point in middle of line
(. b :floor_selection)
(assert-list-eq "floor_selection" (nums 0 1 0 0) (first (. b :get_selected)))

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

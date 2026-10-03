(import "lib/text/buffer.inc")

(report-header "Buffer Edges: empty buffer, the ends, line joins, selections, undo, load and save")

;a buffer always ends with one empty line, the end of file line, so the
;text "ab" is held as "ab\n" and "\n"

(defun be-new (&optional text flags)
	; a buffer holding text, the cursor left at the end of it
	(defq b (Buffer flags))
	(if text (. b :insert text))
	b)

(defun be-text (b)
	(apply (const cat) (. b :get_buffer_lines)))

(defun be-csr (b)
	(slice (. b :get_cursor) 0 4))

(defun be-csrs (b)
	(map (# (slice %0 0 4)) (. b :get_cursors_sorted)))

(defun be-run (b ops)
	; do each op, a method name, or a list of a method and its arguments
	(each (# (if (list? %0) (apply . (cat (list b) %0)) (. b %0))) ops)
	(list (be-text b) (be-csr b)))

(defun be-at (text cx cy &rest ops)
	; a buffer of text, the cursor put at cx cy, the ops done, gives (text cursor)
	(defq b (be-new text))
	(. b :set_cursor cx cy)
	(be-run b ops))

(defun be-sel (text cx cy ax ay &rest ops)
	; as be-at, with a selection, from the anchor ax ay to the cursor cx cy
	(defq b (be-new text))
	(. b :set_cursor cx cy ax ay)
	(be-run b ops))

; --- an empty buffer ---
(test-cases
	(be-text (be-new)) "\n"
	(be-text (be-new "")) "\n"
	(. (be-new) :get_size) '(0 0)
	(be-csr (be-new)) '(0 0 0 0)
	(length (. (be-new) :get_cursors)) 1
	(. (be-new) :get_text_line 0) "\n"
	(. (be-new) :copy) "")

;nothing moves the cursor, or changes the text
(each (lambda (op)
		(assert-list-eq (cat "empty buffer " op) '("\n" (0 0 0 0)) (be-at "" 0 0 op)))
	'(:left :right :up :down :home :end :top :bottom :backspace :delete :cut))

; --- what text a buffer holds ---
(test-cases
	(be-text (be-new "a")) "a\n\n"
	;a final line end makes no difference
	(be-text (be-new "a\n")) "a\n\n"
	(be-text (be-new "a\n\n")) "a\n\n\n"
	(be-text (be-new "\n")) "\n\n"
	(be-csr (be-new "ab\ncd")) '(2 1 2 1)
	(be-csr (be-new "ab\ncd\n")) '(0 2 0 2)
	;a line is given with its line end
	(. (be-new "ab\ncd") :get_text_line 0) "ab\n"
	(. (be-new "ab\ncd") :get_text_line 1) "cd\n"
	(. (be-new "ab\ncd") :get_text_line 2) "\n")

; --- moving at the ends ---
(test-cases
	;at the start, left, up and backspace do nothing
	(be-at "ab\ncd" 0 0 :left) '("ab\ncd\n\n" (0 0 0 0))
	(be-at "ab\ncd" 0 0 :up) '("ab\ncd\n\n" (0 0 0 0))
	(be-at "ab\ncd" 0 0 :backspace) '("ab\ncd\n\n" (0 0 0 0))
	;left and right wrap from line to line
	(be-at "ab\ncd" 2 0 :right) '("ab\ncd\n\n" (0 1 0 1))
	(be-at "ab\ncd" 0 1 :left) '("ab\ncd\n\n" (2 0 2 0))
	;past the last text line is the end of file line
	(be-at "ab\ncd" 2 1 :right) '("ab\ncd\n\n" (0 2 0 2))
	(be-at "ab\ncd" 2 1 :down) '("ab\ncd\n\n" (0 2 0 2))
	(be-at "ab\ncd" 1 0 :bottom) '("ab\ncd\n\n" (0 2 0 2))
	(be-at "ab\ncd" 1 1 :top) '("ab\ncd\n\n" (0 0 0 0))
	(be-at "ab\ncd" 1 0 :home) '("ab\ncd\n\n" (0 0 0 0))
	(be-at "ab\ncd" 1 0 :end) '("ab\ncd\n\n" (2 0 2 0))
	;up and down onto a shorter line clip to its end
	(be-at "abc\nd" 3 0 :down) '("abc\nd\n\n" (1 1 1 1))
	(be-at "a\nbcd" 3 1 :up) '("a\nbcd\n\n" (1 0 1 0)))

; --- deleting at a line end joins the lines, but the last line end stays ---
(test-cases
	(be-at "ab\ncd" 0 1 :backspace) '("abcd\n\n" (2 0 2 0))
	(be-at "ab\ncd" 2 0 :delete) '("abcd\n\n" (2 0 2 0))
	(be-at "ab\ncd" 2 1 :delete) '("ab\ncd\n\n" (2 1 2 1)))

; --- inserting ---
(test-cases
	(be-at "ab" 1 0 '(:insert "")) '("ab\n\n" (1 0 1 0))
	(be-at "ab" 1 0 '(:insert "X")) '("aXb\n\n" (2 0 2 0))
	(be-at "ab" 0 0 '(:insert "c")) '("cab\n\n" (1 0 1 0))
	(be-at "ab" 2 0 '(:insert "c")) '("abc\n\n" (3 0 3 0))
	;a line end splits the line
	(be-at "ab" 1 0 '(:insert "\n")) '("a\nb\n\n" (0 1 0 1))
	(be-at "ab" 1 0 '(:insert "\n\n")) '("a\n\nb\n\n" (0 2 0 2))
	(be-at "ab" 1 0 '(:insert "X\nY")) '("aX\nYb\n\n" (1 1 1 1)))

; --- selections, the anchor can be either side of the cursor ---
(test-cases
	(be-sel "abcd" 1 0 3 0 :delete) '("ad\n\n" (1 0 1 0))
	(be-sel "abcd" 3 0 1 0 :delete) '("ad\n\n" (1 0 1 0))
	(be-sel "abcd" 1 0 3 0 :backspace) '("ad\n\n" (1 0 1 0))
	;typing replaces the selection
	(be-sel "abcd" 1 0 3 0 '(:insert "X")) '("aXd\n\n" (2 0 2 0))
	(be-sel "abcd" 3 0 1 0 '(:insert "X")) '("aXd\n\n" (2 0 2 0))
	(be-sel "ab\ncd" 1 0 1 1 '(:insert "X")) '("aXd\n\n" (2 0 2 0))
	;over several lines
	(be-sel "ab\ncd\nef" 1 0 1 2 :delete) '("af\n\n" (1 0 1 0))
	(be-sel "ab\ncd\nef" 1 2 1 0 :delete) '("af\n\n" (1 0 1 0))
	;all the text, which leaves one blank line, and all the lines, which leaves none
	(be-sel "ab\ncd\nef" 0 0 2 2 :delete) '("\n\n" (0 0 0 0))
	(be-sel "ab\ncd\nef" 0 0 0 3 :delete) '("\n" (0 0 0 0))
	;left and right drop the selection, at its start and its end
	(be-sel "abcd" 1 0 3 0 :left) '("abcd\n\n" (1 0 1 0))
	(be-sel "abcd" 3 0 1 0 :left) '("abcd\n\n" (1 0 1 0))
	(be-sel "abcd" 1 0 3 0 :right) '("abcd\n\n" (3 0 3 0))
	(be-sel "abcd" 3 0 1 0 :right) '("abcd\n\n" (3 0 3 0)))

; --- copy ---
(defun be-copy (text cx cy ax ay)
	(defq b (be-new text))
	(. b :set_cursor cx cy ax ay)
	(. b :copy))

(test-cases
	(be-copy "abcd" 1 0 3 0) "bc"
	(be-copy "abcd" 3 0 1 0) "bc"
	(be-copy "ab\ncd" 1 0 1 1) "b\nc"
	(be-copy "ab\ncd" 0 0 0 2) "ab\ncd\n"
	(be-copy "abcd" 2 0 2 0) ""
	(. (be-new "ab\ncd") :icopy 1 0 1 1) "b\nc"
	(. (be-new "ab\ncd") :icopy 1 1 1 0) "b\nc"
	(. (be-new "ab\ncd") :icopy 0 0 0 0) "")

; --- a cursor is clipped to the text ---
(test-cases
	(slice (. (be-new "ab\ncd") :clip_cursor 99 99) 0 4) '(0 2 0 2)
	(slice (. (be-new "ab\ncd") :clip_cursor -5 -5) 0 4) '(0 0 0 0)
	(slice (. (be-new "ab\ncd") :clip_cursor 99 0) 0 4) '(2 0 2 0)
	(slice (. (be-new "ab\ncd") :clip_cursor 1 1 99 99) 0 4) '(1 1 0 2)
	(be-at "ab\ncd" 99 99) '("ab\ncd\n\n" (0 2 0 2))
	(be-at "ab\ncd" -1 -1) '("ab\ncd\n\n" (0 0 0 0)))

; --- several cursors ---
(defun be-multi (text csrs &rest ops)
	; a buffer of text with a cursor for each of csrs, the ops done, gives (text cursors)
	(defq b (be-new text))
	(apply . (cat (list b :set_cursor) (first csrs)))
	(each (# (apply . (cat (list b :add_cursor) %0))) (rest csrs))
	(each (# (if (list? %0) (apply . (cat (list b) %0)) (. b %0))) ops)
	(list (be-text b) (be-csrs b)))

(test-cases
	;a cursor added where one already is, is not added
	(be-multi "abc" '((1 0) (1 0))) '("abc\n\n" ((1 0 1 0)))
	(be-multi "abc" '((1 0) (2 0) (1 0))) '("abc\n\n" ((1 0 1 0) (2 0 2 0)))
	;selections that overlap become one, ones that only touch stay two
	(be-multi "abcdef" '((0 0 3 0) (2 0 5 0))) '("abcdef\n\n" ((0 0 5 0)))
	(be-multi "abcdef" '((0 0 2 0) (2 0 4 0))) '("abcdef\n\n" ((0 0 2 0) (2 0 4 0)))
	;cursors that move to the same place become one
	(be-multi "abcdef" '((0 0 2 0) (3 0 5 0)) :home) '("abcdef\n\n" ((0 0 0 0)))
	;edits on different lines
	(be-multi "ab\ncd" '((1 0) (1 1)) '(:insert "X")) '("aXb\ncXd\n\n" ((2 0 2 0) (2 1 2 1)))
	(be-multi "ab\ncd" '((1 0) (1 1)) '(:insert "\n")) '("a\nb\nc\nd\n\n" ((0 1 0 1) (0 3 0 3)))
	(be-multi "ab\ncd" '((1 0) (1 1)) :backspace) '("b\nd\n\n" ((0 0 0 0) (0 1 0 1)))
	;a join moves the cursor on the joined line
	(be-multi "ab\ncd" '((0 0) (0 1)) :backspace) '("abcd\n\n" ((0 0 0 0) (2 0 2 0)))
	(be-multi "ab\ncd" '((2 0) (2 1)) :delete) '("abcd\n\n" ((2 0 2 0) (4 0 4 0)))
	;back to just the last cursor added
	(be-multi "abc" '((1 0) (2 0)) :primary_cursor) '("abc\n\n" ((2 0 2 0))))

; --- undo and redo, on a buffer made with +buffer_flag_undo ---
(defun be-undo (text &rest ops)
	(defq b (be-new text +buffer_flag_undo))
	(be-run b ops))

(test-cases
	(be-undo "ab" '(:insert "c") :undo) '("ab\n\n" (2 0 2 0))
	(be-undo "ab" '(:insert "c") :undo :undo :redo) '("ab\n\n" (2 0 2 0))
	(be-undo "ab" '(:insert "c") :undo :undo :redo :redo) '("abc\n\n" (3 0 3 0))
	;more undo or redo than there is does nothing more
	(first (be-undo "ab" :undo :redo :redo)) "ab\n\n"
	(first (be-undo "ab" :redo)) "ab\n\n"
	(first (be-undo "" :undo :redo)) "\n"
	;a new edit after an undo drops what could have been redone
	(first (be-undo "ab" '(:insert "c") :undo '(:insert "d") :redo)) "abd\n\n"
	(first (be-undo "ab" :backspace :undo)) "ab\n\n"
	(first (be-undo "ab\ncd" :backspace :backspace :backspace :undo :undo :undo)) "ab\ncd\n\n"
	;the cursor and its selection come back too
	(be-undo "ab\ncd" '(:set_cursor 0 1) :backspace :undo) '("ab\ncd\n\n" (0 1 0 1))
	(be-undo "abc" '(:set_cursor 0 0 3 0) :cut :undo) '("abc\n\n" (0 0 3 0))
	(be-undo "abc" '(:set_cursor 0 0 3 0) '(:paste "XY") :undo) '("abc\n\n" (0 0 3 0))
	(first (be-undo "ab\ncd" '(:set_cursor 0 0 0 2) :delete :undo)) "ab\ncd\n\n"
	;rewind undoes everything, and it can all be redone
	(first (be-undo "ab" '(:insert "cd") :rewind :redo :redo)) "abcd\n\n"
	;after clear_undo there is nothing to undo
	(first (be-undo "ab" :clear_undo :undo)) "ab\n\n")

;several edits in one undoable are one step
(defq be_b (be-new "ab" +buffer_flag_undo))
(undoable be_b (. be_b :insert "cd") (. be_b :insert "ef"))
(assert-eq "undoable group, done" "abcdef\n\n" (be-text be_b))
(. be_b :undo)
(assert-eq "undoable group, undone as one" "ab\n\n" (be-text be_b))

;an edit at several cursors is one step, and the cursors come back
(defq be_b (be-new "a b a" +buffer_flag_undo))
(. be_b :set_cursor 0 0 1 0)
(. be_b :add_cursor 4 0 5 0)
(. be_b :insert "X")
(assert-eq "multi cursor edit" "X b X\n\n" (be-text be_b))
(. be_b :undo)
(assert-eq "multi cursor undo text" "a b a\n\n" (be-text be_b))
(assert-list-eq "multi cursor undo cursors" '((0 0 1 0) (4 0 5 0)) (be-csrs be_b))
(. be_b :redo)
(assert-eq "multi cursor redo" "X b X\n\n" (be-text be_b))

;a buffer without the undo flag keeps no history
(defq be_b (be-new "ab"))
(. be_b :insert "c")
(. be_b :undo)
(assert-eq "no undo flag, undo does nothing" "abc\n\n" (be-text be_b))

; --- load and save, through a stream ---
(defun be-load (text)
	(defq b (Buffer))
	(. b :stream_load (string-stream text))
	b)

(defun be-save (b)
	(defq s (memory-stream))
	(. b :stream_save s)
	(stream-seek s 0 0)
	(ifn (read-blk s 10000) ""))

(test-cases
	(be-text (be-load "")) "\n"
	(be-text (be-load "a")) "a\n\n"
	(be-text (be-load "a\n")) "a\n\n"
	(be-text (be-load "a\nb")) "a\nb\n\n"
	(be-text (be-load "a\n\nb\n")) "a\n\nb\n\n"
	;a saved file ends with one line end, whether what was loaded did or not
	(be-save (be-load "")) ""
	(be-save (be-load "a")) "a\n"
	(be-save (be-load "a\n")) "a\n"
	(be-save (be-load "a\nb")) "a\nb\n"
	(be-save (be-load "a\n\nb\n")) "a\n\nb\n"
	(be-save (be-new "typed")) "typed\n"
	(be-save (be-new)) ""
	;carriage returns are dropped on load
	(be-text (be-load "a\r\nb\r\n")) "a\nb\n\n"
	(second (. (be-load "abc\nde") :get_size)) 2)

; --- find ---
(defun be-found (text pattern)
	; the spans found on each line
	(defq b (be-load text))
	(. b :find pattern :nil :nil :nil)
	(map (# (if %0 (map (const first) %0) '())) (. b :get_buffer_found)))

(defun be-next (text pattern cx &rest ops)
	(defq b (be-load text))
	(. b :find pattern :nil :nil :nil)
	(. b :set_cursor cx 0)
	(each (# (. b %0)) ops)
	(be-csr b))

(test-cases
	(be-found "one two\nthree" "t") '(((4 5)) ((0 1)) ())
	(be-found "one two\nthree" "zz") '(() () ())
	(be-found "" "a") '(())
	;find_next selects the next match, and stays on the last one
	(be-next "aXa" "a" 0 :find_next) '(1 0 0 0)
	(be-next "aXa" "a" 0 :find_next :find_next) '(3 0 2 0)
	(be-next "aXa" "a" 0 :find_next :find_next :find_next) '(3 0 2 0)
	;with nothing before, or nothing found, the cursor stays
	(be-next "aXa" "a" 0 :find_prev) '(0 0 0 0)
	(be-next "aXa" "zz" 1 :find_next) '(1 0 1 0))

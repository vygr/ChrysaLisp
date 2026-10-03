(import "lib/text/document.inc")
(import "lib/text/edit.inc")

(report-header "Edit Edges: find and replace with whole words, regexp, no matches, empty text")

(defq *file* "test.txt" *edit* (Document +buffer_flag_syntax))

(defun ee-replace (text rep pattern &rest flags)
	; put text in the document, find pattern, put a cursor on each match,
	; replace them, and give the text that results
	(edit-select-all) (edit-delete)
	(edit-set-focus +invalid_focus)
	(edit-insert text)
	(edit-top)
	(apply edit-find (cat (list pattern) flags))
	(edit-cursors)
	(edit-replace rep)
	(edit-select-all)
	(edit-get-text))

(defun ee-count (text pattern &rest flags)
	; how many matches a find gives cursors for. There is always one cursor,
	; so count the cursors that have something selected.
	(edit-select-all) (edit-delete)
	(edit-set-focus +invalid_focus)
	(edit-insert text)
	(edit-top)
	(apply edit-find (cat (list pattern) flags))
	(edit-cursors)
	(length (filter (lambda ((cx cy ax ay &ignore)) (or (/= cx ax) (/= cy ay)))
		(. *edit* :get_cursors))))

; --- plain find and replace ---
(test-cases
	(ee-replace "cat cats concat cat" "X" "cat") "X Xs conX X"
	(ee-replace "a.b axb" "X" "a.b") "X axb"
	(ee-replace "one\ntwo\none" "1" "one") "1\ntwo\n1"
	;replace with nothing deletes
	(ee-replace "a-b-c" "" "-") "abc"
	;the replacement can hold the pattern
	(ee-replace "a b" "aa" "a") "aa b")

; --- whole words ---
(test-cases
	(ee-replace "cat cats concat cat" "X" "cat" :w) "X cats concat X"
	(ee-replace "cat\ncats\ncat" "X" "cat" :w) "X\ncats\nX"
	(ee-replace "a.b axb" "X" "a.b" :w) "X axb"
	(ee-count "cat cats concat cat" "cat" :w) 2
	(ee-count "cat cats concat cat" "cat") 4)

; --- regexp, and regexp with whole words ---
(test-cases
	(ee-replace "cat cats dog dogs" "X" "cat|dog" :x) "X Xs X Xs"
	;the word breaks apply to every alternative
	(ee-replace "cat cats dog dogs" "X" "cat|dog" :x :w) "X cats X dogs"
	(ee-count "cat cats dog dogs" "cat|dog" :x :w) 2
	;capture groups, with and without whole words
	(ee-replace "tom@home ann@work" "$2:$1" "(\\w+)@(\\w+)" :x) "home:tom work:ann"
	(ee-replace "tom@home ann@work" "$2:$1" "(\\w+)@(\\w+)" :x :w) "home:tom work:ann"
	(ee-replace "a1 b22 c333" "<$0>" "\\d+" :x) "a<1> b<22> c<333>")

; --- ignore case ---
(test-cases
	(ee-count "Line line LINE" "line") 1
	(ee-count "Line line LINE" "line" :i) 3
	(ee-count "Line line LINE" "LINE" :i) 3
	(ee-count "Line liner LINE" "line" :i :w) 2)

	;ignore case replace, the matches are found in the text as it is
(test-cases
	(ee-replace "Line line LINE" "x" "line" :i) "x x x"
	(ee-replace "Line liner LINE" "x" "line" :i :w) "x liner x")

; --- nothing found leaves the text alone ---
(test-cases
	(ee-count "some text" "zzz") 0
	(ee-replace "some text" "X" "zzz") "some text"
	(ee-replace "some text" "X" "zzz" :w) "some text"
	(ee-replace "some text" "X" "z+" :x) "some text"
	;an empty pattern finds nothing
	(ee-count "some text" "") 0
	(ee-replace "some text" "X" "") "some text")

; --- an empty document ---
(test-cases
	(ee-count "" "a") 0
	(ee-replace "" "X" "a") ""
	(ee-replace "" "X" "a" :w) ""
	(ee-replace "" "X" "a+" :x) "")

; --- replacing in a selection that has no match in it ---
(edit-select-all) (edit-delete)
(edit-set-focus +invalid_focus)
(edit-insert "alpha beta")
(edit-top)
(edit-find "gamma")
(edit-select-all)
(edit-replace "X")
(edit-select-all)
(assert-eq "replace over a selection with no match" "alpha beta" (edit-get-text))

;the text functions the editor's replace is built on, with nothing to do
(test-cases
	(replace-str "" "a" "b") ""
	(replace-regex "" "a" "b") ""
	(replace-str "abc" "z" "--") "abc"
	(replace-str "abc" "" "x") "abc"
	(replace-matches "" (list) "x") ""
	(replace-matches "abc" (list) "x") "abc"
	(replace-edits "" (list) "x") '()
	(replace-str-edits "" "a" "b") '()
	(found? "abc" "") :nil
	(found? "" "") :nil
	(substr "abc" "") '()
	(substr "" "a") '())

; --- several cursors, one on every match of a find ---
(defun ee-multi (text pattern action &rest flags)
	; put text in the document, put a cursor on each match of pattern, run
	; the action, and give all the text, exactly as it is, less the line
	; end a document always finishes with
	(edit-select-all) (edit-delete)
	(edit-set-focus +invalid_focus)
	(edit-insert text)
	(edit-top)
	(apply edit-find (cat (list pattern) flags))
	(edit-cursors)
	(action)
	(edit-select-all)
	(if (ends-with "\n" (defq all (. *edit* :copy))) (most all) all))

;typing, deleting and case changes happen at every cursor
(test-cases
	(ee-multi "a b a" "a" (# (edit-insert "X"))) "X b X"
	(ee-multi "a b a" "a" (# (edit-insert "XYZ"))) "XYZ b XYZ"
	;typing replaces the selection, even with nothing
	(ee-multi "a b a" "a" (# (edit-insert ""))) " b "
	(ee-multi "a b a" "a" (# (edit-delete))) " b "
	(ee-multi "a b a" "a" (# (edit-backspace))) " b "
	(ee-multi "ab cd ab" "ab" (# (edit-upper))) "AB cd AB"
	(ee-multi "AB cd AB" "AB" (# (edit-lower))) "ab cd ab"
	(ee-multi "a b a" "a" (# (edit-cut))) " b "
	;matches that touch each other
	(ee-multi "aaa" "a" (# (edit-insert "bb"))) "bbbbbb"
	(ee-multi "aaa" "a" (# (edit-delete))) ""
	;matches on several lines
	(ee-multi "x\nx\nx" "x" (# (edit-insert "yy"))) "yy\nyy\nyy"
	(ee-multi "x1\nx2\nx3" "x" (# (edit-delete))) "1\n2\n3"
	;text with a line break in it, at every cursor
	(ee-multi "a,a" "a" (# (edit-insert "1\n2"))) "1\n2,1\n2"
	(ee-multi "a,a" "," (# (edit-break))) "a\na")

;copy gives each selection, split by a form feed, and the list form keeps empty ones
(defq ee_copy "" ee_parts (list))
(test-cases
	(ee-multi "a b a" "a" (# (setq ee_copy (. *edit* :copy)))) "a b a"
	ee_copy "a\fa"
	(ee-multi "ab b cd" "\\w+" (# (setq ee_parts (. *edit* :copy_parts))) :x) "ab b cd"
	ee_parts '("ab" "b" "cd"))

;paste, one part for each cursor
(test-cases
	(ee-multi "a b a" "a" (# (edit-paste "1\f2"))) "1 b 2"
	;when the parts do not match the cursors, each cursor gets them all, as lines
	(ee-multi "a b a" "a" (# (edit-paste "same"))) "same\n b same\n"
	;paste_parts takes a list, so a part can be empty, which deletes
	(ee-multi "a b a" "a" (# (. *edit* :paste_parts (list "" "Z")))) " b Z"
	(ee-multi "a b a" "a" (# (. *edit* :paste_parts (list "" "")))) " b "
	(ee-multi "a b a" "a" (# (. *edit* :paste_parts (list "longer" "x")))) "longer b x")

;moving the cursors, then typing
(test-cases
	(ee-multi "a\nb\nc" "\\w" (# (edit-home) (edit-insert ">")) :x) ">a\n>b\n>c"
	(ee-multi "a\nb\nc" "\\w" (# (edit-end) (edit-insert ";")) :x) "a;\nb;\nc;"
	(ee-multi "ab\ncd" "\\w\\w" (# (edit-left) (edit-insert "<")) :x) "<ab\n<cd"
	(ee-multi "ab\ncd" "\\w\\w" (# (edit-right) (edit-insert ">")) :x) "ab>\ncd>")

;cursors that land on the same place become one
(test-cases
	(ee-multi "a a" "a" (# (edit-home) (edit-insert ">"))) ">a a"
	(ee-multi "a a" "a" (# (edit-end) (edit-insert "<"))) "a a<"
	(ee-multi "a\na" "a" (# (edit-top) (edit-insert "^"))) "^a\na")

;back to one cursor
(ee-multi "a b a" "a" (# (edit-primary)))
(assert-eq "primary leaves one cursor" 1 (length (. *edit* :get_cursors)))

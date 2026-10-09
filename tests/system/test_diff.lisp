(report-header "Streams: diff and patch, a patch of a diff gives the other file")
(import "lib/streams/diff.inc")

(defun df-text (lines) (apply (const cat) (cat (list "") (map (# (cat %0 (ascii-char 10))) lines))))
(defun df-diff (a b)
	;the diff of two texts, as text
	(defq out (string-stream (cat "")))
	(stream-diff (string-stream a) (string-stream b) out)
	(str out))
(defun df-patch (a d)
	;a text with a diff put to it
	(defq out (string-stream (cat "")))
	(stream-patch (string-stream a) (string-stream d) out)
	(str out))

;a diff, put to the first of the two, is the second. There was no test of
;this, and (stream-patch) had not worked for some time
(defq df_base (map (# (cat "line " (str %0))) (range 0 60)))
(each (lambda ((name a b))
	(setq a (df-text a) b (df-text b))
	(assert-true (cat name ", there and back") (eql b (df-patch a (df-diff a b))))
	(assert-true (cat name ", and the other way") (eql a (df-patch b (df-diff b a)))))
	(list
		(list "a line changed" '("a" "b" "c" "d" "e") '("a" "x" "c" "d" "e"))
		(list "a line gone, a line more" '("a" "b" "c" "d" "e") '("a" "x" "c" "e" "f"))
		(list "the first line" '("a" "b" "c") '("z" "b" "c"))
		(list "the last line" '("a" "b" "c") '("a" "b" "z"))
		(list "lines at the start" '("c" "d") '("a" "b" "c" "d"))
		(list "lines at the end" '("a" "b") '("a" "b" "c" "d"))
		(list "nothing the same" '("a" "b" "c") '("x" "y"))
		(list "every line the same line" '("x" "x" "x" "x") '("x" "x" "y" "x" "x" "x"))
		(list "one line each" '("a") '("b"))
		(list "a block moved" df_base (cat (slice df_base 20 40) (slice df_base 0 20) (slice df_base 40 60)))
		(list "every third line gone" df_base (filter (lambda (&) (/= 0 (% (!) 3))) df_base))
		(list "a block put in" df_base (cat (slice df_base 0 30) '("new 1" "new 2" "new 3") (slice df_base 30 60)))))

(assert-eq "two that are the same have no diff" "" (df-diff (df-text df_base) (df-text df_base)))
(assert-eq "and a patch of nothing changes nothing" (df-text df_base) (df-patch (df-text df_base) ""))
(assert-true "from nothing" (eql (df-text '("a" "b")) (df-patch "" (df-diff "" (df-text '("a" "b"))))))
(assert-true "to nothing" (eql "" (df-patch (df-text '("a" "b")) (df-diff (df-text '("a" "b")) ""))))

;a block of changes is written as one, a run deleted, a run added, or the
;one changed for the other, as diff(1) writes them
(assert-eq "a change, a delete and an add, each as diff(1) has it"
	(df-text '("2c2" "< b" "---" "> x" "4d3" "< d" "5a5" "> f"))
	(df-diff (df-text '("a" "b" "c" "d" "e")) (df-text '("a" "x" "c" "e" "f"))))
(assert-eq "a run of lines deleted is one entry"
	(df-text '("2,4d1" "< b" "< c" "< d"))
	(df-diff (df-text '("a" "b" "c" "d" "e")) (df-text '("a" "e"))))
(assert-eq "a run of lines added is one entry"
	(df-text '("1a2,4" "> b" "> c" "> d"))
	(df-diff (df-text '("a" "e")) (df-text '("a" "b" "c" "d" "e"))))
(assert-eq "a run changed for a run of another length"
	(df-text '("2,3c2,5" "< b" "< c" "---" "> w" "> x" "> y" "> z"))
	(df-diff (df-text '("a" "b" "c" "d")) (df-text '("a" "w" "x" "y" "z" "d"))))
(assert-eq "from nothing, the one entry" (df-text '("0a1,2" "> one" "> two")) (df-diff "" (df-text '("one" "two"))))
(assert-eq "to nothing, the one entry" (df-text '("1,2d0" "< one" "< two")) (df-diff (df-text '("one" "two")) ""))

;a diff as other tools write one, with ranges and changes
(defq df_other (df-text '("2,3c2" "< b" "< c" "---" "> x" "5a5,6" "> f" "> g")))
(assert-true "a diff with ranges and a change, as diff(1) writes them"
	(eql (df-text '("a" "x" "d" "e" "f" "g")) (df-patch (df-text '("a" "b" "c" "d" "e")) df_other)))
(assert-true "a single line deleted, as diff(1) writes it"
	(eql (df-text '("a" "c")) (df-patch (df-text '("a" "b" "c")) (df-text '("2d1" "< b")))))

(undef (env) 'df_base 'df_other)

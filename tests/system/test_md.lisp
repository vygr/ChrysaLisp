(report-header "Md: a table keeps to the page, a long path is cut to fit its cell, and a table too wide is scrolled")

(import "gui/lisp.inc")

(defun md-make (lines)
	;a page of that markdown, 640 wide, and how wide it then wants to be
	(def (defq md (Md)) :page_width 640)
	(. md :populate_lines lines)
	(list md (first (. md :pref_size))))

(defun md-texts (view)
	;the text of every Text of a view, in order, those of no text left out
	(filter (const nempty?) (map (# (if (def? :text %0) (str (get :text %0)) ""))
		(filter (# (Text? %0)) (. view :flatten)))))

;the table of docs/history/press/README.md is where this was seen, a link
;to a file with a long name made the Docs app page many times its width
(defq md_path "1990-04_personal_computer_world.pdf"
	md_lines (list
		"| Date | Publication | Article | Author | File |"
		"|------|-------------|---------|--------|------|"
		(cat "| Apr 1990 | Personal Computer World | Newsprint | not credited | [pdf](" md_path ") |")
		"| Jan 1991 | Parallelogram | Tao Systems advertisement | | [pdf](a.pdf) |"))
(bind '(md_md md_w) (md-make md_lines))
(assert-true "a table with a long path in a cell is no wider than the page" (<= md_w 660))
(defq md_all (md-texts md_md) md_joined (apply (const cat) (map (const trim) md_all)))
(assert-true "the path is all there, in its parts" (nempty? (substr md_joined (cat "[pdf](" md_path ")"))))
(assert-true "and is more than one part" (notany (# (nempty? (substr %0 md_path))) md_all))
(assert-true "it is cut after a _ - . or /" (some (# (ends-with "_" (trim %0))) md_all))

;a cell with nothing in it is a cell
(defq md_rows (filter (# (Grid? %0)) (. md_md :flatten)) md_last (. (last md_rows) :children))
(assert-eq "a row has its five cells" 5 (length md_last))
(assert-list-eq "what follows an empty cell stays in its own column" '("[pdf](a.pdf) ")
	(md-texts (last md_last)))
(assert-list-eq "and the empty one is empty" '() (md-texts (elem-get md_last 3)))

;a word with nowhere to cut it is cut where it must be
(bind '(md_md md_w) (md-make (list "| a | b | c | d |" "|---|---|---|---|" (cat "| " (pad "" 90 "W") " | b | c | d |"))))
(assert-true "a long word with no place to cut is still kept to the page" (<= md_w 660))

;text that is not a table is as it was
(bind '(md_md md_w) (md-make (list "some words of a paragraph, lib/text/format.inc and all")))
(assert-list-eq "a paragraph is its words" '("some " "words " "of " "a " "paragraph, " "lib/text/format.inc " "and " "all ")
	(md-texts md_md))

;so many columns that they can not all be on the page: a scroll
(defq md_cols (map (# (cat "col" (str %0))) (range 0 16)))
(bind '(md_md md_w) (md-make (list (cat "| " (join md_cols " | ") " |")
	(cat "|" (join (map (lambda (&) "---") md_cols) "|") "|") (cat "| " (join md_cols " | ") " |"))))
(assert-true "a table of 16 columns is put in a scroll" (some (# (Scroll? %0)) (. md_md :flatten)))
(assert-true "and the page is no wider for it" (<= md_w 660))
(bind '(md_md md_w) (md-make md_lines))
(assert-eq "one of 5 is not" :nil (some (# (Scroll? %0)) (. md_md :flatten)))

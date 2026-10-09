(report-header "Md: a table keeps to the page, a long path is cut to fit its cell, and a table too wide is scrolled")

(import "usr/env.inc")
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

;the table of docs/history/press/README.md is where this was seen, a
;file with a long name made the Docs app page many times its width
(defq md_path "docs/history/press/1990-04_personal_computer_world.pdf"
	md_lines (list
		"| Date | Publication | Article | Author | File |"
		"|------|-------------|---------|--------|------|"
		(cat "| Apr 1990 | Personal Computer World | Newsprint | not credited | " md_path " |")
		"| Jan 1991 | Parallelogram | Tao Systems advertisement | | [pdf](a.pdf) |"))
(bind '(md_md md_w) (md-make md_lines))
(assert-true "a table with a long path in a cell is no wider than the page" (<= md_w 660))
(defq md_all (md-texts md_md) md_joined (apply (const cat) (map (const trim) md_all)))
(assert-true "the path is all there, in its parts" (nempty? (substr md_joined md_path)))
(assert-true "and is more than one part" (notany (# (nempty? (substr %0 md_path))) md_all))
(assert-true "it is cut after a _ - . or /" (some (# (find (last (trim %0)) "_-./")) md_all))

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

;a link is its text, as a Link, which knows where it goes, in an Md that
;was given an event for a Link to send
(defq md_line "see [More power](a/b.md) and ![a pic](x.png), not [this] (that) nor `[code](x)`")
(def (defq md_md (Md)) :page_width 640 :link_event 99)
(. md_md :populate_lines (list md_line))
(defq md_links (filter (# (Link? %0)) (. md_md :flatten)))
(assert-list-eq "the text of a link is what is shown, each word of it" '("More " "power " "a " "pic")
	(map (# (str (get :text %0))) md_links))
(assert-list-eq "and each word knows where the link goes" '("a/b.md" "a/b.md" "x.png" "x.png")
	(map (# (get :link %0)) md_links))
(assert-list-eq "what is round a link is as it was, and a comma after one sits against it"
	'("see " "More " "power " "and " "a " "pic" ", ")
	(slice (md-texts md_md) 0 7))
(assert-true "what is not a link, and what is quoted as code, is left"
	(and (find "[this] " (md-texts md_md)) (find "[code](x) " (md-texts md_md))))
(assert-true "each Link is connected to the event" (every (# (eql (get :targets %0) (array 99))) md_links))
(def (defq md_md (Md)) :page_width 640 :link_event 99)
(. md_md :populate_lines (list "# A title with [a link](t.md)"))
(assert-list-eq "a link in a heading is one too" '("t.md" "t.md")
	(map (# (get :link %0)) (filter (# (Link? %0)) (. md_md :flatten))))

;the documents name each other in code quotes, and the name is the link
(def (defq md_md (Md)) :page_width 640 :link_event 99)
(. md_md :populate_lines (list "As stated in its own documentation ([`lisp.md`](../lisp/lisp.md)), and **in bold ([`a b.md`](c.md))**."))
(defq md_links (filter (# (Link? %0)) (. md_md :flatten)))
(assert-list-eq "a link whose text is quoted as code is a link" '("lisp.md" "a" "b.md")
	(map (# (trim (str (get :text %0)))) md_links))
(assert-list-eq "to where it says" '("../lisp/lisp.md" "c.md" "c.md") (map (# (get :link %0)) md_links))
(assert-true "and is green, not the blue of code" (every (# (= (get :ink_color %0) +argb_green6)) md_links))
(assert-list-eq "a bracket a link is written against sits against it, both sides"
	'("documentation " "(" "lisp.md" "), ") (slice (md-texts md_md) 5 9))

;an Md that was given none, the News app's say, has nothing to follow a
;link with, and shows it as it is written, where it goes can be read
(bind '(md_md md_w) (md-make (list md_line)))
(assert-eq "with no event there are no Links" :nil (some (# (Link? %0)) (. md_md :flatten)))
(assert-true "and a link is as it is written" (find "power](a/b.md) " (md-texts md_md)))

;where a link of a document goes
(assert-eq "a file beside the document" "docs/lisp/b.md" (md-link-file "docs/lisp/a.md" "b.md"))
(assert-eq "one in a folder up and across" "docs/gui/b.md" (md-link-file "docs/lisp/a.md" "../gui/b.md"))
(assert-eq "a . is no move" "docs/lisp/x/b.md" (md-link-file "docs/lisp/a.md" "./x/b.md"))
(assert-eq "a place in a file is not part of its name" "docs/lisp/b.md" (md-link-file "docs/lisp/a.md" "b.md#here"))
(assert-eq "from the root" "docs/b.md" (md-link-file "docs/lisp/a.md" "/docs/b.md"))
(assert-eq "more folders up than there are stops at the root" "b.md" (md-link-file "docs/a.md" "../../../b.md"))

;the Docs app follows a link only to a document of its tree, and asks the
;tree, so whatever the tree was filled from is the limit. A tree as the
;app fills its own
(ui-window md_window ()
	(ui-files md_files "Project" 0 :nil))
(. md_files :populate "docs" '(".md"))
(assert-true "a document under the root is in the tree" (. md_files :find_node "docs/gui/event_dispatch.md"))
(assert-true "by a link from another, up and across"
	(. md_files :find_node (md-link-file "docs/ai_digest/summary.md" "../gui/event_dispatch.md")))
(assert-eq "one above the root is not" :nil (. md_files :find_node (md-link-file "docs/intro/intro.md" "../../README.md")))
(assert-eq "nor one that is not there" :nil (. md_files :find_node "docs/gui/no_such.md"))
(assert-eq "nor a file that is there and is not a document" :nil (. md_files :find_node "docs/history/press/1991-12_byte.pdf"))
(. md_files :empty)
(. md_files :populate "docs/gui" '(".md"))
(assert-eq "with another root, what was in is out" :nil (. md_files :find_node "docs/ai_digest/summary.md"))
(assert-true "and what is under it is in" (. md_files :find_node "docs/gui/event_dispatch.md"))


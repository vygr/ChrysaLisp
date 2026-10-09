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

;docs/test.md is the page a person tries the text handler on, and its last
;section is links of every kind. Here it is drawn, as the Docs app does,
;and each link asked of the tree, so the page and the code are kept in step
(defq md_page (list))
(lines! (lambda (line) (push md_page line) :nil) (file-stream "docs/test.md"))
(assert-true "docs/test.md has a section of links" (find "### Links" md_page))
(def (defq md_md (Md)) :page_width 640 :link_event 99)
(. md_md :populate_lines md_page)
(defq md_links (filter (# (Link? %0)) (. md_md :flatten))
	md_targets (reduce (# (if (find (get :link %1) %0) %0 (push %0 (get :link %1)))) md_links (list))
	md_places (filter (# (starts-with "#" %0)) md_targets)
	md_targets (filter (# (not (starts-with "#" %0))) md_targets))
(. md_files :empty)
(. md_files :populate "docs" '(".md"))
(defq md_go (filter (# (and (not (find ":" %0)) (ends-with ".md" (defq f (md-link-file "docs/test.md" %0)))
	(. md_files :find_node f))) md_targets))
(assert-eq "it has 16 files its links go to" 16 (length md_targets))
(assert-eq "11 of them are documents of the tree, and are followed" 11 (length md_go))
(assert-list-eq "the rest are the web, a pdf, a picture, out of the root, and one not there"
	'("https://github.com/vygr/ChrysaLisp" "history/press/1991-12_byte.pdf" "../screen_shot_5.png" "../README.md" "no_such_document.md")
	(filter (# (not (find %0 md_go))) md_targets))
(assert-eq "what is quoted there is not a link" :nil (some (# (eql (get :link %0) "target.md")) md_links))
(assert-true "the link in the heading is one" (find "gui/comms.md" md_targets))
(assert-true "and the page is no wider for a link too long for a line" (<= (first (. md_md :pref_size)) 660))

;a place, what is after the # of a link, is a heading by its name
(assert-eq "the name of a heading" "heading-level-1-34pt" (md-anchor "Heading Level 1 (34pt)"))
(assert-eq "of one with a link in it" "a-heading-with-a-link-in-it" (md-anchor "A heading with [a link](gui/comms.md) in it"))
(assert-eq "of one with code and marks in it" "the-each-function--friends" (md-anchor "The `(each)` function & friends!"))
(assert-list-eq "the page has three links to a place in itself" '("#standard-table-with-inline-styles" "#text-handler-master-test-suite" "#no-such-heading") md_places)
(assert-list-eq "two are headings of it, one is not" '(:t :t :nil)
	(map (# (if (. md_md :find_anchor (rest %0)) :t :nil)) md_places))
(defq md_top (. md_md :find_anchor "text-handler-master-test-suite") md_tab (. md_md :find_anchor "standard-table-with-inline-styles"))
(bind '(w h) (. md_md :pref_size))
(. md_md :change 0 0 w h)
(assert-eq "the first heading is at the top of the page" 0 (second (. md_md :get_relative md_top)))
(assert-true "the table's is well down it" (> (second (. md_md :get_relative md_tab)) 1000))
(assert-true "a link to a place in another document knows the document" (find "lisp/iteration.md#predication" md_targets))
(assert-eq "and the file is that with the place left off" "docs/lisp/iteration.md" (md-link-file "docs/test.md" "lisp/iteration.md#predication"))
(defq md_it (list))
(lines! (lambda (line) (push md_it line) :nil) (file-stream "docs/lisp/iteration.md"))
(assert-true "which that document has a heading of" (some (# (and (starts-with "#" %0) (eql (md-anchor (trim-start %0 (const (char-class "# ")))) "predication"))) md_it))
(assert-eq "an Md that was never drawn has no places" :nil (. (Md) :find_anchor "x"))

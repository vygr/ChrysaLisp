(report-header "Chat: files and pictures, the message that carries one and how it is shown")

(import "usr/env.inc")
(import "gui/lisp.inc")

;the app is loaded and not run. Chris: "We want that to handle file
;exchange and images as well as .md text etc."
(catch (import "apps/desktop/chat/app.lisp") :t)

(defq cf_nl (ascii-char 10) cf_data (cat "a" cf_nl "b" (ascii-char 0) "c"))
(assert-eq "a file's message is who it is for, who sent it, its name and its bytes, with lines in them"
	(str (list "kim" "lee" "notes.bin" cf_data))
	(str (parse-file-msg (cat "FILE:kim" cf_nl "lee" cf_nl "notes.bin" cf_nl cf_data))))
(assert-eq "to all it is for no one" "" (first (parse-file-msg (cat "FILE:" cf_nl "lee" cf_nl "x" cf_nl "y"))))
(assert-eq "one cut short is no file" :nil (parse-file-msg (cat "FILE:" cf_nl "lee")))

;a picture is shown, no wider than the page
(defq cf_view (create-file-view "lee" "screen_shot_1.png" 400))
(defq cf_canvas (first (. (second (. cf_view :children)) :children)))
(assert-true "a picture is shown as a picture" (Canvas? cf_canvas))
(assert-true "no wider than the page" (<= (first (. cf_canvas :pref_size)) 400))
;a .md is shown as its words, and anything else as where it is
(defq cf_md (create-file-view "lee" "docs/test.md" 400))
(assert-eq "a .md is its words, one Md" 1 (length (. cf_md :children)))
(assert-true "with more lines than a note of it" (> (length (get :text_lines (first (. cf_md :children)))) 4))
(defq cf_other (create-file-view "lee" "README.md.zzz" 400))
(assert-true "anything else says where it was kept"
	(some (# (nempty? (substr %0 "README.md.zzz"))) (get :text_lines (first (. cf_other :children)))))
(assert-eq "the name is the file's, from where it is in a folder, whole"
	"**lee** *sent* `test.md`" (first (get :text_lines (first (. cf_md :children)))))
(assert-eq "one that came is shown by the name it was sent by, not the one it is kept as"
	"**lee** *sent* `cube.png`" (first (get :text_lines (first (. (create-file-view "lee" "apps/demos/model/data/chat_1792098960527513_cube.png" 400) :children)))))

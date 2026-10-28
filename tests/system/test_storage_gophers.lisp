(report-header "System: Storage, hundreds of gophers at once, the gophers scene, and the library one by one")

(import "service/storage/scene.inc")
(import "service/storage/gophers.inc")

;each of the library over blocks of many sizes, as a gopher would go, against
;what is worked out over the whole
(defq sg_text (cat "a member, two members,\nand a member that is cut by the end of a block\n"
	"with lines of all lengths\n\n\nand more members at the end member"))
(defq sg_lines 0)
(each (# (if (eql %0 "\n") (++ sg_lines))) sg_text)
(each (lambda (size)
	(assert-eq (cat "bytes, blocks of " (str size)) (length sg_text) (gopher-fold (gopher-bytes) sg_text size))
	(assert-eq (cat "lines, blocks of " (str size)) sg_lines (gopher-fold (gopher-lines) sg_text size))
	(assert-eq (cat "a word, blocks of " (str size)) 5 (first (gopher-fold (gopher-count "member") sg_text size)))
	(assert-eq (cat "the longest line, blocks of " (str size)) 46 (first (gopher-fold (gopher-longest) sg_text size)))
	(assert-eq (cat "the head, blocks of " (str size)) (slice sg_text 0 30) (gopher-fold (gopher-head 30) sg_text size)))
	'(1 2 5 7 16 4096))

;all of them out at once, to members all round
(defq sg_result (scene-run "gophers" (lambda (line) line) :nil :t))
(assert-true "the gophers scene ran to its end" sg_result)
(if sg_result (assert-eq "and every gopher came back right" 0 (second sg_result)))

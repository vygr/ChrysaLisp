(report-header "System: Storage, a recording played back: the store at any event, forward and back")

(import "apps/system/storage/replay.inc")

(defq rp (replay-new) rp_lines '(
	"100 m-a peer+ m-b"
	"101 m-a peer+ m-b"
	"110 m-a block+ B1"
	"111 m-a block+ B2"
	"112 m-a object M1 B1 B2"
	"113 m-a set M1 8192 docs/a file"
	"120 m-b meet m-a"
	"121 m-b learnt M1 m-a docs/a file"
	"122 m-b block+ B1"
	"123 m-b fetched B1 m-a"
	"130 m-a block- B2"
	"140 m-a delete - 0 docs/a file"
	"150 m-b gopher G1 docs/a file"
	"151 m-b gopher-step 0 B1 G1 docs/a file"
	"152 m-a gopher-step 1 B2 G1 docs/a file"))
(each (# (replay-add rp %0)) rp_lines)
(assert-eq "every line is an event" 15 (length (. rp :find :events)))
(assert-list-eq "the members, as they came" '("m-a" "m-b") (. rp :find :members))
(assert-eq "a path with a space in it is had whole" "docs/a file" (elem-get (elem-get (. rp :find :events) 5) +ev_path))
(replay-seek rp 6)
(assert-true "five done: m-a keeps both blocks" (and (. (replay-held rp "m-a") :find "B1") (. (replay-held rp "m-a") :find "B2")))
(assert-eq "and has the path" "M1" (replay-path rp "m-a" "docs/a file"))
(assert-list-eq "the object is made of its blocks" '("B1" "B2") (replay-blocks rp "M1"))
(assert-eq "m-b has not heard of it" :nil (replay-path rp "m-b" "docs/a file"))
(replay-seek rp 12)
(assert-eq "eleven: m-a has it deleted" "-" (replay-path rp "m-a" "docs/a file"))
(assert-eq "and m-b as it learnt it" "M1" (replay-path rp "m-b" "docs/a file"))
(assert-eq "m-a let go of a block" :nil (. (replay-held rp "m-a") :find "B2"))
(assert-list-eq "who m-a sees, told twice, is once" '("m-b") (. (. rp :find :peers) :find "m-a"))
(replay-seek rp 15)
(assert-eq "the end: the gopher went to two" 2 (length (second (replay-walk rp "G1"))))
(assert-eq "it is the last that set out" "G1" (. rp :find :last_gopher))
(assert-eq "and the last to set out to its object" "G1" (. (. rp :find :path_gopher) :find "docs/a file"))
(assert-eq "and its path is kept" "docs/a file" (first (replay-walk rp "G1")))
(assert-eq "an event of it is known to be" "G1" (replay-gopher-tag (elem-get (. rp :find :events) 13)))
(replay-seek rp 4)
(assert-eq "back to three: m-a has no path" :nil (replay-path rp "m-a" "docs/a file"))
(assert-true "and has the block it let go of later" (. (replay-held rp "m-a") :find "B2"))
(assert-eq "m-b keeps nothing" :nil (. (replay-held rp "m-b") :find "B1"))
(assert-eq "no gopher yet" :nil (replay-walk rp "G1"))
(replay-seek rp 0)
(assert-list-eq "back to the start, nobody is seen" '() (. (. rp :find :peers) :find "m-a"))
(replay-seek rp 15)
(assert-eq "and forward again to the same end" "-" (replay-path rp "m-a" "docs/a file"))
(assert-true "an event of the path is about it" (replay-about? (elem-get (. rp :find :events) 5) "docs/a file" '("B1" "B2")))
(assert-true "and one of a block of it" (replay-about? (elem-get (. rp :find :events) 8) "docs/a file" '("B1" "B2")))
(assert-eq "one of who it sees is not" :nil (replay-about? (elem-get (. rp :find :events) 0) "docs/a file" '("B1" "B2")))
(replay-trim rp 5)
(assert-eq "trimmed, the last five events are kept" 5 (length (. rp :find :events)))
(assert-eq "and it is at the end of them" 5 (. rp :find :at))
(assert-eq "the store is as it was" "-" (replay-path rp "m-a" "docs/a file"))
(replay-seek rp 0)
(assert-eq "back to the first kept, what was done before stands" "M1" (replay-path rp "m-b" "docs/a file"))

;each event is the line it was read from, so a recording saved is one that
;storage -r writes, and opens again the same
(defq rp_again (replay-new))
(each (# (replay-add rp_again %0)) rp_lines)
(assert-true "every event gives back its line" (every (# (eql (replay-line %1) %0)) rp_lines (. rp_again :find :events)))

;a gopher late at a member goes to its slow lane: the step is tried again,
;it is one step, and the gopher is not one that set out again
(defq rl (replay-new))
(each (# (replay-add rl %0)) '(
	"200 m-a gopher G2 docs/b"
	"201 m-a gopher G3 docs/b"
	"202 m-a gopher-step 0 B1 G2 docs/b"
	"203 m-a gopher-back G2 docs/b"
	"204 m-a gopher-lane G2 docs/b"
	"205 m-a gopher-step 0 B1 G2 docs/b"))
(replay-seek rl 6)
(assert-eq "late and tried again in the lane, one step" 1 (length (second (replay-walk rl "G2"))))
(assert-eq "and the lane is not a setting out" "G3" (. (. rl :find :path_gopher) :find "docs/b"))
(defq rd (replay-new))
(each (# (replay-add rd %0)) '(
	"300 m-a gopher G4 docs/c"
	"301 m-a gopher-step 0 B1 G4 docs/c"
	"302 m-a gopher-done G4 docs/c"
	"303 m-b gopher G5 docs/c"))
(replay-seek rd 4)
(assert-eq "the last done of an object is kept, one set out after it does not move it" "G4" (. (. rd :find :path_done) :find "docs/c"))
(replay-seek rd 2)
(assert-eq "and before it was done, it is not" :nil (. (. rd :find :path_done) :find "docs/c"))
(replay-seek rl 4)
(assert-eq "back to the back, the step it is to try again is gone" 0 (length (second (replay-walk rl "G2"))))
(replay-seek rl 3)
(assert-eq "and before it, it is there again" 1 (length (second (replay-walk rl "G2"))))

;a scene run by the app's own runner, ./run.lisp, and played back
(import "service/storage/app.inc")
(defq rp2 (replay-new) rp_box (mail-mbox) rp_end :nil
	rp_run (open-child "apps/system/storage/run.lisp" +kn_call_run))
(mail-send rp_run (cat (hex-encode rp_box) (ascii-char 10) "stop"))
(until rp_end
	(defq rp_msg (mail-read rp_box))
	(cond
		((starts-with "lines" rp_msg) (each! (# (replay-add rp2 %0)) (list (split rp_msg (ascii-char 10))) 1 -1))
		((starts-with "end " rp_msg) (setq rp_end (rest (split rp_msg " "))))))
(assert-list-eq "the runner ran the scene to its end, every check held" '("6" "0") rp_end)
(assert-list-eq "its five members are in the recording" '("m-a" "m-b" "m-c" "m-d" "m-e") (sort (. rp2 :find :members)))
(assert-true "what the scene said is in it too" (some (# (eql (elem-get %0 +ev_kind) "say")) (. rp2 :find :events)))
(replay-seek rp2 (length (. rp2 :find :events)))
(defq rp_m (replay-path rp2 "m-a" "big"))
(assert-true "at the end every member has the object" (every (# (eql (replay-path rp2 %0 "big") rp_m)) '("m-a" "m-b" "m-c" "m-d" "m-e")))
(defq rp_blocks (replay-blocks rp2 rp_m))
(assert-eq "and what it is made of is known" 20 (length rp_blocks))
(assert-true "every block is kept by three or more of them, as it was recorded"
	(every (lambda (b) (>= (length (filter (# (. (replay-held rp2 %0) :find b)) '("m-a" "m-b" "m-c" "m-d" "m-e"))) 3)) rp_blocks))
(replay-seek rp2 0)
(assert-eq "and at the start nobody has it" :nil (replay-path rp2 "m-a" "big"))

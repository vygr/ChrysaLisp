(report-header "System: a store's members meeting, which of two entries keeps a path, and where a block is kept")

(import "lib/storage/store.inc")

(defq sm_all '("aaaa" "bbbb" "cccc"))

(defun sm-new (member &optional members)
	;a store of the one id, this member's, that knows who the members are
	(defq stream (string-stream (cat (exfat-zeros 0x200000))))
	(store-make stream 0x200000 "meet" member 4096)
	(defq store (store-open stream member))
	(store-join store (ifn members sm_all))
	store)

(defun sm-tell (from to)
	;one member tells another what it has, its blocks too. How many paths
	;were not as they were there
	(each (# (if (defq data (store-block-get from %0)) (store-block-put to data))) (store-blocks from))
	(length (store-meet to (store-entries from))))

(defun sm-talk (&rest stores)
	;every one tells every other, till none has anything new, and not for ever
	(defq rounds 0 new 1)
	(while (and (> new 0) (< (++ rounds) 10))
		(setq new 0)
		(each (lambda (from) (each (lambda (to) (unless (eql from to) (setq new (+ new (sm-tell from to))))) stores)) stores))
	rounds)

(defun sm-same? (&rest stores)
	(every (# (eql (store-digest %0) (store-digest (first stores)))) stores))

(defun sm-seen (store name) (elem-get (store-find store name) +store_entry_seen))
(defun sm-paths (store) (map (const first) (store-list store)))
(defun sm-aside (store start)
	;the path that begins so, the other name something was kept under
	(some (# (if (starts-with start %0) %0)) (sm-paths store)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; told, and told back
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq sm_a (sm-new "aaaa") sm_b (sm-new "bbbb") sm_c (sm-new "cccc"))
(assert-list-eq "the members, as each has them" sm_all (store-members sm_b))
(assert-true "two with nothing in them have nothing to tell" (sm-same? sm_a sm_b))
(store-put sm_a "p" "one")
(assert-true "one has written: they are not the same" (not (sm-same? sm_a sm_b)))
(assert-list-eq "the writer is the one member that has it" '("aaaa") (sm-seen sm_a "p"))
(assert-eq "told, the other has one path new to it" 1 (sm-tell sm_a sm_b))
(assert-eq "and the object" "one" (store-get sm_b "p"))
(assert-list-eq "it knows the two of them have it" '("aaaa" "bbbb") (sm-seen sm_b "p"))
(assert-eq "told back, the writer learns that" 1 (sm-tell sm_b sm_a))
(assert-list-eq "so it does too" '("aaaa" "bbbb") (sm-seen sm_a "p"))
(assert-true "and they are the same" (sm-same? sm_a sm_b))
(assert-eq "told again there is nothing new" 0 (sm-tell sm_a sm_b))
(assert-eq "two of three have it: most" :t (store-most? sm_a (store-find sm_a "p")))

;written by one who knew: it is simply taken
(store-put sm_b "p" "two" (store-now sm_b "p"))
(assert-eq "written over by one who had it, and told" 1 (sm-tell sm_b sm_a))
(assert-eq "the first has the new" "two" (store-get sm_a "p"))
(assert-list-eq "and there is no other name, nothing was written apart" '("p") (sm-paths sm_a))
(assert-eq "told back, the writer learns the other has it too" 1 (sm-tell sm_a sm_b))
(assert-eq "and told again, nothing" 0 (sm-tell sm_a sm_b))
(sm-talk sm_a sm_b sm_c)
(assert-true "all three talk, and are the same" (sm-same? sm_a sm_b sm_c))
(assert-list-eq "all three have it" sm_all (sm-seen sm_c "p"))

;;;;;;;;;;;;;;;;;;;;;
; written apart
;;;;;;;;;;;;;;;;;;;;;

;a laptop taken home: what only it wrote is taken when it comes back
(store-put sm_b "user/me/notes" "mine")
(store-put sm_b "user/me/more" "also mine")
(store-put sm_a "rack/x" "theirs")
(sm-talk sm_a sm_b sm_c)
(assert-list-eq "paths only the one that was away wrote, and one only the others did: all there, and no other names"
	'("p" "rack/x" "user/me/more" "user/me/notes") (sm-paths sm_a))
(assert-eq "as it wrote them" "mine" (store-get sm_c "user/me/notes"))

;two write the one path, neither knowing of the other, and neither had by most
(store-put sm_a "q" "from a")
(store-put sm_b "q" "from b")
(assert-eq "two that wrote a path apart: the counts say so" :apart
	(store-vv-cmp (elem-get (store-find sm_a "q") +store_entry_vv) (elem-get (store-find sm_b "q") +store_entry_vv)))
(sm-talk sm_a sm_b)
(assert-true "they talk, and are the same" (sm-same? sm_a sm_b))
(defq sm_q (store-get sm_a "q") sm_lost (if (eql sm_q "from a") "from b" "from a")
	sm_aside (sm-aside sm_a "q~"))
(assert-true "one of the two keeps the path" (find sm_q '("from a" "from b")))
(assert-eq "the same one on both" sm_q (store-get sm_b "q"))
(assert-true "the other is kept under another name" sm_aside)
(assert-eq "as it was written" sm_lost (store-get sm_a sm_aside))
(assert-eq "on both" sm_lost (store-get sm_b sm_aside))
(assert-list-eq "the path that was kept comes after both of what was written" '(:after :after)
	(list (store-vv-cmp (elem-get (store-find sm_a "q") +store_entry_vv) '(("aaaa" 1)))
		(store-vv-cmp (elem-get (store-find sm_a "q") +store_entry_vv) '(("bbbb" 1)))))
(sm-talk sm_a sm_b sm_c)
(assert-eq "the third, told after, has it as they do" sm_q (store-get sm_c "q"))
(assert-true "and all are the same" (sm-same? sm_a sm_b sm_c))

;most of the members have one of them: it keeps the path, whatever the other is
(store-put sm_a "r" "the rack's")
(sm-talk sm_a sm_c)
(assert-eq "two of three have what the rack wrote" :t (store-most? sm_a (store-find sm_a "r")))
(store-put sm_b "r" "the laptop's first")
(store-put sm_b "r" "the laptop's second")
(store-put sm_b "r" "the laptop's third")
(assert-eq "the one away wrote it three times, and nobody else has it" :nil (store-most? sm_b (store-find sm_b "r")))
(sm-talk sm_a sm_b sm_c)
(assert-eq "it comes back: what most had keeps the path, though written fewer times" "the rack's" (store-get sm_b "r"))
(assert-eq "what it wrote last is kept under another name" "the laptop's third"
	(store-get sm_a (sm-aside sm_a "r~")))
(assert-true "and all are the same" (sm-same? sm_a sm_b sm_c))

;a delete and a write, apart
(store-put sm_a "s" "base")
(sm-talk sm_a sm_b sm_c)
(store-delete sm_a "s")
(sm-talk sm_a sm_c)
(store-put sm_b "s" "written while it was being deleted")
(sm-talk sm_a sm_b sm_c)
(assert-eq "deleted by most, written by one apart: it is deleted" :nil (store-get sm_b "s"))
(assert-eq "and what was written is kept under another name" "written while it was being deleted"
	(store-get sm_c (sm-aside sm_c "s~")))
(store-put sm_a "t" "base")
(sm-talk sm_a sm_b sm_c)
(store-put sm_a "t" "written by most")
(sm-talk sm_a sm_c)
(store-delete sm_b "t")
(sm-talk sm_a sm_b sm_c)
(assert-eq "written by most, deleted by one apart: it is there" "written by most" (store-get sm_b "t"))
(assert-eq "and a delete leaves nothing to keep under another name" :nil
	(sm-aside sm_b "t~"))
(assert-true "and all are the same" (sm-same? sm_a sm_b sm_c))

;all three write a path apart, and are told in the other order
(defq sm_x (sm-new "aaaa") sm_y (sm-new "bbbb") sm_z (sm-new "cccc"))
(store-put sm_x "u" "x's") (store-put sm_y "u" "y's") (store-put sm_z "u" "z's")
(sm-talk sm_z sm_y sm_x)
(assert-true "three that each wrote a path apart come to the same" (sm-same? sm_x sm_y sm_z))
(assert-list-eq "and nothing that any wrote is lost: one keeps the path and two are under other names"
	'("x's" "y's" "z's") (sort (map (# (store-get sm_x %0)) (sm-paths sm_x))))
(defq sm_x2 (sm-new "aaaa") sm_y2 (sm-new "bbbb") sm_z2 (sm-new "cccc"))
(store-put sm_x2 "u" "x's") (store-put sm_y2 "u" "y's") (store-put sm_z2 "u" "z's")
(sm-tell sm_x2 sm_y2) (sm-tell sm_z2 sm_x2) (sm-talk sm_y2 sm_z2 sm_x2)
(assert-eq "told in another order, it is the same one that keeps the path" (store-get sm_x "u") (store-get sm_x2 "u"))

;;;;;;;;;
; members
;;;;;;;;;

(defq sm_m (sm-new "aaaa" '("aaaa" "bbbb")))
(assert-eq "members another has that this has not are new to it" :t (store-join sm_m '("bbbb" "cccc" "dddd")))
(assert-eq "told the same again, nothing is" :nil (store-join sm_m '("cccc" "aaaa")))
(assert-list-eq "they are all members, in order" '("aaaa" "bbbb" "cccc" "dddd") (store-members sm_m))
(assert-eq "one is retired" :t (store-join sm_m '() '("cccc")))
(assert-list-eq "and is no member" '("aaaa" "bbbb" "dddd") (store-members sm_m))
(assert-list-eq "joined again by one who had not heard, it stays retired" '("aaaa" "bbbb" "dddd")
	(progn (store-join sm_m '("cccc")) (store-members sm_m)))
(store-put sm_m "v" "x")
(assert-eq "one of three members has an entry: not most" :nil (store-most? sm_m (store-find sm_m "v")))
(store-meet sm_m (list (list "v" (store-now sm_m "v") 1 '(("aaaa" 1)) "aaaa" '("cccc" "dddd") :nil)))
(assert-eq "one that is retired does not count: two of three that are members do" :t
	(store-most? sm_m (store-find sm_m "v")))
(defq sm_m2 (store-open (get :stream (get :vol sm_m)) "aaaa"))
(assert-list-eq "opened again, who is retired is kept" '("aaaa" "bbbb" "dddd") (store-members sm_m2))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; where a block is kept
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq sm_five '("aaaa" "bbbb" "cccc" "dddd" "eeee") sm_h (sm-new "aaaa" sm_five) sm_h2 (sm-new "dddd" sm_five)
	sm_names (map (# (sha256 (str "block" %0))) (range 0 300)))
(assert-eq "a block is kept by three of five members" 3 (length (store-holders sm_h (first sm_names))))
(assert-true "by three that are not the same one twice"
	(every (# (= 3 (length (store-seen (store-holders sm_h %0) (list))))) sm_names))
(assert-true "every member works out the same, none is asked"
	(every (# (eql (str (store-holders sm_h %0)) (str (store-holders sm_h2 %0)))) sm_names))
(defq sm_share (map (lambda (who) (length (filter (# (find who (store-holders sm_h %0))) sm_names))) sm_five))
(assert-eq "of 300 blocks, 900 copies are kept" 900 (reduce (const +) sm_share 0))
(assert-true "each member about its share, a fifth of them, 180: between 140 and 220"
	(every (# (< 140 %0 220)) sm_share))
;a member goes: only what it kept is kept by another
(defq sm_four '("aaaa" "bbbb" "cccc" "eeee")
	sm_moved (filter (# (nql (str (sort (store-holders sm_h %0))) (str (sort (store-holders sm_h %0 sm_four))))) sm_names))
(assert-true "with one member gone, the blocks that have another keeper are those it kept, and no others"
	(every (# (find "dddd" (store-holders sm_h %0))) sm_moved))
(assert-eq "all of those it kept" (elem-get sm_share 3) (length sm_moved))
(assert-true "and each of them has one new keeper, the other two as they were"
	(every (# (= 2 (length (filter (lambda (who) (find who (store-holders sm_h %0 sm_four))) (store-holders sm_h %0))))) sm_moved))
(assert-eq "with fewer members than copies, they all keep it" 2 (length (store-holders sm_h (first sm_names) '("aaaa" "bbbb"))))

;;;;;;;;;;;;;;;;;;;;;;;;
; a member's share of it
;;;;;;;;;;;;;;;;;;;;;;;;

;an object of 20 blocks, put at one of five members, which has all of it
(defq sm_s1 (sm-new "aaaa" sm_five) sm_s2 (sm-new "bbbb" sm_five)
	sm_obj (apply (const cat) (map (# (cat (sha256 (str "share" %0)) (exfat-zeros 4064))) (range 0 20))))
(store-put sm_s1 "big" sm_obj)
(bind '(sm_wants sm_extras) (store-share sm_s1 sm_five))
(assert-list-eq "the one it was put at wants nothing, it has it all" '() sm_wants)
(assert-true "some of its blocks are for others to keep, not it" (< 0 (length sm_extras) 20))
(assert-true "each of those with the three that are to, and it not one of them"
	(every (lambda ((name holders)) (and (= (length holders) 3) (not (find "aaaa" holders)))) sm_extras))
(assert-eq "what it is to keep and what it is not are all 20" 20
	(+ (length sm_extras) (length (filter (# (find "aaaa" (store-holders sm_s1 %0))) (second (store-manifest sm_s1 (store-now sm_s1 "big")))))))
;another member is told of the path, and has none of it
(store-meet sm_s2 (store-entries sm_s1))
(bind '(sm_wants2 sm_extras2) (store-share sm_s2 sm_five))
(assert-list-eq "one that has only heard of the path wants its manifest, and can not yet know what else"
	(list (store-now sm_s1 "big")) sm_wants2)
(store-block-put sm_s2 (store-block-get sm_s1 (store-now sm_s1 "big")))
(bind '(sm_wants2 sm_extras2) (store-share sm_s2 sm_five))
(assert-true "with the manifest, it wants the blocks it is one of the keepers of, and no others"
	(and (nempty? sm_wants2) (every (# (find "bbbb" (store-holders sm_s2 %0))) sm_wants2)))
(assert-list-eq "and has nothing that is not its to keep" '() sm_extras2)
(store-pin sm_s2 "big")
(assert-eq "pinned, it wants all 20" 20 (length (first (store-share sm_s2 sm_five))))
(store-pin sm_s2 "big" :t)
;with only two of the members there, they keep everything between them
(assert-list-eq "with two members there, a member has nothing that is not its to keep" '() (second (store-share sm_s1 '("aaaa" "bbbb"))))

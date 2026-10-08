(report-header "Sync: a tree made the same on another machine")
(import "lib/sync/sync.inc")

;the rules of a .gitignore
(defq sy_rules (sync-rules (cat ".vs/" (ascii-char 10) "/obj/" (ascii-char 10) "# a comment" (ascii-char 10)
	"  os " (ascii-char 13) (ascii-char 10) "*.o" (ascii-char 10) "/usr/*/*.tre" (ascii-char 10) (ascii-char 10) "!kept" (ascii-char 10))))
(defun sy-out? (path is_dir) (sync-ignored? sy_rules (split path "/") is_dir))
(test-cases
	(sy-out? ".git" :t) :t
	(sy-out? "obj" :t) :t
	(sy-out? "src/obj" :t) :nil
	(sy-out? "obj" :nil) :nil
	(sy-out? ".vs" :t) :t
	(sy-out? "apps/.vs" :t) :t
	(sy-out? "os" :nil) :t
	(sy-out? "docs/os" :nil) :t
	(sy-out? "cos" :nil) :nil
	(sy-out? "src/host/main.o" :nil) :t
	(sy-out? "src/host/main.on" :nil) :nil
	(sy-out? "o" :nil) :nil
	(sy-out? "usr/chris/env.tre" :nil) :t
	(sy-out? "usr/env.tre" :nil) :nil
	(sy-out? "lib/usr/chris/env.tre" :nil) :nil
	(sy-out? "kept" :nil) :nil
	(sy-out? "README.md" :nil) :nil)

;where the service will write
(test-cases
	(sync-safe? "a/b.txt") :t
	(sync-safe? "a b/c d.txt") :t
	(sync-safe? "") :nil
	(sync-safe? "/etc/passwd") :nil
	(sync-safe? "../up.txt") :nil
	(sync-safe? "a/../../up.txt") :nil
	(sync-safe? "a//b") :nil
	(sync-safe? ".git/config") :nil
	(sync-safe? "a/.git/config") :nil
	(sync-safe? "c:/windows") :nil
	(sync-safe? "~/Documents/x") :nil
	(sync-safe? (cat "a" (ascii-char 0) "/../b")) :nil
	(sync-safe? (cat "a" (ascii-char 10) "b")) :nil
	(sync-safe? (cat "a" (ascii-char 92) "b")) :nil)

;under the root as the host has it. A folder on the way has to be a folder
(save "x" "tests/scratch/sync_fence/real/file.txt")
(test-cases
	(sync-inside? "tests/scratch/sync_fence" "real/file.txt") :t
	(sync-inside? "tests/scratch/sync_fence" "real/new.txt") :t
	(sync-inside? "tests/scratch/sync_fence" "not/there/yet.txt") :t
	(sync-inside? "tests/scratch/sync_fence" "real") :nil
	(sync-inside? "tests/scratch/sync_fence" "real/file.txt/below.txt") :nil
	(sync-inside? "tests/scratch/sync_fence" "real/file.txt" (Fset 3)) :t)
(pii-remove "tests/scratch/sync_fence/real/file.txt")

;what differs
(defq sy_diff (sync-diff '(("a" 1 "H1") ("b" 2 "H2") ("c" 3 "H3")) '(("a" 1 "H1") ("b" 2 "XX") ("d" 4 "H4"))))
(assert-list-eq "what is not the same, and what is not there, is sent" '("b" "c") (first sy_diff))
(assert-list-eq "what is only there" '("d") (second sy_diff))
(assert-list-eq "a list as text and back"
	'("with space.txt" 12 "ABCD") (first (sync-list-read (sync-list-text '(("with space.txt" 12 "ABCD"))))))

;a tree, a service with a root of its own, and a push from one to the other
(defq sy_src "tests/scratch/sync_src" sy_dst "tests/scratch/sync_dst" sy_name "*SyncTest"
	sy_big (apply (const cat) (map (# (cat "line " (str %0) (ascii-char 10))) (range 0 30000))))
(defun sy-clear (root)
	(each (# (pii-remove (cat root "/" %0))) (sync-walk root (sync-rules ""))))
(sy-clear sy_src) (sy-clear sy_dst)
(save "one" (cat sy_src "/one.txt"))
(save "two" (cat sy_src "/deep/er/two.txt"))
(save sy_big (cat sy_src "/big.bin"))
(save "" (cat sy_src "/empty.txt"))
(save "not sent" (cat sy_src "/skip.o"))
(save "not sent" (cat sy_src "/obj/built.txt"))
(save "old" (cat sy_dst "/one.txt"))
(save "stays" (cat sy_dst "/extra.txt"))
(save "theirs" (cat sy_dst "/obj/built.txt"))
(defq sy_text (cat "/obj/" (ascii-char 10) "*.o"))

(assert-eq "the files of a tree, less what the rules leave out" 4 (length (sync-walk sy_src (sync-rules sy_text))))
(assert-eq "a list has the hash of each" (hex-encode (sha256 "one"))
	(third (some (# (if (eql (first %0) "one.txt") %0)) (sync-list sy_src (sync-rules sy_text)))))

(mail-send (open-child "service/sync/app.lisp" +kn_call_pin) (cat sy_name (ascii-char 10) sy_dst))
(defq sy_wait 0 sy_svc :nil)
;the service is there when it has said so, which is not at once
(while (and (empty? (sync-services sy_name)) (< (setq sy_wait (inc sy_wait)) 1500))
	(task-sleep 10000))
(setq sy_svc (if (nempty? (sync-services sy_name)) (first (first (sync-services sy_name)))))
(assert-true "the service says it is there" sy_svc)
(assert-eq "and where it writes" sy_dst (last (first (sync-services sy_name))))

(defq sy_res (sync-push sy_svc sy_src sy_text :t))
(assert-eq "a check finds what differs" 4 (length (elem-get sy_res 4)))
(assert-list-eq "and what is only there" '("extra.txt") (elem-get sy_res 5))
(assert-eq "and changes nothing" "old" (load (cat sy_dst "/one.txt")))

(setq sy_res (sync-push sy_svc sy_src sy_text))
(assert-eq "a push sends what differs" 4 (first sy_res))
(assert-eq "none failed" 0 (elem-get sy_res 3))
(assert-eq "a file that was there and different" "one" (load (cat sy_dst "/one.txt")))
(assert-eq "a file in folders that were not there" "two" (load (cat sy_dst "/deep/er/two.txt")))
(assert-true "a file of more than one part" (eql sy_big (load (cat sy_dst "/big.bin"))))
(assert-true "a file of nothing" (pii-fstat (cat sy_dst "/empty.txt")))
(assert-eq "and it is of nothing" 0 (second (pii-fstat (cat sy_dst "/empty.txt"))))
(assert-true "what the rules leave out is not sent" (not (pii-fstat (cat sy_dst "/skip.o"))))
(assert-eq "nor written over" "theirs" (load (cat sy_dst "/obj/built.txt")))
(assert-eq "what is only there stays" "stays" (load (cat sy_dst "/extra.txt")))

(setq sy_res (sync-push sy_svc sy_src sy_text))
(assert-eq "a push again sends nothing" 0 (first sy_res))
(save "one changed" (cat sy_src "/one.txt"))
(setq sy_res (sync-push sy_svc sy_src sy_text :nil :t))
(assert-eq "one file changed, one sent" 1 (first sy_res))
(assert-eq "and it is the new one" "one changed" (load (cat sy_dst "/one.txt")))
(assert-eq "with remove, what is only there goes" 1 (third sy_res))
(assert-true "it has gone" (not (pii-fstat (cat sy_dst "/extra.txt"))))
(assert-eq "but not what the rules leave out" "theirs" (load (cat sy_dst "/obj/built.txt")))

;what the service will not do
(defq sy_mbox (mail-mbox))
(sync-tell sy_svc sy_mbox +sync_type_put (cat "../sync_escape.txt" (ascii-char 10) "out") 0 3)
(assert-eq "a path up out of its root is refused" -2 (first (sync-hear sy_mbox)))
(assert-true "and not written" (not (pii-fstat "tests/scratch/sync_escape.txt")))
(sync-tell sy_svc sy_mbox +sync_type_del "../sync_src/one.txt")
(assert-eq "so is a remove of one" -2 (first (sync-hear sy_mbox)))
(assert-true "and it is still there" (pii-fstat (cat sy_src "/one.txt")))
(sync-tell sy_svc sy_mbox +sync_type_put (cat "late.txt" (ascii-char 10) "part") 4 8)
(assert-eq "a part of a file with no start is refused" -3 (first (sync-hear sy_mbox)))
;a service given a root that is not in the system's tree takes nothing
(mail-send (open-child "service/sync/app.lisp" +kn_call_pin) (cat "*SyncTestOut" (ascii-char 10) "/tmp"))
(setq sy_wait 0)
(while (and (empty? (sync-services "*SyncTestOut")) (< (setq sy_wait (inc sy_wait)) 1500)) (task-sleep 10000))
(defq sy_out (first (first (sync-services "*SyncTestOut"))))
(sync-tell sy_out sy_mbox +sync_type_put (cat "sync_outside_probe.txt" (ascii-char 10) "out") 0 3)
(assert-eq "a service with a root outside the tree refuses a file" -1 (first (sync-hear sy_mbox)))
(assert-true "and it is not written" (not (pii-fstat "/tmp/sync_outside_probe.txt")))
(sync-tell sy_out sy_mbox +sync_type_list "")
(assert-eq "and will not list it" -1 (first (sync-hear sy_mbox)))
(sync-tell sy_out sy_mbox +sync_type_quit "")
(sync-hear sy_mbox)

(sync-tell sy_svc sy_mbox 99 "")
(assert-eq "and what it does not know" -1 (first (sync-hear sy_mbox)))

(sync-tell sy_svc sy_mbox +sync_type_quit "")
(sync-hear sy_mbox)
(setq sy_wait 0)
(while (and (nempty? (sync-services sy_name)) (< (setq sy_wait (inc sy_wait)) 1500)) (task-sleep 10000))
(assert-true "told to stop, it is gone" (empty? (sync-services sy_name)))
(assert-eq "and a push to it gets no answer" :nil
	(progn (sync-tell sy_svc sy_mbox +sync_type_list "") (sync-hear sy_mbox 300000)))

(sy-clear sy_src) (sy-clear sy_dst)
(undef (env) 'sy_rules 'sy_diff 'sy_src 'sy_dst 'sy_name 'sy_big 'sy_text 'sy_wait 'sy_svc 'sy_res 'sy_mbox 'sy_out)

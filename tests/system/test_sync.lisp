(report-header "Sync: a tree made the same on another machine")
(import "lib/sync/sync.inc")

;the rules of a .gitignore
(defq sy_rules (sync-rules (cat ".vs/" "\n" "/obj/" "\n" "# a comment" "\n"
	"  os " "\r" "\n" "*.o" "\n" "/usr/*/*.tre" "\n" "\n" "!kept" "\n")))
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
	(sync-safe? "mesh_key") :nil
	(sync-safe? "sync_from") :nil
	(sync-safe? "waypoint") :nil
	(sync-safe? "keys/rig") :nil
	(sync-safe? "docs/mesh_key") :t
	(sync-safe? "monkeys/a") :t
	(sync-safe? "a/../../up.txt") :nil
	(sync-safe? "a//b") :nil
	(sync-safe? ".git/config") :nil
	(sync-safe? "a/.git/config") :nil
	(sync-safe? "c:/windows") :nil
	(sync-safe? "~/Documents/x") :nil
	(sync-safe? "a\x00/../b") :nil
	(sync-safe? "a\nb") :nil
	(sync-safe? "a\\b") :nil)

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

;a tree, a service with a root of its own, and a push from one to the other
(defq sy_src "tests/scratch/sync_src" sy_dst "tests/scratch/sync_dst" sy_name "*SyncTest"
	sy_big (apply (const cat) (map (# (cat "line " (str %0) "\n")) (range 0 30000))))
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
(defq sy_text "/obj/\n*.o")

(assert-eq "the files of a tree, less what the rules leave out" 4 (length (sync-walk sy_src (sync-rules sy_text))))
(assert-eq "a list has the hash of each" (hex-encode (sha256 "one"))
	(third (some (# (if (eql (first %0) "one.txt") %0)) (sync-list sy_src (sync-rules sy_text)))))

(mail-send (open-child "service/sync/app.lisp" +kn_call_pin) (cat sy_name "\n" sy_dst))
;the service is there when it has said so, which is not at once
(while (empty? (sync-services sy_name)) (task-sleep 10000))
(defq sy_svc (if (nempty? (sync-services sy_name)) (first (first (sync-services sy_name)))))
(assert-true "the service says it is there" sy_svc)
(assert-eq "and where it writes" sy_dst (last (first (sync-services sy_name))))

;a second one of the same name on this machine does not start
(mail-send (open-child "service/sync/app.lisp" +kn_call_pin) (cat sy_name "\n" "tests/scratch/sync_other"))
(task-sleep 500000)
(assert-eq "one sync service of a name for a machine" 1 (length (sync-services sy_name)))
(assert-eq "and it is the first" sy_dst (last (first (sync-services sy_name))))

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

;it has a file this one has not, so the tops of the two are not the same
(defq sy_top (hash-tree-root (sync-tree sy_src (sync-rules sy_text))))
(assert-true "one file more there, and the tops of the two trees differ" (nql sy_top (sync-root sy_svc sy_text)))
;a change three folders down is found from the top
(save "deep" (cat sy_src "/a/b/c/deep.txt"))
(save "deep" (cat sy_dst "/a/b/c/deep.txt"))
(save "side" (cat sy_dst "/a/b/side.txt"))
(save "only there" (cat sy_dst "/there/only/x.txt"))
(save "deeper" (cat sy_src "/a/b/c/deep.txt"))
(setq sy_res (sync-push sy_svc sy_src sy_text :t))
(assert-list-eq "a file changed three folders down is what differs" '("a/b/c/deep.txt") (elem-get sy_res 4))
(assert-list-eq "and what is only there, in a folder we both have and in one only it has"
	'("a/b/side.txt" "extra.txt" "there/only/x.txt") (elem-get sy_res 5))
(setq sy_res (sync-push sy_svc sy_src sy_text))
(assert-eq "it is sent" "deeper" (load (cat sy_dst "/a/b/c/deep.txt")))
(assert-eq "and no other" 1 (first sy_res))
(each (# (pii-remove %0)) (list (cat sy_src "/a/b/c/deep.txt") (cat sy_dst "/a/b/c/deep.txt")
	(cat sy_dst "/a/b/side.txt") (cat sy_dst "/there/only/x.txt")))
(setq sy_res (sync-push sy_svc sy_src sy_text :t))
(assert-eq "taken away again, nothing differs" 0 (length (elem-get sy_res 4)))

;the mode of a file, who may read, write and run it. Not on a host that has none
(unless (eql (os) 'Windows)
	(defun sy-mode (file) (logand (third (pii-fstat file)) 511))
	(save "#!/bin/bash" (cat sy_src "/run.sh"))
	(pii-chmod (cat sy_src "/run.sh") 493)
	(setq sy_res (sync-push sy_svc sy_src sy_text))
	(assert-eq "a new script is sent" 1 (first sy_res))
	(assert-eq "and can be run there as here" 493 (sy-mode (cat sy_dst "/run.sh")))
	(pii-chmod (cat sy_src "/run.sh") 420)
	(setq sy_res (sync-push sy_svc sy_src sy_text :t))
	(assert-eq "a file the same but for its mode is not one that differs" 0 (length (elem-get sy_res 4)))
	(assert-eq "it is one with another mode" 1 (elem-get sy_res 6))
	(assert-eq "and a check changes nothing" 493 (sy-mode (cat sy_dst "/run.sh")))
	(setq sy_res (sync-push sy_svc sy_src sy_text))
	(assert-eq "it is not sent again" 0 (first sy_res))
	(assert-eq "its mode is set" 420 (sy-mode (cat sy_dst "/run.sh")))
	(assert-eq "and counted" 1 (elem-get sy_res 6))
	(pii-chmod (cat sy_src "/run.sh") 493)
	(setq sy_res (sync-push sy_svc sy_src sy_text :nil :nil :nil :t))
	(assert-eq "told there are no modes, none is set" 420 (sy-mode (cat sy_dst "/run.sh")))
	(pii-chmod (cat sy_src "/run.sh") 420)
	(pii-remove (cat sy_src "/run.sh")) (pii-remove (cat sy_dst "/run.sh")))
(save "one changed" (cat sy_src "/one.txt"))
(setq sy_res (sync-push sy_svc sy_src sy_text :nil :t))
(assert-eq "one file changed, one sent" 1 (first sy_res))
(assert-eq "and it is the new one" "one changed" (load (cat sy_dst "/one.txt")))
(assert-eq "with remove, what is only there goes" 1 (third sy_res))
(assert-true "it has gone" (not (pii-fstat (cat sy_dst "/extra.txt"))))
(assert-eq "but not what the rules leave out" "theirs" (load (cat sy_dst "/obj/built.txt")))
;a folder a remove leaves empty goes with it, one with anything left in it stays
(save "gone" (cat sy_dst "/old/deep/left.txt"))
(save "gone" (cat sy_dst "/kept/left.txt"))
(save "left out" (cat sy_dst "/kept/made.o"))
(setq sy_res (sync-push sy_svc sy_src sy_text :nil :t))
(assert-eq "two only there go" 2 (third sy_res))
(assert-eq "and the folders they left empty" :nil (sync-kind sy_dst "old"))
(assert-eq "not one that still has in it what the rules leave out" "d" (sync-kind sy_dst "kept"))
(assert-eq "and the root is never removed" "d" (sync-kind "tests/scratch" "sync_dst"))
(pii-remove (cat sy_dst "/kept"))
;now the two are the same, and one number from each says so
(setq sy_top (hash-tree-root (sync-tree sy_src (sync-rules sy_text))))
(assert-eq "the top of the service's tree, by the same rules, is the top of this one" sy_top
	(sync-root sy_svc sy_text))
(assert-true "by other rules it is not" (nql sy_top (sync-root sy_svc "")))

;what the service will not do
(defq sy_mbox (mail-mbox))
(sync-tell sy_svc sy_mbox +sync_type_put "../sync_escape.txt\nout" 0 3)
(assert-eq "a path up out of its root is refused" -2 (first (sync-hear sy_mbox)))
(assert-true "and not written" (not (pii-fstat "tests/scratch/sync_escape.txt")))
(sync-tell sy_svc sy_mbox +sync_type_del "../sync_src/one.txt")
(assert-eq "so is a remove of one" -2 (first (sync-hear sy_mbox)))
(assert-true "and it is still there" (pii-fstat (cat sy_src "/one.txt")))
(sync-tell sy_svc sy_mbox +sync_type_put "late.txt\npart" 4 8)
(assert-eq "a part of a file with no start is refused" -3 (first (sync-hear sy_mbox)))
;a service given a root that is not in the system's tree takes nothing
(mail-send (open-child "service/sync/app.lisp" +kn_call_pin) "*SyncTestOut\n/tmp")
(while (empty? (sync-services "*SyncTestOut")) (task-sleep 10000))
(defq sy_out (first (first (sync-services "*SyncTestOut"))))
(sync-tell sy_out sy_mbox +sync_type_put "sync_outside_probe.txt\nout" 0 3)
(assert-eq "a service with a root outside the tree refuses a file" -1 (first (sync-hear sy_mbox)))
(assert-true "and it is not written" (not (pii-fstat "/tmp/sync_outside_probe.txt")))
(sync-tell sy_out sy_mbox +sync_type_root "")
(assert-eq "and will not say what is there" -1 (first (sync-hear sy_mbox)))
(sync-tell sy_out sy_mbox +sync_type_quit "")
(sync-hear sy_mbox)

(sync-tell sy_svc sy_mbox +sync_type_mode "../sync_src/one.txt" 0 0 493)
(assert-eq "so is a mode set on one" -2 (first (sync-hear sy_mbox)))
(sync-tell sy_svc sy_mbox 99 "")
(assert-eq "and what it does not know" -1 (first (sync-hear sy_mbox)))
(sync-tell sy_svc sy_mbox +sync_type_list "")
(assert-eq "the whole list, as an older sync asks for it, it no longer knows" -1 (first (sync-hear sy_mbox)))
(sync-tell sy_svc sy_mbox +sync_type_put "late2.txt\nx" 0 1)
(sync-hear sy_mbox)
(sync-tell sy_svc sy_mbox +sync_type_folder "")
(assert-eq "a folder asked for with no tree worked out, after a write, is refused" -5 (first (sync-hear sy_mbox)))
(pii-remove (cat sy_dst "/late2.txt"))

;a push from a tree exactly at its waypoint notes it there too, when that
;tree is then the same; and the herd's, asked only as the herd, is taken by
;a tree at an older waypoint and not by one at its own or with a change
(defq sy_rt (cat sy_text "\n" "/waypoint"))
(save sy_rt (cat sy_src "/.gitignore"))
(save (sync-waypoint-text "cccc" 300 (sync-top sy_src)) (cat sy_src "/waypoint"))
(setq sy_res (sync-push sy_svc sy_src sy_rt :nil :t))
(assert-eq "a push from a tree at its waypoint, none failed" 0 (elem-get sy_res 3))
(assert-eq "notes that waypoint there" "cccc" (first (sync-waypoint sy_dst)))
(assert-true "and there it is exactly at it" (sync-clean sy_dst))
(assert-eq "as the herd, at that waypoint already, it is refused" :refused
	(sync-push sy_svc sy_src sy_rt :nil :t :nil :nil :t))
(save (sync-waypoint-text "dddd" 400 (sync-top sy_src)) (cat sy_src "/waypoint"))
(setq sy_res (sync-push sy_svc sy_src sy_rt :nil :t :nil :nil :t))
(assert-eq "a newer one of the same tree, as the herd, sends nothing" 0 (first sy_res))
(assert-eq "and is noted" "dddd" (first (sync-waypoint sy_dst)))
(save "mine" (cat sy_dst "/mine.txt"))
(save (sync-waypoint-text "eeee" 500 (sync-top sy_src)) (cat sy_src "/waypoint"))
(assert-eq "a tree with a change of its own refuses the herd" :refused
	(sync-push sy_svc sy_src sy_rt :nil :t :nil :nil :t))
(pii-remove (cat sy_dst "/mine.txt"))
(save "new" (cat sy_src "/new.txt"))
(save (sync-waypoint-text "eeee" 500 (sync-top sy_src)) (cat sy_src "/waypoint"))
(setq sy_res (sync-push sy_svc sy_src sy_rt :nil :t :nil :nil :t))
(assert-eq "a newer one with a change, as the herd, sends it" "new" (load (cat sy_dst "/new.txt")))
(assert-eq "and is noted" "eeee" (first (sync-waypoint sy_dst)))
(each (# (pii-remove (cat sy_src %0))) '("/.gitignore" "/waypoint" "/new.txt"))
(each (# (pii-remove (cat sy_dst %0))) '("/.gitignore" "/waypoint" "/new.txt"))

;a sync from before there were trees knows nothing of a top, and is told
;apart. It says it does not know each thing it is asked: a top, and of a
;push, who it is from and a top
(defq sy_old (mail-mbox))
(open-task (str `(progn
		(defq mbox (mail-mbox))
		(mail-send (hex-decode ,(hex-encode sy_old)) mbox)
		(times 3 (defq msg (mail-read mbox))
			(mail-send (slice msg 0 +net_id_size) (char -1 +int_size)))))
	(task-nodeid) +kn_call_pin 0 (mail-mbox))
(defq sy_old_svc (mail-read sy_old))
(assert-eq "its top is asked for and it is known to be old" :old (sync-root sy_old_svc sy_text))
(assert-eq "and a push to it sends nothing, and says so" :old (sync-push sy_old_svc sy_src sy_text))

(sync-tell sy_svc sy_mbox +sync_type_quit "")
(sync-hear sy_mbox)
(while (nempty? (sync-services sy_name)) (task-sleep 10000))
(assert-true "told to stop, it is gone" (empty? (sync-services sy_name)))
(assert-eq "and a push to it gets no answer" :nil
	(progn (sync-tell sy_svc sy_mbox +sync_type_list "") (sync-hear sy_mbox 300000)))

;who takes a sync from whom, lib/sync/sync.inc: this machine and those
;named, anything; any other, by the herd, only to a tree that is exactly at
;a waypoint and has no .git, from one at a newer waypoint
(save "a tree" (cat sy_dst "/kept.txt"))
(defq sy_me (hex-encode (system-id)) sy_far "far away node id\x00"
	sy_top (sync-top sy_dst))
(assert-eq "a tree with no waypoint is at none" :nil (sync-clean sy_dst))
(save (sync-waypoint-text "aaaa" 100 sy_top) (cat sy_dst "/waypoint"))
(assert-eq "noted, it is at it, exactly" "aaaa" (first (sync-clean sy_dst)))
(test-cases
	(sync-from (task-nodeid) "" sy_dst :nil sy_me (list)) :rig
	(sync-from (task-nodeid) "" sy_dst :nil "" (list sy_me)) :rig
	(sync-from sy_far "" sy_dst :nil sy_me (list)) :no
	(sync-from sy_far (sync-waypoint-text "bbbb" 200 "x") sy_dst :nil sy_me (list)) :herd
	(sync-from sy_far (sync-waypoint-text "aaaa" 200 "x") sy_dst :nil sy_me (list)) :no
	(sync-from sy_far (sync-waypoint-text "bbbb" 50 "x") sy_dst :nil sy_me (list)) :no
	(sync-from (task-nodeid) "" sy_dst :nil sy_me (list) :t) :no
	(sync-from (task-nodeid) (sync-waypoint-text "bbbb" 200 "x") sy_dst :nil sy_me (list) :t) :herd)
(save "a change of its own" (cat sy_dst "/mine.txt"))
(assert-eq "changed, it is at no waypoint" :nil (sync-clean sy_dst))
(assert-eq "and the herd may not touch it" :no (sync-from sy_far (sync-waypoint-text "bbbb" 200 "x") sy_dst :nil sy_me (list)))
(pii-remove (cat sy_dst "/mine.txt"))
;one with no waypoint noted that is already exactly the tree offered
(pii-remove (cat sy_dst "/waypoint"))
(assert-eq "already exactly it, only the waypoint is to be noted" :herd
	(sync-from sy_far (sync-waypoint-text "bbbb" 200 sy_top) sy_dst :nil sy_me (list)))
(save (sync-waypoint-text "bbbb" 100 sy_top) (cat sy_dst "/waypoint"))
(assert-eq "but not one it is at already" :no
	(sync-from sy_far (sync-waypoint-text "bbbb" 200 sy_top) sy_dst :nil sy_me (list)))
;a tree of its own with a .git, at a waypoint
(defq sy_git "tests/scratch/sync_git")
(save "a tree" (cat sy_git "/kept.txt")) (save "" (cat sy_git "/.git/HEAD"))
(save (sync-waypoint-text "aaaa" 100 (sync-top sy_git)) (cat sy_git "/waypoint"))
(assert-true "a tree with a .git at a waypoint" (sync-clean sy_git))
(assert-eq "is not one the herd touches, git keeps that in step" :no (sync-from sy_far (sync-waypoint-text "bbbb" 200 "x") sy_git :nil sy_me (list)))
(each (# (pii-remove (cat sy_git %0))) '("/.git/HEAD" "/waypoint" "/kept.txt"))
(each (# (pii-remove (cat sy_dst %0))) '("/waypoint" "/kept.txt"))

;a file changed again in the second its hash was taken, to the same size,
;has the same time and size: the hash kept is not trusted, and when it is
;asked for again, seconds later, it is of what is there now
(defq sy_kept "tests/scratch/sync_kept")
(save "old" (cat sy_src "/same.txt"))
(sync-list sy_src (sync-rules "") sy_kept)
(save "new" (cat sy_src "/same.txt"))
(task-sleep 3000000)
(assert-eq "a file changed in the second it was hashed is hashed again"
	(hex-encode (sha256 "new"))
	(third (some (# (if (eql (first %0) "same.txt") %0)) (sync-list sy_src (sync-rules "") sy_kept))))
(pii-remove sy_kept) (pii-remove (cat sy_src "/same.txt"))

(sy-clear sy_src) (sy-clear sy_dst)
(undef (env) 'sy_kept 'sy_rt 'sy_git 'sy_me 'sy_far 'sy_rules 'sy_top 'sy_old 'sy_old_svc 'sy_src 'sy_dst 'sy_name 'sy_big 'sy_text 'sy_svc 'sy_res 'sy_mbox 'sy_out)

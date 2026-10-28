(report-header "Apps: Stores, the task that copies between files and stores, merges, deletes and sets copies")

(import "service/storage/app.inc")

;two services of names of their own, on stores in files of their own, and
;a folder of files of the host
(defq sw_pid (str (pii-pid))
	sw_a_image (cat "obj/tmp_stores_a_" sw_pid ".img")
	sw_b_image (cat "obj/tmp_stores_b_" sw_pid ".img")
	sw_host (cat "obj/tmp_stores_" sw_pid)
	sw_a (storage-start sw_a_image :size 0x800000 :id "work_a" :name (cat "@StoresA" sw_pid) :block 64
		:group (cat "*StoresA" sw_pid) :tick 0)
	sw_b (storage-start sw_b_image :size 0x800000 :id "work_b" :name (cat "@StoresB" sw_pid) :block 64
		:group (cat "*StoresB" sw_pid) :tick 0)
	sw_t1 "track one" sw_t2 (cat (str-alloc 5000)) sw_loose "loose")

(defun sw-run (what from to rule names &optional copies stop)
	;the task told what to do, and stop at once if said, and all it said,
	;the last of it its end
	(defq reply (mail-mbox) task (open-child "apps/system/stores/work.lisp" +kn_call_run) said (list) going :t)
	(mail-send task (str (list (hex-encode reply) what from to rule names (ifn copies 0))))
	(if stop (mail-send task "stop"))
	(while (and going (defq msg (mail-read-timeout reply (task-timeout 30))))
		(push said (defq it (first (read (string-stream msg)))))
		(if (eql (first it) :done) (setq going :nil)))
	said)

(defun sw-end (said)
	;(done skipped failed stopped) of what it said
	(defq end (last said))
	(if (eql (first end) :done) (list (elem-get end 1) (elem-get end 2) (elem-get end 3) (elem-get end 6))))

(defun sw-read (file)
	;what is in a file, read as it is now
	(if (defq stream (file-stream file)) (ifn (read-blk stream 0x100000) "") ""))

(defun sw-store (svc folder)
	(list :store (hex-encode svc) folder))

(each (lambda (i) (set-byte sw_t2 i (logand (* i 13) 0xff))) (range 0 (length sw_t2)))
(save sw_t1 (cat sw_host "/Album/t1.txt"))
(save sw_t2 (cat sw_host "/Album/sub/t2.bin"))
(save sw_loose (cat sw_host "/loose.txt"))
(save "hidden" (cat sw_host "/Album/.hidden"))

(assert-true "two services, of stores they made" (and sw_a sw_b))
(when (and sw_a sw_b)
	;files into a store, a folder and a file
	(defq sw_said (sw-run :copy (list :host sw_host) (sw-store sw_a "in") :skip '(("Album" :t) ("loose.txt" :nil))))
	(assert-list-eq "it says what there is first, three files, how long" (list :plan 3 (+ (length sw_t1) (length sw_t2) (length sw_loose)))
		(first sw_said))
	(assert-list-eq "and at the end that it did all three" '(3 0 0 :nil) (sw-end sw_said))
	(assert-eq "a file under the folder is an object under its name" sw_t1 (storage-get "in/Album/t1.txt" sw_a))
	(assert-eq "and one in a folder under that" sw_t2 (storage-get "in/Album/sub/t2.bin" sw_a))
	(assert-eq "the file taken" sw_loose (storage-get "in/loose.txt" sw_a))
	(assert-eq "not one whose name begins with a dot" :nil (storage-stat "in/Album/.hidden" sw_a))
	(assert-list-eq "again, what is there is skipped" '(0 3 0 :nil)
		(sw-end (sw-run :copy (list :host sw_host) (sw-store sw_a "in") :skip '(("Album" :t) ("loose.txt" :nil)))))

	(assert-list-eq "and kept both, what is the same is left, and not kept again" '(0 3 0 :nil)
		(sw-end (sw-run :copy (list :host sw_host) (sw-store sw_a "in") :both '(("Album" :t) ("loose.txt" :nil)))))
	(assert-eq "by no name of its own" :nil (storage-stat "in/loose (2).txt" sw_a))
	(save "loose, and changed" (cat sw_host "/loose.txt"))
	(assert-list-eq "a file changed is not the same" '(1 0 0 :nil)
		(sw-end (sw-run :copy (list :host sw_host) (sw-store sw_a "in") :both '(("loose.txt" :nil)))))
	(assert-eq "and is kept by a name of its own" "loose, and changed" (storage-get "in/loose (2).txt" sw_a))
	(storage-delete "in/loose (2).txt" :nil sw_a)
	(save sw_loose (cat sw_host "/loose.txt"))

	;from store to store, a folder merged into one of the same name
	(storage-put "Album/t1.txt" "another track" :nil sw_b)
	(assert-list-eq "to another store, what is not there copied, what is not the same skipped" '(1 1 0 :nil)
		(sw-end (sw-run :copy (sw-store sw_a "in") (sw-store sw_b "") :skip '(("Album" :t)))))
	(assert-eq "the one not there is" sw_t2 (storage-get "Album/sub/t2.bin" sw_b))
	(assert-eq "the one that was not the same is as it was" "another track" (storage-get "Album/t1.txt" sw_b))
	(assert-list-eq "both kept, the one there and the new, what is the same left" '(1 1 0 :nil)
		(sw-end (sw-run :copy (sw-store sw_a "in") (sw-store sw_b "") :both '(("Album" :t)))))
	(assert-eq "the new by a name of its own" sw_t1 (storage-get "Album/t1 (2).txt" sw_b))
	(assert-eq "the one there as it was" "another track" (storage-get "Album/t1.txt" sw_b))
	(assert-list-eq "replaced, what is the same left" '(1 1 0 :nil)
		(sw-end (sw-run :copy (sw-store sw_a "in") (sw-store sw_b "") :replace '(("Album" :t)))))
	(assert-eq "the one there is the new" sw_t1 (storage-get "Album/t1.txt" sw_b))

	;out of a store to files of the host
	(assert-list-eq "a folder of a store out to the host" '(2 0 0 :nil)
		(sw-end (sw-run :copy (sw-store sw_a "in") (list :host (cat sw_host "/out")) :skip '(("Album" :t)))))
	(assert-eq "each file what its object is" sw_t1 (load (cat sw_host "/out/Album/t1.txt")))
	(assert-eq "under the folders of the paths" sw_t2 (load (cat sw_host "/out/Album/sub/t2.bin")))
	(assert-list-eq "again, the files there are skipped" '(0 2 0 :nil)
		(sw-end (sw-run :copy (sw-store sw_a "in") (list :host (cat sw_host "/out")) :skip '(("Album" :t)))))
	(assert-list-eq "and kept both, the same are left" '(0 2 0 :nil)
		(sw-end (sw-run :copy (sw-store sw_a "in") (list :host (cat sw_host "/out")) :both '(("Album" :t)))))
	(assert-eq "by no name of their own" :nil (pii-fstat (cat sw_host "/out/Album/t1 (2).txt")))
	(save "mine" (cat sw_host "/out/Album/t1.txt"))
	(assert-list-eq "replaced, a file there told to stop" '(0 0 0 :t)
		(sw-end (sw-run :copy (sw-store sw_a "in/Album") (list :host (cat sw_host "/out/Album")) :replace '(("t1.txt" :nil)) 0 :t)))
	(assert-eq "is as it was" "mine" (sw-read (cat sw_host "/out/Album/t1.txt")))
	(assert-list-eq "replaced, it is the object" '(1 0 0 :nil)
		(sw-end (sw-run :copy (sw-store sw_a "in/Album") (list :host (cat sw_host "/out/Album")) :replace '(("t1.txt" :nil)))))
	(assert-eq "all of it" sw_t1 (sw-read (cat sw_host "/out/Album/t1.txt")))

	;how many keep, and deleted
	(assert-list-eq "how many are to keep each under a folder, set" '(2 0 0 :nil)
		(sw-end (sw-run :copies (sw-store sw_a "in") (sw-store sw_a "in") :skip '(("Album" :t)) 2)))
	(assert-list-eq "it is" '(2 2) (map (# (storage-copies %0 :nil sw_a)) '("in/Album/t1.txt" "in/Album/sub/t2.bin")))
	(assert-list-eq "a folder deleted, all under it" '(2 0 0 :nil)
		(sw-end (sw-run :delete (sw-store sw_a "in") (sw-store sw_a "in") :skip '(("Album" :t)))))
	(assert-list-eq "what is left" '("in/loose.txt") (map (const first) (storage-list "" sw_a)))

	;stopped
	(storage-put "big" (cat (str-alloc 100000)) :nil sw_a)
	(assert-list-eq "told to stop, it stops, and says so" '(0 0 0 :t)
		(sw-end (sw-run :copy (sw-store sw_a "") (sw-store sw_b "") :skip '(("big" :nil)) 0 :t)))
	(assert-eq "what it was doing is not done" :nil (storage-stat "big" sw_b)))

(each (lambda (svc image)
		(when svc (storage-stop svc))
		(pii-remove image))
	(list sw_a sw_b) (list sw_a_image sw_b_image))
(each (const pii-remove) (list (cat sw_host "/Album/t1.txt") (cat sw_host "/Album/sub/t2.bin") (cat sw_host "/loose.txt")
	(cat sw_host "/Album/.hidden") (cat sw_host "/out/Album/t1.txt") (cat sw_host "/out/Album/sub/t2.bin")))
(each (const pii-remove) (list (cat sw_host "/Album/sub") (cat sw_host "/Album") (cat sw_host "/out/Album/sub")
	(cat sw_host "/out/Album") (cat sw_host "/out") sw_host))

(report-header "System: Storage, an object copied from one store to another, and to and from files of the host")

(import "service/storage/app.inc")

;three services of names of their own, on stores in files of their own:
;two of one size of block, and one of another
(defq sc_pid (str (pii-pid))
	sc_a_image (cat "obj/tmp_store_copy_a_" sc_pid ".img")
	sc_b_image (cat "obj/tmp_store_copy_b_" sc_pid ".img")
	sc_c_image (cat "obj/tmp_store_copy_c_" sc_pid ".img")
	sc_file (cat "obj/tmp_store_copy_" sc_pid ".file")
	sc_a (storage-start sc_a_image :size 0x800000 :id "copy_a" :name (cat "@StoreCopyA" sc_pid) :block 64
		:group (cat "*StoreCopyA" sc_pid) :tick 0)
	sc_b (storage-start sc_b_image :size 0x800000 :id "copy_b" :name (cat "@StoreCopyB" sc_pid) :block 64
		:group (cat "*StoreCopyB" sc_pid) :tick 0)
	sc_c (storage-start sc_c_image :size 0x800000 :id "copy_c" :name (cat "@StoreCopyC" sc_pid) :block 100
		:group (cat "*StoreCopyC" sc_pid) :tick 0))

(defun sc-bytes (len seed)
	(defq out (cat (str-alloc len)))
	(each (# (set-byte out %0 (logand (+ (* %0 7) (>> %0 8) seed) 0xff))) (range 0 len))
	out)

(defun sc-ticks (&optional most)
	;a tick that adds up what it is told, and says stop once it has been
	;told most bytes, if most is given
	(setq sc_told 0)
	`(lambda (bytes) (setq sc_told (+ sc_told bytes)) (< sc_told ,(ifn most 0x7fffffffffffffff))))

(defun sc-read (file)
	;what is in a file, read as it is now
	(if (defq stream (file-stream file)) (ifn (read-blk stream 0x100000) "") ""))

(assert-true "three services, of stores they made" (and sc_a sc_b sc_c))
(when (and sc_a sc_b sc_c)
	;more blocks than a block of a manifest names, so its manifest is a tree
	(defq sc_big (sc-bytes (+ (* 64 (inc +store_manifest_names)) 10) 3) sc_small (sc-bytes 100 5) sc_told 0)
	(storage-put "x/big" sc_big :nil sc_a)
	(storage-put "x/small" sc_small :nil sc_a)
	(storage-put "x/none" "" :nil sc_a)
	(storage-copies "x/small" 2 sc_a)

	;from one store to another
	(assert-eq "a big object is copied to another store" :ok (first (storage-copy-object "x/big" "y/big" sc_a sc_b :nil (sc-ticks))))
	(assert-eq "and is what it was there" sc_big (storage-get "y/big" sc_b))
	(assert-eq "it was told every byte as it went" (length sc_big) sc_told)
	(assert-eq "a small one" :ok (first (storage-copy-object "x/small" "y/small" sc_a sc_b)))
	(assert-eq "is what it was" sc_small (storage-get "y/small" sc_b))
	(assert-eq "with how many are to keep it" 2 (storage-copies "y/small" :nil sc_b))
	(assert-eq "one of nothing" :ok (first (storage-copy-object "x/none" "y/none" sc_a sc_b)))
	(assert-eq "is nothing" "" (storage-get "y/none" sc_b))
	(assert-eq "one there is not is none" :none (first (storage-copy-object "x/nothing" "y/nothing" sc_a sc_b)))

	;only what the other store has not got is sent
	(defq sc_blocks (. (storage-info sc_b) :find :blocks))
	(assert-eq "copied again, to another path" :ok (first (storage-copy-object "x/big" "z/big" sc_a sc_b :nil (sc-ticks))))
	(assert-eq "no block more is kept" sc_blocks (. (storage-info sc_b) :find :blocks))
	(assert-eq "and it was told every byte all the same" (length sc_big) sc_told)
	(assert-eq "a copy within the one store" :ok (first (storage-copy-object "x/big" "x/again" sc_a sc_a)))
	(assert-eq "is what it was" sc_big (storage-get "x/again" sc_a))

	;provided the path is what it was said to be
	(assert-eq "not to a path that is there, said to be none" :stale
		(first (storage-copy-object "x/small" "y/big" sc_a sc_b "")))
	(assert-eq "which is as it was" sc_big (storage-get "y/big" sc_b))
	(assert-eq "to it, said what it is" :ok (first (storage-copy-object "x/small" "y/big" sc_a sc_b
		(elem-get (storage-stat "y/big" sc_b) +store_entry_hash))))
	(assert-eq "which is the copy" sc_small (storage-get "y/big" sc_b))

	;stopped
	(assert-eq "a copy told to stop part of the way" :stopped
		(first (storage-copy-object "x/big" "w/big" sc_a sc_c :nil (sc-ticks 2000))))
	(assert-eq "sets no path" :nil (storage-stat "w/big" sc_c))
	(assert-eq "stopped part of the way between stores of one size of block" :stopped
		(first (storage-copy-object "x/big" "w/big" sc_a sc_b :nil (sc-ticks 2000))))
	(assert-eq "sets none either" :nil (storage-stat "w/big" sc_b))

	;to a store of another size of block it is read and put again
	(assert-eq "to a store of other blocks" :ok (first (storage-copy-object "x/big" "y/big" sc_a sc_c :nil (sc-ticks))))
	(assert-eq "is what it was" sc_big (storage-get "y/big" sc_c))
	(assert-eq "told every byte" (length sc_big) sc_told)
	(assert-eq "and back" :ok (first (storage-copy-object "y/big" "y/back" sc_c sc_a)))
	(assert-eq "is what it was" sc_big (storage-get "y/back" sc_a))

	;to and from files of the host
	(assert-list-eq "an object to a file" (list :ok (length sc_big)) (storage-get-file "x/big" sc_file sc_a (sc-ticks)))
	(assert-eq "which is the object" sc_big (load sc_file))
	(assert-eq "told every byte" (length sc_big) sc_told)
	(assert-eq "a file put, told every byte" :ok (first (storage-put-file "f/big" sc_file :nil sc_b (sc-ticks))))
	(assert-eq "as it went" (length sc_big) sc_told)
	(assert-eq "it is the file" sc_big (storage-get "f/big" sc_b))
	(assert-eq "a put told to stop" :stopped (first (storage-put-file "f/part" sc_file :nil sc_b (sc-ticks 1000))))
	(assert-eq "sets no path" :nil (storage-stat "f/part" sc_b))
	(assert-list-eq "a get over a file there, told to stop" '(:stopped :nil) (storage-get-file "x/big" sc_file sc_a (sc-ticks 1000)))
	(assert-eq "leaves the file as it was" sc_big (sc-read sc_file))
	(assert-eq "and nothing next to it" 0 (age (cat sc_file ".part")))
	(assert-list-eq "one over a file there" (list :ok (length sc_small)) (storage-get-file "x/small" sc_file sc_a))
	(assert-eq "writes it over, all of it" sc_small (sc-read sc_file))
	(assert-eq "and leaves nothing next to it" 0 (age (cat sc_file ".part")))
	(pii-remove sc_file)
	(assert-list-eq "a get to no file, told to stop" '(:stopped :nil) (storage-get-file "x/big" sc_file sc_a (sc-ticks 1000)))
	(assert-eq "leaves no file" 0 (age sc_file))
	(assert-list-eq "no object, no file" '(:none :nil) (storage-get-file "x/nothing" sc_file sc_a))
	(assert-eq "an object of nothing, a file of nothing" "" (progn (storage-get-file "x/none" sc_file sc_a) (load sc_file)))
	(pii-remove sc_file)

	;a service that says nothing is waited for the once, not for each block
	(defq sc_quiet (mail-mbox) sc_t0 (pii-time) sc_blocks (map (# (sc-bytes 64 %0)) (range 0 8)))
	(assert-eq "blocks put to a service that says nothing are lost" :lost
		(storage-puts sc_quiet sc_blocks (map (const sha256) sc_blocks)))
	(assert-true "in the time of one wait, not of each" (< (- (pii-time) sc_t0) (* 2 +storage_wait 1000000))))

(each (lambda (svc image)
		(when svc (storage-stop svc))
		(pii-remove image))
	(list sc_a sc_b sc_c) (list sc_a_image sc_b_image sc_c_image))

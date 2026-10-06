(report-header "System: exFAT, a volume in a memory stream")

(import "lib/fs/exfat.inc")

(defun xf-volume (size &optional label cluster_shift)
	;a new volume, formatted and mounted, and the stream it is in
	(defq stream (string-stream (cat (exfat-zeros size))))
	(list (and (exfat-format stream size label cluster_shift) (exfat-mount stream)) stream))

(defun xf-bytes (len seed)
	;bytes that are not all the same, and differ with the seed
	(defq out (cat (exfat-zeros len)))
	(each (# (set-byte out %0 (logand (+ (* %0 7) (>> %0 8) seed) 0xff))) (range 0 len))
	out)

(defun xf-names (vol path)
	(sort (map (const first) (exfat-list vol path))))

(defun xf-flags (stream)
	;the flags of the volume, as the device has them
	(stream-seek stream 0 0)
	(getf (read-blk stream 512) +exfat_boot_flags))

;;;;;;;;;;;;;;;;;;;;;;;;
; format and mount
;;;;;;;;;;;;;;;;;;;;;;;;

(bind '(vol stream) (xf-volume 0x200000 "TESTVOL"))
(assert-true "format and mount" vol)
(assert-eq "new volume is empty" 0 (length (exfat-list vol "")))
(assert-eq "cluster size" 4096 (exfat-cluster-size vol))
(assert-true "not unclean" (not (get :unclean vol)))
(defq free0 (exfat-free vol))
(assert-true "free space" (> free0 0x180000))
(assert-eq "upper case of a" 0x41 (exfat-upper vol 0x61))
(assert-eq "upper case of e acute" 0xc9 (exfat-upper vol 0xe9))
(assert-eq "too small to format" :nil
	(exfat-format (string-stream (cat (exfat-zeros 0x10000))) 0x10000))
(assert-eq "not a volume" :nil (exfat-mount (string-stream (cat (exfat-zeros 0x10000)))))

;;;;;;;;;;;;;;;;;;;;;;;;
; files
;;;;;;;;;;;;;;;;;;;;;;;;

(defq small "Hello from ChrysaLisp" big (xf-bytes 20000 1))
(assert-true "save small" (exfat-save vol "hello.txt" small))
(assert-eq "load small" small (exfat-load vol "hello.txt"))
(assert-eq "name with no regard to case" small (exfat-load vol "HELLO.TXT"))
(assert-true "save empty" (exfat-save vol "empty" ""))
(assert-eq "load empty" "" (exfat-load vol "empty"))
(assert-true "save big" (exfat-save vol "big.bin" big))
(assert-eq "load big" big (exfat-load vol "big.bin"))
(assert-eq "size of big" 20000 (third (exfat-find vol "big.bin")))
(defq long_name "a name that is a good deal longer than fifteen characters.dat")
(assert-true "save long name" (exfat-save vol long_name small))
(assert-eq "load long name" small (exfat-load vol long_name))
(defq uni_name (cat "caf" (num-to-utf8 0xe9) " " (num-to-utf8 0x3b1) (num-to-utf8 0x1f600) ".txt"))
(assert-true "save unicode name" (exfat-save vol uni_name small))
(assert-true "list has unicode name" (find uni_name (xf-names vol "")))
(assert-eq "upper case of unicode name" small
	(exfat-load vol (cat "CAF" (num-to-utf8 0xc9) " " (num-to-utf8 0x391) (num-to-utf8 0x1f600) ".TXT")))
(assert-eq "no such file" :nil (exfat-load vol "nothing"))
(assert-eq "file count" 5 (length (exfat-list vol "")))

;;;;;;;;;;;;;;;;;;;;;;;;
; replace
;;;;;;;;;;;;;;;;;;;;;;;;

(defq index (elem-get (exfat-find vol "big.bin") 5) bigger (xf-bytes 50000 2))
(assert-true "replace with bigger" (exfat-save vol "big.bin" bigger))
(assert-eq "load bigger" bigger (exfat-load vol "big.bin"))
(assert-eq "entries stay where they were" index (elem-get (exfat-find vol "big.bin") 5))
(assert-true "replace with smaller" (exfat-save vol "big.bin" small))
(assert-eq "load smaller" small (exfat-load vol "big.bin"))
(assert-true "replace with empty" (exfat-save vol "big.bin" ""))
(assert-eq "load emptied" "" (exfat-load vol "big.bin"))
(assert-eq "file count after replace" 5 (length (exfat-list vol "")))

;;;;;;;;;;;;;;;;;;;;;;;;
; directories
;;;;;;;;;;;;;;;;;;;;;;;;

(assert-true "mkdir" (exfat-mkdir vol "docs"))
(assert-eq "mkdir again" :nil (exfat-mkdir vol "docs"))
(assert-true "mkdir nested" (exfat-mkdir vol "docs/deep"))
(assert-eq "mkdir with no parent" :nil (exfat-mkdir vol "none/deep"))
(assert-true "save in directory" (exfat-save vol "docs/deep/note.txt" small))
(assert-eq "load from directory" small (exfat-load vol "docs/deep/note.txt"))
(assert-eq "save over a directory" :nil (exfat-save vol "docs" small))
(assert-eq "load a directory" :nil (exfat-load vol "docs"))
(assert-eq "delete a directory with things in it" :nil (exfat-delete vol "docs"))
(defq walked (list))
(exfat-walk vol "docs" (lambda (path entry) (push walked path)))
(assert-list-eq "walk" '("docs/deep" "docs/deep/note.txt") walked)

;a directory with more in it than one cluster holds
(exfat-mkdir vol "many")
(exfat-begin vol)
(each (# (exfat-save vol (cat "many/file number " (str %0) ".txt") (str %0))) (range 0 150))
(exfat-end vol)
(assert-eq "directory grown" 150 (length (exfat-list vol "many")))
(assert-true "directory is more than a cluster" (> (third (exfat-find vol "many")) 4096))
(assert-eq "first of many" "0" (exfat-load vol "many/file number 0.txt"))
(assert-eq "last of many" "149" (exfat-load vol "many/file number 149.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;
; rename
;;;;;;;;;;;;;;;;;;;;;;;;

(assert-true "rename" (exfat-rename vol "hello.txt" "greeting.txt"))
(assert-eq "old name gone" :nil (exfat-find vol "hello.txt"))
(assert-eq "new name" small (exfat-load vol "greeting.txt"))
(assert-true "rename by case alone" (exfat-rename vol "greeting.txt" "Greeting.TXT"))
(assert-true "case as given" (find "Greeting.TXT" (xf-names vol "")))
(assert-true "rename to a longer name" (exfat-rename vol "Greeting.TXT" "a very much longer greeting than before.txt"))
(assert-eq "longer name" small (exfat-load vol "a very much longer greeting than before.txt"))
(assert-true "move to a directory" (exfat-rename vol "a very much longer greeting than before.txt" "docs/hi.txt"))
(assert-eq "moved" small (exfat-load vol "docs/hi.txt"))
(assert-eq "rename onto another" :nil (exfat-rename vol "docs/hi.txt" "empty"))
(assert-eq "rename of nothing" :nil (exfat-rename vol "nothing" "something"))
(assert-eq "directory into itself" :nil (exfat-rename vol "docs" "docs/deep/docs"))
(assert-true "move a directory" (exfat-rename vol "docs/deep" "deep"))
(assert-eq "what was in it came too" small (exfat-load vol "deep/note.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;
; delete, and the room it gives back
;;;;;;;;;;;;;;;;;;;;;;;;

(defq before (exfat-free vol))
(exfat-save vol "gone.bin" big)
(assert-eq "room taken" (- before 20480) (exfat-free vol))
(assert-true "delete" (exfat-delete vol "gone.bin"))
(assert-eq "room given back" before (exfat-free vol))
(assert-eq "delete again" :nil (exfat-delete vol "gone.bin"))
(assert-true "delete empty directory" (and (exfat-mkdir vol "bare") (exfat-delete vol "bare")))
(assert-eq "the root is not deleted" :nil (exfat-delete vol ""))

;;;;;;;;;;;;;;;;;;;;;;;;
; the volume says when it is being written
;;;;;;;;;;;;;;;;;;;;;;;;

(assert-eq "not busy" 0 (logand (xf-flags stream) +exfat_flag_busy))
(exfat-begin vol)
(assert-eq "busy" +exfat_flag_busy (logand (xf-flags stream) +exfat_flag_busy))
(exfat-save vol "held.txt" small)
(assert-true "unclean if mounted now" (get :unclean (exfat-mount (string-stream (cat (progn (stream-seek stream 0 2) (str stream)))))))
(exfat-end vol)
(assert-eq "not busy again" 0 (logand (xf-flags stream) +exfat_flag_busy))

;;;;;;;;;;;;;;;;;;;;;;;;
; mounted again, it is all still there
;;;;;;;;;;;;;;;;;;;;;;;;

(defq names (xf-names vol "") free (exfat-free vol) again (exfat-mount stream))
(assert-true "mount again" again)
(assert-true "clean" (not (get :unclean again)))
(assert-list-eq "same names" names (xf-names again ""))
(assert-eq "same room" free (exfat-free again))
(assert-eq "same file" small (exfat-load again "docs/hi.txt"))
(assert-eq "same many" 150 (length (exfat-list again "many")))

;;;;;;;;;;;;;;;;;;;;;;;;
; a small volume of small clusters, broken up, then full
;;;;;;;;;;;;;;;;;;;;;;;;

(bind '(vol stream) (xf-volume 0x100000 :nil 0))
(assert-true "format with 512 byte clusters" vol)
(assert-eq "small cluster size" 512 (exfat-cluster-size vol))
(exfat-begin vol)
(each (# (exfat-save vol (cat "f" (str %0)) (xf-bytes 3000 %0))) (range 0 40))
(each (# (exfat-delete vol (cat "f" (str %0)))) (range 0 40 2))
(exfat-end vol)
;all the room after them is taken, which leaves the gaps of 6 clusters
(assert-true "fill the room after" (exfat-save vol "filler.bin" (xf-bytes (- (exfat-free vol) (* 20 3072)) 3)))
;no run of free clusters is long enough, so this one has a chain
(defq spread (xf-bytes 30000 99))
(assert-true "save into the gaps" (exfat-save vol "spread.bin" spread))
(assert-eq "it has a chain" :nil (elem-get (exfat-find vol "spread.bin") 4))
(assert-eq "load from the gaps" spread (exfat-load vol "spread.bin"))
(assert-eq "those left alone are whole" (xf-bytes 3000 7) (exfat-load vol "f7"))

;fill it
(defq room (exfat-free vol) most (xf-bytes (- room 2048) 5))
(assert-true "nearly fill" (exfat-save vol "most.bin" most))
(assert-eq "too big to fit" :nil (exfat-save vol "over.bin" (xf-bytes 8192 6)))
(assert-eq "nothing left behind" :nil (exfat-find vol "over.bin"))
(assert-eq "room as it was" 2048 (exfat-free vol))
;no room for old and new together, the old is let go first
(defq other (xf-bytes (- room 1024) 8))
(assert-true "replace when nearly full" (exfat-save vol "most.bin" other))
(assert-eq "replaced" other (exfat-load vol "most.bin"))
;no room even so, and the old is still there
(assert-eq "replace too big" :nil (exfat-save vol "most.bin" (xf-bytes (+ room 4096) 9)))
(assert-eq "old still there" other (exfat-load vol "most.bin"))
(assert-eq "broken up file still whole" spread (exfat-load vol "spread.bin"))
(defq again (exfat-mount stream))
(assert-eq "full volume mounts again" other (exfat-load again "most.bin"))
(assert-eq "same room on the full volume" (exfat-free vol) (exfat-free again))

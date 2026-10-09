(report-header "System: FAT32, a volume in a memory stream, read only")

(import "lib/fs/fat32.inc")

;there is nothing to format a FAT32 volume with, it is only read, so
;the test lays one out by hand. Sectors of 512 bytes, a cluster a
;sector, 32 reserved sectors, two tables of 8 sectors, and the root
;at cluster 2
(defq +ff_sectors 1024 +ff_reserved 32 +ff_fat_sectors 8 +ff_heap (+ 32 8 8))

(defun ff-fat (image cluster next)
	;an entry of the table, in both copies of it
	(set-int image (+ (* +ff_reserved 512) (* cluster 4)) next)
	(set-int image (+ (* (+ +ff_reserved +ff_fat_sectors) 512) (* cluster 4)) next))

(defun ff-new (label)
	;an empty volume, as a string to be filled in
	(defq image (exfat-zeros (* +ff_sectors 512)))
	(set-short image 11 512) (set-byte image 13 1) (set-short image 14 +ff_reserved)
	(set-byte image 16 2) (set-int image 32 +ff_sectors) (set-int image 36 +ff_fat_sectors)
	(set-int image 44 2) (set-short image 510 0xaa55)
	(each (# (set-byte image (+ 71 (!)) (code %0))) (pad label 11 "           "))
	(each (# (set-byte image (+ 82 (!)) (code %0))) "FAT32   ")
	(ff-fat image 0 0x0ffffff8) (ff-fat image 1 0x0fffffff) (ff-fat image 2 0x0fffffff)
	image)

(defun ff-put (image clusters data)
	;data in these clusters, chained in this order
	(each (lambda (cluster)
		(defq part (slice data (* (!) 512) (min (length data) (* (inc (!)) 512))))
		(each (# (set-byte image (+ (* (+ +ff_heap (- cluster 2)) 512) (!)) (code %0))) part)
		(ff-fat image cluster (if (= (!) (dec (length clusters))) 0x0fffffff (elem-get clusters (inc (!))))))
		clusters)
	image)

(defun ff-entry (name attrs cluster len &optional flags)
	;the entry of a file, its name as 11 characters
	(setf-> (cat (pad name 11 "           ") (exfat-zeros 21))
		(+fat32_entry_attributes attrs) (+fat32_entry_case (ifn flags 0))
		(+fat32_entry_cluster_hi (>> cluster 16)) (+fat32_entry_cluster_lo (logand cluster 0xffff))
		(+fat32_entry_length len)))

(defun ff-long (name short &optional sum)
	;the entries of a long name, the last part first, for the file
	;with this 8 and 3 name
	(defq units (exfat-units name) out (list))
	(setd sum (fat32-checksum (pad short 11 "           ")))
	(if (/= (% (length units) 13) 0) (push units 0))
	(while (/= (% (length units) 13) 0) (push units 0xffff))
	(defq count (/ (length units) 13))
	(each (lambda (i)
		(defq part (apply (const cat) (map (# (char %0 2)) (slice units (* i 13) (* (inc i) 13))))
			e (exfat-zeros 32))
		(setf-> e (+fat32_long_order (+ (inc i) (if (= i (dec count)) +fat32_long_last 0)))
			(+fat32_long_attributes +fat32_attr_long) (+fat32_long_checksum sum))
		(each (# (set-byte e (+ +fat32_long_name1 (!)) (code %0))) (slice part 0 10))
		(each (# (set-byte e (+ +fat32_long_name2 (!)) (code %0))) (slice part 10 22))
		(each (# (set-byte e (+ +fat32_long_name3 (!)) (code %0))) (slice part 22 26))
		(push out e)) (range (dec count) -1 -1))
	(apply (const cat) out))

(defun ff-bytes (len seed)
	;bytes that are not all the same, and differ with the seed
	(defq out (cat (exfat-zeros len)))
	(each (# (set-byte out %0 (logand (+ (* %0 7) (>> %0 8) seed) 0xff))) (range 0 len))
	out)

(defun ff-names (vol path)
	(sort (map (const first) (fat32-list vol path))))

;;;;;;;;;;;;;;;;;;;;;;;
; a volume, and its root
;;;;;;;;;;;;;;;;;;;;;;;

(defq image (ff-new "BOOTNAME")
	plain (ff-bytes 700 1) spread (ff-bytes 1300 2) inner (ff-bytes 40 3)
	thirteen "thirteen.char" fourteen "fourteen.chars" accent "café résumé.txt")
;the root is two clusters, 10 and 3, with more entries than one holds
(ff-put image '(2 10) (cat
	(ff-entry "ROOTNAME" +fat32_attr_label 0 0)
	(ff-entry "README  TXT" 0x20 4 700)
	(ff-entry "NOTES   TXT" 0x20 4 700 (+ +fat32_case_name +fat32_case_ext))
	(ff-entry "HALF    TXT" 0x20 4 700 +fat32_case_ext)
	(ff-entry (cat (char 0xe5) "ONE    TXT") 0x20 4 700)
	(ff-long thirteen "THIRTE~1CHA") (ff-entry "THIRTE~1CHA" 0x20 4 700)
	(ff-long fourteen "FOURTE~1CHA") (ff-entry "FOURTE~1CHA" 0x20 4 700)
	(ff-long accent "CAFRSU~1TXT") (ff-entry "CAFRSU~1TXT" 0x20 4 700)
	;a long name that is not this file's, its checksum is another's
	(ff-long "not my name.txt" "OTHER   TXT" 0x11) (ff-entry "ORPHAN  TXT" 0x20 4 700)
	(ff-entry "SPREAD  BIN" 0x20 20 1300)
	(ff-entry "EMPTY   DAT" 0x20 0 0)
	(ff-entry "SUB        " +fat32_attr_dir 30 0)
	(ff-entry "LOOP    BIN" 0x20 40 100000)
	(ff-entry "TOPBITS BIN" 0x20 50 1024)))
(ff-put image '(4 5) plain)
;a file whose clusters are neither together nor in order
(ff-put image '(20 25 22) spread)
(ff-put image '(30) (cat
	(ff-entry ".          " +fat32_attr_dir 30 0)
	(ff-entry "..         " +fat32_attr_dir 0 0)
	(ff-long "a file in a directory.txt" "AFILEI~1TXT") (ff-entry "AFILEI~1TXT" 0x20 31 40)
	(ff-entry "DEEPER     " +fat32_attr_dir 32 0)))
(ff-put image '(31) inner)
(ff-put image '(32) (cat
	(ff-entry ".          " +fat32_attr_dir 32 0)
	(ff-entry "..         " +fat32_attr_dir 30 0)
	(ff-entry "LAST    TXT" 0x20 31 40)))
;a chain that goes round on itself
(ff-fat image 40 41) (ff-fat image 41 40)
;an entry of the table with its top 4 bits set, they are not part of it
(ff-put image '(50 51) (ff-bytes 1024 4))
(ff-fat image 50 0xf0000033)

(defq vol (fat32-mount (string-stream image)))
(assert-true "mount" vol)
(assert-eq "cluster size" 512 (exfat-cluster-size vol))
(assert-eq "the name the root gives" "ROOTNAME" (get :label vol))
(assert-list-eq "the root, over two clusters, a deleted file left out"
	(sort (list "README.TXT" "notes.txt" "HALF.txt" thirteen fourteen accent
		"ORPHAN.TXT" "SPREAD.BIN" "EMPTY.DAT" "SUB" "LOOP.BIN" "TOPBITS.BIN"))
	(ff-names vol ""))
(assert-eq "a file" plain (fat32-load vol "README.TXT"))
(assert-eq "a name shown in lower case" plain (fat32-load vol "/notes.txt"))
(assert-eq "found with no regard to case" plain (fat32-load vol "ReadMe.txt"))
(assert-eq "a long name of 13, with no end mark" plain (fat32-load vol thirteen))
(assert-eq "a long name of 14" plain (fat32-load vol fourteen))
(assert-eq "a long name with accents" plain (fat32-load vol accent))
(assert-eq "and in another case" plain (fat32-load vol "CAFÉ RÉSUMÉ.TXT"))
(assert-eq "a long name that is another file's is not used" :nil (fat32-find vol "not my name.txt"))
(assert-eq "clusters out of order" spread (fat32-load vol "spread.bin"))
(assert-list-eq "its chain" '(20 25 22) (fat32-chain vol 20))
(assert-eq "an empty file" "" (fat32-load vol "empty.dat"))
(assert-eq "a chain that goes round is cut off" (get :cluster_count vol)
	(length (fat32-chain vol 40)))
(assert-eq "the top bits of a table entry are left out" (ff-bytes 1024 4) (fat32-load vol "topbits.bin"))
(assert-eq "not there" :nil (fat32-find vol "nothing.txt"))
(assert-eq "a file is not a directory" :nil (fat32-list vol "README.TXT"))
(assert-eq "a directory is not a file" :nil (fat32-load vol "SUB"))

;;;;;;;;;;;;;;;;;;;;;
; directories, a walk
;;;;;;;;;;;;;;;;;;;;;

(assert-list-eq "a directory, without its dots" '("DEEPER" "a file in a directory.txt") (ff-names vol "SUB"))
(assert-eq "a file in a directory" inner (fat32-load vol "/sub/A File In A Directory.TXT"))
(assert-eq "and deeper" inner (fat32-load vol "SUB/DEEPER/LAST.TXT"))
(assert-eq "a path, found, is every entry down to it" 4 (length (fat32-path vol "SUB/DEEPER/LAST.TXT")))
(bind '(name dir size start no_fat index count) (fat32-find vol "sub/a file in a directory.txt"))
(assert-list-eq "an entry" '(:nil 40 31 :nil 2 2) (list dir size start no_fat index count))
(defq walked (list))
(fat32-walk vol "" (lambda (path entry) (push walked path)))
(assert-eq "a walk finds them all" 15 (length walked))
(assert-true "a directory before what is in it"
	(< (find "/SUB" walked) (find "/SUB/DEEPER" walked) (find "/SUB/DEEPER/LAST.TXT" walked)))

;;;;;;;;;;;;;;;;;;;;;;
; what is not a volume
;;;;;;;;;;;;;;;;;;;;;;

;with no name in the root the one in the first sector is the name
(defq image (ff-new "BOOTNAME"))
(assert-eq "the name the first sector gives" "BOOTNAME" (get :label (fat32-mount (string-stream image))))
(assert-eq "an empty root" 0 (length (fat32-list (fat32-mount (string-stream image)) "")))
(assert-eq "not a volume" :nil (fat32-mount (string-stream (cat (exfat-zeros 0x10000)))))
;a FAT16 volume has a root of a set size, and the length of its table at 22
(defq image (ff-new "FAT16")) (set-short image 17 512) (set-short image 22 8)
(assert-eq "a FAT16 volume is not taken" :nil (fat32-mount (string-stream image)))
(defq image (ff-new "NOMARK")) (set-short image 510 0)
(assert-eq "no mark at the end of the first sector" :nil (fat32-mount (string-stream image)))
;and an exFAT volume is not one, nor a FAT32 one an exFAT
(defq stream (string-stream (cat (exfat-zeros 0x200000))))
(exfat-format stream 0x200000 "EX")
(assert-eq "an exFAT volume is not FAT32" :nil (fat32-mount stream))
(assert-eq "a FAT32 volume is not exFAT" :nil (exfat-mount (string-stream (ff-new "FAT"))))

(undef (env) 'image 'vol 'plain 'spread 'inner 'walked 'stream)

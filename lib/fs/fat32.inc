;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FAT32, the file system, read from a volume. It is not written.
; It is here for the volumes that are FAT32 and will stay so, the boot
; partition of a Raspberry Pi is one. exFAT is the file system proper.
;
; A volume is anything that gives bytes at an offset, as it is for
; exFAT, and the device is read through the blocks exFAT keeps. What
; is found is given as exFAT gives it, so what reads the one reads
; the other.
;
; Nothing here calls itself, a walk keeps its own list of what is
; still to do.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;a volume is an environment, with
;	:stream, the device, :file if it is a file of the host
;	:sector_shift :cluster_shift, the sizes of a sector and a cluster
;	:fat_offset :heap_offset, in sectors, :cluster_count :root
;	:cache :cached :dirty, the blocks of the device last read
;	:upcase, the upper case of each character that has one
;	:label, the name of the volume
;an entry of a directory is (name dir size start no_fat index count)
;	start is its first cluster, no_fat is always :nil, the chain of a
;	file is always in the table
;	index is where its entries are in the directory, count how many
;	entries of a long name come before the one for the file
;a path, found, is the entries from the root down to it, (root ... entry)

(import "./exfat.inc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the structures on the disk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the first sector of the volume. Its fields sit where they fall, so
;those that are not on a boundary of their own size are read by hand
(defq +fat32_boot_sector_size 11 +fat32_boot_cluster_sectors 13
	+fat32_boot_reserved 14 +fat32_boot_fats 16 +fat32_boot_root_entries 17
	+fat32_boot_fat16_length 22 +fat32_boot_fat_length 36
	+fat32_boot_root 44 +fat32_boot_label 71 +fat32_boot_type 82
	+fat32_boot_mark 510 +fat32_boot_size 512)

;a directory is entries of 32 bytes. One for a file, its name as 8 and 3
;characters, and before it, last part first, those of its long name
(structure +fat32_entry 0
	(struct name 11)
	(ubyte attributes case create_10ms)
	(ushort create_time create_date access_date cluster_hi
		modify_time modify_date cluster_lo)
	(uint length))

(structure +fat32_long 0
	(ubyte order)
	(struct name1 10)
	(ubyte attributes type checksum)
	(struct name2 12)
	(ushort zero)
	(struct name3 4))

(defq +fat32_attr_label 0x08 +fat32_attr_dir 0x10 +fat32_attr_long 0x0f
	+fat32_case_name 0x08 +fat32_case_ext 0x10 +fat32_long_last 0x40
	+fat32_free 0xe5 +fat32_mask 0x0fffffff +fat32_bad 0x0ffffff7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the volume, and its chains
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fat32-next (vol cluster)
	;the cluster that follows this one, the top 4 bits are not part of it.
	;Every file has a chain, so it is read from the block as it is kept,
	;an entry of the table never lies over the end of a block
	(defq offset (+ (<< (get :fat_offset vol) (get :sector_shift vol)) (* cluster 4)))
	(logand (get-uint (exfat-block vol (>> offset +exfat_block_shift))
		(logand offset (const (dec +exfat_block_size)))) +fat32_mask))

(defun fat32-chain (vol start)
	; (fat32-chain vol start) -> (cluster ...)
	;the clusters of a file or a directory. A chain that goes round on
	;itself is cut off at the size of the volume
	(defq count (get :cluster_count vol) limit (+ count 2) clusters (list))
	(while (and (>= start 2) (< start limit) (< (length clusters) count))
		(push clusters start)
		(setq start (fat32-next vol start)))
	clusters)

(defun fat32-data (vol start len &optional keep)
	; (fat32-data vol start len [keep]) -> str
	;the bytes of a file or a directory, a run of clusters is one read.
	;A len of :nil is all of the chain, a directory has no length. keep is
	;for a directory, its blocks are kept
	(defq size (exfat-cluster-size vol) parts (list)
		clusters (fat32-chain vol start) i 0)
	(if len (setq clusters (slice clusters 0 (min (length clusters) (/ (+ len (dec size)) size)))))
	(defq n (length clusters))
	(while (< i n)
		(defq j (inc i))
		(while (and (< j n) (= (elem-get clusters j) (+ (elem-get clusters i) (- j i))))
			(setq j (inc j)))
		(push parts (exfat-read vol (exfat-cluster-offset vol (elem-get clusters i)) (* (- j i) size) keep))
		(setq i j))
	(defq data (apply (const cat) (cat (list "") parts)))
	(if len (slice data 0 (min len (length data))) data))

(defun fat32-shift (n)
	;the power of 2 that n is, :nil if it is not one
	(defq shift 0)
	(while (< (<< 1 shift) n) (setq shift (inc shift)))
	(if (and (> n 0) (= (<< 1 shift) n)) shift))

(defun fat32-mount (device)
	; (fat32-mount path | stream) -> :nil | vol
	;a path is a file of the host that is the image of a disk. A stream
	;can be a memory stream. A volume that is FAT12 or FAT16 is not taken.
	(when (defq stream (if (str? device) (file-stream device) device))
		(def (defq vol (env 1)) :stream stream :file (str? device)
			:cache (Fmap 101) :cached 0 :dirty (Fmap 31) :depth 0)
		(defq boot (exfat-read vol 0 +fat32_boot_size))
		(when (and boot (= (length boot) +fat32_boot_size)
				(= (get-ushort boot +fat32_boot_mark) 0xaa55)
				(= (get-ushort boot +fat32_boot_root_entries) 0)
				(= (get-ushort boot +fat32_boot_fat16_length) 0)
				(defq sector_shift (fat32-shift (get-ushort boot +fat32_boot_sector_size)))
				(defq cluster_shift (fat32-shift (get-ubyte boot +fat32_boot_cluster_sectors)))
				(> (defq fat_length (get-uint boot +fat32_boot_fat_length)) 0)
				(> (defq fats (get-ubyte boot +fat32_boot_fats)) 0))
			(defq reserved (get-ushort boot +fat32_boot_reserved)
				heap_offset (+ reserved (* fats fat_length))
				total (get-uint boot 32))
			(if (= total 0) (setq total (get-ushort boot 19)))
			(def vol
				:fat_offset reserved :heap_offset heap_offset
				:cluster_count (min (>> (- total heap_offset) cluster_shift)
					(- (/ (<< fat_length sector_shift) 4) 2))
				:root (get-uint boot +fat32_boot_root)
				:sector_shift sector_shift
				:cluster_shift (+ sector_shift cluster_shift)
				:label (trim (slice boot +fat32_boot_label (+ +fat32_boot_label 11)))
				:upcase (Fmap 101))
			;names are found with no regard to case, by the table exFAT has
			(exfat-upcase-load vol (exfat-upcase-table))
			;the name of the volume, if the root has one, is the one to go by
			(defq data (fat32-data vol (get :root vol) :nil :t) i 0 lead 1)
			(while (and (< i (length data)) (/= lead 0))
				(defq e (slice data i (+ i +fat32_entry_size))
					lead (code e 1 0) attrs (getf e +fat32_entry_attributes))
				(when (and (/= lead 0) (/= lead +fat32_free)
						(= (logand attrs +fat32_attr_long) +fat32_attr_label))
					(def vol :label (trim (getf e +fat32_entry_name)))
					(setq lead 0))
				(setq i (+ i +fat32_entry_size)))
			vol)))

;;;;;;;;;;;;;;;;;;;;;
; names and entries
;;;;;;;;;;;;;;;;;;;;;

(defun fat32-short (e)
	;the 8 and 3 name of an entry. It is held in upper case, with a bit
	;each for a name and an end that are to be shown in lower. A character
	;past 127 is of a code page that is not known here, it is taken as
	;Latin 1
	(defq flags (getf e +fat32_entry_case) name (getf e +fat32_entry_name)
		base (trim-end (slice name 0 8)) ext (trim-end (slice name 8 11)))
	(if (= (code base 1 0) 0x05) (setq base (cat (char 0xe5) (slice base 1 -1))))
	(if (/= (logand flags +fat32_case_name) 0) (setq base (to-lower base)))
	(if (/= (logand flags +fat32_case_ext) 0) (setq ext (to-lower ext)))
	(apply (const cat) (map (# (num-to-utf8 (code %0)))
		(if (nempty? ext) (cat base "." ext) base))))

(defun fat32-checksum (name)
	;the checksum of an 8 and 3 name, each part of a long name has it
	(reduce (# (logand (+ (if (= (logand %0 1) 1) 0x80 0) (>> %0 1) (code %1)) 0xff)) name 0))

(defun fat32-entries (vol start)
	; (fat32-entries vol start) -> ((name dir size start no_fat index count) ...)
	;the files and directories of a directory. The parts of a long name
	;come before the entry of their file, the last part first, and are
	;its name if they are all there and their checksum is that of its
	;8 and 3 name. Then that is its name.
	(defq data (fat32-data vol start :nil :t) out (list) i 0
		n (/ (length data) +fat32_entry_size) lead 1
		parts (list) sum -1 want 0)
	(while (and (< i n) (/= lead 0))
		(defq e (slice data (* i +fat32_entry_size) (* (inc i) +fat32_entry_size))
			lead (code e 1 0) attrs (getf e +fat32_entry_attributes))
		(cond
			((or (= lead 0) (= lead +fat32_free))
				(setq want 0))
			((= (logand attrs +fat32_attr_long) +fat32_attr_long)
				;a part of a long name
				(bind '(order check) (getf-> e +fat32_long_order +fat32_long_checksum))
				(defq units (cat (getf e +fat32_long_name1) (getf e +fat32_long_name2)
					(getf e +fat32_long_name3)))
				(cond
					((/= (logand order +fat32_long_last) 0)
						(setq parts (list units) sum check
							want (dec (logand order 0x3f))))
					((and (> want 0) (= (logand order 0x3f) want) (= check sum))
						(push parts units)
						(setq want (dec want)))
					((setq want -1))))
			((/= (logand attrs +fat32_attr_label) 0)
				;the name of the volume
				(setq want 0 parts (list)))
			(:t ;a file or a directory
				(defq short (getf e +fat32_entry_name) count 0 name :nil)
				(when (and (= want 0) (nempty? parts) (= (fat32-checksum short) sum))
					;the parts are last first, and the name ends at a 0
					(defq units (apply (const cat) (reverse parts)) len 0)
					(while (and (< (* len 2) (length units)) (/= (get-ushort units (* len 2)) 0))
						(setq len (inc len)))
					(setq name (exfat-name units len) count (length parts)))
				(unless name (setq name (fat32-short e)))
				(unless (or (eql name ".") (eql name ".."))
					(push out (list name (/= (logand attrs +fat32_attr_dir) 0)
						(getf e +fat32_entry_length)
						(+ (<< (getf e +fat32_entry_cluster_hi) 16) (getf e +fat32_entry_cluster_lo))
						:nil (- i count) count)))
				(setq want 0 parts (list))))
		(setq i (inc i)))
	out)

;;;;;;;;;;;;;;;;;;;;;;;;;;
; finding and reading
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fat32-root (vol)
	;the root as an entry
	(list "" :t 0 (get :root vol) :nil -1 0))

(defun fat32-dir (vol entry)
	; (fat32-dir vol entry) -> ((name dir size start no_fat index count) ...)
	;what is in a directory
	(fat32-entries vol (elem-get entry 3)))

(defun fat32-path (vol path)
	; (fat32-path vol path) -> :nil | (root ... entry)
	;the entries from the root down to a path
	(defq found (list (fat32-root vol)))
	(each (lambda (part)
		(when found
			(defq entry (if (second (last found))
				(some (# (if (exfat-same? vol (first %0) part) %0))
					(fat32-dir vol (last found)))))
			(if entry (push found entry) (setq found :nil))))
		(filter (# (nempty? %0)) (split path "/")))
	found)

(defun fat32-find (vol path)
	; (fat32-find vol path) -> :nil | (name dir size start no_fat index count)
	(if (defq found (fat32-path vol path)) (last found)))

(defun fat32-list (vol path)
	; (fat32-list vol path) -> :nil | ((name dir size start no_fat index count) ...)
	(when (and (defq entry (fat32-find vol path)) (second entry))
		(fat32-dir vol entry)))

(defun fat32-load (vol path)
	; (fat32-load vol path) -> :nil | str
	(when (and (defq entry (fat32-find vol path)) (not (second entry)))
		(fat32-data vol (elem-get entry 3) (third entry))))

(defun fat32-walk (vol path fnc)
	; (fat32-walk vol path fnc) -> vol
	; (fnc path entry)
	;every file and directory under a path, a directory before what is in
	;it. It keeps a list of the directories still to do, it does not call
	;itself, so the depth of the tree costs no stack.
	(defq todo (list path))
	(while (defq path (pop todo))
		(defq dirs (list))
		(each (lambda (entry)
			(defq child (cat path "/" (first entry)))
			(fnc child entry)
			(if (second entry) (push dirs child))) (ifn (fat32-list vol path) '()))
		;so that they come off the list in the order they were found
		(reach (# (push todo %0)) dirs))
	vol)

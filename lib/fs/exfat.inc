;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; exFAT, the file system, read from and written to a volume.
; A volume is anything that gives bytes at an offset, here a file of
; the host that is the image of a disk. Nothing of the host's own
; file system code is used, the structures on the disk are read here.
;
; Nothing here calls itself. A task has a small stack, and a tree of
; directories can be as deep as it likes, so a walk keeps its own list
; of what is still to do.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;a volume is an environment, with
;	:stream, the device, :file if it is a file of the host
;	:sector_shift :cluster_shift, the sizes of a sector and a cluster
;	:fat_offset :heap_offset, in sectors, :cluster_count :root
;	:bitmap, a bit for each cluster that is in use, and where it is kept,
;	:dirty_lo :dirty_hi the bytes of it that have changed, :hint where
;	the next free cluster is looked for
;	:marks, the bitmap as it is written first, the clusters let go since
;	it was last written still in use, :freed if there are any. A free
;	cluster is looked for in it, not in the bitmap
;	:cache :cached, the blocks of the device last read, :dirty those of
;	them that were changed and are still to be written
;	:depth, how many (exfat-begin) are open, :unclean if the volume was
;	being written when it was last let go of
;	:upcase, the upper case of each character that has one
;	:free, how many clusters are free, once it has been counted
;	:dirs, each directory that has been read, by its first cluster: its
;	entries, and them by the upper case of their names. A directory is
;	read again only when it has been written, (exfat-dir-put)
;an entry of a directory is (name dir size start no_fat index count)
;	start is its first cluster. no_fat is a file whose clusters run on
;	from there, with no chain
;	index is where its entries are in the directory, count how many more
;	follow the first of them
;a path, found, is the entries from the root down to it, (root ... entry)

(import "././date/date.inc")
(import "./upcase.inc")

;the first run of so many clear bits of a bitmap, lib/fs/lisp.vp
(ffi "lib/fs/fs_bit_run" fs-bit-run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the structures on the disk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the first sector of the volume
(structure +exfat_boot 0
	(struct jump 3)
	(struct name 8)
	(struct zero 53)
	(long partition_offset volume_length)
	(uint fat_offset fat_length heap_offset cluster_count root serial)
	(ushort revision flags)
	(ubyte sector_shift cluster_shift fats drive percent))

;a directory is entries of 32 bytes, the first byte is the type. A file
;is a file entry, a stream entry, then name entries
(enums +exfat_type 0
	(enum end))
(defq +exfat_type_bitmap 0x81 +exfat_type_upcase 0x82 +exfat_type_label 0x83
	+exfat_type_file 0x85 +exfat_type_stream 0xc0 +exfat_type_name 0xc1
	+exfat_attr_dir 0x10 +exfat_attr_archive 0x20
	+exfat_flag_alloc 1 +exfat_flag_no_fat 2 +exfat_name_units 15
	+exfat_last 0xffffffff +exfat_in_use 0x80 +exfat_flag_busy 2)

(structure +exfat_entry 0
	(ubyte type)
	(struct rest 31))

(structure +exfat_file 0
	(ubyte type count)
	(ushort checksum attributes reserved1)
	(uint create modify access)
	(ubyte create_10ms modify_10ms create_tz modify_tz access_tz)
	(struct reserved2 7))

(structure +exfat_stream 0
	(ubyte type flags reserved1 name_length)
	(ushort name_hash reserved2)
	(long valid_length)
	(uint reserved3 first_cluster)
	(long data_length))

(structure +exfat_name 0
	(ubyte type flags)
	(struct name 30))

(structure +exfat_bitmap 0
	(ubyte type flags)
	(struct reserved 18)
	(uint first_cluster)
	(long data_length))

(structure +exfat_upcase 0
	(ubyte type)
	(struct reserved1 3)
	(uint checksum)
	(struct reserved2 12)
	(uint first_cluster)
	(long data_length))

(structure +exfat_label 0
	(ubyte type length)
	(struct label 22)
	(struct reserved 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the device, in blocks of sectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;a real device is read and written a whole sector at a time, at the start
;of a sector, and each read and write is slow. So the device is only ever
;asked for whole blocks, a block is 8 sectors, and the blocks last read
;are kept. A small write changes the blocks that are kept, and they go to
;the device together, a run of them as one write, at (exfat-flush). A file
;that is the image of a disk does not care, and is treated the same.

(defq +exfat_block_shift 12 +exfat_block_size 4096 +exfat_cache_blocks 512
	;this many whole blocks, or more, are written straight to the device
	+exfat_direct_blocks 4
	;between an (exfat-begin) and its (exfat-end) such writes wait, as they
	;are, till there are this many bytes of them
	+exfat_pending_most 4194304)

(defun exfat-raw-write (vol offset data)
	;whole sectors to the device
	(task-breathe)
	(defq stream (get :stream vol))
	(stream-seek stream offset 0)
	(write-blk stream data))

(defun exfat-pending-write (vol)
	; (exfat-pending-write vol) -> vol
	;the writes of whole blocks that wait, to the device, in the order
	;they are on it, those that are next to each other as one
	(when (nempty? (defq pending (get :pending vol)))
		(sort pending (# (- (first %0) (first %1))))
		(defq i 0 n (length pending))
		(while (< i n)
			(defq at (first (elem-get pending i)) end at parts (list "") j i)
			(while (and (< j n) (= (first (elem-get pending j)) end))
				(push parts (second (elem-get pending j)))
				(setq end (+ end (length (second (elem-get pending j)))) j (inc j)))
			(exfat-raw-write vol at (apply (const cat) parts))
			(setq i j))
		(def vol :pending (list) :pending_size 0))
	vol)

(defun exfat-pending-over (vol offset len)
	; (exfat-pending-over vol offset len) -> vol
	;what waits is written now if any of it is of those bytes: what is read
	;of them is what was written, and a write of them again is the last
	(defq end (+ offset len))
	(if (some (# (and (< (first %0) end) (> (+ (first %0) (length (second %0))) offset))) (get :pending vol))
		(exfat-pending-write vol))
	vol)

(defun exfat-raw-read (vol offset len)
	;whole blocks from the device, :nil if they are not all there. Each
	;time the device is gone to, the task gives way first: a volume at
	;work is a great deal of reading and writing, and the node is not its
	;alone
	(task-breathe)
	(exfat-pending-over vol offset len)
	(defq stream (get :stream vol))
	(stream-seek stream offset 0)
	(and (defq data (read-blk stream len)) (= (length data) len) data))

(defun exfat-drop (vol index)
	;a block is no longer kept, and is not to be written
	(when (. (defq cache (get :cache vol)) :find index)
		(. cache :erase index)
		(. (get :dirty vol) :erase index)
		(def vol :cached (dec (get :cached vol)))))

(defun exfat-zeros (len)
	; (exfat-zeros len) -> str
	;always a string of its own, it is often changed in place
	(defq z (const (char 0 8)))
	(while (<= (length z) len) (setq z (cat z z)))
	(slice z 0 len))

(defun exfat-cluster-size (vol)
	(<< 1 (get :cluster_shift vol)))

(defun exfat-cluster-offset (vol cluster)
	;where a cluster is on the device, the first is cluster 2
	(+ (<< (get :heap_offset vol) (get :sector_shift vol))
		(<< (- cluster 2) (get :cluster_shift vol))))

(defun exfat-stream-flush (vol)
	;what the stream holds, to the device, if it is a file
	(if (get :file vol) (stream-flush (get :stream vol)))
	vol)

(defun exfat-bitmap-write (vol bitmap)
	;the clusters of a bitmap, of the bytes of it that have changed, to the
	;device now, whole sectors, as they are not kept: the blocks of them
	;that are kept are let go
	(defq size (exfat-cluster-size vol) clusters (get :bitmap_clusters vol))
	(each! (lambda () (defq i (!)) (defq at (* i size) part (slice bitmap at (min (length bitmap) (+ at size)))
				offset (exfat-cluster-offset vol (elem-get clusters i)))
			(each! (# (exfat-drop vol (!))) '() (>> offset +exfat_block_shift) (>> (+ offset (length part) (const (dec +exfat_block_size))) +exfat_block_shift))
			(exfat-raw-write vol offset
				(cat part (exfat-zeros (- (logand (+ (length part) 511) -512) (length part)))))) '() (/ (get :dirty_lo vol) size) (inc (/ (get :dirty_hi vol) size))))

(defun exfat-flush (vol)
	; (exfat-flush vol) -> vol
	;the blocks that were changed, to the device, in the order they are on
	;it, and those that are next to each other as one write. A volume let
	;go of part way, its task killed, has at worst room lost, never a
	;cluster that is had and marked free: first the bitmap with the
	;clusters given out in use and those let go still in use, the marks,
	;then what has them or let them go, then the bitmap as it is
	(defq lo (get :dirty_lo vol))
	(when (>= lo 0)
		(exfat-bitmap-write vol (get :marks vol))
		(exfat-stream-flush vol))
	(exfat-pending-write vol)
	(defq cache (get :cache vol) dirty (list))
	(. (get :dirty vol) :each (lambda (index _) (push dirty index)))
	(sort dirty (const -))
	(defq i 0 n (length dirty))
	(while (< i n)
		(defq j (inc i))
		(while (and (< j n) (= (elem-get dirty j) (+ (elem-get dirty i) (- j i))))
			(setq j (inc j)))
		(exfat-raw-write vol (<< (elem-get dirty i) +exfat_block_shift)
			(apply (const cat) (cat (list "") (map! (# (. cache :find %0)) (list dirty) i j))))
		(setq i j))
	(. (get :dirty vol) :empty)
	(when (>= lo 0)
		(when (get :freed vol)
			(exfat-stream-flush vol)
			(exfat-bitmap-write vol (get :bitmap vol))
			(def vol :marks (cat (get :bitmap vol)) :freed :nil))
		(def vol :dirty_lo -1 :dirty_hi -1))
	vol)

(defun exfat-keep (vol index block)
	;a block is kept. When there are too many, those that were changed go
	;to the device and all are let go
	(defq cache (get :cache vol))
	(unless (. cache :find index)
		(when (>= (get :cached vol) +exfat_cache_blocks)
			(exfat-flush vol)
			(. cache :empty)
			(def vol :cached 0))
		(def vol :cached (inc (get :cached vol))))
	(. cache :insert index block)
	block)

(defun exfat-block (vol index)
	;a block of the device, from those kept if it is there
	(or (. (get :cache vol) :find index)
		;the last block of a device can be short of a whole block
		(exfat-keep vol index (ifn (exfat-raw-read vol (<< index +exfat_block_shift) +exfat_block_size)
				(apply (const cat) (cat (list "") (filter (const identity) (map! (lambda () (exfat-raw-read vol (+ (<< index +exfat_block_shift) (* (!) 512)) 512)) '() 0 8))))))))

(defun exfat-read (vol offset len &optional keep)
	; (exfat-read vol offset len [keep]) -> str
	;bytes of the device. A block that is kept comes from where it is kept,
	;it may have been changed. A run of blocks that are not is one read of
	;the device, and they are kept if the run is short or if keep is asked
	;for, as it is for a directory, which is read over and over.
	(defq lo (>> offset +exfat_block_shift) hi (>> (+ offset len (const (dec +exfat_block_size))) +exfat_block_shift)
		base (<< lo +exfat_block_shift) cache (get :cache vol) parts (list "") i lo)
	(while (< i hi)
		(cond
			((defq block (. cache :find i))
				(push parts block)
				(setq i (inc i)))
			(:t (defq j (inc i))
				(while (and (< j hi) (not (. cache :find j))) (setq j (inc j)))
				(defq data (if (> (- j i) 1)
						(exfat-raw-read vol (<< i +exfat_block_shift) (<< (- j i) +exfat_block_shift))))
				(cond
					;the last block of a device can be short, so a block at a time
					((not data) (each! (# (push parts (exfat-block vol (!)))) '() i j))
					((or keep (<= (- j i) +exfat_direct_blocks))
						(each! (# (push parts (exfat-keep vol (!) (slice data
											(<< (- (!) i) +exfat_block_shift) (<< (- (inc (!)) i) +exfat_block_shift))))) '() i j))
					((push parts data)))
				(setq i j))))
	(slice (apply (const cat) parts) (- offset base) (+ (- offset base) len)))

(defun exfat-write (vol offset data)
	; (exfat-write vol offset data) -> vol
	;bytes to the device. A run of whole blocks goes straight to it. The
	;rest, the ends of a long write and all of a short one, change the
	;blocks that are kept, a block that is only part written is read first.
	;Between an (exfat-begin) and its (exfat-end) the run waits, as it is:
	;the files written one after another are next to each other, and go
	;to the device as one write, not one each, which a flash stick is slow
	;at. A write of what waits writes that first
	(exfat-pending-over vol offset (length data))
	(defq end (+ offset (length data)) dirty (get :dirty vol)
		lo (>> offset +exfat_block_shift) hi (>> (+ end (const (dec +exfat_block_size))) +exfat_block_shift)
		whole_lo (>> (+ offset (const (dec +exfat_block_size))) +exfat_block_shift)
		whole_hi (>> end +exfat_block_shift))
	(if (< (- whole_hi whole_lo) +exfat_direct_blocks) (setq whole_hi whole_lo))
	(each! (lambda () (defq base (<< (!) +exfat_block_shift)
				from (max offset base) to (min end (+ base +exfat_block_size)))
			(cond
				((and (>= (!) whole_lo) (< (!) whole_hi))
					(exfat-drop vol (!)))
				((= (- to from) +exfat_block_size)
					(exfat-keep vol (!) (slice data (- from offset) (- to offset)))
					(. dirty :insert (!) :t))
				(:t (defq old (exfat-block vol (!)))
					(exfat-keep vol (!) (cat (slice old 0 (- from base))
							(slice data (- from offset) (- to offset))
							(slice old (min (length old) (- to base)) -1)))
					(. dirty :insert (!) :t)))) '() lo hi)
	(when (> whole_hi whole_lo)
		(defq at (<< whole_lo +exfat_block_shift)
			run (slice data (- at offset) (- (<< whole_hi +exfat_block_shift) offset)))
		(cond
			((= (get :depth vol) 0) (exfat-raw-write vol at run))
			(:t (push (get :pending vol) (list at run))
				(def vol :pending_size (+ (get :pending_size vol) (length run)))
				(if (> (get :pending_size vol) +exfat_pending_most) (exfat-pending-write vol)))))
	vol)

(defun exfat-sync (vol)
	; (exfat-sync vol) -> vol
	;all that was written is on the device. A file stream holds its last
	;write till it is flushed or closed, and a task that ends with the
	;stream open would lose it. A memory stream is not flushed, a flush
	;ends the string where it stands.
	(exfat-flush vol)
	(exfat-stream-flush vol))

(defun exfat-busy (vol busy)
	;the volume says of itself that it is being written, and that goes to
	;the device at once, before anything else does. If the device is pulled
	;out, or the power goes, whoever mounts it next can tell.
	(defq boot (cat (exfat-block vol 0))
		flags (getf boot +exfat_boot_flags))
	(setf boot +exfat_boot_flags (if busy (logior flags +exfat_flag_busy)
			(logand flags (lognot +exfat_flag_busy))))
	(exfat-raw-write vol 0 (slice boot 0 512))
	(. (get :cache vol) :insert 0 boot)
	(if (get :file vol) (stream-flush (get :stream vol)))
	vol)

(defun exfat-next (vol cluster)
	;the cluster that follows this one, from the allocation table
	(get-uint (exfat-read vol (+ (<< (get :fat_offset vol) (get :sector_shift vol))
				(* cluster 4)) 4) 0))

(defun exfat-chain-part (vol cluster most)
	; (exfat-chain-part vol cluster most) -> (clusters next)
	;of a chain from this cluster on, as many as most, from the allocation
	;table, and the cluster after them, :nil if the chain ends. The table a
	;block of it at a time, the next of each cluster read from the block it
	;is in, not asked of the cache one by one
	(defq limit (+ (get :cluster_count vol) 2) clusters (list)
		fat (<< (get :fat_offset vol) (get :sector_shift vol)) at_block -1 block "")
	(while (and (>= cluster 2) (< cluster limit) (< (length clusters) most))
		(push clusters cluster)
		(defq at (+ fat (<< cluster 2)))
		(unless (= (>> at +exfat_block_shift) at_block)
			(setq at_block (>> at +exfat_block_shift) block (exfat-block vol at_block)))
		(setq cluster (get-uint block (logand at (const (dec +exfat_block_size))))))
	(list clusters (and (>= cluster 2) (< cluster limit) cluster)))

(defun exfat-chain (vol start len no_fat)
	; (exfat-chain vol start len no_fat) -> (cluster ...)
	;the clusters of a file or a directory. A len of :nil is all of the chain
	(defq size (exfat-cluster-size vol) limit (+ (get :cluster_count vol) 2)
		clusters (list))
	(cond
		((< start 2) clusters)
		(no_fat (map! (const !) '() start (+ start (/ (+ len (dec size)) size))))
		((first (exfat-chain-part vol start (if len (/ (+ len (dec size)) size) limit))))))

(defun exfat-data (vol start len no_fat &optional keep)
	; (exfat-data vol start len no_fat [keep]) -> str
	;the bytes of a file or a directory, a run of clusters is one read.
	;keep is for a directory, its blocks are kept
	(defq size (exfat-cluster-size vol) parts (list)
		clusters (exfat-chain vol start len no_fat) i 0 n (length clusters))
	(while (< i n)
		(defq j (inc i))
		(while (and (< j n) (= (elem-get clusters j) (+ (elem-get clusters i) (- j i))))
			(setq j (inc j)))
		(push parts (exfat-read vol (exfat-cluster-offset vol (elem-get clusters i)) (* (- j i) size) keep))
		(setq i j))
	(defq data (apply (const cat) (cat (list "") parts)))
	(if len (slice data 0 len) data))

(defun exfat-ascii? (data n)
	;are the first n bytes of 16 bit characters all ASCII, 8 bytes at a time
	(defq i 0 ascii :t)
	(while (and ascii (< i n))
		(setq ascii (= 0 (logand (code data (defq w (min 8 (- n i))) i) (const 0xff80ff80ff80ff80)))
			i (+ i w)))
	ascii)

(defun exfat-name-units (data units)
	;a name of any characters, a character at a time
	(defq out (list) i 0)
	(while (< i units)
		(defq c (get-ushort data (* i 2)))
		(setq i (inc i))
		;two that make one character between them
		(when (and (<= 0xd800 c 0xdbff) (< i units))
			(defq d (get-ushort data (* i 2)))
			(when (<= 0xdc00 d 0xdfff)
				(setq c (+ 0x10000 (<< (- c 0xd800) 10) (- d 0xdc00)) i (inc i))))
		(push out (num-to-utf8 c)))
	(apply (const cat) (cat (list "") out)))

(defun exfat-name (data units)
	;a name is 16 bit characters, low byte first, here made UTF-8. One of
	;ASCII, as the names of a store's blocks are, is its low bytes, which
	;are what is left of it when it is split at each 0: none of them is 0
	;if there are as many as it has characters
	(defq n (* units 2))
	(if (and (exfat-ascii? data n)
			(= (length (defq out (apply (const cat) (cat (list "") (split (slice data 0 n) (const (char 0))))))) units))
		out
		(exfat-name-units data units)))

(defun exfat-units (name)
	; (exfat-units name) -> (unit ...)
	;a UTF-8 name as the 16 bit characters the disk holds
	(defq out (list) i 0 n (length name))
	(while (< i n)
		(defq c (code name 1 i) more (cond ((< c 0x80) 0) ((< c 0xe0) 1) ((< c 0xf0) 2) (:t 3)))
		(setq c (logand c (elem-get '(0x7f 0x1f 0x0f 0x07) more)) i (inc i))
		(while (and (> more 0) (< i n))
			(setq c (logior (<< c 6) (logand (code name 1 i) 0x3f)) i (inc i) more (dec more)))
		(cond
			((< c 0x10000) (push out c))
			;one character that takes two
			(:t (setq c (- c 0x10000))
				(push out (+ 0xd800 (>> c 10)) (+ 0xdc00 (logand c 0x3ff))))))
	out)

(defun exfat-upcase-load (vol data)
	;the table of the volume, the upper case of every character in turn.
	;0xffff then a count stands for that many that are their own upper case
	(defq map (get :upcase vol) i 0 c 0 n (/ (length data) 2))
	(while (< i n)
		(defq u (get-ushort data (* i 2)))
		(setq i (inc i))
		(cond
			((and (= u 0xffff) (< i n))
				(setq c (+ c (get-ushort data (* i 2))) i (inc i)))
			(:t (if (/= u c) (. map :insert c u))
				(setq c (inc c))))))

(defun exfat-upper (vol unit)
	(ifn (. (get :upcase vol) :find unit) unit))

(defun exfat-mount (device)
	; (exfat-mount path | stream) -> :nil | vol
	;a path is a file of the host that is the image of a disk, opened
	;to be read and written. A stream can be a memory stream.
	(when (defq stream (if (str? device) (file-stream device +file_open_append) device))
		(def (defq vol (env 1)) :stream stream :file (str? device)
			:cache (Fmap 101) :cached 0 :dirty (Fmap 31) :depth 0 :pending (list) :pending_size 0
			:hint 2 :dirty_lo -1 :dirty_hi -1)
		(defq boot (exfat-read vol 0 +exfat_boot_size))
		(when (and boot (= (length boot) +exfat_boot_size)
				(eql (getf boot +exfat_boot_name) "EXFAT   "))
			(bind '(fat_offset heap_offset cluster_count root flags sector_shift cluster_shift)
				(getf-> boot +exfat_boot_fat_offset +exfat_boot_heap_offset
					+exfat_boot_cluster_count +exfat_boot_root +exfat_boot_flags
					+exfat_boot_sector_shift +exfat_boot_cluster_shift))
			(def vol
				:unclean (/= (logand flags +exfat_flag_busy) 0)
				:fat_offset fat_offset :heap_offset heap_offset
				:cluster_count cluster_count :root root
				:sector_shift sector_shift
				:cluster_shift (+ sector_shift cluster_shift)
				:bitmap :nil :bitmap_first 0 :upcase (Fmap 101) :free :nil :dirs (Fmap 101))
			;the root has an entry for the bitmap and one for the upper case table
			(defq data (exfat-data vol root :nil :nil :t) i 0
				n (/ (length data) +exfat_entry_size) type 1)
			(while (and (< i n) (/= type +exfat_type_end))
				(defq e (slice data (* i +exfat_entry_size) (* (inc i) +exfat_entry_size)))
				(setq type (getf e +exfat_entry_type) i (inc i))
				(cond
					((= type +exfat_type_bitmap)
						(bind '(start len) (getf-> e +exfat_bitmap_first_cluster +exfat_bitmap_data_length))
						(def vol :bitmap_first start :bitmap_clusters (exfat-chain vol start len :nil)
							:bitmap (cat (exfat-data vol start len :nil)))
						(def vol :marks (cat (get :bitmap vol)) :freed :nil))
					((= type +exfat_type_upcase)
						(bind '(start len) (getf-> e +exfat_upcase_first_cluster +exfat_upcase_data_length))
						(exfat-upcase-load vol (exfat-data vol start len :nil)))))
			;its upper case of ASCII is ASCII's own, a to z and nothing else
			(def vol :ascii (some! (# (= (exfat-upper vol (!)) (if (<= 97 (!) 122) (- (!) 32) (!)))) '() :t 0 128))
			(if (get :bitmap vol) vol))))

(defun exfat-hash (vol units)
	;the hash of a name, of its upper case, a byte at a time
	(reduce (lambda (hash unit)
			(setq unit (exfat-upper vol unit)
				hash (logand (+ (if (= (logand hash 1) 1) 0x8000 0) (>> hash 1) (logand unit 0xff)) 0xffff))
			(logand (+ (if (= (logand hash 1) 1) 0x8000 0) (>> hash 1) (>> unit 8)) 0xffff)) units 0))

(defun ror-sum (data bits &optional skip)
	; (ror-sum data bits [skip]) -> num
	;the checksum of the FAT file systems: the sum turned right a bit and
	;each byte added, bits wide, the bytes at the indices of skip left out.
	;exFAT's entries and its boot sectors, FAT32's long names
	(defq top (<< 1 (dec bits)) mask (dec (<< 1 bits)) sum 0 i 0 n (length data))
	(while (< i n)
		(unless (and skip (find i skip))
			(setq sum (logand (+ (if (bits? sum 1) top 0) (>> sum 1) (code data 1 i)) mask)))
		(setq i (inc i)))
	sum)

(defmacro exfat-checksum (set)
	;the checksum of the entries of a file, all but the checksum itself
	(static-qq (ror-sum ,set 16 '(2 3))))

(defun exfat-entries-part (data i out &optional most)
	; (exfat-entries-part data i out [most]) -> i
	;the files and directories of a directory's data from entry i on, onto
	;out, as many entries as most if given: where to go on from, the end
	;of the data at the end mark. A file is a set of entries, one for the
	;file, one for its data, then as many as its name needs. Each is read
	;where it is, a folder of a store has thousands
	(defq n (/ (length data) +exfat_entry_size) stop (if most (min n (+ i most)) n) type 1)
	(while (and (< i stop) (/= (setq type (code data 1 (defq at (* i +exfat_entry_size)))) +exfat_type_end))
		(defq count 0)
		(when (= type +exfat_type_file)
			(setq count (code data 1 (+ at +exfat_file_count)))
			(defq attrs (code data 2 (+ at +exfat_file_attributes)) s (+ at +exfat_entry_size))
			(when (and (>= count 2) (<= (+ i count) (dec n))
					(= (code data 1 s) +exfat_type_stream))
				(defq flags (code data 1 (+ s +exfat_stream_flags))
					units (code data 1 (+ s +exfat_stream_name_length))
					size (code data 8 (+ s +exfat_stream_data_length))
					start (code data 4 (+ s +exfat_stream_first_cluster))
					parts (list "") left units k 2)
				;the name, its parts in their entries, made one before it is read
				(while (and (<= k count) (> left 0))
					(when (= (code data 1 (defq e (* (+ i k) +exfat_entry_size))) +exfat_type_name)
						(push parts (slice data (+ e +exfat_name_name) (+ e +exfat_name_name (* 2 (min left +exfat_name_units)))))
						(setq left (- left +exfat_name_units)))
					(setq k (inc k)))
				(push out (list (exfat-name (apply (const cat) parts) (- units (max 0 left)))
						(/= (logand attrs +exfat_attr_dir) 0) size start
						(/= (logand flags +exfat_flag_no_fat) 0) i count))))
		(setq i (+ i (inc count))))
	(if (and (< i n) (= type +exfat_type_end)) n i))

(defun exfat-entries (vol start len no_fat)
	; (exfat-entries vol start len no_fat) -> ((name dir size start no_fat index count) ...)
	;the files and directories of a directory, (exfat-entries-part)
	(defq out (list))
	(exfat-entries-part (exfat-data vol start len no_fat :t) 0 out)
	out)

(defun exfat-same? (vol a b)
	;are two names the same, as the volume sees it, with no regard to case
	(defq a (exfat-units a) b (exfat-units b))
	(and (= (length a) (length b))
		(every (# (= (exfat-upper vol %0) (exfat-upper vol %1))) a b)))

(defun exfat-root (vol)
	;the root as an entry, its length is not kept, its chain is followed
	(list "" :t :nil (get :root vol) :nil -1 0))

(defq +exfat_ascii (apply (const cat) (map! (# (char (!))) '() 1 128)))

(defun exfat-key-units (vol name)
	(apply (const cat) (cat (list "") (map (# (char (exfat-upper vol %0) 2)) (exfat-units name)))))

(defun exfat-key (vol name)
	;a name as the volume sees it, with no regard to case. Of a volume
	;whose upper case of ASCII is ASCII's own, a name of ASCII is its own
	;upper case, and any other is its 16 bit characters after a 0, which
	;no name of ASCII has in it
	(cond
		((not (get :ascii vol)) (exfat-key-units vol name))
		((= (bskip +exfat_ascii name 0) (length name)) (to-upper name))
		((cat (char 0) (exfat-key-units vol name)))))

(defun exfat-dir-read (vol entry)
	; (exfat-dir-read vol entry) -> (entries index)
	;a directory as it was last read, or read now. Every name looked for
	;on the way to a file was a read of each directory on the way, and of
	;every entry in it, a store of blocks has 256 in one
	(bind '(& & size start no_fat &rest _) entry)
	(defq dirs (get :dirs vol))
	(unless (defq got (. dirs :find start))
		(defq entries (exfat-entries vol start size no_fat) index (Fmap 31))
		(each (# (. index :insert (exfat-key vol (first %0)) %0)) entries)
		(. dirs :insert start (setq got (list entries index))))
	got)

(defun exfat-dir (vol entry)
	; (exfat-dir vol entry) -> ((name dir size start no_fat index count) ...)
	;what is in a directory, a list of its own
	(cat (first (exfat-dir-read vol entry))))

(defun exfat-path (vol path)
	; (exfat-path vol path) -> :nil | (root ... entry)
	;the entries from the root down to a path
	;every file that is found, saved or removed comes by here, and many
	;of them in one batch touch the device not at all: it gives way here too
	(task-breathe)
	(defq found (list (exfat-root vol)))
	(each (lambda (part)
			(when found
				(defq entry (if (second (last found))
						(. (second (exfat-dir-read vol (last found))) :find (exfat-key vol part))))
				(if entry (push found entry) (setq found :nil))))
		(filter (# (nempty? %0)) (split path "/")))
	found)

(defun exfat-find (vol path)
	; (exfat-find vol path) -> :nil | (name dir size start no_fat index count)
	(if (defq found (exfat-path vol path)) (last found)))

(defun exfat-list (vol path)
	; (exfat-list vol path) -> :nil | ((name dir size start no_fat index count) ...)
	(and (defq entry (exfat-find vol path)) (second entry)
		(exfat-dir vol entry)))

(defun exfat-load (vol path)
	; (exfat-load vol path) -> :nil | str
	(when (and (defq entry (exfat-find vol path)) (not (second entry)))
		(bind '(& & size start no_fat &rest _) entry)
		(exfat-data vol start size no_fat)))

(defun fs-walk (vol path fnc lister)
	; (fs-walk vol path fnc lister) -> vol
	; (fnc path entry) -> :stop | any, (lister vol path) -> :nil | entries
	;every file and directory under a path, a directory before what is in
	;it, of any file system whose lister gives what is in a directory, as
	;exfat-list and fat32-list do. It keeps a list of the directories still
	;to do, it does not call itself, so the depth of the tree costs no stack.
	;fnc giving :stop ends the walk there, no more read: not a throw. fnc
	;is called from inside it, so the
	;names it uses are not these: vol path fnc lister todo dirs entry child
	(defq todo (list path))
	(while (defq path (pop todo))
		(defq dirs (list))
		(some (lambda (entry)
				(defq child (cat path "/" (first entry)))
				(cond
					((eql (fnc child entry) :stop) (setq dirs (list) todo (list)))
					((second entry) (push dirs child) :nil)))
			(ifn (lister vol path) '()))
		;so that they come off the list in the order they were found
		(reach (# (push todo %0)) dirs))
	vol)

(defun exfat-walk (vol path fnc)
	; (exfat-walk vol path fnc) -> vol
	; (fnc path entry)
	;every file and directory under a path, a directory before what is in it
	(fs-walk vol path fnc exfat-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; writing, the bitmap and the clusters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exfat-used? (vol cluster)
	;is a cluster in use, the bitmap has a bit for each, cluster 2 is bit 0
	(defq bit (- cluster 2))
	(/= 0 (logand (code (get :bitmap vol) 1 (>> bit 3)) (<< 1 (logand bit 7)))))

(defun exfat-mark (vol cluster used)
	(defq bit (- cluster 2) bitmap (get :bitmap vol) i (>> bit 3)
		mask (<< 1 (logand bit 7)) old (code bitmap 1 i) marks (get :marks vol))
	(set-byte bitmap i (if used (logior old mask) (logand old (logxor mask 0xff))))
	;one given out is in use in the marks too, one let go is still in use
	;there till the bitmap that has it free is written
	(if used (set-byte marks i (logior (code marks 1 i) mask)) (def vol :freed :t))
	;a check under way is told, it says nothing of a cluster changed while
	;it was, (exfat-check-step)
	(if (defq touched (get :touched vol)) (set-byte touched i (logior (code touched 1 i) mask)))
	;the count of the clusters that are free, once it has been counted
	(when (defq free (get :free vol))
		(defq was (/= 0 (logand old mask)))
		(cond
			((and used (not was)) (def vol :free (dec free)))
			((and was (not used)) (def vol :free (inc free)))))
	(defq lo (get :dirty_lo vol))
	(def vol :dirty_lo (if (< lo 0) i (min lo i)) :dirty_hi (max (get :dirty_hi vol) i))
	;a cluster that is free again is where to look next
	(unless used (def vol :hint (min (get :hint vol) cluster))))

(defun exfat-begin (vol)
	; (exfat-begin vol) -> vol
	;the start of some writing. Every (exfat-begin) has its (exfat-end), and
	;they can be one inside another. The writing in between goes to the
	;device at the last (exfat-end), so many small files cost little more
	;than one.
	(if (= (get :depth vol) 0) (exfat-busy vol :t))
	(def vol :depth (inc (get :depth vol)))
	vol)

(defun exfat-end (vol)
	; (exfat-end vol) -> vol
	(def vol :depth (dec (get :depth vol)))
	(when (= (get :depth vol) 0)
		(exfat-sync vol)
		(exfat-busy vol :nil))
	vol)

(defun exfat-fat-set (vol cluster next)
	;the cluster that follows a cluster, to the allocation table
	(exfat-write vol (+ (<< (get :fat_offset vol) (get :sector_shift vol)) (* cluster 4))
		(char next 4)))

(defun exfat-scan (bitmap from to want loose)
	;look for a run of free clusters from one cluster up to another, of a
	;bitmap, cluster 2 its bit 0, (fs-bit-run). If there is none the first
	;few free ones are noted, for clusters had with a chain through them
	(defq found (fs-bit-run bitmap (- from 2) (- to 2) want))
	(cond
		(found (+ found 2))
		(:t (defq bit (- from 2))
			(while (and (< (length loose) want) (setq bit (fs-bit-run bitmap bit (- to 2) 1)))
				(push loose (+ bit 2))
				(setq bit (inc bit)))
			:nil)))

(defun exfat-alloc (vol want &optional bitmap)
	; (exfat-alloc vol want [bitmap]) -> :nil | (clusters no_fat)
	;clusters for a file. A run of free clusters if there is one long
	;enough, and the file then needs no chain. If not, the first free
	;clusters there are, with a chain through them. :nil if there are
	;too few. The clusters are marked as used. They are free in the marks,
	;one let go is not given out again till that is written, unless the
	;bitmap is given to look in
	(defq limit (+ (get :cluster_count vol) 2) hint (max 2 (min (get :hint vol) limit))
		bitmap (ifn bitmap (get :marks vol))
		loose (list) found (exfat-scan bitmap hint limit want loose))
	;nothing after where the last was found, so from the start
	(unless found
		(setq loose (list) found (exfat-scan bitmap 2 limit want loose)))
	(cond
		(found
			(defq clusters (map! (const !) '() found (+ found want)))
			(each (# (exfat-mark vol %0 :t)) clusters)
			(def vol :hint (+ found want))
			(list clusters :t))
		((= (length loose) want)
			(each (# (exfat-mark vol %0 :t)) loose)
			(each (# (exfat-fat-set vol %0 %1)) loose (cat (rest loose) (list +exfat_last)))
			(list loose :nil))))

(defun exfat-release (vol start len no_fat)
	;the clusters of a file are free again
	(each (# (exfat-mark vol %0 :nil)) (exfat-chain vol start len no_fat)))

(defun exfat-store (vol clusters data)
	;data to clusters, a run is one write. What is left of the last block
	;is made zero. The rest of the last cluster is not written, nobody is
	;given it to read, and a directory is always whole clusters
	(defq size (exfat-cluster-size vol) i 0 n (length clusters)
		data (cat data (exfat-zeros (- (min (* n size)
						(logand (+ (length data) (const (dec +exfat_block_size))) (const (neg +exfat_block_size)))) (length data)))))
	(while (< i n)
		(defq j (inc i))
		(while (and (< j n) (= (elem-get clusters j) (+ (elem-get clusters i) (- j i))))
			(setq j (inc j)))
		(if (< (* i size) (length data))
			(exfat-write vol (exfat-cluster-offset vol (elem-get clusters i))
				(slice data (* i size) (min (length data) (* j size)))))
		(setq i j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; writing, the directories
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exfat-stamp ()
	;the time now, as a directory entry keeps it, MS-DOS's date over its time
	(bind '(dos_time dos_date) (date-dos (date)))
	(logior (<< dos_date 16) dos_time))

(defun exfat-zone ()
	;the time zone a stamp is in, quarter hours from UTC, and the bit
	;that says it is known, or a reader takes the stamp as its own time
	(logior 0x80 (logand (/ (timezone-offset-seconds *local_timezone*) 900) 0x7f)))

(defun exfat-set (vol name dir size start no_fat)
	; (exfat-set vol name dir size start no_fat) -> str
	;the entries of a file, one for the file, one for its data, and as
	;many as its name needs, with the checksum of them all
	(defq units (exfat-units name) stamp (exfat-stamp) zone (exfat-zone)
		names (/ (+ (length units) (const (dec +exfat_name_units))) +exfat_name_units)
		parts (list
			(setf-> (exfat-zeros +exfat_file_size)
				(+exfat_file_type +exfat_type_file)
				(+exfat_file_count (inc names))
				(+exfat_file_attributes (if dir +exfat_attr_dir +exfat_attr_archive))
				(+exfat_file_create stamp)
				(+exfat_file_modify stamp)
				(+exfat_file_access stamp)
				(+exfat_file_create_tz zone)
				(+exfat_file_modify_tz zone)
				(+exfat_file_access_tz zone))
			(setf-> (exfat-zeros +exfat_stream_size)
				(+exfat_stream_type +exfat_type_stream)
				(+exfat_stream_flags (logior +exfat_flag_alloc (if no_fat +exfat_flag_no_fat 0)))
				(+exfat_stream_name_length (length units))
				(+exfat_stream_name_hash (exfat-hash vol units))
				(+exfat_stream_valid_length size)
				(+exfat_stream_first_cluster start)
				(+exfat_stream_data_length size))))
	(each! (lambda () (defq i (!)) (defq e (setf-> (exfat-zeros +exfat_name_size) (+exfat_name_type +exfat_type_name)))
			(each! (# (set-short e (+ 2 (* (- (!) (* i +exfat_name_units)) 2)) %0)) (list units) (* i +exfat_name_units) (min (length units) (* (inc i) +exfat_name_units)))
			(push parts e)) '() 0 names)
	(defq eset (apply (const cat) parts))
	(set-short eset +exfat_file_checksum (exfat-checksum eset))
	eset)

(defun exfat-dir-put (vol entry index data)
	;entries to a directory, from an index on. They can cross from one
	;cluster of the directory to the next, so they go an entry at a time
	(bind '(& & size start no_fat &rest _) entry)
	(defq clusters (exfat-chain vol start size no_fat) shift (get :cluster_shift vol))
	;it is read again the next time it is wanted
	(. (get :dirs vol) :erase start)
	(each! (lambda () (defq at (* (+ index (!)) +exfat_entry_size))
			(exfat-write vol (+ (exfat-cluster-offset vol (elem-get clusters (>> at shift)))
					(logand at (dec (<< 1 shift))))
				(slice data (* (!) +exfat_entry_size) (* (inc (!)) +exfat_entry_size)))) '() 0 (/ (length data) +exfat_entry_size)))

(defun exfat-dir-data (vol entry)
	(bind '(& & size start no_fat &rest _) entry)
	(exfat-data vol start size no_fat :t))

(defun exfat-grow (vol found)
	; (exfat-grow vol found) -> :nil | found
	;a directory is given one more cluster. found is the entries from the
	;root down to it, and comes back with the directory as it now is.
	(when (defq got (exfat-alloc vol 1))
		(bind '(name dir size start no_fat index count) (defq entry (last found)))
		(defq fresh (first (first got)) cluster_size (exfat-cluster-size vol)
			clusters (exfat-chain vol start size no_fat))
		(exfat-store vol (list fresh) (exfat-zeros cluster_size))
		(cond
			;it still runs on with no chain
			((and no_fat (= fresh (inc (last clusters)))))
			(:t ;a chain, made now if it had none
				(when no_fat
					(each (# (exfat-fat-set vol %0 %1)) clusters (rest clusters))
					(setq no_fat :nil))
				(exfat-fat-set vol (last clusters) fresh)
				(exfat-fat-set vol fresh +exfat_last)))
		;the root keeps no length. Any other has its own entry in the
		;directory above it, which is put right
		(when (>= index 0)
			(setq size (+ size cluster_size))
			(defq parent (elem-get found (- (length found) 2))
				eset (slice (exfat-dir-data vol parent) (* index +exfat_entry_size)
					(* (+ index count 1) +exfat_entry_size))
				eset (cat (slice eset 0 +exfat_entry_size)
					(setf-> (slice eset +exfat_entry_size (const (* 2 +exfat_entry_size)))
						(+exfat_stream_flags (logior +exfat_flag_alloc (if no_fat +exfat_flag_no_fat 0)))
						(+exfat_stream_valid_length size)
						(+exfat_stream_data_length size))
					(slice eset (const (* 2 +exfat_entry_size)) -1)))
			(set-short eset +exfat_file_checksum (exfat-checksum eset))
			(exfat-dir-put vol parent index eset))
		(cat (slice found 0 -2) (list (list name dir size start no_fat index count)))))

(defun exfat-slot (vol found want)
	; (exfat-slot vol found want) -> :nil | (index found)
	;room for that many entries, one after another, in a directory. A
	;deleted entry is room, and so is everything from the end mark on.
	;If there is none the directory is grown, till there is.
	(defq index :nil)
	(while (and found (not index))
		(defq data (exfat-dir-data vol (last found)) n (/ (length data) +exfat_entry_size)
			i 0 run 0 ended :nil)
		(while (and (< i n) (not index))
			(defq type (code data 1 (* i +exfat_entry_size)))
			(if (= type +exfat_type_end) (setq ended :t))
			(setq run (if (or ended (< type +exfat_in_use)) (inc run) 0) i (inc i))
			(if (= run want) (setq index (- i want))))
		(unless index (setq found (exfat-grow vol found))))
	(if index (list index found)))

(defun exfat-split (path)
	; (exfat-split path) -> (parent name)
	(defq parts (filter (# (nempty? %0)) (split path "/")))
	(list (join (slice parts 0 -2) "/") (ifn (last parts) "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; writing, what an app would ask for
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exfat-free (vol)
	; (exfat-free vol) -> bytes
	;the room there is left, the clusters that are not in use. Counted
	;the first time it is asked, and kept as clusters are marked: it is
	;asked with every put to a store, and the count was a look at every
	;bit of the bitmap
	(unless (get :free vol)
		;a long at a time: all in use, or none, is most of a bitmap, and a
		;long that is neither a byte at a time, each one's count from a table
		(defq bitmap (get :bitmap vol) count (get :cluster_count vol) used 0
			bits (const (reduce! (lambda (out) (defq byte (!) n 0)
					(while (/= byte 0) (setq n (+ n (logand byte 1)) byte (>> byte 1)))
					(push out n)) '() (cap 256 (nums)) 0 256))
			longs (>> (length bitmap) 3))
		(each! (lambda () (defq at (<< (!) 3) word (get-long bitmap at))
				(cond
					((= word -1) (setq used (+ used 64)))
					((/= word 0)
						(each! (# (setq used (+ used (elem-get bits (code bitmap 1 (+ at (!))))))) '() 0 8))))
			'() 0 longs)
		(each! (# (setq used (+ used (elem-get bits (code bitmap 1 (!)))))) '() (<< longs 3) (length bitmap))
		(def vol :free (- count used)))
	(<< (get :free vol) (get :cluster_shift vol)))

(defun exfat-dir-keep (vol folder had index new)
	;a directory as it was read, had, with its entry at index put right as
	;new, or gone if new is :nil, not read again: a folder of many files was
	;read, all of it, for each one removed or written over
	(when had
		(defq old (some (# (if (= (elem-get %0 5) index) %0)) (first had))
			entries (filter (# (/= (elem-get %0 5) index)) (first had)))
		(if old (. (second had) :erase (exfat-key vol (first old))))
		(when new
			(push entries new)
			(. (second had) :insert (exfat-key vol (first new)) new))
		(. (get :dirs vol) :insert (elem-get folder 3) (list entries (second had)))))

(defun exfat-add (vol path dir data)
	;a new file or directory, of this data, in the directory it names
	(bind '(parent name) (exfat-split path))
	(defq found (exfat-path vol parent) size (length data) cluster_size (exfat-cluster-size vol) ok :nil)
	(when (and found (second (last found)) (nempty? name))
		(exfat-begin vol)
		(when (defq got (if (> size 0)
					(exfat-alloc vol (/ (+ size (dec cluster_size)) cluster_size)) (list (list) :nil)))
			(bind '(clusters no_fat) got)
			(defq eset (exfat-set vol name dir size (if (empty? clusters) 0 (first clusters)) no_fat)
				slot (exfat-slot vol found (/ (length eset) +exfat_entry_size)))
			(cond
				(slot
					(exfat-store vol clusters data)
					(defq at (first slot) folder (last (second slot))
						had (. (get :dirs vol) :find (elem-get folder 3)))
					(exfat-dir-put vol folder at eset)
					;the directory as it was read, with the new one put in it, not
					;read again: a folder of many files was read, all of it, for
					;each one added
					(when had
						(defq new (list name dir size (if (empty? clusters) 0 (first clusters)) no_fat at
								(dec (/ (length eset) +exfat_entry_size))))
						(push (first had) new)
						(. (second had) :insert (exfat-key vol name) new)
						(. (get :dirs vol) :insert (elem-get folder 3) had))
					(setq ok :t))
				;no room for its entries, so its clusters are free again
				(:t (each (# (exfat-mark vol %0 :nil)) clusters))))
		(exfat-end vol))
	ok)

(defun exfat-eset (vol found)
	;the entries of what was found, as they are in the directory above it
	(bind '(& & & & & index count) (last found))
	(cat (slice (exfat-dir-data vol (elem-get found (- (length found) 2)))
			(* index +exfat_entry_size) (* (+ index count 1) +exfat_entry_size))))

(defun exfat-delete (vol path)
	; (exfat-delete vol path) -> :nil | :t
	;a file, or a directory that has nothing in it
	(defq found (exfat-path vol path))
	(when (and found (> (length found) 1))
		(bind '(name dir size start no_fat index count) (last found))
		(when (or (not dir) (empty? (exfat-dir vol (last found))))
			(exfat-begin vol)
			(defq eset (exfat-eset vol found))
			;an entry that is not in use has the top bit of its type clear
			(each! (lambda () (defq at (* (!) +exfat_entry_size))
					(set-byte eset at (logand (code eset 1 at) (const (dec +exfat_in_use))))) '() 0 (inc count))
			(defq folder (elem-get found (- (length found) 2)) had (. (get :dirs vol) :find (elem-get folder 3)))
			(exfat-dir-put vol folder index eset)
			(exfat-dir-keep vol folder had index :nil)
			;a directory that is gone is not kept, its clusters will be another's
			(. (get :dirs vol) :erase start)
			(exfat-release vol start size no_fat)
			(exfat-end vol)
			:t)))

(defun exfat-replace (vol found data)
	;a file that is there is given new data. Its entries stay where they
	;are and are changed. The new data is put beside the old, which is let
	;go when the new is in place. Only if there is no room for both is the
	;old let go first.
	(bind '(name dir size start no_fat index count) (last found))
	(defq new_size (length data) cluster_size (exfat-cluster-size vol)
		want (/ (+ new_size (dec cluster_size)) cluster_size)
		old (exfat-chain vol start size no_fat))
	(exfat-begin vol)
	(defq got (if (> want 0) (exfat-alloc vol want) (list (list) :nil)))
	(unless got
		(each (# (exfat-mark vol %0 :nil)) old)
		(if (setq got (exfat-alloc vol want (get :bitmap vol)))
			(setq old (list))
			(each (# (exfat-mark vol %0 :t)) old)))
	(when got
		(bind '(clusters no_fat) got)
		(exfat-store vol clusters data)
		(defq eset (exfat-eset vol found) stamp (exfat-stamp) zone (exfat-zone)
			eset (cat
				(setf-> (slice eset 0 +exfat_entry_size)
					(+exfat_file_modify stamp) (+exfat_file_access stamp)
					(+exfat_file_modify_10ms 0)
					(+exfat_file_modify_tz zone) (+exfat_file_access_tz zone))
				(setf-> (slice eset +exfat_entry_size (const (* 2 +exfat_entry_size)))
					(+exfat_stream_flags (logior +exfat_flag_alloc (if no_fat +exfat_flag_no_fat 0)))
					(+exfat_stream_valid_length new_size)
					(+exfat_stream_first_cluster (if (empty? clusters) 0 (first clusters)))
					(+exfat_stream_data_length new_size))
				(slice eset (const (* 2 +exfat_entry_size)) -1)))
		(set-short eset +exfat_file_checksum (exfat-checksum eset))
		(defq folder (elem-get found (- (length found) 2)) had (. (get :dirs vol) :find (elem-get folder 3)))
		(exfat-dir-put vol folder index eset)
		(exfat-dir-keep vol folder had index (list name dir new_size (if (empty? clusters) 0 (first clusters)) no_fat index count))
		(each (# (exfat-mark vol %0 :nil)) old))
	(exfat-end vol)
	(if got :t))

(defun exfat-save (vol path data)
	; (exfat-save vol path data) -> :nil | :t
	;a file of this data. One of that name is replaced, and is still there
	;as it was if there is no room for the new data.
	(defq found (exfat-path vol path))
	(cond
		((not found) (exfat-add vol path :nil data))
		((second (last found)) :nil)
		((exfat-replace vol found data))))

(defun exfat-mkdir (vol path)
	; (exfat-mkdir vol path) -> :nil | :t
	;a new directory, of one cluster with nothing in it
	(unless (exfat-find vol path)
		(exfat-add vol path :t (exfat-zeros (exfat-cluster-size vol)))))

(defun exfat-rename (vol from to)
	; (exfat-rename vol from to) -> :nil | :t
	;a file or a directory is given a new name, a new directory to be in,
	;or both. Its data is not moved. :nil if there is one of that name
	;there, or if a directory would end up inside itself.
	(bind '(parent name) (exfat-split to))
	(defq found (exfat-path vol from) target (exfat-path vol parent) there (exfat-path vol to) ok :nil)
	(when (and found (> (length found) 1) target (second (last target)) (nempty? name))
		(bind '(& dir size start no_fat index count) (last found))
		(when (and
				;the same file under a name that differs only by case is not in the way
				(or (not there) (and (= (length there) (length found))
						(= (elem-get (elem-get there (- (length there) 2)) 3) (elem-get (elem-get found (- (length found) 2)) 3))
						(= (elem-get (last there) 5) index)))
				(not (and dir (some (# (and (second %0) (= (elem-get %0 3) start))) target))))
			(exfat-begin vol)
			;the entry for the file and the one for its data are kept, with
			;their dates, the name is new
			(defq old (exfat-eset vol found) units (exfat-units name)
				fresh (exfat-set vol name dir size start no_fat)
				eset (cat
					(setf-> (slice old 0 +exfat_entry_size)
						(+exfat_file_count (getf fresh +exfat_file_count)))
					(setf-> (slice old +exfat_entry_size (const (* 2 +exfat_entry_size)))
						(+exfat_stream_name_length (length units))
						(+exfat_stream_name_hash (exfat-hash vol units)))
					(slice fresh (const (* 2 +exfat_entry_size)) -1)))
			(set-short eset +exfat_file_checksum (exfat-checksum eset))
			;the old entries are let go first, the new can go where they were
			(each! (lambda () (defq at (* (!) +exfat_entry_size))
					(set-byte old at (logand (code old 1 at) (const (dec +exfat_in_use))))) '() 0 (inc count))
			(exfat-dir-put vol (elem-get found (- (length found) 2)) index old)
			(cond
				((defq slot (exfat-slot vol target (/ (length eset) +exfat_entry_size)))
					(exfat-dir-put vol (last (second slot)) (first slot) eset)
					(setq ok :t))
				;no room in the new directory, so it is back where it was
				(:t (each! (lambda () (defq at (* (!) +exfat_entry_size))
							(set-byte old at (logior (code old 1 at) +exfat_in_use))) '() 0 (inc count))
					(exfat-dir-put vol (elem-get found (- (length found) 2)) index old)))
			(exfat-end vol)))
	ok)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a new volume, with nothing in it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro exfat-sum (data skip)
	;the checksum of the boot sectors and of the upper case table, 32 bit
	(static-qq (ror-sum ,data 32 ,skip)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; checking, the volume as it stands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;a check of a volume that can stop and go on, (exfat-check-step): the
;walk it is on, :what each file and directory has, :who has a cluster
;two have, then :lost the bitmap, :mend, :done; the directories still to
;read; the files and directories found to look at, taken from the end;
;the directory being read, (path data i), and what is found in it so
;far; the one at hand, (path start len no_fat want entry), how many of
;its clusters have been looked at, the next of its chain, and how many
;not of the volume and marked free; a directory being compared with how
;it is kept, (path entry data i kept k same); if it mends; the lines so
;far; the clusters seen; those two have; how many in use had by nothing;
;what is to be mended
(enums +exfat_ck 0
	(enum phase todo items dir found item at next out free cmp
		mend lines seen twice lost used freed reread))

(defun exfat-check-first (vol)
	; (exfat-check-first vol) -> (todo items)
	;where a walk of the volume starts: the root, the bitmap and the upper
	;case table, then what is in the root
	(defq root (get :root vol) csize (exfat-cluster-size vol)
		data (exfat-data vol root :nil :nil) i 0 n (/ (length data) +exfat_entry_size)
		items (list (list "/" root :nil :nil :nil (exfat-root vol))))
	(while (< i n)
		(defq e (slice data (* i +exfat_entry_size) (* (inc i) +exfat_entry_size)) type (getf e +exfat_entry_type))
		(cond
			((= type +exfat_type_bitmap)
				(bind '(start len) (getf-> e +exfat_bitmap_first_cluster +exfat_bitmap_data_length))
				(push items (list "(bitmap)" start len :nil (/ (+ len (dec csize)) csize) :nil)))
			((= type +exfat_type_upcase)
				(bind '(start len) (getf-> e +exfat_upcase_first_cluster +exfat_upcase_data_length))
				(push items (list "(upcase)" start len :nil (/ (+ len (dec csize)) csize) :nil))))
		(setq i (if (= type +exfat_type_end) n (inc i))))
	(list (list (list "" root :nil :nil)) (reverse items)))

(defun exfat-bit? (bitmap cluster)
	;is a cluster's bit set in a bitmap of the volume's
	(defq bit (- cluster 2))
	(/= 0 (logand (code bitmap 1 (>> bit 3)) (<< 1 (logand bit 7)))))

(defun exfat-check-begin (vol &optional mend)
	; (exfat-check-begin vol [mend])
	;a check of the volume, a step at a time, (exfat-check-step). Every
	;cluster marked meanwhile is noted, and nothing is said of it
	(bind '(todo items) (exfat-check-first vol))
	(defq size (length (get :bitmap vol)))
	(def vol :touched (exfat-zeros size)
		:check (list :what todo items :nil (list) :nil 0 :nil 0 0 :nil
			mend (list) (exfat-zeros size) (Fmap 31) 0 (list) (list) (list))))

(defun exfat-check-clusters (vol ck item at next out free)
	;the next of the clusters of one, 1024 of them, and what is said of it
	;when they are done, or it is the one at hand, to go on with. A cluster
	;marked since the check began is passed over. Of a directory, then, how
	;it is kept is compared with how the device has it, and it is walked
	(bind '(path start len no_fat want entry) item)
	(defq limit (+ (get :cluster_count vol) 2) most (- (ifn want limit) at))
	(cond
		((or (not next) (<= most 0)) (defq clusters (list) next :nil))
		(no_fat (defq clusters (map! (const !) '() next (+ next (min most 1024))) next (+ next (length clusters)))
			(if (>= (+ at (length clusters)) want) (setq next :nil)))
		(:t (bind '(clusters next) (exfat-chain-part vol next (min most 1024)))))
	(cond
		((eql (elem-get ck +exfat_ck_phase) :what)
			(defq used (elem-get ck +exfat_ck_used)
				touched (get :touched vol) seen (elem-get ck +exfat_ck_seen) twice (elem-get ck +exfat_ck_twice)
				bitmap (get :bitmap vol))
			(each (lambda (c)
					(defq bit (- c 2) i (>> bit 3) m (<< 1 (logand bit 7)))
					(cond
						((or (< c 2) (>= c limit)) (++ out))
						((/= 0 (logand (code touched 1 i) m)))
						(:t (if (/= 0 (logand (code seen 1 i) m))
								(. twice :insert c (list))
								(set-byte seen i (logior (code seen 1 i) m)))
							(when (= 0 (logand (code bitmap 1 i) m)) (++ free) (push used c)))))
				clusters))
		(:t (defq twice (elem-get ck +exfat_ck_twice))
			(each (# (if (defq who (. twice :find %0)) (push who path))) clusters)))
	(setq at (+ at (length clusters)))
	(cond
		;more of it, it is the one at hand
		(next (elem-set ck +exfat_ck_item item)
			(elem-set ck +exfat_ck_at at)
			(elem-set ck +exfat_ck_next next)
			(elem-set ck +exfat_ck_out out)
			(elem-set ck +exfat_ck_free free))
		(:t (elem-set ck +exfat_ck_item :nil)
			(when (eql (elem-get ck +exfat_ck_phase) :what)
				(defq lines (elem-get ck +exfat_ck_lines))
				(if (> out 0) (push lines (cat path ": " (str out) " clusters not of the volume")))
				(if (> free 0) (push lines (cat path ": " (str free) " clusters in use marked free")))
				(and want (< at want)
					(push lines (cat path ": a chain of " (str at) " clusters, its length wants " (str want))))
				;a directory as it is kept, to compare with the device as it is now
				(and entry (second entry)
					(defq kept (. (get :dirs vol) :find (elem-get entry 3)))
					(elem-set ck +exfat_ck_cmp (list path entry (exfat-data vol (elem-get entry 3) (elem-get entry 2) (elem-get entry 4) :t) 0
						(sort (cat (first kept)) (# (- (elem-get %0 5) (elem-get %1 5)))) 0 :t))))
			;a directory in a directory is walked, the root is already
			(and entry (second entry) (>= (elem-get entry 5) 0)
				(push (elem-get ck +exfat_ck_todo) (list path (elem-get entry 3) (elem-get entry 2) (elem-get entry 4)))))))

(defun exfat-check-item (vol ck item)
	;one of the walk begun, and its first clusters, unless it was changed
	;since its directory was read: its first cluster marked since the
	;check began
	(bind '(path start len no_fat want entry) item)
	(defq bit (- start 2))
	(unless (and (>= start 2) (< start (+ (get :cluster_count vol) 2))
			(/= 0 (logand (code (get :touched vol) 1 (>> bit 3)) (<< 1 (logand bit 7)))))
		(exfat-check-clusters vol ck item 0 (and (>= start 2) (or (not len) (> len 0)) start) 0 0)))

(defun exfat-check-compare (vol ck)
	;the next 256 entries of a directory as the device has it, against how
	;it is kept, and what is said when they are all done
	(bind '(path entry data i kept k same) (defq cmp (elem-get ck +exfat_ck_cmp)))
	(defq got (list) n (/ (length data) +exfat_entry_size) i (exfat-entries-part data i got 256))
	(each (lambda (e)
			(if (or (>= k (length kept)) (nql (str e) (str (elem-get kept k)))) (setq same :nil))
			(setq k (inc k)))
		got)
	(elem-set cmp 3 i)
	(elem-set cmp 5 k)
	(elem-set cmp 6 same)
	(when (>= i n)
		(elem-set ck +exfat_ck_cmp :nil)
		(unless (and same (= k (length kept)))
			(push (elem-get ck +exfat_ck_reread) (elem-get entry 3))
			(push (elem-get ck +exfat_ck_lines) (cat path ": the directory as it is kept is not as the device has it")))))

(defun exfat-check-step (vol &optional until)
	; (exfat-check-step vol [until]) -> :nil | :t
	;more of a check, till it is time until, if there is one, one thing at
	;least. :t if there is more to do. Each file and directory as the
	;device has it, a directory read 256 entries at a time, a chain 1024
	;clusters at a time, then who has a cluster that two have, then the
	;bitmap 4096 bytes at a time. A mend waits till nothing is being
	;written, or is not done if there is no until
	(defq ck (get :check vol) once :t)
	(while (and ck (not (eql (elem-get ck +exfat_ck_phase) :done)) (or once (not until) (< (pii-time) until)))
		(setq once :nil)
		(task-breathe)
		(defq phase (elem-get ck +exfat_ck_phase))
		(cond
			((elem-get ck +exfat_ck_cmp) (exfat-check-compare vol ck))
			((defq item (elem-get ck +exfat_ck_item))
				(exfat-check-clusters vol ck item (elem-get ck +exfat_ck_at) (elem-get ck +exfat_ck_next)
					(elem-get ck +exfat_ck_out) (elem-get ck +exfat_ck_free)))
			((defq dir (elem-get ck +exfat_ck_dir))
				;the directory being read, its next 256 entries
				(bind '(path data i) dir)
				(defq found (elem-get ck +exfat_ck_found) got (list) csize (exfat-cluster-size vol)
					i (exfat-entries-part data i got 256))
				(each (lambda (e)
						(defq len (elem-get e 2))
						(push found (list (cat path "/" (first e)) (elem-get e 3) len (elem-get e 4) (/ (+ len (dec csize)) csize) e)))
					got)
				(elem-set dir 2 i)
				(when (>= i (/ (length data) +exfat_entry_size))
					(elem-set ck +exfat_ck_dir :nil)
					(elem-set ck +exfat_ck_items (reverse found))
					(elem-set ck +exfat_ck_found (list))))
			;the next found, and its first clusters, most have no more
			((nempty? (defq items (elem-get ck +exfat_ck_items)))
				(exfat-check-item vol ck (pop items)))
			((nempty? (defq todo (elem-get ck +exfat_ck_todo)))
				(bind '(path start len no_fat) (pop todo))
				(elem-set ck +exfat_ck_dir (list path (exfat-data vol start len no_fat :t) 0)))
			((and (eql phase :what) (not (. (elem-get ck +exfat_ck_twice) :empty?)))
				;a walk again, for who has a cluster two have
				(bind '(todo items) (exfat-check-first vol))
				(elem-set ck +exfat_ck_todo todo)
				(elem-set ck +exfat_ck_items items)
				(elem-set ck +exfat_ck_phase :who))
			((find phase '(:what :who))
				(defq touched (get :touched vol) lines (elem-get ck +exfat_ck_lines))
				(. (elem-get ck +exfat_ck_twice) :each (lambda (c who)
						(unless (exfat-bit? touched c)
							(push lines (cat "cluster " (str c) " is had by " (join who ", "))))))
				(elem-set ck +exfat_ck_phase :lost)
				(elem-set ck +exfat_ck_at 0))
			((eql phase :lost)
				;in use, and had by nothing, nor marked since the check began
				(defq bitmap (get :bitmap vol) seen (elem-get ck +exfat_ck_seen) touched (get :touched vol)
					at (elem-get ck +exfat_ck_at) end (min (length bitmap) (+ at 4096))
					lost (elem-get ck +exfat_ck_lost) freed (elem-get ck +exfat_ck_freed))
				(each! (lambda ()
						(defq b (logand (code bitmap 1 (!)) (logxor (logior (code seen 1 (!)) (code touched 1 (!))) 0xff)) c (+ (<< (!) 3) 2))
						(while (/= b 0)
							(when (/= (logand b 1) 0)
								(++ lost)
								(if (elem-get ck +exfat_ck_mend) (push freed c)))
							(setq b (>> b 1) c (inc c))))
					'() at end)
				(elem-set ck +exfat_ck_lost lost)
				(elem-set ck +exfat_ck_at end)
				(when (= end (length bitmap))
					(if (> lost 0) (push (elem-get ck +exfat_ck_lines) (cat (str lost) " clusters marked in use are had by nothing")))
					(elem-set ck +exfat_ck_phase :mend)))
			;a mend, of what has not been marked since it was found
			((or (not (elem-get ck +exfat_ck_mend)) (and (> (get :depth vol) 0) (not until)))
				(elem-set ck +exfat_ck_phase :done))
			((= (get :depth vol) 0)
				(defq touched (get :touched vol)
					used (filter (# (not (exfat-bit? touched %0))) (elem-get ck +exfat_ck_used))
					freed (filter (# (not (exfat-bit? touched %0))) (elem-get ck +exfat_ck_freed))
					reread (elem-get ck +exfat_ck_reread))
				(when (or (nempty? used) (nempty? freed) (nempty? reread))
					(def vol :touched :nil)
					(exfat-begin vol)
					(each (# (exfat-mark vol %0 :t)) used)
					(each (# (exfat-mark vol %0 :nil)) freed)
					(each (# (. (get :dirs vol) :erase %0)) reread)
					(exfat-end vol)
					(push (elem-get ck +exfat_ck_lines) (cat "mended: " (str (length used)) " clusters marked in use, "
							(str (length freed)) " free, " (str (length reread)) " directories read again")))
				(elem-set ck +exfat_ck_phase :done))))
	(and ck (not (eql (elem-get ck +exfat_ck_phase) :done))))

(defun exfat-check-end (vol)
	; (exfat-check-end vol) -> :nil | (line ...)
	;a check is over, what it found
	(when (defq ck (get :check vol))
		(def vol :check :nil :touched :nil)
		(elem-get ck +exfat_ck_lines)))

(defun exfat-check-busy? (vol)
	; (exfat-check-busy? vol) -> :nil | :t
	(if (get :check vol) :t))

(defun exfat-check (vol &optional mend)
	; (exfat-check vol [mend]) -> (line ...)
	;what is wrong with the volume, a line each, none if nothing is, all
	;at once, (exfat-check-step). Of each file and directory, as the
	;device has it: clusters that are not of the volume, clusters in use
	;that the bitmap says are free, a chain that ends before its length
	;does, and a directory as it is kept here, (exfat-dir-read), that is
	;not as the device has it. Then each cluster that two have, and who
	;has it, and how many clusters are marked in use that nothing has:
	;room lost, no harm. It only reads, unless mend is asked for and
	;nothing is being written: then a cluster in use is marked in use, one
	;that nothing has is free, a directory is read again, and a last line
	;says so. A cluster two have, a chain cut short and one not of the
	;volume are only said
	(exfat-check-begin vol mend)
	(while (exfat-check-step vol))
	(exfat-check-end vol))

(defun exfat-cluster-shift (size)
	; (exfat-cluster-shift size) -> num
	;the sectors of a cluster of a volume of that size, as a power of 2:
	;4096 byte clusters up to 256MB, 32768 up to 32GB and 131072 over that
	(cond ((<= size 0x10000000) 3) ((<= size 0x800000000) 6) (8)))

(defun exfat-format (device size &optional label cluster_shift)
	; (exfat-format path | stream size [label cluster_shift]) -> :nil | :t
	;a new volume of that many bytes with nothing in it. A path is a new
	;file of the host, the image of a disk. A stream is written where it
	;is, and must be that long, a memory stream of zeros say.
	;cluster_shift is the sectors of a cluster as a power of 2,
	;(exfat-cluster-shift) if it is not given.
	(defq sectors (>> size 9)
		cluster_shift (ifn cluster_shift (exfat-cluster-shift size))
		cluster_sectors (<< 1 cluster_shift) cluster_size (<< cluster_sectors 9)
		fat_offset 128
		;the table has an entry for a cluster, and two more
		fat_length (logand (+ (>> (+ (* (+ (>> sectors cluster_shift) 2) 4) 511) 9) 127) -128)
		heap_offset (+ fat_offset fat_length)
		cluster_count (>> (max 0 (- sectors heap_offset)) cluster_shift)
		upcase (exfat-upcase-table)
		bitmap_len (>> (+ cluster_count 7) 3)
		bitmap_clusters (/ (+ bitmap_len (dec cluster_size)) cluster_size)
		upcase_clusters (/ (+ (length upcase) (dec cluster_size)) cluster_size)
		upcase_first (+ 2 bitmap_clusters) root (+ upcase_first upcase_clusters)
		used (+ bitmap_clusters upcase_clusters 1)
		units (if label (exfat-units label) (list)))
	(when (and (> cluster_count (+ used 1)) (<= (length units) 11)
			(defq stream (if (str? device) (file-stream device +file_open_write) device)))
		(defun fpad (data len) (cat data (exfat-zeros (- len (length data)))))
		(defun fput (sector data) (stream-seek stream (<< sector 9) 0) (write-blk stream data))
		(defun fcluster (index) (+ heap_offset (<< (- index 2) cluster_shift)))
		;the boot sectors, 12 of them, and the same again as a spare
		(defq boot (setf-> (cat (char 0xeb) (char 0x76) (char 0x90) "EXFAT   " (exfat-zeros 499))
				(+exfat_boot_volume_length sectors)
				(+exfat_boot_fat_offset fat_offset) (+exfat_boot_fat_length fat_length)
				(+exfat_boot_heap_offset heap_offset) (+exfat_boot_cluster_count cluster_count)
				(+exfat_boot_root root) (+exfat_boot_serial (logand (pii-time) 0xffffffff))
				(+exfat_boot_revision 0x100)
				(+exfat_boot_sector_shift 9) (+exfat_boot_cluster_shift cluster_shift)
				(+exfat_boot_fats 1) (+exfat_boot_drive 0x80) (+exfat_boot_percent 0xff))
			mark (cat (char 0x55) (char 0xaa))
			region (apply (const cat) (cat (list boot mark)
					(map! (lambda () (cat (exfat-zeros 510) mark)) '() 0 8)
					(list (exfat-zeros 1024))))
			sum (char (exfat-sum region '(106 107 112)) 4)
			region (cat region (apply (const cat) (map! (lambda () sum) '() 0 128))))
		;all that comes before the clusters is zero, but for these
		(fput 0 (exfat-zeros (<< heap_offset 9)))
		(fput 0 region)
		(fput 12 region)
		;the table, the first two entries are not clusters, then the chain
		;of the bitmap, of the upper case table, and of the root
		(defq chain (list 0xfffffff8 +exfat_last))
		(each (lambda (count)
				(each! (lambda () (push chain (+ (length chain) 1))) '() 1 count)
				(push chain +exfat_last)) (list bitmap_clusters upcase_clusters 1))
		(fput fat_offset (fpad (apply (const cat) (map (# (char %0 4)) chain))
				(logand (+ (* (length chain) 4) 511) -512)))
		;the bitmap, with its own clusters, the table's and the root's in use
		(defq bitmap (exfat-zeros bitmap_len))
		(each! (# (set-byte bitmap (>> (!) 3) (logior (code bitmap 1 (>> (!) 3)) (<< 1 (logand (!) 7))))) '() 0 used)
		(fput (fcluster 2) (fpad bitmap (* bitmap_clusters cluster_size)))
		(fput (fcluster upcase_first) (fpad upcase (* upcase_clusters cluster_size)))
		;the root, the label if there is one, the bitmap, the table
		(defq entries (list))
		(when (nempty? units)
			(defq e (setf-> (exfat-zeros +exfat_label_size)
					(+exfat_label_type +exfat_type_label) (+exfat_label_length (length units))))
			(each (# (set-short e (+ 2 (* (!) 2)) %0)) units)
			(push entries e))
		(push entries
			(setf-> (exfat-zeros +exfat_bitmap_size)
				(+exfat_bitmap_type +exfat_type_bitmap)
				(+exfat_bitmap_first_cluster 2) (+exfat_bitmap_data_length bitmap_len))
			(setf-> (exfat-zeros +exfat_upcase_size)
				(+exfat_upcase_type +exfat_type_upcase) (+exfat_upcase_checksum (exfat-sum upcase '()))
				(+exfat_upcase_first_cluster upcase_first) (+exfat_upcase_data_length (length upcase))))
		(fput (fcluster root) (fpad (apply (const cat) entries) cluster_size))
		;a new file is as long as the volume
		(when (str? device)
			(fput (dec sectors) (exfat-zeros 512)))
		(if (str? device) (stream-flush stream))
		:t))

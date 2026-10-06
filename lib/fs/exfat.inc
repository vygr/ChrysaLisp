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
;	:cache :cached, the blocks of the device last read
;	:upcase, the upper case of each character that has one
;an entry of a directory is (name dir size start no_fat index count)
;	start is its first cluster. no_fat is a file whose clusters run on
;	from there, with no chain
;	index is where its entries are in the directory, count how many more
;	follow the first of them
;a path, found, is the entries from the root down to it, (root ... entry)

(import "lib/date/date.inc")

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
	+exfat_last 0xffffffff +exfat_in_use 0x80)

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
;of a sector, and each read is slow. So the device is only ever asked for
;whole blocks, a block is 8 sectors, and the blocks last read are kept.
;A file that is the image of a disk does not care, and is treated the same.

(defq +exfat_block_shift 12 +exfat_block_size 4096 +exfat_cache_blocks 512)

(defun exfat-raw-read (vol offset len)
	;whole blocks from the device, :nil if they are not all there
	(defq stream (get :stream vol))
	(stream-seek stream offset 0)
	(if (and (defq data (read-blk stream len)) (= (length data) len)) data))

(defun exfat-raw-write (vol offset data)
	;whole sectors to the device
	(defq stream (get :stream vol))
	(stream-seek stream offset 0)
	(write-blk stream data))

(defun exfat-block (vol index)
	;a block of the device, from those kept if it is there
	(defq cache (get :cache vol))
	(or (. cache :find index)
		(progn
			(when (>= (get :cached vol) +exfat_cache_blocks)
				(. cache :empty)
				(def vol :cached 0))
			;the last block of a device can be short of a whole block
			(defq data (ifn (exfat-raw-read vol (<< index +exfat_block_shift) +exfat_block_size)
				(apply (const cat) (cat (list "") (filter (const identity) (map (lambda (i)
					(exfat-raw-read vol (+ (<< index +exfat_block_shift) (* i 512)) 512))
					(range 0 8)))))))
			(. cache :insert index data)
			(def vol :cached (inc (get :cached vol)))
			data)))

(defun exfat-read (vol offset len)
	; (exfat-read vol offset len) -> str
	;bytes of the device. A long read goes straight to the device, a
	;short one comes from the blocks it is in.
	(defq lo (>> offset +exfat_block_shift) hi (>> (+ offset len (dec +exfat_block_size)) +exfat_block_shift)
		base (<< lo +exfat_block_shift))
	(slice (if (> (- hi lo) 4)
			(ifn (exfat-raw-read vol base (<< (- hi lo) +exfat_block_shift))
				(apply (const cat) (map (# (exfat-block vol %0)) (range lo hi))))
			(apply (const cat) (map (# (exfat-block vol %0)) (range lo hi))))
		(- offset base) (+ (- offset base) len)))

(defun exfat-write (vol offset data)
	; (exfat-write vol offset data) -> vol
	;bytes to the device. Whole sectors are written, so a write that is
	;not whole sectors is read, changed and written back. A seek puts what
	;was written before it where it belongs, the last write of all is put
	;there by (exfat-sync)
	(defq len (length data) lo (logand offset -512) hi (logand (+ offset len 511) -512))
	(unless (and (= lo offset) (= hi (+ offset len)))
		(defq old (exfat-read vol lo (- hi lo)))
		(setq data (cat (slice old 0 (- offset lo)) data (slice old (- (+ offset len) lo) -1))))
	;the blocks kept are no longer what is on the device
	(defq cache (get :cache vol))
	(each (# (when (. cache :find %0)
			(. cache :erase %0)
			(def vol :cached (dec (get :cached vol)))))
		(range (>> lo +exfat_block_shift) (>> (+ hi (dec +exfat_block_size)) +exfat_block_shift)))
	(exfat-raw-write vol lo data)
	vol)

(defun exfat-sync (vol)
	; (exfat-sync vol) -> vol
	;all that was written is on the device. A file stream holds its last
	;write till it is flushed or closed, and a task that ends with the
	;stream open would lose it, here the bitmap, the last thing written.
	;A memory stream is not flushed, a flush ends the string where it stands.
	(if (get :file vol) (stream-flush (get :stream vol)))
	vol)

(defun exfat-zeros (len)
	; (exfat-zeros len) -> str
	(defq z (const (char 0 8)))
	(while (< (length z) len) (setq z (cat z z)))
	(slice z 0 len))

(defun exfat-mount (device)
	; (exfat-mount path | stream) -> :nil | vol
	;a path is a file of the host that is the image of a disk, opened
	;to be read and written. A stream can be a memory stream.
	(when (defq stream (if (str? device) (file-stream device +file_open_append) device))
		(def (defq vol (env 1)) :stream stream :file (str? device)
			:cache (Fmap 101) :cached 0 :hint 2 :dirty_lo -1 :dirty_hi -1)
		(defq boot (exfat-read vol 0 +exfat_boot_size))
		(when (and boot (= (length boot) +exfat_boot_size)
				(eql (getf boot +exfat_boot_name) "EXFAT   "))
			(bind '(fat_offset heap_offset cluster_count root sector_shift cluster_shift)
				(getf-> boot +exfat_boot_fat_offset +exfat_boot_heap_offset
					+exfat_boot_cluster_count +exfat_boot_root
					+exfat_boot_sector_shift +exfat_boot_cluster_shift))
			(def vol
				:fat_offset fat_offset :heap_offset heap_offset
				:cluster_count cluster_count :root root
				:sector_shift sector_shift
				:cluster_shift (+ sector_shift cluster_shift)
				:bitmap :nil :bitmap_first 0 :upcase (Fmap 101))
			;the root has an entry for the bitmap and one for the upper case table
			(defq data (exfat-data vol root :nil :nil) i 0
				n (/ (length data) +exfat_entry_size) type 1)
			(while (and (< i n) (/= type +exfat_type_end))
				(defq e (slice data (* i +exfat_entry_size) (* (inc i) +exfat_entry_size)))
				(setq type (getf e +exfat_entry_type) i (inc i))
				(cond
					((= type +exfat_type_bitmap)
						(bind '(start len) (getf-> e +exfat_bitmap_first_cluster +exfat_bitmap_data_length))
						(def vol :bitmap_first start
							:bitmap (cat (exfat-data vol start len :nil))))
					((= type +exfat_type_upcase)
						(bind '(start len) (getf-> e +exfat_upcase_first_cluster +exfat_upcase_data_length))
						(exfat-upcase-load vol (exfat-data vol start len :nil)))))
			(if (get :bitmap vol) vol))))

(defun exfat-cluster-size (vol)
	(<< 1 (get :cluster_shift vol)))

(defun exfat-cluster-offset (vol cluster)
	;where a cluster is on the device, the first is cluster 2
	(+ (<< (get :heap_offset vol) (get :sector_shift vol))
		(<< (- cluster 2) (get :cluster_shift vol))))

(defun exfat-next (vol cluster)
	;the cluster that follows this one, from the allocation table
	(get-uint (exfat-read vol (+ (<< (get :fat_offset vol) (get :sector_shift vol))
		(* cluster 4)) 4) 0))

(defun exfat-chain (vol start len no_fat)
	; (exfat-chain vol start len no_fat) -> (cluster ...)
	;the clusters of a file or a directory. A len of :nil is all of the chain
	(defq size (exfat-cluster-size vol) limit (+ (get :cluster_count vol) 2)
		clusters (list))
	(cond
		((< start 2) clusters)
		(no_fat (range start (+ start (/ (+ len (dec size)) size))))
		(:t (defq want (if len (/ (+ len (dec size)) size) limit))
			(while (and (>= start 2) (< start limit) (< (length clusters) want))
				(push clusters start)
				(setq start (exfat-next vol start)))
			clusters)))

(defun exfat-data (vol start len no_fat)
	; (exfat-data vol start len no_fat) -> str
	;the bytes of a file or a directory, a run of clusters is one read
	(defq size (exfat-cluster-size vol) parts (list)
		clusters (exfat-chain vol start len no_fat) i 0 n (length clusters))
	(while (< i n)
		(defq j (inc i))
		(while (and (< j n) (= (elem-get clusters j) (+ (elem-get clusters i) (- j i))))
			(setq j (inc j)))
		(push parts (exfat-read vol (exfat-cluster-offset vol (elem-get clusters i)) (* (- j i) size)))
		(setq i j))
	(defq data (apply (const cat) (cat (list "") parts)))
	(if len (slice data 0 len) data))

(defun exfat-name (data units)
	;a name is 16 bit characters, low byte first, here made UTF-8
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

(defun exfat-hash (vol units)
	;the hash of a name, of its upper case, a byte at a time
	(reduce (lambda (hash unit)
		(setq unit (exfat-upper vol unit)
			hash (logand (+ (if (= (logand hash 1) 1) 0x8000 0) (>> hash 1) (logand unit 0xff)) 0xffff))
		(logand (+ (if (= (logand hash 1) 1) 0x8000 0) (>> hash 1) (>> unit 8)) 0xffff)) units 0))

(defun exfat-checksum (set)
	;the checksum of the entries of a file, all but the checksum itself
	(defq sum 0)
	(each (lambda (i)
		(unless (<= 2 i 3)
			(setq sum (logand (+ (if (= (logand sum 1) 1) 0x8000 0) (>> sum 1) (code set 1 i)) 0xffff))))
		(range 0 (length set)))
	sum)

(defun exfat-entries (vol start len no_fat)
	; (exfat-entries vol start len no_fat) -> ((name dir size start no_fat index count) ...)
	;the files and directories of a directory. A file is a set of entries,
	;one for the file, one for its data, then as many as its name needs
	(defq data (exfat-data vol start len no_fat) out (list) i 0
		n (/ (length data) +exfat_entry_size) type 1)
	(defun entry (i)
		(slice data (* i +exfat_entry_size) (* (inc i) +exfat_entry_size)))
	(while (and (< i n) (/= (setq type (getf (defq e (entry i)) +exfat_entry_type)) +exfat_type_end))
		(defq count 0)
		(when (= type +exfat_type_file)
			(bind '(count attrs) (getf-> e +exfat_file_count +exfat_file_attributes))
			(when (and (>= count 2) (<= (+ i count) (dec n))
					(= (getf (defq s (entry (inc i))) +exfat_entry_type) +exfat_type_stream))
				(bind '(flags left size start) (getf-> s +exfat_stream_flags
					+exfat_stream_name_length +exfat_stream_data_length +exfat_stream_first_cluster))
				(defq name "" k 2)
				(while (and (<= k count) (> left 0))
					(when (= (getf (defq e (entry (+ i k))) +exfat_entry_type) +exfat_type_name)
						(setq name (cat name (exfat-name (getf e +exfat_name_name)
								(min left +exfat_name_units)))
							left (- left +exfat_name_units)))
					(setq k (inc k)))
				(push out (list name (/= (logand attrs +exfat_attr_dir) 0) size start
					(/= (logand flags +exfat_flag_no_fat) 0) i count))))
		(setq i (+ i (inc count))))
	out)

(defun exfat-same? (vol a b)
	;are two names the same, as the volume sees it, with no regard to case
	(defq a (exfat-units a) b (exfat-units b))
	(and (= (length a) (length b))
		(every (# (= (exfat-upper vol %0) (exfat-upper vol %1))) a b)))

(defun exfat-root (vol)
	;the root as an entry, its length is not kept, its chain is followed
	(list "" :t :nil (get :root vol) :nil -1 0))

(defun exfat-dir (vol entry)
	; (exfat-dir vol entry) -> ((name dir size start no_fat index count) ...)
	;what is in a directory
	(bind '(& & size start no_fat &rest _) entry)
	(exfat-entries vol start size no_fat))

(defun exfat-path (vol path)
	; (exfat-path vol path) -> :nil | (root ... entry)
	;the entries from the root down to a path
	(defq found (list (exfat-root vol)))
	(each (lambda (part)
		(when found
			(defq entry (if (second (last found))
				(some (# (if (exfat-same? vol (first %0) part) %0))
					(exfat-dir vol (last found)))))
			(if entry (push found entry) (setq found :nil))))
		(filter (# (nempty? %0)) (split path "/")))
	found)

(defun exfat-find (vol path)
	; (exfat-find vol path) -> :nil | (name dir size start no_fat index count)
	(if (defq found (exfat-path vol path)) (last found)))

(defun exfat-list (vol path)
	; (exfat-list vol path) -> :nil | ((name dir size start no_fat index count) ...)
	(when (and (defq entry (exfat-find vol path)) (second entry))
		(exfat-dir vol entry)))

(defun exfat-load (vol path)
	; (exfat-load vol path) -> :nil | str
	(when (and (defq entry (exfat-find vol path)) (not (second entry)))
		(bind '(& & size start no_fat &rest _) entry)
		(exfat-data vol start size no_fat)))

(defun exfat-walk (vol path fnc)
	; (exfat-walk vol path fnc) -> vol
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
			(if (second entry) (push dirs child))) (ifn (exfat-list vol path) '()))
		;so that they come off the list in the order they were found
		(reach (# (push todo %0)) dirs))
	vol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; writing, the bitmap and the clusters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exfat-used? (vol cluster)
	;is a cluster in use, the bitmap has a bit for each, cluster 2 is bit 0
	(defq bit (- cluster 2))
	(/= 0 (logand (code (get :bitmap vol) 1 (>> bit 3)) (<< 1 (logand bit 7)))))

(defun exfat-mark (vol cluster used)
	(defq bit (- cluster 2) bitmap (get :bitmap vol) i (>> bit 3)
		mask (<< 1 (logand bit 7)) old (code bitmap 1 i))
	(set-byte bitmap i (if used (logior old mask) (logand old (logxor mask 0xff))))
	(defq lo (get :dirty_lo vol))
	(def vol :dirty_lo (if (< lo 0) i (min lo i)) :dirty_hi (max (get :dirty_hi vol) i))
	;a cluster that is free again is where to look next
	(unless used (def vol :hint (min (get :hint vol) cluster))))

(defun exfat-bitmap-save (vol)
	;the bitmap back to the clusters it is kept in, the ones that changed
	(defq bitmap (get :bitmap vol) size (exfat-cluster-size vol)
		lo (get :dirty_lo vol) hi (get :dirty_hi vol))
	(when (>= lo 0)
		(defq clusters (exfat-chain vol (get :bitmap_first vol) (length bitmap) :nil))
		(each (lambda (i)
			(defq at (* i size))
			(exfat-write vol (exfat-cluster-offset vol (elem-get clusters i))
				(slice bitmap at (min (length bitmap) (+ at size)))))
			(range (/ lo size) (inc (/ hi size))))
		(def vol :dirty_lo -1 :dirty_hi -1)))

(defun exfat-fat-set (vol cluster next)
	;the cluster that follows a cluster, to the allocation table
	(exfat-write vol (+ (<< (get :fat_offset vol) (get :sector_shift vol)) (* cluster 4))
		(char next 4)))

(defun exfat-scan (vol from to want loose)
	;look for a run of free clusters from one cluster up to another. The
	;first few free ones are noted on the way, in case there is no run.
	;A byte of the bitmap that is all in use is stepped over in one.
	(defq cluster from run 0 found :nil bitmap (get :bitmap vol))
	(while (and (< cluster to) (not found))
		(cond
			((and (= (logand (- cluster 2) 7) 0) (<= (+ cluster 8) to)
					(= (logand (code bitmap 1 (>> (- cluster 2) 3)) 0xff) 0xff))
				(setq run 0 cluster (+ cluster 8)))
			((exfat-used? vol cluster)
				(setq run 0 cluster (inc cluster)))
			(:t (setq run (inc run))
				(if (< (length loose) want) (push loose cluster))
				(if (= run want) (setq found (- cluster (dec want))))
				(setq cluster (inc cluster)))))
	found)

(defun exfat-alloc (vol want)
	; (exfat-alloc vol want) -> :nil | (clusters no_fat)
	;clusters for a file. A run of free clusters if there is one long
	;enough, and the file then needs no chain. If not, the first free
	;clusters there are, with a chain through them. :nil if there are
	;too few. The clusters are marked as used.
	(defq limit (+ (get :cluster_count vol) 2) hint (max 2 (min (get :hint vol) limit))
		loose (list) found (exfat-scan vol hint limit want loose))
	;nothing after where the last was found, so from the start
	(unless found
		(setq loose (list) found (exfat-scan vol 2 limit want loose)))
	(cond
		(found
			(defq clusters (range found (+ found want)))
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
	;data to clusters, the end of the last is made zero, a run is one write
	(defq size (exfat-cluster-size vol) i 0 n (length clusters)
		data (cat data (exfat-zeros (- (* n size) (length data)))))
	(while (< i n)
		(defq j (inc i))
		(while (and (< j n) (= (elem-get clusters j) (+ (elem-get clusters i) (- j i))))
			(setq j (inc j)))
		(exfat-write vol (exfat-cluster-offset vol (elem-get clusters i))
			(slice data (* i size) (* j size)))
		(setq i j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; writing, the directories
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exfat-stamp ()
	;the time now, as a directory entry keeps it, months from 1
	(bind '(sec min hour day month year &rest _) (date))
	(logior (<< (- year 1980) 25) (<< (inc month) 21) (<< day 16) (<< hour 11) (<< min 5) (>> sec 1)))

(defun exfat-zone ()
	;the time zone a stamp is in, quarter hours from UTC, and the bit
	;that says it is known, or a reader takes the stamp as its own time
	(logior 0x80 (logand (/ (timezone-offset-seconds *local_timezone*) 900) 0x7f)))

(defun exfat-set (vol name dir size start no_fat)
	; (exfat-set vol name dir size start no_fat) -> str
	;the entries of a file, one for the file, one for its data, and as
	;many as its name needs, with the checksum of them all
	(defq units (exfat-units name) stamp (exfat-stamp) zone (exfat-zone)
		names (/ (+ (length units) (dec +exfat_name_units)) +exfat_name_units)
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
	(each (lambda (i)
		(defq e (setf-> (exfat-zeros +exfat_name_size) (+exfat_name_type +exfat_type_name)))
		(each (# (set-short e (+ 2 (* (!) 2)) %0))
			(slice units (* i +exfat_name_units) (min (length units) (* (inc i) +exfat_name_units))))
		(push parts e)) (range 0 names))
	(defq eset (apply (const cat) parts))
	(set-short eset +exfat_file_checksum (exfat-checksum eset))
	eset)

(defun exfat-dir-put (vol entry index data)
	;entries to a directory, from an index on. They can cross from one
	;cluster of the directory to the next, so they go an entry at a time
	(bind '(& & size start no_fat &rest _) entry)
	(defq clusters (exfat-chain vol start size no_fat) shift (get :cluster_shift vol))
	(each (lambda (i)
		(defq at (* (+ index i) +exfat_entry_size))
		(exfat-write vol (+ (exfat-cluster-offset vol (elem-get clusters (>> at shift)))
				(logand at (dec (<< 1 shift))))
			(slice data (* i +exfat_entry_size) (* (inc i) +exfat_entry_size))))
		(range 0 (/ (length data) +exfat_entry_size))))

(defun exfat-dir-data (vol entry)
	(bind '(& & size start no_fat &rest _) entry)
	(exfat-data vol start size no_fat))

(defun exfat-grow (vol found)
	; (exfat-grow vol found) -> :nil | found
	;a directory is given one more cluster. found is the entries from the
	;root down to it, and comes back with the directory as it now is.
	(when (defq got (exfat-alloc vol 1))
		(bind '(name dir size start no_fat index count) (defq entry (last found)))
		(defq fresh (first (first got)) cluster_size (exfat-cluster-size vol)
			clusters (exfat-chain vol start size no_fat))
		(exfat-store vol (list fresh) "")
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
					(setf-> (slice eset +exfat_entry_size (* 2 +exfat_entry_size))
						(+exfat_stream_flags (logior +exfat_flag_alloc (if no_fat +exfat_flag_no_fat 0)))
						(+exfat_stream_valid_length size)
						(+exfat_stream_data_length size))
					(slice eset (* 2 +exfat_entry_size) -1)))
			(set-short eset +exfat_file_checksum (exfat-checksum eset))
			(exfat-dir-put vol parent index eset))
		(exfat-bitmap-save vol)
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

(defun exfat-add (vol path dir data)
	;a new file or directory, of this data, in the directory it names
	(bind '(parent name) (exfat-split path))
	(defq found (exfat-path vol parent) size (length data) cluster_size (exfat-cluster-size vol)
		got (if (> size 0) (exfat-alloc vol (/ (+ size (dec cluster_size)) cluster_size)) (list (list) :nil)))
	(when (and found (second (last found)) (nempty? name) got)
		(bind '(clusters no_fat) got)
		(exfat-store vol clusters data)
		(defq eset (exfat-set vol name dir size (if (empty? clusters) 0 (first clusters)) no_fat)
			slot (exfat-slot vol found (/ (length eset) +exfat_entry_size)))
		(when slot
			(exfat-dir-put vol (last (second slot)) (first slot) eset)
			(exfat-bitmap-save vol)
			(exfat-sync vol)
			:t)))

(defun exfat-delete (vol path)
	; (exfat-delete vol path) -> :nil | :t
	;a file, or a directory that has nothing in it
	(defq found (exfat-path vol path))
	(when (and found (> (length found) 1))
		(bind '(name dir size start no_fat index count) (last found))
		(when (or (not dir) (empty? (exfat-dir vol (last found))))
			(defq parent (elem-get found (- (length found) 2))
				eset (cat (slice (exfat-dir-data vol parent) (* index +exfat_entry_size)
					(* (+ index count 1) +exfat_entry_size))))
			;an entry that is not in use has the top bit of its type clear
			(each (lambda (i)
				(defq at (* i +exfat_entry_size))
				(set-byte eset at (logand (code eset 1 at) (dec +exfat_in_use)))) (range 0 (inc count)))
			(exfat-dir-put vol parent index eset)
			(exfat-release vol start size no_fat)
			(exfat-bitmap-save vol)
			(exfat-sync vol)
			:t)))

(defun exfat-save (vol path data)
	; (exfat-save vol path data) -> :nil | :t
	;a file of this data. One of that name is replaced.
	(defq old (exfat-find vol path))
	(cond
		((and old (second old)) :nil)
		(:t (if old (exfat-delete vol path))
			(exfat-add vol path :nil data))))

(defun exfat-mkdir (vol path)
	; (exfat-mkdir vol path) -> :nil | :t
	;a new directory, of one cluster with nothing in it
	(unless (exfat-find vol path)
		(exfat-add vol path :t (exfat-zeros (exfat-cluster-size vol)))))

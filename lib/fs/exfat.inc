;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; exFAT, the file system, read from a volume.
; A volume is anything that gives bytes at an offset, here a file of
; the host that is the image of a disk. Nothing of the host's own
; file system code is used, the structures on the disk are read here.
;
; Nothing here calls itself. A task has a small stack, and a tree of
; directories can be as deep as it likes, so a walk keeps its own list
; of what is still to do.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;a volume is an environment, with
;	:stream, the device
;	:sector_shift :cluster_shift, the sizes of a sector and a cluster
;	:fat_offset :heap_offset, in sectors, :cluster_count :root
;an entry of a directory is (name dir size first no_fat)
;	no_fat is a file whose clusters run on from first, with no chain

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
	+exfat_attr_dir 0x10 +exfat_flag_no_fat 2 +exfat_name_units 15)

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


(defun exfat-read (vol offset len)
	; (exfat-read vol offset len) -> str
	;bytes of the device
	(defq stream (get :stream vol))
	(stream-seek stream offset 0)
	(read-blk stream len))

(defun exfat-mount (path)
	; (exfat-mount path) -> :nil | vol
	(when (defq stream (file-stream path))
		(def (defq vol (env 1)) :stream stream)
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
				:cluster_shift (+ sector_shift cluster_shift))
			vol)))

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

(defun exfat-chain (vol first len no_fat)
	; (exfat-chain vol first len no_fat) -> (cluster ...)
	;the clusters of a file or a directory. A len of :nil is all of the chain
	(defq size (exfat-cluster-size vol) limit (+ (get :cluster_count vol) 2)
		clusters (list))
	(cond
		((< first 2) clusters)
		(no_fat (range first (+ first (/ (+ len (dec size)) size))))
		(:t (defq want (if len (/ (+ len (dec size)) size) limit))
			(while (and (>= first 2) (< first limit) (< (length clusters) want))
				(push clusters first)
				(setq first (exfat-next vol first)))
			clusters)))

(defun exfat-data (vol first len no_fat)
	; (exfat-data vol first len no_fat) -> str
	;the bytes of a file or a directory, a run of clusters is one read
	(defq size (exfat-cluster-size vol) parts (list)
		clusters (exfat-chain vol first len no_fat) i 0 n (length clusters))
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

(defun exfat-entries (vol first len no_fat)
	; (exfat-entries vol first len no_fat) -> ((name dir size first no_fat) ...)
	;the files and directories of a directory. A file is a set of entries,
	;one for the file, one for its data, then as many as its name needs
	(defq data (exfat-data vol first len no_fat) out (list) i 0
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
					(/= (logand flags +exfat_flag_no_fat) 0)))))
		(setq i (+ i (inc count))))
	out)

(defun exfat-find (vol path)
	; (exfat-find vol path) -> :nil | (name dir size first no_fat)
	;the entry for a path from the root, names are matched without case
	(defq entry (list "" :t :nil (get :root vol) :nil))
	(each (lambda (part)
		(when entry
			(bind '(& dir size first no_fat) entry)
			(setq entry (if dir (some (# (if (eql (to-lower (first %0)) (to-lower part)) %0))
				(exfat-entries vol first size no_fat))))))
		(filter (# (nempty? %0)) (split path "/")))
	entry)

(defun exfat-list (vol path)
	; (exfat-list vol path) -> :nil | ((name dir size first no_fat) ...)
	(when (and (defq entry (exfat-find vol path)) (second entry))
		(bind '(& & size first no_fat) entry)
		(exfat-entries vol first size no_fat)))

(defun exfat-load (vol path)
	; (exfat-load vol path) -> :nil | str
	(when (and (defq entry (exfat-find vol path)) (not (second entry)))
		(bind '(& & size first no_fat) entry)
		(exfat-data vol first size no_fat)))

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

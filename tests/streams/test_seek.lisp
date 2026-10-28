(report-header "Streams: Seek Operations")

; --- 1. String Stream Seek ---
(defq ss (string-stream "abcdefghijklmnopqrstuvwxyz"))
(assert-eq "sstream read-char start" (ascii-code "a") (read-char ss))

(stream-seek ss 5 0) ; Seek absolute to 5 ('f')
(assert-eq "sstream seek absolute" (ascii-code "f") (read-char ss))

(stream-seek ss 2 1) ; Seek relative +2 to 8 ('i')
(assert-eq "sstream seek relative positive" (ascii-code "i") (read-char ss))

(stream-seek ss -3 1) ; Seek relative -3 to 6 ('g')
(assert-eq "sstream seek relative negative" (ascii-code "g") (read-char ss))

(stream-seek ss -2 2) ; Seek from end -2 to 24 ('y')
(assert-eq "sstream seek from end" (ascii-code "y") (read-char ss))

(defq ss :nil)

; --- 2. Memory Stream Seek ---
(defq ms (memory-stream))
(write-blk ms "abcdefghijklmnopqrstuvwxyz")
(stream-seek ms 0 0)
(assert-eq "mstream read-char start" (ascii-code "a") (read-char ms))

(stream-seek ms 5 0) ; Seek absolute to 5 ('f')
(assert-eq "mstream seek absolute" (ascii-code "f") (read-char ms))

(stream-seek ms 2 1) ; Seek relative +2 to 8 ('i')
(assert-eq "mstream seek relative positive" (ascii-code "i") (read-char ms))

(stream-seek ms -3 1) ; Seek relative -3 to 6 ('g')
(assert-eq "mstream seek relative negative" (ascii-code "g") (read-char ms))

(stream-seek ms -2 2) ; Seek from end -2 to 24 ('y')
(assert-eq "mstream seek from end" (ascii-code "y") (read-char ms))

(defq ms :nil)

; --- 3. File Stream Seek ---
(defq temp_file "obj/tmp_seek_test.txt")
(save "abcdefghijklmnopqrstuvwxyz" temp_file)
(defq fs (file-stream temp_file))
(assert-true "fstream open" (not (nil? fs)))
(assert-eq "fstream read-char start" (ascii-code "a") (read-char fs))

; Absolute seek from start (Typically succeeds as it resets the buffer or seeks to an absolute index)
(stream-seek fs 5 0) ; Seek absolute to 5 ('f')
(assert-eq "fstream seek absolute" (ascii-code "f") (read-char fs))

; Relative seek forward (Demonstrates the buffering offset discrepancy)
(stream-seek fs 2 1) ; Seek relative +2 to 8 ('i')
(assert-eq "fstream seek relative positive" (ascii-code "i") (read-char fs))

; Relative seek backward (Demonstrates the buffering offset discrepancy)
(stream-seek fs -3 1) ; Seek relative -3 to 6 ('g')
(assert-eq "fstream seek relative negative" (ascii-code "g") (read-char fs))

; Seek from end
(stream-seek fs -2 2) ; Seek from end -2 to 24 ('y')
(assert-eq "fstream seek from end" (ascii-code "y") (read-char fs))

; Clean up stream to safely release OS file descriptor prior to removal
(defq fs :nil)
(pii-remove temp_file)

;a file stream flushed, and flushed again with nothing written between.
;The second had no buffer to send, and worked out a length less than
;nothing for the host to write, class/fstream/class.vp. A host is free to
;do what it likes with that, found on a host that did not ignore it
(defq fl_file "tests/scratch/flush_twice.txt" fl_stream (file-stream fl_file +file_open_write))
(write-blk fl_stream "one ")
(stream-flush fl_stream)
(stream-flush fl_stream)
(stream-flush fl_stream)
(write-blk fl_stream "two")
(stream-flush fl_stream)
(setq fl_stream :nil)
(assert-eq "flushed more than once, a file is what was written to it" "one two" (load fl_file))
(defq fl_stream (file-stream fl_file +file_open_write))
(stream-flush fl_stream)
(setq fl_stream :nil)
(assert-eq "and one flushed with nothing ever written is empty" 0 (length (ifn (load fl_file) "")))
(pii-remove fl_file)


;a read or write of more than a file stream's buffer goes to the host past
;the buffer, class/fstream/class.vp :read_direct and :write_direct. What
;the buffer holds goes first, and small reads and writes after are as they were
(defq dr_file "obj/tmp_direct_test.bin"
	dr_big (apply (const cat) (map (# (char (logand (* %0 7) 255))) (range 0 20000)))
	dr_mid (apply (const cat) (map (# (char (logand (* %0 13) 255))) (range 0 9000)))
	dr_all (cat "small" dr_big "tiny" dr_mid "end")
	dr_stream (file-stream dr_file +file_open_write))
(each (# (write-blk dr_stream %0)) (list "small" dr_big "tiny" dr_mid "end"))
(setq dr_stream :nil)
(assert-eq "small, big, small, big and small writes, a file of them in order" dr_all (load dr_file))
(defq dr_stream (file-stream dr_file))
(assert-eq "a small read" "sma" (read-blk dr_stream 3))
(assert-eq "a big read, what was in the buffer first" (slice dr_all 3 15003) (read-blk dr_stream 15000))
(assert-eq "a small read after it" (slice dr_all 15003 15010) (read-blk dr_stream 7))
(assert-eq "a big read past the end is what there is" (slice dr_all 15010 -1) (read-blk dr_stream 50000))
(assert-eq "and then the end" :nil (read-blk dr_stream 50000))
(stream-seek dr_stream 100 0)
(assert-eq "a big read after a seek" (slice dr_all 100 10100) (read-blk dr_stream 10000))
(stream-seek dr_stream -5 1)
(assert-eq "a relative seek after a big read" (slice dr_all 10095 10105) (read-blk dr_stream 10))
(setq dr_stream :nil)
(pii-remove dr_file)

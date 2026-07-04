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
(defq temp_file "tmp_seek_test.txt")
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
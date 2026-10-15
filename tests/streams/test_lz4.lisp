(report-header "Streams: LZ4 Compression")
(import "lib/streams/lz4.inc")

(defun test-lz4 (name in_str window_size)
	(defq in_ms (string-stream in_str))
	(defq out_ms (memory-stream))
	(lz4-compress in_ms out_ms window_size)
	(stream-seek out_ms 0 0)
	(defq dec_ms (memory-stream))
	(lz4-decompress out_ms dec_ms window_size)
	(stream-seek dec_ms 0 0)
	(assert-eq name in_str (read-blk dec_ms (length in_str))))

; Test 1: Simple short string
(test-lz4 "LZ4 Short" "Hello World!" 65536)

; Test 2: Repetitive string (should compress well)
(test-lz4 "LZ4 Repetitive" "AAAAABBBBBCCCCCDDDDD" 65536)

; Test 3: Longer repetitive string
(defq long_rep "")
(times 100 (setq long_rep (cat long_rep "ABCDEF")))
(test-lz4 "LZ4 Long Repetitive" long_rep 65536)

; Test 4: Small window size (multiple blocks)
(defq small_win_str "")
(times 100 (setq small_win_str (cat small_win_str "1234567890")))
(test-lz4 "LZ4 Small Window" small_win_str 64)

; Test 5: No compression case (random-ish data)
(test-lz4 "LZ4 Uncompressed" "zyxwvutsrqponmlkjihgfedcba0987654321" 65536)

; Test 6: Overlapping matches
(test-lz4 "LZ4 Overlap" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" 65536)

; Test 7: Large literal block (> 15 bytes)
(defq large_lit "" i 0)
(while (< i 100)
	(setq large_lit (cat large_lit (char (+ (code "a") (% i 26)))))
	(++ i))
(test-lz4 "LZ4 Large Literals" large_lit 65536)

; Test 8: Large match block (> 19 bytes)
(defq large_match "START")
(times 300 (setq large_match (cat large_match "A")))
(setq large_match (cat large_match "END"))
(test-lz4 "LZ4 Large Match" large_match 65536)

; Test 9: Large dataset with sliding window (256KB)
(defq large_data "")
(times 1024 (setq large_data (cat large_data "ChrysaLisp LZ4 Sliding Window Test Case Data Block ")))
(test-lz4 "LZ4 Large Sliding Window" large_data 65536)

;the frame's head, the size of a block it says and the sum of the two
;bytes, as the lz4 tool writes them
(defun lz4-head (window_size)
	(defq out_ms (memory-stream))
	(lz4-compress (string-stream "x") out_ms window_size)
	(stream-seek out_ms 0 0)
	(map (# (code %0)) (slice (read-blk out_ms 7) 4 7)))
(assert-list-eq "a head of 64KB blocks" '(0x40 0x40 0xc0) (lz4-head 65536))
(assert-list-eq "of 256KB" '(0x40 0x50 0x77) (lz4-head 100000))
(assert-list-eq "of 1MB" '(0x40 0x60 0x96) (lz4-head 1048576))
(assert-list-eq "of 4MB, the most" '(0x40 0x70 0xdf) (lz4-head 100000000))

;blocks of 1MB, given to a decompress told of 64KB: the frame says how big
;its blocks are, and that is what is used. A block of what does not pack
;goes as it is, the whole 300KB of it
(defq lz4_big (cap 300000 (list)) lz4_seed 1)
(times 300000 (push lz4_big (char (logand (>> (setq lz4_seed (logand (+ (* lz4_seed 1103515245) 12345) 0x7fffffff)) 16) 0xff))))
(setq lz4_big (apply (const cat) lz4_big))
(defq lz4_in (memory-stream) lz4_out (memory-stream))
(lz4-compress (string-stream lz4_big) lz4_in 1048576)
(stream-seek lz4_in 0 0)
(assert-true "a frame of 1MB blocks read with a window of 64KB" (lz4-decompress lz4_in lz4_out 65536))
(stream-seek lz4_out 0 0)
(assert-true "and it is what went in" (eql lz4_big (read-blk lz4_out 300000)))

;a block bigger than its frame says is not read
(defq lz4_bad (memory-stream))
(write-int lz4_bad 0x184D2204)
(write-byte lz4_bad '(0x40 0x40 0xc0))
(write-int lz4_bad (logior 0x80000000 70000))
(write-blk lz4_bad (str-alloc 70000))
(write-int lz4_bad 0)
(stream-seek lz4_bad 0 0)
(assert-eq "a block bigger than the frame says, refused" :nil (lz4-decompress lz4_bad (memory-stream)))

;a frame the lz4 tool made, of the size of its content, a sum of each
;block and of the whole, all passed over
(defq lz4_tool (hex-decode "04224d187c408a00000000000000803f000000f01074686520717569636b2062726f776e20666f78206a756d7073206f766572201f00a16c617a7920646f6720310f000f2e00151f322e0017506f67203320a26e490300000000d53d6cc5")
	lz4_fox "the quick brown fox jumps over the lazy dog 1 the quick brown fox jumps over the lazy dog 2 the quick brown fox jumps over the lazy dog 3 "
	lz4_out (memory-stream))
(assert-true "a frame of the lz4 tool" (lz4-decompress (string-stream lz4_tool) lz4_out))
(stream-seek lz4_out 0 0)
(assert-eq "is read" lz4_fox (read-blk lz4_out (length lz4_fox)))

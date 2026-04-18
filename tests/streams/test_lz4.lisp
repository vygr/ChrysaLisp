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

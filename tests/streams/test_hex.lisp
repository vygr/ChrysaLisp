(report-header "Streams: Hex Encoding & Decoding")
(import "lib/streams/hex.inc")

; --- Roundtrip with default flags (offset + chars) ---
(defq hex_in_str "Hello World! 1234567890\nSecond line of test data.")
(defq in_ms (string-stream hex_in_str) out_enc_ms (memory-stream))
(hex-encode-stream in_ms out_enc_ms 8)
(stream-seek out_enc_ms 0 0)

(defq out_dec_ms (memory-stream))
(hex-decode-stream out_enc_ms out_dec_ms 8)
(stream-seek out_dec_ms 0 0)
(assert-eq "Hex Roundtrip Default" hex_in_str (read-blk out_dec_ms (length hex_in_str)))

; --- Verify formatting of first line ---
(stream-seek out_enc_ms 0 0)
(defq first_line (read-line out_enc_ms))
(assert-eq "Hex First Line" "00000000 48 65 6C 6C 6F 20 57 6F Hello Wo" first_line)

; --- Roundtrip with no chars column ---
(defq in_ms2 (string-stream "Testing no-chars flag"))
(defq enc_ms2 (memory-stream))
(hex-encode-stream in_ms2 enc_ms2 8 +hex_stream_flag_offset)
(stream-seek enc_ms2 0 0)
(defq dec_ms2 (memory-stream))
(hex-decode-stream enc_ms2 dec_ms2 8 +hex_stream_flag_offset)
(stream-seek dec_ms2 0 0)
(assert-eq "Hex Roundtrip No Chars" "Testing no-chars flag" (read-blk dec_ms2 21))

; --- Roundtrip with no offset column ---
(defq in_ms3 (string-stream "Testing no-offset flag"))
(defq enc_ms3 (memory-stream))
(hex-encode-stream in_ms3 enc_ms3 8 +hex_stream_flag_chars)
(stream-seek enc_ms3 0 0)
(defq dec_ms3 (memory-stream))
(hex-decode-stream enc_ms3 dec_ms3 8 +hex_stream_flag_chars)
(stream-seek dec_ms3 0 0)
(assert-eq "Hex Roundtrip No Offset" "Testing no-offset flag" (read-blk dec_ms3 22))

; --- Roundtrip with chunk size 16 ---
(defq in_ms4 (string-stream "Chunk size sixteen testing block data"))
(defq enc_ms4 (memory-stream))
(hex-encode-stream in_ms4 enc_ms4 16)
(stream-seek enc_ms4 0 0)
(defq dec_ms4 (memory-stream))
(hex-decode-stream enc_ms4 dec_ms4 16)
(stream-seek dec_ms4 0 0)
(assert-eq "Hex Roundtrip Chunk 16" "Chunk size sixteen testing block data" (read-blk dec_ms4 37))

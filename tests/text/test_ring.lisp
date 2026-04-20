(import "lib/text/ring.inc")

(report-header "Ring Buffer")

(defun get-ring-content (ring len)
	(defq ms (memory-stream))
	(. ring :write_stream ms len)
	(stream-seek ms 0 0)
	(defq res (read-blk ms len))
	res)

; --- Basic Write ---
(defq r (Ring 16))
(. r :write "Hello")
(assert-eq "Basic write" "Hello" (get-ring-content r 5))
(assert-eq "Pos check" 5 (. r :get_pos))

; --- Wrapping Write ---
(. r :write "12345678901") ; Total 16 bytes
(assert-eq "Full buffer" "Hello12345678901" (get-ring-content r 16))
(assert-eq "Pos check 2" 16 (. r :get_pos))

(. r :write "ABC") ; Wraps around
(assert-eq "Wrapped content" "lo12345678901ABC" (get-ring-content r 16))
(assert-eq "Pos check 3" 19 (. r :get_pos))

; --- Match (No overlap) ---
(. r :match 14 3) ; should write "123" again
(assert-eq "Match no-overlap" "2345678901ABC123" (get-ring-content r 16))
(assert-eq "Pos check 4" 22 (. r :get_pos))

; --- Match (Overlap / RLE) ---
(. r :write "!") ; pos 23
(. r :match 1 5) ; repeat "!" 5 times. pos becomes 28.
(assert-eq "Match overlap" "8901ABC123!!!!!!" (get-ring-content r 16))
(assert-eq "Pos check 5" 28 (. r :get_pos))

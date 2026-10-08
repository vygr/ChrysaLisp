(report-header "Crypto: random bytes from the host")
(import "lib/crypto/random.inc")

(defq rb_a (random-bytes 32) rb_b (random-bytes 32))
(assert-true "a str" (str? rb_a))
(assert-eq "of the size asked for" 32 (length rb_a))
(assert-true "two of them are not the same" (not (eql rb_a rb_b)))
(assert-eq "none at all" "" (random-bytes 0))
(assert-eq "one byte" 1 (length (random-bytes 1)))
(assert-eq "a size that is not a whole number of words" 13 (length (random-bytes 13)))
;every value a byte can have turns up in 64KB, and no one value far more
;than its share, which is 256. A source that was stuck, or was all text,
;would not pass
(defq rb_big (random-bytes 65536) rb_counts (map (lambda (_) 0) (range 0 256)))
(each (# (elem-set rb_counts (code %0) (inc (elem-get rb_counts (code %0))))) rb_big)
(assert-eq "64KB of them" 65536 (length rb_big))
(assert-eq "every value of a byte turns up" 256 (length (filter (# (> %0 0)) rb_counts)))
(assert-true "and none far more than its share" (< (reduce (const max) rb_counts 0) 400))
(assert-true "nor far less" (> (reduce (const min) rb_counts 65536) 150))
(assert-error "a size below 0" (random-bytes -1))
(assert-error "a size that is not a number" (random-bytes "32"))

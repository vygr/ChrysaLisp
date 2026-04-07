(report-header "Streams: Bit Pool Primitives")

(defq bit_ms (memory-stream))
(defq w_bit_pool (array 0 0))
(write-bits bit_ms w_bit_pool 0xA 4)
(write-bits bit_ms w_bit_pool 0x5 4)
(flush-bits bit_ms w_bit_pool)
(stream-seek bit_ms 0 0)

(defq r_bit_pool (array 0 0))
(assert-eq "read-bits 1" 0xA (read-bits bit_ms r_bit_pool 4))
(assert-eq "read-bits 2" 0x5 (read-bits bit_ms r_bit_pool 4))

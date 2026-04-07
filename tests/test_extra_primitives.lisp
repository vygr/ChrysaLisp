(report-header "Extra Primitives (Huffman, Bits, Mail)")

(import "lib/streams/huffman.inc")
(import "lib/streams/rle.inc")

; --- RLE Compression ---
(defq rle_in_str "AAAAABBBCCDAAAAA")
(defq rle_in_ms (string-stream rle_in_str))
(defq rle_out_ms (memory-stream))
(rle-compress rle_in_ms rle_out_ms 8 8)
(stream-seek rle_out_ms 0 0)
(defq rle_dec_ms (memory-stream))
(rle-decompress rle_out_ms rle_dec_ms 8 8)
(stream-seek rle_dec_ms 0 0)
(assert-eq "RLE Roundtrip" rle_in_str (read-blk rle_dec_ms (length rle_in_str)))

; --- Huffman (Static) Compression ---
(defq huff_in_str "the quick brown fox jumps over the lazy dog")
(defq huff_in_ms (string-stream huff_in_str))
(defq freq_map (huffman-build-freq-map huff_in_ms 8))
(stream-seek huff_in_ms 0 0)
(defq huff_out_ms (memory-stream))
(defq model (build_tree_and_codebook freq_map))
(push model 8) ; token_bits = 8
(huffman-compress-static huff_in_ms huff_out_ms model)
(stream-seek huff_out_ms 0 0)
(defq huff_dec_ms (memory-stream))
(huffman-decompress-static huff_out_ms huff_dec_ms model)
(stream-seek huff_dec_ms 0 0)
(assert-eq "Huffman Static Roundtrip" huff_in_str (read-blk huff_dec_ms (length huff_in_str)))

; --- Bit Stream Primitives ---
(defq bit_ms (memory-stream))
(defq w_bit_pool (array 0 0))
(write-bits bit_ms w_bit_pool 0xA 4)
(write-bits bit_ms w_bit_pool 0x5 4)
(flush-bits bit_ms w_bit_pool)
(stream-seek bit_ms 0 0)

(defq r_bit_pool (array 0 0))
(assert-eq "read-bits 1" 0xA (read-bits bit_ms r_bit_pool 4))
(assert-eq "read-bits 2" 0x5 (read-bits bit_ms r_bit_pool 4))

; --- Mail Messaging ---
(defq test_mbox (mail-mbox))
(defq service_name "TestService")
(defq service_info "TestInfo")
(defq service_key (mail-declare test_mbox service_name service_info))
(assert-true "mail-declare" (not (nil? service_key)))
(task-sleep 10000)
(defq enquiries (mail-enquire service_name))
(assert-true "mail-enquire" (nempty? enquiries))
(mail-forget service_key)

; --- Core Environment Utils ---
(defq e_parent (env))
(defq e_child (env-push e_parent))
((lambda ()
	(defq local_sym 555)
	(export-symbols '(local_sym))
))
(assert-eq "export-symbols" 555 (get 'local_sym e_parent))

(defclass TestClassExtra () :nil (defmethod :test (this) 123))
(export-classes '(TestClassExtra))
(assert-true "export-classes" (not (nil? (get 'TestClassExtra))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Random bytes, from the host.
;
; (random-bytes size) -> a str of that many bytes that can not be
; guessed, from the host's own source, /dev/urandom on a Mac and on
; Linux, RtlGenRandom on Windows. It is what a key or a nonce is
; made from.
;
; (random num), of the language, is not that. It gives numbers
; from a seed, quick, and good for a game or a test, and anyone
; who knows the seed knows them all.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi (path-to-absolute "./random_bytes") random-bytes)
; (random-bytes size) -> str

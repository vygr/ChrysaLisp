(report-header "System: a buf, a block of memory the nodes of this machine share")

(import "class/buf/lisp.inc")

;made, under a key at random, and opened again by the key
(defq made (buf 4096) key (buf-key made) found (buf 4096 key))
(assert-true "made" made)
(assert-true "a buf" (buf? made))
(assert-true "not a buf" (not (buf? "4096")))
(assert-true "it has a key" (/= key 0))
(assert-eq "its size" 4096 (buf-size made))
(assert-true "the one that made it owns the key" (bits? (getf made +buf_flags 0) +buf_flag_owner))
(assert-true "opened by its key" found)
(assert-eq "the one that opened it has the key" key (buf-key found))
(assert-true "and does not own it" (not (bits? (getf found +buf_flags 0) +buf_flag_owner)))
(assert-true "mapped where it is mapped, not the one made" (/= 0 (getf found +buf_data 0)))
(assert-true "two keys differ" (/= key (buf-key (buf 4096))))

;what can not be had
(assert-eq "one that was never made is not opened" :nil (buf 4096 (+ key 1)))
;a block is whole pages, so one only a little bigger may be there
(assert-eq "one far bigger than was made is not opened" :nil (buf (* 4096 1024) key))
(assert-eq "no size is none" :nil (buf 0))
(assert-eq "less than none is none" :nil (buf -1))
(assert-error "a size that is not a number" (buf "4096"))

;its bytes, written and read as (obj-set) and (obj-get) do an object's
;fields, but from the start of the block
(defq b (buf 4096))
(assert-eq "a new block is zero" 0 (buf-get b 0 +type_long))
(each (lambda ((type off val want))
		(buf-set b off type val)
		(assert-eq (cat "a " (str type) " at " (str off)) want (buf-get b off type)))
	(list (list +type_byte 1 -2 -2) (list +type_ubyte 2 254 254) (list +type_ubyte 3 -1 255)
		(list +type_short 4 -300 -300) (list +type_ushort 6 65000 65000)
		(list +type_int 8 -70000 -70000) (list +type_uint 12 0xfffffffe 0xfffffffe)
		(list +type_long 16 -5000000000 -5000000000) (list +type_fixed 24 1.5 1.5)
		(list +type_double 32 (n2d 2.5) (n2d 2.5))))
(assert-eq "the last long of the block" 99 (buf-get (buf-set b 4088 +type_long 99) 4088 +type_long))
(buf-set b 100 11 "hello world")
(assert-eq "bytes in and out, as a str" "hello world" (buf-get b 100 11))
(assert-eq "each byte of them" (code "w") (buf-get b 106 +type_ubyte))
(assert-eq "a buf-set gives the buf" b (buf-set b 0 +type_byte 0))
;a vector of numbers in whole, its elements' bytes, and back by push
(defq bv (doubles (n2d 1.5) (n2d -2.25) (n2d 1000)) bn (nums 7 -8 9))
(buf-set b 200 (* 3 +long_size) bv)
(assert-eq "a doubles in whole, back by push" bv (push (doubles) (buf-get b 200 (* 3 +long_size))))
(buf-set b 232 (* 3 +long_size) bn)
(assert-eq "a nums in whole" bn (push (nums) (buf-get b 232 (* 3 +long_size))))
(assert-eq "an element of it" -8 (buf-get b 240 +type_long))
(assert-eq "part of a vector" 1000 (n2i (buf-get (buf-set b 256 +long_size (doubles (n2d 1000))) 256 +type_double)))
(assert-error "a vector with fewer bytes than asked" (buf-set b 200 (* 4 +long_size) bv))
(assert-error "a str with fewer bytes than asked" (buf-set b 100 20 "hello"))
(assert-error "not a list, its elements are objects" (buf-set b 0 8 (list 1)))
(assert-error "not a map" (buf-set b 0 8 (pmap :a 1)))

(assert-error "past the end" (buf-get b 4089 +type_long))
(assert-error "bytes past the end" (buf-get b 4090 8))
(assert-error "not at its alignment" (buf-get b 4 +type_long))
(assert-error "before the start" (buf-get b -8 +type_long))
(assert-error "no object references" (buf-get b 0 0))
(assert-error "no object references set" (buf-set b 0 0 b))
;the type past the last, +type_float
(assert-error "not a type" (buf-get b 0 (dec +type_float)))
(assert-eq "a float" (n2f -2.5) (buf-get (buf-set b 8 +type_float (n2f -2.5)) 8 +type_float))
(assert-error "not a buf" (buf-get "abcdefgh" 0 +type_long))

;opened on another node of this machine, if there is one, a process of its
;own, where it is mapped at an address of its own
(defq here (task-nodeid) there (some (# (if (nql %0 here) %0)) (lisp-nodes :t))
	reply (mail-mbox) long (task-timeout 30))
(cond
	(there
		;it reads what this one wrote, and writes back for this one to read
		(buf-set made 0 +type_long 12345678)
		(open-task (str `(progn
					(import "class/buf/lisp.inc")
					(defq b (buf 4096 ,key))
					(when b (buf-set b 8 +type_long (+ (buf-get b 0 +type_long) 1)))
					(mail-send (hex-decode ,(hex-encode reply))
						(if b (str (buf-key b) " " (buf-size b) " " (buf-get b 0 +type_long)) "none"))))
			there +kn_call_pin 0 (mail-mbox))
		(assert-eq "opened by its key on another node, and read there" (str key " 4096 12345678")
			(ifn (mail-read-timeout reply long) "silent"))
		(assert-eq "and what it wrote is seen here" 12345679 (buf-get made 8 +type_long)))
	(:t (test-skip "opened by its key on another node" "this machine has one node")))

;the one that made it goes. The one that opened it keeps the block, the
;key has gone. Windows has no way to let go of a name that is still held,
;there the key goes with the last to hold it
(setq made :nil)
(assert-eq "the block outlasts the one that made it" key (buf-key found))
(unless (eql (os) 'Windows)
	(assert-eq "the key went with the one that made it" :nil (buf 4096 key)))
(setq found :nil)
(assert-eq "the key has gone when no one holds it" :nil (buf 4096 key))

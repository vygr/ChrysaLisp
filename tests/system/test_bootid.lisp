(report-header "Boot id: what a boot image was built from, as one number, the same on every CPU")

(import "lib/boot/id.inc")

(defq bi_dir (cat "obj/" (cpu) "/" (abi) "/tests/") bi_a (cat bi_dir "bootid_a.vp") bi_b (cat bi_dir "bootid_b.inc"))
(save "(def-func 'a) (def-func-end)" bi_a)
(save "(defq +b 1)" bi_b)
(defq bi_id (boot-id-of (list bi_a bi_b)))
(assert-eq "an id is the 64 hex digits of a hash" 64 (length bi_id))
(assert-eq "and lower case" bi_id (to-lower bi_id))
(assert-eq "the same files give the same id" bi_id (boot-id-of (list bi_a bi_b)))
(assert-eq "in whatever order they are given" bi_id (boot-id-of (list bi_b bi_a)))
(assert-true "fewer files, another id" (nql bi_id (boot-id-of (list bi_a))))
(save "(defq +b 2)" bi_b)
(assert-true "a change of one character in one of them, another id" (nql bi_id (boot-id-of (list bi_a bi_b))))
(save "(defq +b 1)" bi_b)
(assert-eq "put back, the id it was" bi_id (boot-id-of (list bi_a bi_b)))
(pii-remove bi_a) (pii-remove bi_b)

;this node's own, written when its boot image was made. An emulated node
;has one only if the vp64 image was made on this machine since there were ids
(defq bi_mine (boot-id))
(assert-true "this node's id is an id, or it has none" (or (eql bi_mine "") (= (length bi_mine) 64)))
(assert-eq "it is where the boot image is" bi_mine
	(trim (ifn (load (boot-id-file (second (split (load-path) "/")) (third (split (load-path) "/")))) "")
		(const (char-class " \t\r\n"))))

(report-header "VP: what an instruction does is the same on every machine, docs/vm/vp_vm.md")

;VP functions written here, assembled for the CPU this node is on as the
;shader back end does it, lib/gpu/vp.inc (sv-native), and run: on each
;native host, the VP64 emulator and the machines of QEMU this checks the
;translator of that CPU against the spec

(defq vs_ops '(seq sne slt sle sgt sge) vs_lf "\n")

(defun vs-native (name text)
	; (vs-native name text) -> func
	;the function assembled afresh from the text, the translator may have
	;changed since one was made
	(bind '(& *cpu* *abi*) (map (const sym) (split (load-path) "/")))
	(defq obj (cat "obj/" *cpu* "/" *abi* "/" name))
	(pii-remove obj)
	(save text (cat obj ".vp"))
	(within-compile-env (lambda ()
		(each (lambda (entry) (vp-translate-save entry)) (vp-compile (cat obj ".vp")))))
	(pii-remove (cat obj ".vp"))
	(ffi name))

(defun vs-head (name)
	(cat "(include {lib/asm/func.inc})" vs_lf "(include {class/num/class.inc})" vs_lf
		"(include {class/lisp/class.inc})" vs_lf "(def-func {" name "})" vs_lf
		"(vp-push :r0)" vs_lf "(list-bind-args :r1 '(:r2 :r3) '(:num :num))" vs_lf
		"(vp-xor-rr :r4 :r4)" vs_lf))

(defun vs-tail ()
	(cat "(call :num :create '(:r4) '(:r1))" vs_lf "(vp-pop :r0)" vs_lf "(vp-ret)" vs_lf "(def-func-end)" vs_lf))

;each set of a and b, and of a and 5, a bit of the answer: 1 is its bit, 0
;none, -1 or any other is wrong. Then a && b, !a, and b after the &&
(defq vs_sets (vs-native "tests/vp_spec_sets" (cat (vs-head "tests/vp_spec_sets")
	(apply (const cat) (map (lambda (op i)
		(cat "(vp-cpy-rr :r2 :r5)" vs_lf "(vp-" op "-rr :r3 :r5)" vs_lf
			"(vp-shl-cr " (str i) " :r5)" vs_lf "(vp-add-rr :r5 :r4)" vs_lf
			"(vp-cpy-rr :r2 :r5)" vs_lf "(vp-" op "-cr 5 :r5)" vs_lf
			"(vp-shl-cr " (str (+ i 6)) " :r5)" vs_lf "(vp-add-rr :r5 :r4)" vs_lf))
		vs_ops (range 0 6)))
	"(vp-cpy-rr :r2 :r5)" vs_lf "(vp-land-rr :r3 :r5)" vs_lf "(vp-shl-cr 12 :r5)" vs_lf "(vp-add-rr :r5 :r4)" vs_lf
	"(vp-cpy-rr :r2 :r5)" vs_lf "(vp-lnot-rr :r5)" vs_lf "(vp-shl-cr 13 :r5)" vs_lf "(vp-add-rr :r5 :r4)" vs_lf
	"(vp-shl-cr 16 :r3)" vs_lf "(vp-add-rr :r3 :r4)" vs_lf
	(vs-tail))))

(defun vs-sets-want (a b)
	(defq bit (lambda (t i) (if t (<< 1 i) 0)))
	(+ (bit (= a b) 0) (bit (/= a b) 1) (bit (< a b) 2) (bit (<= a b) 3) (bit (> a b) 4) (bit (>= a b) 5)
		(bit (= a 5) 6) (bit (/= a 5) 7) (bit (< a 5) 8) (bit (<= a 5) 9) (bit (> a 5) 10) (bit (>= a 5) 11)
		(bit (and (/= a 0) (/= b 0)) 12) (bit (= a 0) 13) (<< b 16)))

(defq vs_pairs '((3 3) (2 7) (7 2) (5 5) (0 4) (4 0) (0 0) (-3 2) (2 -3) (9 9)))
(each (lambda ((a b))
		(assert-eq (cat "the sets, && and ! of " (str a) " and " (str b) ", each 0 or 1, and b as it was")
			(vs-sets-want a b) (vs_sets a b)))
	vs_pairs)

;each branch to a label over 8KB on, so that a CPU whose branches reach
;less, RISC-V's 4KB, takes its long form, and back: of two the same and
;two not, each taken or not as the compare says
(defq vs_long (vs-native "tests/vp_spec_long" (cat (vs-head "tests/vp_spec_long")
	(apply (const cat) (map (lambda (op i)
		(cat "(vp-b" (slice op 1 -1) "-rr :r3 :r2 'far" (str i) ")" vs_lf "(vp-label 'back" (str i) ")" vs_lf))
		vs_ops (range 0 6)))
	"(vp-jmp 'done)" vs_lf "(vp-align 8192)" vs_lf
	(apply (const cat) (map (lambda (i)
		(cat "(vp-label 'far" (str i) ")" vs_lf "(vp-or-cr " (str (<< 1 i)) " :r4)" vs_lf "(vp-jmp 'back" (str i) ")" vs_lf))
		(range 0 6)))
	"(vp-label 'done)" vs_lf
	(vs-tail))))

(defun vs-long-want (a b)
	(defq bit (lambda (t i) (if t (<< 1 i) 0)))
	(+ (bit (= a b) 0) (bit (/= a b) 1) (bit (< a b) 2) (bit (<= a b) 3) (bit (> a b) 4) (bit (>= a b) 5)))

(each (lambda ((a b))
		(assert-eq (cat "the long branches of " (str a) " and " (str b))
			(vs-long-want a b) (vs_long a b)))
	vs_pairs)

;each float branch of a and b as floats, a 999 made a NaN, 0.0 / 0.0: of a
;NaN, as IEEE has it, every compare is false but bne
(defq vs_ff (vs-native "tests/vp_spec_ff" (cat (vs-head "tests/vp_spec_ff")
	"(vp-cvt-rf :r2 :f0)" vs_lf "(vp-cvt-rf :r3 :f1)" vs_lf
	"(vp-bne-cr 999 :r2 'real)" vs_lf "(vp-xor-rr :r5 :r5)" vs_lf "(vp-cvt-rf :r5 :f0)" vs_lf
	"(vp-div-ff :f0 :f0)" vs_lf "(vp-label 'real)" vs_lf
	(apply (const cat) (map (lambda (op i)
		(cat "(vp-b" (slice op 1 -1) "-ff :f1 :f0 'took" (str i) ")" vs_lf "(vp-jmp 'next" (str i) ")" vs_lf
			"(vp-label 'took" (str i) ")" vs_lf "(vp-or-cr " (str (<< 1 i)) " :r4)" vs_lf "(vp-label 'next" (str i) ")" vs_lf))
		vs_ops (range 0 6)))
	(vs-tail))))

(each (lambda ((a b))
		(assert-eq (cat "the float branches of " (str a) " and " (str b))
			(vs-long-want a b) (vs_ff a b)))
	'((3 3) (2 7) (7 2) (-4 -4) (-4 1)))
(assert-eq "of a NaN, only bne is taken" 2 (vs_ff 999 1))
(assert-eq "of a NaN and 0, only bne too" 2 (vs_ff 999 0))

;constants of any size: what does not fit an instruction of a target is made
;another way, docs/vm/vp_vm.md. Each op of a and a constant past 32 bits,
;and a load and a store at an offset of 2^33, from a base 2^33 below a word
(defq vs_big_c '((add 0x10000000003) (sub 0x800000000) (and 0x0f0f0f0f0f0f) (or 0x200000000000)
	(xor 0x123456789a) (mul 0x100000003) (min 0x30000000000) (max -0x40000000000)))
(defq vs_big (vs-native "tests/vp_spec_big" (cat (vs-head "tests/vp_spec_big")
	"(vp-cpy-rr :r2 :r4)" vs_lf
	(apply (const cat) (map (lambda ((op c)) (cat "(vp-" op "-cr " (str c) " :r4)" vs_lf)) vs_big_c))
	;a compare and branch, and a set, of a constant past 32 bits
	"(vp-blt-cr 0x7000000000 :r3 'small)" vs_lf "(vp-or-cr 1 :r4)" vs_lf "(vp-label 'small)" vs_lf
	"(vp-cpy-rr :r3 :r5)" vs_lf "(vp-sgt-cr 0x7000000000 :r5)" vs_lf "(vp-shl-cr 1 :r5)" vs_lf "(vp-xor-rr :r5 :r4)" vs_lf
	;a word on the stack, by a base 2^33 below it
	"(vp-alloc 8)" vs_lf "(vp-cpy-rr :rsp :r6)" vs_lf "(vp-sub-cr 0x200000000 :r6)" vs_lf
	"(vp-cpy-ri :r4 :r6 0x200000000)" vs_lf "(vp-cpy-ir :r6 0x200000000 :r7)" vs_lf
	"(vp-add-cr 5 :r7)" vs_lf "(vp-cpy-ri-i :r7 :r6 0x200000000)" vs_lf "(vp-cpy-ir-i :r6 0x200000000 :r4)" vs_lf
	"(vp-free 8)" vs_lf
	(vs-tail))))

(defun vs-big-want (a b)
	(defq r a)
	(each (lambda ((op c))
		(setq r (case op
			(add (+ r c)) (sub (- r c)) (and (logand r c)) (or (logior r c))
			(xor (logxor r c)) (mul (* r c)) (min (min r c)) (max (max r c)))))
		vs_big_c)
	(if (>= b 0x7000000000) (setq r (logior r 1)))
	(if (> b 0x7000000000) (setq r (logxor r 2)))
	;the low 4 bytes of the word, 5 more, as an int read back signed
	(defq w (+ (logand r 0xffffffff) 5))
	(setq w (logand w 0xffffffff))
	(if (>= w 0x80000000) (- w 0x100000000) w))

(each (lambda ((a b))
		(assert-eq (cat "constants past 32 bits, of " (str a) " and " (str b))
			(vs-big-want a b) (vs_big a b)))
	'((3 3) (123456789 0x6fffffffff) (-987654321 0x7000000000) (0x7fffffffff 0x7000000001) (-1 -1)))

;an address of a base and an offset at each edge of what a target's add
;of a constant takes, 12 bits, 9 and 13 signed: the arm64 took -4096 for
;one it could, and gave the base
(defq vs_lea_c '(-4097 -4096 -4095 -257 -256 -1 1 255 256 4095 4096 4097))
(defq vs_lea (vs-native "tests/vp_spec_lea" (cat (vs-head "tests/vp_spec_lea")
	(apply (const cat) (map (# (cat "(vp-lea-i :r2 " (str %0) " :r5)" vs_lf
		"(vp-mul-cr 3 :r4)" vs_lf "(vp-add-rr :r5 :r4)" vs_lf)) vs_lea_c))
	(vs-tail))))

(each (lambda (a)
		(assert-eq (cat "an address at each edge of an offset, of " (str a))
			(reduce (# (+ (* %0 3) a %1)) vs_lea_c 0) (vs_lea a 0)))
	'(0 100000 -100000))

;a load and the add of what it loaded, with its base moved between them,
;then with a float stored over it between them: the add is of what was
;loaded. The x86_64 fuses a load into the op of what it loaded, and must
;not over either
(defq vs_fuse (vs-native "tests/vp_spec_fuse" (cat (vs-head "tests/vp_spec_fuse")
	"(vp-alloc 16)" vs_lf "(vp-cpy-ri :r2 :rsp 0)" vs_lf "(vp-cpy-ri :r3 :rsp 8)" vs_lf
	"(vp-cpy-rr :rsp :r6)" vs_lf
	"(vp-cpy-ir :r6 0 :r5)" vs_lf "(vp-add-cr 8 :r6)" vs_lf "(vp-add-rr :r5 :r4)" vs_lf
	"(vp-xor-rr :r5 :r5)" vs_lf "(vp-mul-cr 3 :r4)" vs_lf
	"(vp-cpy-ir :r6 0 :r5)" vs_lf "(vp-cvt-rf :r4 :f0)" vs_lf "(vp-cpy-fi :f0 :r6 0)" vs_lf
	"(vp-add-rr :r5 :r4)" vs_lf "(vp-xor-rr :r5 :r5)" vs_lf
	"(vp-free 16)" vs_lf
	(vs-tail))))

(each (lambda ((a b))
		(assert-eq (cat "a load and its add, its base moved or a float stored between, of " (str a) " and " (str b))
			(+ (* a 3) b) (vs_fuse a b)))
	'((1 1000) (-5 77) (123456 -654321)))

(undef (env) 'vs_ops 'vs_lf 'vs_sets 'vs_long 'vs_ff 'vs_big 'vs_big_c 'vs_pairs 'vs_lea 'vs_lea_c 'vs_fuse)

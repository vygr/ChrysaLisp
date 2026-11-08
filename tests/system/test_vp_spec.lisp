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

(defq vs_pairs '((3 3) (2 7) (7 2) (5 5) (0 4) (4 0) (0 0) (-3 2) (2 -3) (9 9)))

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
	"(vp-cvt-rd :r2 :d0)" vs_lf "(vp-cvt-rd :r3 :d1)" vs_lf
	"(vp-bne-cr 999 :r2 'real)" vs_lf "(vp-xor-rr :r5 :r5)" vs_lf "(vp-cvt-rd :r5 :d0)" vs_lf
	"(vp-div-dd :d0 :d0)" vs_lf "(vp-label 'real)" vs_lf
	(apply (const cat) (map (lambda (op i)
		(cat "(vp-b" (slice op 1 -1) "-dd :d1 :d0 'took" (str i) ")" vs_lf "(vp-jmp 'next" (str i) ")" vs_lf
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
	;two compares and branches of a constant past 32 bits
	"(vp-blt-cr 0x7000000000 :r3 'small)" vs_lf "(vp-or-cr 1 :r4)" vs_lf "(vp-label 'small)" vs_lf
	"(vp-ble-cr 0x7000000000 :r3 'not_big)" vs_lf "(vp-xor-cr 2 :r4)" vs_lf "(vp-label 'not_big)" vs_lf
	;a word on the stack, by a base 2^33 below it
	"(vp-alloc 8)" vs_lf "(vp-cpy-rr :rsp :r6)" vs_lf "(vp-sub-cr 0x200000000 :r6)" vs_lf
	"(vp-store-rrc :r4 :r6 0x200000000)" vs_lf "(vp-load-rcr :r6 0x200000000 :r7)" vs_lf
	"(vp-add-cr 5 :r7)" vs_lf "(vp-store-rrc-i :r7 :r6 0x200000000)" vs_lf "(vp-load-rcr-i :r6 0x200000000 :r4)" vs_lf
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
	(apply (const cat) (map (# (cat "(vp-lea-rcr :r2 " (str %0) " :r5)" vs_lf
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
	"(vp-alloc 16)" vs_lf "(vp-store-rrc :r2 :rsp 0)" vs_lf "(vp-store-rrc :r3 :rsp 8)" vs_lf
	"(vp-cpy-rr :rsp :r6)" vs_lf
	"(vp-load-rcr :r6 0 :r5)" vs_lf "(vp-add-cr 8 :r6)" vs_lf "(vp-add-rr :r5 :r4)" vs_lf
	"(vp-xor-rr :r5 :r5)" vs_lf "(vp-mul-cr 3 :r4)" vs_lf
	"(vp-load-rcr :r6 0 :r5)" vs_lf "(vp-cvt-rd :r4 :d0)" vs_lf "(vp-store-drc :d0 :r6 0)" vs_lf
	"(vp-add-rr :r5 :r4)" vs_lf "(vp-xor-rr :r5 :r5)" vs_lf
	"(vp-free 16)" vs_lf
	(vs-tail))))

(each (lambda ((a b))
		(assert-eq (cat "a load and its add, its base moved or a float stored between, of " (str a) " and " (str b))
			(+ (* a 3) b) (vs_fuse a b)))
	'((1 1000) (-5 77) (123456 -654321)))

;a single in memory, loaded by every form a translator has, an offset
;small, of a scaled field, too big for a field, below the base, and an
;index register, then made a double. What comes back is the double's
;bits, the count of those that differ from the first above them
(import "lib/gpu/shader.inc")
(defq vs_if (vs-native "tests/vp_spec_if" (cat (vs-head "tests/vp_spec_if")
	"(vp-alloc 32)" vs_lf
	(apply (const cat) (map (# (cat "(vp-store-rrc-i :r2 :rsp " (str %0) ")" vs_lf)) '(0 4 8 12 16)))
	"(vp-load-rcf :rsp 0 :f0)" vs_lf
	"(vp-lea-rcr :rsp -1000 :r6)" vs_lf "(vp-load-rcf :r6 1004 :f1)" vs_lf
	"(vp-lea-rcr :rsp -100000 :r6)" vs_lf "(vp-load-rcf :r6 100008 :f2)" vs_lf
	"(vp-lea-rcr :rsp 20 :r6)" vs_lf "(vp-load-rcf :r6 -8 :f3)" vs_lf
	"(vp-cpy-cr 16 :r7)" vs_lf "(vp-load-rrf :rsp :r7 :f4)" vs_lf
	(apply (const cat) (map (# (cat "(vp-cvt-fd :f" (str %0) " :d" (str %0) ")" vs_lf)) (range 0 5)))
	"(vp-cpy-dr :d0 :r4)" vs_lf "(vp-xor-rr :r8 :r8)" vs_lf
	(apply (const cat) (map (# (cat "(vp-cpy-dr :d" (str %0) " :r5)" vs_lf "(vp-beq-rr :r4 :r5 'same" (str %0) ")" vs_lf
		"(vp-add-cr 1 :r8)" vs_lf "(vp-label 'same" (str %0) ")" vs_lf)) (range 1 5)))
	"(vp-shl-cr 60 :r8)" vs_lf "(vp-xor-rr :r8 :r4)" vs_lf
	"(vp-free 32)" vs_lf
	(vs-tail))))

(defun vs-double-of (s)
	;the bits of the double a normal single, a zero or an infinity is
	(defq e (logand (>> s 23) 0xff) sign (<< (logand (>> s 31) 1) 63))
	(cond
		((= e 0) sign)
		((= e 255) (logior sign 0x7ff0000000000000))
		((logior sign (<< (+ e 896) 52) (<< (logand s 0x7fffff) 29)))))

(each (lambda (s)
		(assert-eq (cat "a single " (int-to-hex-str s) " loaded, each way, is its double")
			(vs-double-of s) (vs_if s 0)))
	'(0x3f800000 0xc0200000 0x3eaaaaab 0x7f7fffff 0x00800000 0 0x80000000 0x7f800000 0xff800000))

;the nearest single to a double, in another register, stored by every
;form, and the double as it was: the single's bits, with the count of
;stores that differ from the first, and 1 if the double changed, above them
(defq vs_fi (vs-native "tests/vp_spec_fi" (cat (vs-head "tests/vp_spec_fi")
	"(vp-alloc 32)" vs_lf "(vp-cpy-rd :r3 :d0)" vs_lf "(vp-cvt-df :d0 :f1)" vs_lf
	"(vp-store-frc :f1 :rsp 0)" vs_lf
	"(vp-lea-rcr :rsp -1000 :r6)" vs_lf "(vp-store-frc :f1 :r6 1004)" vs_lf
	"(vp-lea-rcr :rsp -100000 :r6)" vs_lf "(vp-store-frc :f1 :r6 100008)" vs_lf
	"(vp-lea-rcr :rsp 20 :r6)" vs_lf "(vp-store-frc :f1 :r6 -8)" vs_lf
	"(vp-cpy-cr 16 :r7)" vs_lf "(vp-store-frr :f1 :rsp :r7)" vs_lf
	"(vp-load-rcr-ui :rsp 0 :r4)" vs_lf "(vp-xor-rr :r8 :r8)" vs_lf
	(apply (const cat) (map (# (cat "(vp-load-rcr-ui :rsp " (str %0) " :r5)" vs_lf "(vp-beq-rr :r4 :r5 'same" (str %0) ")" vs_lf
		"(vp-add-cr 1 :r8)" vs_lf "(vp-label 'same" (str %0) ")" vs_lf)) '(4 8 12 16)))
	"(vp-cpy-dr :d0 :r5)" vs_lf "(vp-beq-rr :r3 :r5 'kept)" vs_lf "(vp-add-cr 16 :r8)" vs_lf "(vp-label 'kept)" vs_lf
	"(vp-shl-cr 32 :r8)" vs_lf "(vp-add-rr :r8 :r4)" vs_lf
	"(vp-free 32)" vs_lf
	(vs-tail))))

;the bits of the double of a number, 1.0 is a fixed, shader-real makes it a real
(defun vs-bits-of (n) (get-long (set-real (str-alloc 8) 0 (shader-real n)) 0))

;1, a third, a tie to even down and one up, -2.5, too big for a single,
;the largest and the smallest normal single, and 0
(each (lambda (d)
		(defq r (get-real (set-long (str-alloc 8) 0 d) 0))
		(assert-eq (cat "the nearest single to " (long-to-hex-str d) " stored, each way, the register as it was")
			(sh-real-to-float r) (vs_fi 0 d)))
	(list (vs-bits-of 1.0) 0x3fd5555555555555 0x3ff0000010000000 0x3ff0000030000000 (vs-bits-of -2.5)
		0x47f0000000000000 0x47efffffe0000000 0x3810000000000000 0))

;each single op, of a and b made singles, the answer made an int toward 0.
;2^24 and 1 more are the one single, as they are not the one double
(defq vs_s_ops (list
	(list "add" "(vp-add-ff :f1 :f0)" (lambda (a b) (+ a b)))
	(list "sub" "(vp-sub-ff :f1 :f0)" (lambda (a b) (- a b)))
	(list "mul" "(vp-mul-ff :f1 :f0)" (lambda (a b) (* a b)))
	(list "div" "(vp-div-ff :f1 :f0)" (lambda (a b) (/ a b)))
	(list "min" "(vp-min-ff :f1 :f0)" (lambda (a b) (min a b)))
	(list "max" "(vp-max-ff :f1 :f0)" (lambda (a b) (max a b)))
	(list "sqrt" "(vp-mul-ff :f0 :f0) (vp-sqrt-ff :f0 :f2) (vp-cpy-ff :f2 :f0)" (lambda (a b) (abs a)))
	(list "abs" "(vp-abs-ff :f0 :f0)" (lambda (a b) (abs a)))
	(list "neg" "(vp-neg-ff :f0 :f3) (vp-cpy-ff :f3 :f0)" (lambda (a b) (neg a)))
	(list "round" "(vp-cvt-fd :f0 :d5) (vp-cvt-df :d5 :f6) (vp-cpy-ff :f6 :f0)" (lambda (a b) a))))
(each (lambda ((name code want))
		(defq f (vs-native (cat "tests/vp_spec_s" name) (cat (vs-head (cat "tests/vp_spec_s" name))
			"(vp-cvt-rf :r2 :f0)" vs_lf "(vp-cvt-rf :r3 :f1)" vs_lf code vs_lf "(vp-cvt-fr :f0 :r4)" vs_lf
			(vs-tail))))
		(each (lambda ((a b))
				(assert-eq (cat "the single " name " of " (str a) " and " (str b)) (want a b) (f a b)))
			'((3 4) (-7 2) (100 -9) (0 5) (1000 1000))))
	vs_s_ops)
(defq vs_s (vs-native "tests/vp_spec_s" (cat (vs-head "tests/vp_spec_s")
	"(vp-cvt-rf :r2 :f0)" vs_lf "(vp-cvt-rf :r3 :f1)" vs_lf "(vp-add-ff :f1 :f0)" vs_lf "(vp-cvt-fr :f0 :r4)" vs_lf
	(vs-tail))))
(assert-eq "2^24 + 1 a single is 2^24" 16777216 (vs_s 16777217 0))
(assert-eq "and 2^24 and 1 added as singles is 2^24" 16777216 (vs_s 16777216 1))

;a single's bits to a general register and back, 32 of them, the top made 0
(defq vs_sb (vs-native "tests/vp_spec_sb" (cat (vs-head "tests/vp_spec_sb")
	"(vp-cpy-rf :r2 :f7)" vs_lf "(vp-cpy-fr :f7 :r4)" vs_lf
	(vs-tail))))
(each (lambda (a)
		(assert-eq (cat "the 32 bits of " (int-to-hex-str a) " a single and back") (logand a 0xffffffff) (vs_sb a 0)))
	'(0x3f800000 0xbf800000 0x7fc00000 -1 0x123456789))

;each single branch, as the double's above
(defq vs_sf (vs-native "tests/vp_spec_sf" (cat (vs-head "tests/vp_spec_sf")
	"(vp-cvt-rf :r2 :f0)" vs_lf "(vp-cvt-rf :r3 :f1)" vs_lf
	"(vp-bne-cr 999 :r2 'real)" vs_lf "(vp-xor-rr :r5 :r5)" vs_lf "(vp-cvt-rf :r5 :f0)" vs_lf
	"(vp-div-ff :f0 :f0)" vs_lf "(vp-label 'real)" vs_lf
	(apply (const cat) (map (lambda (op i)
		(cat "(vp-b" (slice op 1 -1) "-ff :f1 :f0 'took" (str i) ")" vs_lf "(vp-jmp 'next" (str i) ")" vs_lf
			"(vp-label 'took" (str i) ")" vs_lf "(vp-or-cr " (str (<< 1 i)) " :r4)" vs_lf "(vp-label 'next" (str i) ")" vs_lf))
		vs_ops (range 0 6)))
	(vs-tail))))
(each (lambda ((a b))
		(assert-eq (cat "the single branches of " (str a) " and " (str b))
			(vs-long-want a b) (vs_sf a b)))
	'((3 3) (2 7) (7 2) (-4 -4) (-4 1)))
(assert-eq "2^24 + 1 and 2^24 are the one single" (vs-long-want 0 0) (vs_sf 16777217 16777216))
(assert-eq "of a single NaN, only bne is taken" 2 (vs_sf 999 1))


;a single and its double are one register, a trash of either is of both:
;what a method is declared to trash, of :fN, is the register :dN
(defq vs_trash :nil)
(within-compile-env (lambda ()
	(include "lib/asm/class.inc")
	(setq vs_trash (trash-regs '(:r1 :f0-:f2 :d5 :f7)))))
(assert-list-eq "a trash of a single is of its double, one register"
	'(:r1 :d0 :d1 :d2 :d5 :d7) vs_trash)

(undef (env) 'vs_ops 'vs_lf 'vs_long 'vs_ff 'vs_big 'vs_big_c 'vs_pairs 'vs_lea 'vs_lea_c 'vs_fuse
	'vs_if 'vs_fi 'vs-double-of 'vs-bits-of 'vs_s_ops 'vs_s 'vs_sb 'vs_sf 'vs_trash)

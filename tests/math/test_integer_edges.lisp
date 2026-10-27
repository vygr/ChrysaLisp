(report-header "Integer Edges: overflow, division, shifts, n-ary forms")

(defq edge_max 9223372036854775807 edge_min -9223372036854775808)

; --- 64 bit wrap around, there is no overflow error ---
(test-cases
	(+ edge_max 1) edge_min
	(- edge_min 1) edge_max
	(* edge_max 2) -2
	(neg edge_min) edge_min
	(abs edge_min) edge_min
	(* -1 -9223372036854775807) edge_max
	(max edge_min edge_max) edge_max
	(min edge_min edge_max) edge_min
	0x7fffffffffffffff edge_max
	0xffffffffffffffff -1)

; --- division truncates toward zero, the remainder takes the sign of the dividend ---
(test-cases
	(/ -7 2) -3		(% -7 2) -1
	(/ 7 -2) -3		(% 7 -2) 1
	(/ -7 -2) 3		(% -7 -2) -1
	(/ 0 5) 0		(% 0 5) 0
	(% 5 5) 0)

; --- shifts, the count is taken mod 64 ---
(test-cases
	(<< 1 63) edge_min
	(<< 1 64) 1
	(<< 1 65) 2
	(<< 1 -1) edge_min
	(>> -1 63) 1
	(>> -1 64) -1
	(>>> -1 63) -1
	(>>> -8 64) -8)

; --- n-ary comparisons are monotonic chains ---
(test-cases
	(= 1 1 1) :t		(= 1 1 2) :nil
	(< 1 2 3) :t		(< 1 3 2) :nil
	(<= 1 1 2) :t		(> 3 2 1) :t
	(>= 3 3 1) :t
	(/= 1 2 3) :t		(/= 1 2 1) :nil)

; --- n-ary and no argument bitwise, identity values ---
(test-cases
	(logand) -1		(logior) 0		(logxor) 0
	(logand 0xff) 255
	(logand -1 0xf0 0x3c) 48
	(logior 1 2 4) 7
	(logxor 7 2 1) 4
	(lognot -1) 0
	(min 3 -1 7 -1) -1)

; --- bit counting at the ends ---
(test-cases
	(nlz 0) 64		(nlz 1) 63		(nlz -1) 0
	(nlo -1) 64		(nlo 0) 0
	(ntz 0) 64		(ntz 1) 0		(ntz edge_min) 63
	(nto -1) 64		(nto 0) 0
	(bitcnt edge_min) 1
	(log2 1) 0		(log2 0) :nil	(log2 6) :nil	(log2 -8) :nil)

; --- pow ---
(test-cases
	(pow 2 0) 1		(pow 0 0) 1
	(pow 2 62) 4611686018427387904
	(pow -2 3) -8)

; --- zero and sign ---
(test-cases
	(abs 0) 0		(neg 0) 0		(sign 0) 0
	(sign 5) 1		(sign -5) -1
	(neg? 0) :nil	(pos? 0) :nil
	(odd? -3) :t	(odd? 0) :nil
	(even? 0) :t	(even? -2) :t
	(inc -1) 0		(dec 0) -1
	(sqrt 0) 0		(sqrt 16) 4		(sqrt 17) 4
	(random 1) 0)

; --- number readers ---
(test-cases
	0b101 5		0o17 15		-0x10 -16
	(str-as-num "-5") -5
	(str-as-num "0x1F") 31
	(str-as-num "0b11") 3
	(str-as-num "007") 7
	(str-as-num "") 0
	(str-as-num "9223372036854775807") edge_max)

;a number ends at the first char that is not a digit of its base: what
;follows it, a newline read with it from a file, a space, was taken as
;digits, "5\n" was 44
(test-cases
	(str-as-num (cat "5" (ascii-char 10))) 5
	(str-as-num (cat "12" (ascii-char 10))) 12
	(str-as-num "12 ") 12
	(str-as-num "12:3") 12
	(str-as-num "0x1Fg") 31
	(str-as-num "0b102") 2
	(str-to-num (cat "3.25" (ascii-char 10))) 3.25)

; --- a shift is by the low 6 bits of its count, on every CPU and on the emulator ---
(assert-eq "left by 64 is by 0" 1 (<< 1 64))
(assert-eq "left by 65 is by 1" 2 (<< 1 65))
(assert-eq "left by -1 is by 63" (<< 3 63) (<< 3 -1))
(assert-eq "right by 64 is by 0" -1 (>> -1 64))
(assert-eq "right by 68 is by 4" (>> -1 4) (>> -1 68))
(assert-eq "right, keeping the sign, by 66 is by 2" -4 (>>> -16 66))
(assert-eq "and by -62 is by 2" -4 (>>> -16 -62))

; --- a real made an integer is toward 0 ---
(assert-eq "2.9 is 2" 2 (n2i (n2r 2.9)))
(assert-eq "-2.9 is -2" -2 (n2i (n2r -2.9)))
;one too big to be an integer is not the same on every CPU, the most
;negative number on x86-64 and the biggest on ARM64. The emulator is the
;one thing on whatever it runs: the biggest, and the most negative for one
;too far the other way
(defq ie_big (* (n2r 1000000000) (n2r 1000000000) (n2r 1000000000)))
(when (starts-with "obj/vp64" (load-path))
	(assert-eq "emulated, a real too big is the biggest integer" 9223372036854775807 (n2i ie_big))
	(assert-eq "emulated, one too far below is the most negative" (<< 1 63) (n2i (- (n2r 0) ie_big))))

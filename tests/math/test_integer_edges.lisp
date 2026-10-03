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

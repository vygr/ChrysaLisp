;raw storage: each kind of array's storage, by width, a vector of 100 of
;each, nums and floats beside the raw kinds, bytes, shorts and ints; the
;bulk ops per element
(defq br_n 100000 br_100 (map! (const !) '() 0 100)
	br_kinds (list
		(list "nums" (apply (const nums) br_100))
		(list "floats" (apply (const floats) (map (const n2f) br_100)))
		(list "ints" (apply (const ints) br_100))
		(list "shorts" (apply (const shorts) br_100))
		(list "bytes" (apply (const bytes) br_100))))

(each (lambda ((name v))
		(defq w (cat v) x (elem-get v 90))
		(bench (cat "(elem-get " name " 50)") br_n (times br_n (elem-get v 50)))
		(bench (cat "(elem-set " name " 50 x)") br_n (times br_n (elem-set w 50 x)))
		(bench (cat "(push " name " x) and (pop)") br_n (times br_n (pop (push w x))))
		(bench (cat "(slice " name " 10 90), each") (* br_n 80) (times br_n (slice v 10 90)))
		(bench (cat "(cat " name " " name "), each") (* br_n 200) (times br_n (cat v v)))
		(bench (cat "(find x " name "), each looked at") (* br_n 91) (times br_n (find x v)))
		(bench (cat "(eql " name " " name "), each") (* br_n 100) (times br_n (eql v w)))
		(bench (cat "(fill " name " 100 x), each") (* br_n 100) (times br_n (fill w 100 x))))
	br_kinds)

;to numbers and back, for maths
(defq br_shorts (second (elem-get br_kinds 3)) br_nums (second (first br_kinds)) br_out (nums) br_raw (shorts))
(bench "(nums-convert shorts nums :t), each" (* br_n 100) (times br_n (nums-convert br_shorts br_out :t)))
(bench "(nums-convert nums shorts), each" (* br_n 100) (times br_n (nums-convert br_nums br_raw)))

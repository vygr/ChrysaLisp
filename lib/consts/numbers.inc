;;;;;;;;;;;;;;;;;;
; number constants
;;;;;;;;;;;;;;;;;;

;the double and float constants every library may use, the same set of
;each, apart from vector.inc so a library that wants only these, a
;decoder, has them without the rest: a +name is put into the code where
;it is used, the one instance of it

(defmacro def_nums_consts (c s e f)
	`(bind (map! (# (sym (str ,c (!)))) '() ,s ,e) (map! (# (,f (!))) '() ,s ,e)))
;[0:10]
(def_nums_consts "+double_" 0 11 (const n2d))
(def_nums_consts "+float_" 0 11 (const n2f))
;[-1:-10]
(def_nums_consts "+double_" -10 0 (const n2d))
(def_nums_consts "+float_" -10 0 (const n2f))
;[1/2:1/20]
(def_nums_consts "+double_1/" 2 21 (# (/ (const (n2d 1)) (n2d %0))))
(def_nums_consts "+float_1/" 2 21 (# (n2f (/ (const (n2d 1)) (n2d %0)))))
;[-1/2:-1/20]
(def_nums_consts "+double_-1/" 2 21 (# (/ (const (n2d -1)) (n2d %0))))
(def_nums_consts "+float_-1/" 2 21 (# (n2f (/ (const (n2d -1)) (n2d %0)))))
;pi, as near as each kind holds it
(defq +double_pi (str-to-double "3.141592653589793") +double_hpi (str-to-double "1.5707963267948966")
	+double_2pi (str-to-double "6.283185307179586")
	+float_pi (n2f +double_pi) +float_hpi (n2f +double_hpi) +float_2pi (n2f +double_2pi))

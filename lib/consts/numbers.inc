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
(def_nums_consts "+double_1/" 2 21 (# (recip (n2d %0))))
(def_nums_consts "+float_1/" 2 21 (# (n2f (recip (n2d %0)))))
;[-1/2:-1/20]
(def_nums_consts "+double_-1/" 2 21 (# (neg (recip (n2d %0)))))
(def_nums_consts "+float_-1/" 2 21 (# (n2f (neg (recip (n2d %0))))))
;pi, as near as each kind holds it: bound by its names quoted, as the
;rest are, so the file may be read again where they are seen already, its
;names not taken for their values
(bind '(+double_pi +double_hpi +double_2pi)
	(map (const str-to-double) '("3.141592653589793" "1.5707963267948966" "6.283185307179586")))
(bind '(+float_pi +float_hpi +float_2pi) (map (const n2f) (list +double_pi +double_hpi +double_2pi)))

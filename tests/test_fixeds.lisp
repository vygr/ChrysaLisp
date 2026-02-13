(report-header "Fixeds Vectors")

(defq v1 (fixeds 1.0 2.0 3.0))
(defq v2 (fixeds 4.0 5.0 6.0))
(defq v3 (fixeds -1.0 -2.0 -3.0))

; Basic Arithmetic
(assert-eq "Fixeds Add" (fixeds 5.0 7.0 9.0) (nums-add v1 v2))
(assert-eq "Fixeds Sub" (fixeds -3.0 -3.0 -3.0) (nums-sub v1 v2))
(assert-eq "Fixeds Mul" (fixeds 4.0 10.0 18.0) (nums-mul v1 v2))
(assert-eq "Fixeds Div" (fixeds 0.25 0.4 0.5) (nums-div v1 v2))
(assert-eq "Fixeds Mod" (fixeds 1.0 2.0 3.0) (nums-mod v1 v2))

; Vector Operations
(assert-eq "Fixeds Dot" 32.0 (nums-dot v1 v2))
(assert-eq "Fixeds Scale" (fixeds 2.0 4.0 6.0) (nums-scale v1 2.0))
(assert-eq "Fixeds Abs" (fixeds 1.0 2.0 3.0) (nums-abs v3))
(assert-eq "Fixeds Sum" 6.0 (nums-sum v1))
(assert-eq "Fixeds Max" (fixeds 4.0 5.0 6.0) (nums-max v1 v2))
(assert-eq "Fixeds Min" (fixeds 1.0 2.0 3.0) (nums-min v1 v2))

; Unary Operations
(defq v_fp (fixeds 1.5 -2.5 3.75))
(assert-eq "Fixeds Floor" (fixeds 1.0 -3.0 3.0) (fixeds-floor v_fp))
(assert-eq "Fixeds Ceil" (fixeds 2.0 -2.0 4.0) (fixeds-ceil v_fp))
(assert-eq "Fixeds Frac" (fixeds 0.5 0.5 0.75) (fixeds-frac v_fp))

; Trig (Scalar)
(assert-eq "Fixed Sin 0" 0.0 (sin 0.0))
(assert-eq "Fixed Cos 0" 1.0 (cos 0.0))
; pi/2 is approx 1.570796. +fp_hpi is 1.570796.
(assert-eq "Fixed Sin PI/2" 1.0 (sin 1.570796))
(assert-eq "Fixed Cos PI/2" 0.0 (cos 1.570796))

; verify types
(assert-true "Is fixeds?" (fixeds? v1))

;;;;;;;;;;;;;;;;;;;;;;
; tests/test_struct.lisp
;;;;;;;;;;;;;;;;;;;;;;
(report-header "Structures: getf, setf, enums, bits")

; --- Structure setf/getf ---
(structure +st_test 0
    (byte b_sig)
    (ubyte b_unsig)
    (short s1)
    (int i1)
    (long l1)
    (fixed f1)
    (real r1)
)

(defq sobj (str-alloc +st_test_size))

; Signed vs Unsigned byte
(setf sobj +st_test_b_sig 200)   ; will be -56
(setf sobj +st_test_b_unsig 200) ; will be 200
(assert-eq "getf signed byte" -56 (getf sobj +st_test_b_sig))
(assert-eq "getf unsigned byte" 200 (getf sobj +st_test_b_unsig))

; Other types
(setf sobj +st_test_s1 30000)
(setf sobj +st_test_i1 1000000)
(setf sobj +st_test_l1 123456789012345)
(setf sobj +st_test_f1 123.45)
(setf sobj +st_test_r1 (n2r 3.14159))

(assert-eq "getf short" 30000 (getf sobj +st_test_s1))
(assert-eq "getf int" 1000000 (getf sobj +st_test_i1))
(assert-eq "getf long" 123456789012345 (getf sobj +st_test_l1))
(assert-eq "getf fixed" 123.45 (getf sobj +st_test_f1))
(assert-eq "getf real" (n2r 3.14159) (getf sobj +st_test_r1))

; --- Collective Macros ---
(setf-> sobj
    (+st_test_b_sig 10)
    (+st_test_s1 2000)
    (+st_test_i1 3000000)
)
(defq res (getf-> sobj +st_test_b_sig +st_test_s1 +st_test_i1))
(assert-list-eq "getf-> results" '(10 2000 3000000) res)

; --- Sub-structures ---
(structure +st_inner 0
    (int x y)
)
(structure +st_outer 0
    (byte head)
    (struct sub +st_inner_size)
    (byte tail)
)

(defq out_obj (str-alloc +st_outer_size))
; Access sub-struct field via nested getf (sub-structs present as strings)
(defq sub_obj (getf out_obj +st_outer_sub))

; setf on sub-struct string
(setf sub_obj +st_inner_x 500)
(setf sub_obj +st_inner_y 600)
; Write back the modified sub-structure string to the parent using setf
(setf out_obj +st_outer_sub sub_obj)

; Verify from original out_obj
(assert-eq "sub getf x" 500 (getf (getf out_obj +st_outer_sub) +st_inner_x))
(assert-eq "sub getf y" 600 (getf (getf out_obj +st_outer_sub) +st_inner_y))

; Collective macros on sub-struct
(setf-> sub_obj
    (+st_inner_x 700)
    (+st_inner_y 800)
)
(setf out_obj +st_outer_sub sub_obj)

(defq res (getf-> (getf out_obj +st_outer_sub) +st_inner_x +st_inner_y))
(assert-list-eq "sub getf-> x y" '(700 800) res)

; --- Messaging Pattern ---
; cat a header and a payload, then setf the header fields
(defq payload "DATA")
(defq msg (setf-> (cat (str-alloc +st_test_size) payload)
            (+st_test_b_sig 1)
            (+st_test_s1 42)
))
(assert-eq "msg header field" 1 (getf msg +st_test_b_sig))
(assert-eq "msg header field 2" 42 (getf msg +st_test_s1))
(assert-eq "msg payload" "DATA" (slice msg +st_test_size -1))

; --- enums ---
(enums +en_test 10
    (enum E1 E2 E3)
)
(assert-eq "enum start" 10 +en_test_E1)
(assert-eq "enum next"  11 +en_test_E2)
(assert-eq "enum size"  13 +en_test_size)

; --- bits ---
(bits +bt_test 0
    (bit B0 B1 B2)
)
(assert-eq "bit 0" 1 +bt_test_B0)
(assert-eq "bit 1" 2 +bt_test_B1)
(assert-eq "bit 2" 4 +bt_test_B2)

(assert-true "bits? match" (bits? 5 +bt_test_B0 +bt_test_B2))
(assert-true "bits? miss"  (not (bits? 2 +bt_test_B0 +bt_test_B2)))
(assert-eq "bit-mask" 5 (bit-mask +bt_test_B0 +bt_test_B2))
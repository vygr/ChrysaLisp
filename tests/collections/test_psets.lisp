(report-header "Property Sets (pset)")

; --- Creation and Casting ---
(defq ps (pset 'a 'b 'c))
(assert-true "Is pset?" (pset? ps))
(assert-eq "pset length" 3 (length ps))

; --- Function: pfind (Symbol Keys) ---
(assert-eq "pfind pset existing key" 'b (pfind ps 'b))
(assert-eq "pfind pset missing key" :nil (pfind ps 'd))

; --- Function: pfind (String Keys) ---
(defq ps_str (pset "alpha" "beta" "gamma"))
(assert-eq "pfind pset string key" "beta" (pfind ps_str "beta"))
(assert-eq "pfind pset missing string key" :nil (pfind ps_str "delta"))

; --- Function: pfind (Number Keys) ---
(defq ps_num (pset 10 20 30))
(assert-eq "pfind pset number key" 20 (pfind ps_num 20))
(assert-eq "pfind pset missing number key" :nil (pfind ps_num 40))

; --- Function: pfind (Mixed Key Types) ---
(defq ps_mix (pset 'sym_key "str_key" 100))
(assert-eq "pfind pset mixed symbol" 'sym_key (pfind ps_mix 'sym_key))
(assert-eq "pfind pset mixed string" "str_key" (pfind ps_mix "str_key"))
(assert-eq "pfind pset mixed number" 100 (pfind ps_mix 100))

; --- Function: pinsert ---
(defq ps_ins (pset 'a 'b))
(assert-eq "pinsert pset return value" ps_ins (pinsert ps_ins 'c))
(assert-eq "pfind pset after insert" 'c (pfind ps_ins 'c))
(assert-eq "pinsert pset existing return value" ps_ins (pinsert ps_ins 'a))

; String key insertion
(pinsert ps_ins "new_str")
(assert-eq "pfind pset inserted string" "new_str" (pfind ps_ins "new_str"))

; --- Function: perase ---
(defq ps_del (pset 'first 'middle 'last))
; Erase middle
(assert-eq "perase pset return value" ps_del (perase ps_del 'middle))
(assert-eq "pfind pset after erase middle" :nil (pfind ps_del 'middle))
(assert-eq "pfind pset remaining first" 'first (pfind ps_del 'first))
(assert-eq "pfind pset remaining last" 'last (pfind ps_del 'last))

; Erase on empty / missing
(defq ps_empty (pset))
(assert-eq "pfind pset empty" :nil (pfind ps_empty 'a))
(assert-eq "perase pset empty" ps_empty (perase ps_empty 'a))

; --- Function: pfindi ---
(defq ps_idx (pset 'a 'b 'c))
(assert-eq "pfindi pset first key" 0 (pfindi ps_idx 'a))
(assert-eq "pfindi pset second key" 1 (pfindi ps_idx 'b))
(assert-eq "pfindi pset third key" 2 (pfindi ps_idx 'c))
(assert-eq "pfindi pset missing key" :nil (pfindi ps_idx 'd))

(defq ps_str_idx (pset "x" "y" "z"))
(assert-eq "pfindi pset string key" 1 (pfindi ps_str_idx "y"))
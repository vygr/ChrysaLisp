(report-header "Property Lists (pmap)")

; --- Creation and Casting ---
(defq pl (pmap 'a 1 'b 2 'c 3))
(assert-true "Is pmap?" (pmap? pl))
(assert-eq "pmap length" 6 (length pl))

; --- Function: pfind (Symbol Keys) ---
(assert-eq "pfind existing key a" 1 (pfind pl 'a))
(assert-eq "pfind existing key b" 2 (pfind pl 'b))
(assert-eq "pfind missing key d" :nil (pfind pl 'd))

; --- Function: pfind (String Keys) ---
(defq pl_str (pmap "name" "ChrysaLisp" "type" "OS"))
(assert-eq "pfind pmap string key" "ChrysaLisp" (pfind pl_str "name"))
(assert-eq "pfind pmap missing string key" :nil (pfind pl_str "version"))

; --- Function: pfind (Number Keys) ---
(defq pl_num (pmap 100 "hundred" 200 "two hundred"))
(assert-eq "pfind pmap number key" "hundred" (pfind pl_num 100))
(assert-eq "pfind pmap missing number key" :nil (pfind pl_num 300))

; --- Function: pfind (Mixed Key Types) ---
(defq pl_mix (pmap 'sym_k 10 "str_k" 20 30 "num_v"))
(assert-eq "pfind pmap mixed sym" 10 (pfind pl_mix 'sym_k))
(assert-eq "pfind pmap mixed str" 20 (pfind pl_mix "str_k"))
(assert-eq "pfind pmap mixed num" "num_v" (pfind pl_mix 30))

; --- Function: pinsert ---
(defq pl_ins (pmap 'a 1 'b 2))
(assert-eq "pinsert return value (new key)" pl_ins (pinsert pl_ins 'c 3))
(assert-eq "pfind after pinsert (new key)" 3 (pfind pl_ins 'c))
(assert-eq "pinsert return value (existing key)" pl_ins (pinsert pl_ins 'a 99))
(assert-eq "pfind after pinsert (existing key)" 99 (pfind pl_ins 'a))

; String key pinsert & overwrite
(pinsert pl_ins "str_key" 500)
(assert-eq "pfind pinsert string key" 500 (pfind pl_ins "str_key"))
(pinsert pl_ins "str_key" 600)
(assert-eq "pfind pinsert overwrite string key" 600 (pfind pl_ins "str_key"))

; --- Function: perase ---
(defq pl_del (pmap 'k1 100 'k2 200 'k3 300))
; Erase middle
(assert-eq "perase return value" pl_del (perase pl_del 'k2))
(assert-eq "pfind after perase middle" :nil (pfind pl_del 'k2))
(assert-eq "pfind remaining k1" 100 (pfind pl_del 'k1))
(assert-eq "pfind remaining k3" 300 (pfind pl_del 'k3))

; Erase on empty / missing
(defq pl_empty (pmap))
(assert-eq "pfind pmap empty" :nil (pfind pl_empty 'a))
(assert-eq "perase pmap empty" pl_empty (perase pl_empty 'a))

; --- Function: pfindi ---
(defq pl_idx (pmap 'a 1 'b 2 'c 3))
(assert-eq "pfindi pmap first key" 0 (pfindi pl_idx 'a))
(assert-eq "pfindi pmap second key" 2 (pfindi pl_idx 'b))
(assert-eq "pfindi pmap third key" 4 (pfindi pl_idx 'c))
(assert-eq "pfindi pmap missing key" :nil (pfindi pl_idx 'd))

(defq pl_str_idx (pmap "x" 10 "y" 20 "z" 30))
(assert-eq "pfindi pmap string key" 2 (pfindi pl_str_idx "y"))
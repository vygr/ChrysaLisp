(report-header "Property Lists (pmap)")

; --- Creation and Casting ---
(defq pl (pmap 'a 1 'b 2 'c 3))
(assert-true "Is pmap?" (pmap? pl))
(assert-eq "pmap length" 6 (length pl))

; --- Function: pfind ---
(assert-eq "pfind existing key a" 1 (pfind pl 'a))
(assert-eq "pfind existing key b" 2 (pfind pl 'b))
(assert-eq "pfind missing key d" :nil (pfind pl 'd))

; --- Function: pinsert ---
(defq pl_ins (pmap 'a 1 'b 2))
; pinsert mutates in-place and returns the modified pmap
(assert-eq "pinsert return value (new key)" pl_ins (pinsert pl_ins 'c 3))
(assert-eq "pfind after pinsert (new key)" 3 (pfind pl_ins 'c))
(assert-eq "pinsert return value (existing key)" pl_ins (pinsert pl_ins 'a 99))
(assert-eq "pfind after pinsert (existing key)" 99 (pfind pl_ins 'a))

; --- Function: perase ---
(defq pl_del (pmap 'a 1 'b 2 'c 3))
; perase mutates in-place and returns the modified pmap
(assert-eq "perase return value" pl_del (perase pl_del 'b))
(assert-eq "pfind after perase" :nil (pfind pl_del 'b))

; --- Function: pfindi ---
(defq pl_idx (pmap 'a 1 'b 2 'c 3))
(assert-eq "pfindi pmap first key" 0 (pfindi pl_idx 'a))
(assert-eq "pfindi pmap second key" 2 (pfindi pl_idx 'b))
(assert-eq "pfindi pmap third key" 4 (pfindi pl_idx 'c))
(assert-eq "pfindi pmap missing key" :nil (pfindi pl_idx 'd))

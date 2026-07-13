(report-header "Property Sets (pset)")

; --- Creation and Casting ---
(defq ps (pset 'a 'b 'c))
(assert-true "Is pset?" (pset? ps))
(assert-eq "pset length" 3 (length ps))

; --- Function: pfind ---
; pfind on a pset returns the key itself if present, else :nil
(assert-eq "pfind pset existing key" 'b (pfind ps 'b))
(assert-eq "pfind pset missing key" :nil (pfind ps 'd))

; --- Function: pinsert ---
(defq ps_ins (pset 'a 'b))
; pinsert mutates in-place and returns the modified pset
(assert-eq "pinsert pset return value" ps_ins (pinsert ps_ins 'c))
(assert-eq "pfind pset after insert" 'c (pfind ps_ins 'c))
(assert-eq "pinsert pset existing return value" ps_ins (pinsert ps_ins 'a))

; --- Function: perase ---
(defq ps_del (pset 'a 'b 'c))
; perase mutates in-place and returns the modified pset
(assert-eq "perase pset return value" ps_del (perase ps_del 'b))
(assert-eq "pfind pset after erase" :nil (pfind ps_del 'b))

; --- Function: pfindi ---
(defq ps_idx (pset 'a 'b 'c))
(assert-eq "pfindi pset first key" 0 (pfindi ps_idx 'a))
(assert-eq "pfindi pset second key" 1 (pfindi ps_idx 'b))
(assert-eq "pfindi pset third key" 2 (pfindi ps_idx 'c))
(assert-eq "pfindi pset missing key" :nil (pfindi ps_idx 'd))

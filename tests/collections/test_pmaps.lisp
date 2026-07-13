(report-header "Property Lists (pmap)")

; --- Creation and Casting ---
(defq pl (pmap 'a 1 'b 2 'c 3))
(assert-true "Is pmap?" (pmap? pl))
(assert-eq "pmap length" 6 (length pl))

; --- Finding (pfind) ---
(assert-eq "pfind existing" 1 (pfind pl 'a))
(assert-eq "pfind existing 2" 2 (pfind pl 'b))
(assert-eq "pfind missing" :nil (pfind pl 'd))

; --- Finding Index (pfindi) ---
(assert-eq "pfindi existing" 0 (pfindi pl 'a))
(assert-eq "pfindi existing 2" 2 (pfindi pl 'b))
(assert-eq "pfindi missing" :nil (pfindi pl 'd))

; --- Insertion (pinsert - New Key) ---
(pinsert pl 'd 4)
(assert-eq "pfind after insert" 4 (pfind pl 'd))
(assert-eq "pfindi after insert" 6 (pfindi pl 'd))
(assert-eq "pmap length after insert" 8 (length pl))

; --- Insertion (pinsert - Existing Key / Update) ---
(pinsert pl 'b 200)
(assert-eq "pfind after update" 200 (pfind pl 'b))
(assert-eq "pfindi after update" 2 (pfindi pl 'b))
(assert-eq "pmap length after update" 8 (length pl))

; --- Erasing (perase - Existing Key) ---
(perase pl 'b)
(assert-eq "pfind after erase" :nil (pfind pl 'b))
(assert-eq "pfindi after erase" :nil (pfindi pl 'b))
(assert-eq "pmap length after erase" 6 (length pl))

; --- Erasing (perase - Missing Key) ---
(perase pl 'z)
(assert-eq "pmap length after missing erase" 6 (length pl))

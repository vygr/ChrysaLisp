(report-header "Property Lists (plist)")

; --- Creation and Casting ---
(defq pl (plist 'a 1 'b 2 'c 3))
(assert-true "Is plist?" (plist? pl))
(assert-eq "plist length" 6 (length pl))

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
(assert-eq "plist length after insert" 8 (length pl))

; --- Insertion (pinsert - Existing Key / Update) ---
(pinsert pl 'b 200)
(assert-eq "pfind after update" 200 (pfind pl 'b))
(assert-eq "pfindi after update" 2 (pfindi pl 'b))
(assert-eq "plist length after update" 8 (length pl))
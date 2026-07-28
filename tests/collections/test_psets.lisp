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

; --- Hashslot Caching and Self-Repair ---
(report-header "pset: Hashslot Caching and Self-Repair")

(defun pset-get-hashslot (obj)
	(obj-get obj 12 +type_uint))

(defq hs_s0 (gensym) hs_s1 (gensym) hs_s2 (gensym))

; 1. pinsert proactive hashslot assignment
(defq hs_ps (pset))
(pinsert hs_ps hs_s0)
(assert-eq "pset pinsert sets hashslot index 0" 0 (pset-get-hashslot hs_s0))
(pinsert hs_ps hs_s1)
(assert-eq "pset pinsert sets hashslot index 1" 1 (pset-get-hashslot hs_s1))
(pinsert hs_ps hs_s2)
(assert-eq "pset pinsert sets hashslot index 2" 2 (pset-get-hashslot hs_s2))

; 2. perase swaps last element to erased slot, leaving swapped element with stale hashslot
(perase hs_ps hs_s0) ; erases index 0, swaps hs_s2 (from index 2) into index 0
(assert-eq "pset perase leaves swapped item with stale hashslot" 2 (pset-get-hashslot hs_s2))

; 3. pfind detects stale slot, scans, and repairs hashslot
(assert-eq "pfind finds swapped item" hs_s2 (pfind hs_ps hs_s2))
(assert-eq "pfind repaired hashslot for swapped item" 0 (pset-get-hashslot hs_s2))

; 4. Manual corruption of hashslot is self-repaired on pfindi
(obj-set hs_s1 12 +type_uint 999)
(assert-eq "hashslot manually corrupted" 999 (pset-get-hashslot hs_s1))
(assert-eq "pfindi finds item despite corrupted hashslot" 1 (pfindi hs_ps hs_s1))
(assert-eq "pfindi repaired corrupted hashslot" 1 (pset-get-hashslot hs_s1))

; 5. String key hashslot caching and repair
(defq hs_str0 "s_key0" hs_str1 "s_key1" hs_str2 "s_key2")
(defq hs_ps_str (pset))
(pinsert hs_ps_str hs_str0)
(pinsert hs_ps_str hs_str1)
(pinsert hs_ps_str hs_str2)
(assert-eq "pset string key initial hashslot 0" 0 (pset-get-hashslot hs_str0))
(assert-eq "pset string key initial hashslot 1" 1 (pset-get-hashslot hs_str1))
(assert-eq "pset string key initial hashslot 2" 2 (pset-get-hashslot hs_str2))

(perase hs_ps_str hs_str0) ; erases index 0, swaps hs_str2 to index 0
(assert-eq "pset string key swapped item has stale hashslot" 2 (pset-get-hashslot hs_str2))
(assert-eq "pfind finds swapped string key" hs_str2 (pfind hs_ps_str hs_str2))
(assert-eq "pfind repaired string key hashslot" 0 (pset-get-hashslot hs_str2))

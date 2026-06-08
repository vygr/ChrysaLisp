(report-header "Gather / Scatter")

(defq ms (Fmap 1))
(scatter ms 'k1 10 'k2 20 'k3 30)
(assert-eq "scatter find" 20 (. ms :find 'k2))
(defq gathered (gather ms 'k3 'k1))
(assert-list-eq "gather" '(30 10) gathered)

; --- transfer ---
(defq src_m (Fmap 11) dst_m (Fmap 11))
(scatter src_m 'a 10 'b 20 'c 30 'd 40)
(transfer src_m dst_m 'b 'd)
(assert-eq "transfer b exists" 20 (. dst_m :find 'b))
(assert-eq "transfer d exists" 40 (. dst_m :find 'd))
(assert-eq "transfer a should not exist" :nil (. dst_m :find 'a))

; Test with list representation of keys
(defq dst_m2 (Fmap 11))
(transfer src_m dst_m2 '(a c))
(assert-eq "transfer list a exists" 10 (. dst_m2 :find 'a))
(assert-eq "transfer list c exists" 30 (. dst_m2 :find 'c))
(assert-eq "transfer list b should not exist" :nil (. dst_m2 :find 'b))

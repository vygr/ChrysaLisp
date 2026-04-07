(report-header "Gather / Scatter")

(defq ms (Fmap 1))
(scatter ms 'k1 10 'k2 20 'k3 30)
(assert-eq "scatter find" 20 (. ms :find 'k2))
(defq gathered (gather ms 'k3 'k1))
(assert-list-eq "gather" '(30 10) gathered)

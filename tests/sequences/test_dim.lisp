(report-header "Multidimensional Arrays (dim)")

; 1. Basic 2x2 matrix with array
; Index = x + y * w
(defq d_arr (dim (nums 2 2) (array 1 2 3 4)))
(assert-eq "dim-get array 0,0" 1 (dim-get d_arr (nums 0 0)))
(assert-eq "dim-get array 1,0" 2 (dim-get d_arr (nums 1 0)))
(assert-eq "dim-get array 0,1" 3 (dim-get d_arr (nums 0 1)))
(assert-eq "dim-get array 1,1" 4 (dim-get d_arr (nums 1 1)))

; 2. Using list as backing
(defq d_list (dim (nums 2 2) (list 'a 'b 'c 'd)))
(assert-eq "dim-get list 1,0" 'b (dim-get d_list (nums 1 0)))

; 3. Using nums as backing
(defq d_nums (dim (nums 2 2) (nums 10 20 30 40)))
(assert-eq "dim-get nums 0,1" 30 (dim-get d_nums (nums 0 1)))

; 4. Using fixeds as backing
(defq d_fixeds (dim (nums 2) (fixeds 1.5 2.5)))
(assert-eq "dim-get fixeds 1" 2.5 (dim-get d_fixeds (nums 1)))

; 5. Using reals as backing
(defq d_reals (dim (nums 2) (reals (n2r 1.1) (n2r 2.2))))
(assert-eq "dim-get reals 1" (n2r 2.2) (dim-get d_reals (nums 1)))

; 6. Mutation
(dim-set d_arr (nums 1 1) 99)
(assert-eq "dim-get array 1,1 after set" 99 (dim-get d_arr (nums 1 1)))

; 7. 3D array 2x2x2
; Index = x + y*w + z*w*h
(defq d3 (dim (nums 2 2 2) (array 1 2 3 4 5 6 7 8)))
(assert-eq "dim-get 1,1,1" 8 (dim-get d3 (nums 1 1 1)))

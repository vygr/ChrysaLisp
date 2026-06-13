;vp register lists, maps and utilities
(defq +vp_rregs ''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :rsp)
	+vp_fregs ''(:f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15))
(defq +vp_regs `'(~(last +vp_rregs) ~(last +vp_fregs))
	+vp_rregs_0_14 `'(~(most (last +vp_rregs)))
	+vp_rregs_map (reduce (# (pinsert %0 %1 (num (!)))) +vp_rregs (plist))
	+vp_fregs_map (reduce (# (pinsert %0 %1 (num (!)))) +vp_fregs (plist)))
(defq +vp_regs_map (cat +vp_rregs_map +vp_fregs_map))

; query and index
(defmacro vp-rreg? (%0) (static-qq (pfind +vp_rregs_map ,%0)))
(defmacro vp-freg? (%0) (static-qq (pfind +vp_fregs_map ,%0)))
(defmacro vp-reg? (%0) (static-qq (pfind +vp_regs_map ,%0)))

; (plist) +vp_regs vpset, with :nil for 'erased' to keep slots lined up
(defmacro vpset-copy (%0) (static-qq (cat ,%0)))
(defmacro vpset-insert (%0 %1) (static-qq (pinsert ,%0 ,%1 :t)))
(defmacro vpset-erase (%0 %1) (static-qq (pinsert ,%0 ,%1 :nil)))
(defmacro vpset-union (%0 %1) (static-qq (reduce (lambda (%0 (%1 %2)) (if %2 (vpset-insert %0 %1) %0)) (partition ,%1 2) ,%0)))
(defmacro vpset-diff (%0 %1) (static-qq (reduce (lambda (%0 (%1 %2)) (if %2 (vpset-erase %0 %1) %0)) (partition ,%1 2) ,%0)))
(defmacro vpset-unidif (us ds %2) (static-qq (each (lambda ((%0 %1)) (when %1 (vpset-insert ,us %0) (vpset-erase ,ds %0))) (partition ,%2 2))))
(defmacro vpset-tolist (%0) (static-qq (map (const first) (filter (const second) (partition ,%0 2)))))
(defmacro vpset-size (%0) (static-qq (reduce (lambda (%0 (& %1)) (if %1 (inc %0) %0)) (partition ,%0 2) 0)))
(defmacro vpset-empty? (%0) (static-qq (notany (const second) (partition ,%0 2))))
(defmacro vpset-nempty? (%0) (static-qq (some (const second) (partition ,%0 2))))
(defmacro vpset () (static-qq (vpset-copy (const (reduce (lambda (%0 %1) (vpset-erase %0 %1)) +vp_regs (plist))))))

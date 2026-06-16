;vp register lists, maps and utilities
(defq +vp_rregs ''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :rsp)
	+vp_fregs ''(:f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15))
(defq +vp_regs `'(~(last +vp_rregs) ~(last +vp_fregs))
	+vp_rregs_0_14 `'(~(most (last +vp_rregs)))
	+vp_rregs_map (reduce (# (pinsert %0 %1 (num (!)))) +vp_rregs (plist))
	+vp_fregs_map (reduce (# (pinsert %0 %1 (num (!)))) +vp_fregs (plist))
	+vp_regs_map (cat +vp_rregs_map +vp_fregs_map))

; query and index
(defmacro vp-rreg? (%0) (static-qq (pfind +vp_rregs_map ,%0)))
(defmacro vp-freg? (%0) (static-qq (pfind +vp_fregs_map ,%0)))
(defmacro vp-reg? (%0) (static-qq (pfind +vp_regs_map ,%0)))

; (plist) +vp_regs vpmap, used in trace command, all slots lined up
(defmacro vpmap-copy (%0) (static-qq (cat ,%0)))
(defmacro vpmap-merge (%0 %1) (static-qq (reduce (lambda (%0 (%1 %2)) (if (nql (pfind %0 %1) %2) (pinsert %0 %1 :nil) %0)) (partition ,%1 2) ,%0)))
(defmacro vpmap-clobber (%0 %1) (static-qq (reduce (# (pinsert %0 %1 :nil)) ,%1 ,%0)))
(defmacro vpmap-tolist (%0) (static-qq (map (const first) (filter (# (apply (const nql) %0)) (partition ,%0 2)))))
(defmacro vpmap-changed? (%0 %1) (static-qq (some (lambda (%0 %1) (and (nql %1 :nil) (nql %0 %1))) ,%0 ,%1)))
(defmacro vpmap () (static-qq (vpmap-copy (const (reduce (lambda (%0 %1) (pinsert %0 %1 %1)) +vp_regs (plist))))))

;a time round each iterator over lists that stay in the cache, a thousand
;numbers gone over a thousand times. Set beside bench_iter it says what
;of a time is the work and what is waiting for memory
(defq bs_n 1000 bs_reps 1000 bs_per (* bs_n bs_reps) bs_l (map (# %0) (range 0 bs_n))
	bs_s1 (list bs_l) bs_s2 (list bs_l (map (# (inc %0)) bs_l)))

(bench "each! progn, 1 seq" bs_per (times bs_reps (each! (const progn) bs_s1)))
(bench "each! progn, 2 seqs" bs_per (times bs_reps (each! (const progn) bs_s2)))
(bench "each! eql, 2 seqs" bs_per (times bs_reps (each! (const eql) bs_s2)))
(bench "each! +, 2 seqs" bs_per (times bs_reps (each! (const +) bs_s2)))
(bench "some! eql, 2 seqs" bs_per (times bs_reps (some! (const eql) bs_s2)))
(bench "map! progn, 1 seq" bs_per (times bs_reps (map! (const progn) bs_s1)))
(bench "map! eql, 2 seqs" bs_per (times bs_reps (map! (const eql) bs_s2)))
(bench "map! +, 2 seqs" bs_per (times bs_reps (map! (const +) bs_s2)))
(bench "reduce! progn" bs_per (times bs_reps (reduce! (const progn) bs_s1 0)))
(bench "reduce! eql" bs_per (times bs_reps (reduce! (const eql) bs_s1 0)))
(bench "reduce! +" bs_per (times bs_reps (reduce! (const +) bs_s1 0)))

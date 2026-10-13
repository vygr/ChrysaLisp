;giving way: what a task that lets others run pays when there is none
;other to run, and one that asks if its time is up
(defq bt_n 1000000)

(bench "(task-sleep 0), a yield" bt_n (times bt_n (task-sleep 0)))
(bench "(task-slice)" bt_n (times bt_n (task-slice)))
;what (task-slice) stands on: it asks the host the time, every call. Set
;against the loop with nothing in it, and a built in function that asks
;the host nothing and gives back what is there already
(bench "(pii-time), the host asked" bt_n (times bt_n (pii-time)))
(bench "(eql 1 2), nothing asked" bt_n (times bt_n (eql 1 2)))
(bench "the loop, nothing in it" bt_n (times bt_n :nil))

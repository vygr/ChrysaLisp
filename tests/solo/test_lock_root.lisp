(report-header "Lock Service: the empty key is the root, and covers every key")
(import "service/lock/app.inc")

;the root can be had only when nobody holds any lock, and in a full run a
;test beside it may hold one at any moment, as long as it likes: on a busy
;Pi there was no moment free in 30 seconds. So it is in tests/solo/, run
;when no other module is, and a claim of it is made till it is granted, a
;second at a time, for what a test waits on is what it tests, not a time
(defun lr-claim (key)
	(until (lock-claim-rpc key +lock_mode_write (task-timeout 1)))
	:t)

(defun lr-blocked (key)
	; try for a lock that should be held, :t if it could not be got
	(not (lock-claim-rpc key +lock_mode_write 30000)))

(test-cases
	(lr-claim "") :t
	(lr-blocked "le/under_root") :t
	(lock-release-rpc "") :t
	(lr-claim "le/under_root") :t
	(lr-blocked "") :t
	(lock-release-rpc "le/under_root") :t)

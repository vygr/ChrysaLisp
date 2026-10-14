(report-header "System: Storage, a store put on a volume of another size, a copy on the one machine")

(import "service/storage/app.inc")

;a service of a name of its own, on a store in a file of its own
(defq rz_pid (str (pii-pid)) rz_name (cat "@ResizeTest" rz_pid)
	rz_image (cat "tests/scratch/resize_" rz_pid ".img")
	rz_svc (storage-start rz_image :size 0x200000 :id "resizing" :name rz_name :block 4096
		:group (cat "*ResizeTest" rz_pid) :tick 0)
	rz_data (map (# (apply (const cat) (map (lambda (i) (hex-encode (sha256 (str %0 " " i)))) (range 0 (+ 20 (* %0 30)))))) (range 0 6)))

(assert-true "a store of 2MB is started" rz_svc)
(each (# (storage-put (cat "docs/file " (str (!))) %0 :nil rz_svc)) rz_data)
;and one of 768KB, that with the rest 1MB can not hold
(defq rz_big_data (apply (const cat) (map (# (hex-encode (sha256 (str %0)))) (range 0 12000))))
(storage-put "docs/big" rz_big_data :nil rz_svc)
(storage-pin "docs/" :nil rz_svc)
(defq rz_count (length (storage-list "" rz_svc)))
(assert-eq "seven objects are put" 7 rz_count)

;the names it is given
(assert-eq "the size in the name, beside it" "storage/store.8192MB.img" (storage-resized-name "storage/store.img" 8192))
(assert-eq "and of one resized before, the size it is now" "storage/store.16MB.img" (storage-resized-name "storage/store.8192MB.img" 16))
(assert-eq "a name that ends in a number is kept" "a/disk_2.4MB.img" (storage-resized-name "a/disk_2.img" 4))

;bigger
(defq rz_big (storage-resize 4 rz_svc))
(assert-eq "made bigger, it says where it is" (storage-resized-name rz_image 4) rz_big)
(assert-eq "the old file is gone" 0 (age rz_image))
(assert-true "and the new one is there" (/= 0 (age rz_big)))
(assert-eq "every object is there" rz_count (length (storage-list "" rz_svc)))
(assert-true "and is what was put" (every (# (eql (storage-get (cat "docs/file " (str (!))) rz_svc) %0)) rz_data))
(assert-true "the big one too" (eql (storage-get "docs/big" rz_svc) rz_big_data))
(assert-true "the pin came too" (nempty? (ifn (storage-pinned-by "docs/file 0" rz_svc) (list))))
(assert-eq "the same size again is not done" :nil (storage-resize 4 rz_svc))

;too small, and it is as it was
(assert-eq "too small for what it has is not done" :nil (storage-resize 1 rz_svc))
(assert-eq "nothing is left of it" 0 (age (storage-resized-name rz_image 1)))
(assert-eq "and the store is as it was" rz_count (length (storage-list "" rz_svc)))

;smaller, that fits
(defq rz_small (storage-resize 3 rz_svc))
(assert-eq "made smaller, where it fits" (storage-resized-name rz_image 3) rz_small)
(assert-eq "the one before is gone" 0 (age rz_big))
(assert-true "every object is what was put" (every (# (eql (storage-get (cat "docs/file " (str (!))) rz_svc) %0)) rz_data))
(assert-true "and a put after is kept" (storage-put "docs/after" "after" :nil rz_svc))
(assert-eq "and got back" "after" (storage-get "docs/after" rz_svc))

;a machine that travels pins the area of its user, here a member file of
;the test's own and whoever the session is
(defq rz_said (cat "tests/scratch/resize_member_" rz_pid))
(storage-member-save rz_image 4 :nil :nil rz_said :t)
(defq rz_user_pin (storage-user-pin rz_svc rz_said))
(assert-true "a machine that travels pins usr/<user>/" (or (not rz_user_pin) (starts-with "usr/" rz_user_pin)))
(when rz_user_pin
	(storage-put (cat rz_user_pin "notes") "mine" :nil rz_svc)
	(assert-true "and holds what is put under it" (nempty? (ifn (storage-pinned-by (cat rz_user_pin "notes") rz_svc) (list)))))
(storage-member-save rz_image 4 :nil :nil rz_said)
(assert-eq "one that does not travel pins nothing" :nil (storage-user-pin rz_svc rz_said))
(pii-remove rz_said)

;full to the last cluster, by a put that did not fit, so that its path map
;could not be written to it, and made bigger: what it has is all there
(defq rz_before (length (storage-list "" rz_svc)))
(storage-put "docs/huge" (apply (const cat) (map (# (hex-encode (sha256 (str "huge " %0)))) (range 0 48000))) :nil rz_svc)
(defq rz_full (storage-resize 8 rz_svc))
(assert-eq "full, made bigger" (storage-resized-name rz_image 8) rz_full)
(assert-eq "with every object it had" rz_before (length (filter (# (nql (first %0) "docs/huge")) (storage-list "" rz_svc))))
(assert-true "each what was put" (every (# (eql (storage-get (cat "docs/file " (str (!))) rz_svc) %0)) rz_data))

(storage-stop rz_svc)
(task-sleep 200000)
(each (# (pii-remove %0)) (list rz_image rz_small rz_full (cat rz_full ".member")))

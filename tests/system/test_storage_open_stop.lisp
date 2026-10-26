(report-header "Storage: an open given up when it is told to stop, and the store as it was")

(import "service/storage/app.inc")

;a store in a file of its own, with a block in it, so its open has a folder
;of blocks to walk
(defq os_image (cat "tests/scratch/open_stop_" (str (pii-pid)) ".img")
	os_name (cat "@StoreStop" (str (pii-pid))) os_group (cat "*StoreStop" (str (pii-pid)))
	os_data (cat "a block of the store" (pad "" 200 "x")))
(store-make os_image 0x100000 "stopping" "aaaa" 4096)
(defq os_store (store-open os_image "aaaa") os_block (store-block-put os_store os_data))
(store-close os_store)

(assert-eq "an open whose going says no is given up" :stopped
	(store-open os_image "aaaa" (# :nil)))
(defq os_store (store-open os_image "aaaa"))
(assert-true "and the store opens after as it was, its block there" (store-block-has? os_store os_block))
(store-close os_store)

;the service is started on this node, and runs only once its starter
;waits: the stop is there before it reads a block folder
(defq os_stop (mail-mbox))
(mail-send os_stop "go")
(assert-eq "a start told to stop as it waits is given up" :nil
	(storage-start os_image :name os_name :group os_group :tick 0 :wait :t :stop os_stop))
(assert-eq "and no service of that name is up" :nil (storage-service os_name))
(assert-eq "the stop is left for the caller to read" 0 (mail-poll (list os_stop)))
(assert-eq "as it was sent" "go" (mail-read os_stop))
(defq os_store (store-open os_image "aaaa"))
(assert-true "the store as it was" (store-block-has? os_store os_block))
;and let go of, as a file that is open is not removed on Windows
(store-close os_store)
(setq os_store :nil)

(pii-remove os_image)
(undef (env) 'os_image 'os_name 'os_group 'os_data 'os_store 'os_block 'os_stop)

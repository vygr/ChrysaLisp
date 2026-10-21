;the store and the exFAT under it, a time each 64KB block or file: on an
;image in tests/scratch, so of the code and not of a stick. Each go puts
;new blocks, of a seed of its own, one kept already costs nothing
(import "service/storage/app.inc")
(defq bs_n 200 bs_seed 0 bs_path "" bs_base (cat (char 0 8))
	bs_image (cat "tests/scratch/bench_store_" (str (pii-pid)) ".img"))
(while (< (length bs_base) 65536) (setq bs_base (cat bs_base bs_base)))
(defun bs-block () (cat (char (++ bs_seed) 8) (slice bs_base 8 -1)))

;exFAT alone, files of 64KB into one folder
(pii-remove bs_image)
(store-make bs_image (* 512 1048576) "bench" "me")
(defq bs_store (store-open bs_image "me") bs_vol (get :vol bs_store))
;each go into a new folder, as the store's are, so one go is not of a
;folder bigger than the last's
(defun bs-folder () (exfat-mkdir bs_vol (defq folder (cat "f" (str (++ bs_seed))))) folder)
(bench "exfat-save 64KB, each its own sync" bs_n
	(defq folder (bs-folder))
	(times bs_n (exfat-save bs_vol (cat folder "/" (str (++ bs_seed))) (bs-block))))
(bench "exfat-save 64KB, all in one sync" bs_n
	(defq folder (bs-folder))
	(exfat-begin bs_vol)
	(times bs_n (exfat-save bs_vol (cat folder "/" (str (++ bs_seed))) (bs-block)))
	(exfat-end bs_vol))

;the store's blocks, into its folders
(bench "store-block-put, each its own sync" bs_n
	(times bs_n (store-block-put bs_store (bs-block))))
(bench "store-block-put, all in one sync" bs_n
	(exfat-begin bs_vol)
	(times bs_n (store-block-put bs_store (bs-block)))
	(exfat-end bs_vol))
(store-close bs_store)
(pii-remove bs_image)

;through a service, a storage stream written and read, as a copy of a file is
(defq bs_name (cat "@BenchStore" (str (pii-pid)))
	bs_svc (storage-start bs_image :size (* 512 1048576) :id "bench" :name bs_name
		:group (cat "*BenchStore" (str (pii-pid))) :tick 0))
(when bs_svc
	(bench "storage stream write, a block" bs_n
		(setq bs_path (cat "f" (str bs_seed)))
		(defq bs_out (storage-stream bs_path +file_open_write bs_svc))
		(times bs_n (write-blk bs_out (bs-block)))
		(stream-flush bs_out))
	(bench "storage stream read, a block" bs_n
		;the last written, its blocks the service has to give back
		(defq bs_in (storage-stream bs_path +file_open_read bs_svc))
		(while (read-blk bs_in 65536)))
	(storage-stop bs_svc))
(pii-remove bs_image)

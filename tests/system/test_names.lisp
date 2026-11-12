(report-header "System: Names, a file of the host, an object of this machine's store, or of another's")

(import "lib/files/names.inc")

;what a name is of, and where in it
(assert-eq "a path of the host is of no store" :nil (name-store-top "apps/x.lisp"))
(assert-eq "store: is this machine's" "store:" (name-store-top "store:films/a.mp4"))
(assert-eq "and the path in it" "films/a.mp4" (name-in-store "store:films/a.mp4"))
(assert-eq "store: alone is its top" "" (name-in-store "store:"))
(assert-eq "store://who/ is another's" "store://andy/" (name-store-top "store://andy/films/a.mp4"))
(assert-eq "and the path in it" "films/a.mp4" (name-in-store "store://andy/films/a.mp4"))
(assert-list-eq "its top, with or without the /" '("" "") (list (name-in-store "store://andy/") (name-in-store "store://andy")))
(assert-eq "and it is the top all the same" "store://andy/" (name-store-top "store://andy"))
(assert-eq "a folder of it is where it was" "store://andy" (path-dir "store://andy/a.mp4"))

;two services of names of their own, on stores in files of their own, of
;a group of their own: the one as this machine's, the other as another's
(defq nm_pid (str (pii-pid)) nm_group (cat "*Names" nm_pid) nm_name (cat "@NamesA" nm_pid)
	nm_a_image (cat "tests/scratch/tmp_names_a_" nm_pid ".img")
	nm_b_image (cat "tests/scratch/tmp_names_b_" nm_pid ".img")
	nm_a (storage-start nm_a_image :size 0x400000 :id "names_a" :name nm_name :block 64 :group nm_group :tick 0)
	nm_b (storage-start nm_b_image :size 0x400000 :id "names_b" :name (cat "@NamesB" nm_pid) :block 64
		:group nm_group :tick 0))
(assert-true "two services, of stores they made" (and nm_a nm_b))
(when (and nm_a nm_b)
	(defq nm_stores (storage-stores nm_name nm_group))
	(assert-list-eq "the stores there are, this machine's first, by their ids" '("names_a" "names_b")
		(map (const first) nm_stores))
	(assert-eq "this machine's has no member said" :nil (third (first nm_stores)))
	(assert-eq "the other's is had of its service" nm_b (second (second nm_stores)))
	(assert-eq "and says the member it is" (hex-encode (system-id)) (third (second nm_stores)))
	(assert-eq "a store is found by its id" nm_b (storage-service-of "names_b" nm_name nm_group))
	(assert-eq "or of a machine by the start of its system id" nm_b
		(storage-service-of (slice (hex-encode (system-id)) 0 8) nm_name nm_group))
	(assert-eq "and one that is not there is not" :nil (storage-service-of "names_c" nm_name nm_group))
	;what is had of the other, as a client
	(storage-put "films/a.flm" "the film" :nil nm_b)
	(storage-put "films/old/b.flm" "another" :nil nm_b)
	(assert-eq "an object of it is had" "the film" (storage-get "films/a.flm" (storage-service-of "names_b" nm_name nm_group)))
	(assert-eq "and none of it is kept in this machine's" :nil (storage-stat "films/a.flm" nm_a)))
(each (lambda (svc image)
		(when svc (storage-stop svc))
		(pii-remove image))
	(list nm_a nm_b) (list nm_a_image nm_b_image))

;a store that is not there: none of it, and nothing of this machine's store
;in its place
(assert-eq "a name of a store that is not there has no service" :nil (name-service "store://no_such_store_here/a"))
(assert-eq "no stream" :nil (name-stream "store://no_such_store_here/a"))
(assert-eq "no size" :nil (name-size "store://no_such_store_here/a"))
(assert-list-eq "nothing in it" '(() ()) (name-list "store://no_such_store_here/"))
(assert-eq "and no folder of it but its top" :nil (name-folder? "store://no_such_store_here/films"))

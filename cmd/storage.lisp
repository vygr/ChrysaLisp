(import "lib/options/options.inc")
(import "service/storage/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: storage [options] [path]

    options:
        -h --help: this help info.
        -s --start image: start the service of this system on
            the store in that file of the host, or disk.
        -m --make megabytes: with -s, if there is no store
            there, make one that big.
        -i --id name: with -m, the id of the new store,
            default store.
        -F --format: with -s and -m, make a store of what is
            there even if it is something else, a disk.
        -k --copies num: with -m, how many members keep a
            block, default 3.
        -q --query form: send a gopher to path with that
            function, in double quotes,
            \q(lambda (state data at count) ...)\q, which gives
            (state next). What it comes back with, and the
            members it went to.
        -M --members: who the members are, and which are
            there now. With +id or -id as path, one is
            joined or retired.
        -x --stop: stop the service.
        -l --list: the objects whose paths begin with path,
            all of them if none is given.
        -p --put file: path is set to what is in that file
            of the host.
        -g --get file: what path is, to that file of the host,
            to stdout if the file is -.
        -d --delete: there is no object of that path from now.
        -w --sweep: let go of the blocks no object is made of.
        -c --check: say which blocks are not what their names
            say. -C to let go of them too.
        -P --pin: this system holds all of what begins with
            path. -U no longer.

    The Storage service of this system, an object store. With
    no option, what the store is and what is in it.")
(("-s" "--start") ,(opt-str 'opt_s))
(("-m" "--make") ,(opt-num 'opt_m))
(("-i" "--id") ,(opt-str 'opt_i))
(("-F" "--format") ,(opt-flag 'opt_F))
(("-k" "--copies") ,(opt-num 'opt_k))
(("-q" "--query") ,(opt-str 'opt_q))
(("-M" "--members") ,(opt-flag 'opt_M))
(("-x" "--stop") ,(opt-flag 'opt_x))
(("-l" "--list") ,(opt-flag 'opt_l))
(("-p" "--put") ,(opt-str 'opt_p))
(("-g" "--get") ,(opt-str 'opt_g))
(("-d" "--delete") ,(opt-flag 'opt_d))
(("-w" "--sweep") ,(opt-flag 'opt_w))
(("-c" "--check") ,(opt-flag 'opt_c))
(("-C" "--mend") ,(opt-flag 'opt_C))
(("-P" "--pin") ,(opt-flag 'opt_P))
(("-U" "--unpin") ,(opt-flag 'opt_U))
))

(defun show (entry)
	;a line of an object: how long, how many times written, how many of
	;the members have it, who wrote it, its path
	(bind '(name hash bytes vv writer seen deleted) entry)
	(print (pad (str bytes) 12) " " (pad (str (store-vv-total vv)) 4) " "
		(pad (cat (str (length (filter (# (find %0 members)) seen))) "/" (str (length members))) 6) " "
		(slice writer 0 8) " " name))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_s :nil opt_m 0 opt_i :nil opt_F :nil opt_k 0 opt_q :nil opt_M :nil opt_x :nil opt_l :nil opt_p :nil opt_g :nil
				opt_d :nil opt_w :nil opt_c :nil opt_C :nil opt_P :nil opt_U :nil
				args (options stdio usage)))
		(defq name (if (> (length args) 1) (second args) ""))
		(if opt_s (unless (apply (const storage-start) (cat (list opt_s)
				(if (> opt_m 0) (list :size (* opt_m 1048576)) (list))
				(if opt_i (list :id opt_i) (list)) (if opt_F (list :force :t) (list))
				(if (> opt_k 0) (list :copies opt_k) (list))))
			(print "No store at " opt_s ", and none made.")))
		(defq svc (storage-service) members (if svc (ifn (storage-members :nil svc) (list)) (list)))
		(cond
			((not svc) (unless opt_s (print "The Storage service is not running, storage -s image.")))
			(opt_x (print (if (storage-stop svc) "Stopped." "It did not say.")))
			(opt_p
				(cond
					((eql name "") (print "A path to put it at."))
					((not (defq data (load opt_p))) (print "No file " opt_p))
					(:t (bind '(what entry) (storage-put name data :nil svc))
						(if (eql what :ok) (show entry) (print "Not put: " what)))))
			(opt_g
				(cond
					((not (defq data (storage-get name svc))) (print "No object " name))
					((eql opt_g "-") (prin data) (stream-flush (io-stream 'stdout)))
					((save data opt_g) (print (length data) " bytes to " opt_g))))
			(opt_d (print (first (storage-delete name :nil svc))))
			(opt_q
				;the function, written with or without a ' before it
				(defq form (catch (first (read (string-stream (trim-start opt_q "'")))) :nil)
					back (if (list? form) (storage-walk name :nil form :nil 60 svc)))
				(cond
					(back (print (first back))
						(print "by way of " (join (map (# (slice %0 0 8)) (second back)) " ")))
					((print "It did not come back."))))
			(opt_M
				(if (or (starts-with "+" name) (starts-with "-" name)) (setq members (storage-members name svc)))
				(defq there (map (# (first (split %0 " "))) (filter (const nempty?)
					(split (ifn (storage-ask svc +storage_type_peers) "") (ascii-char 10)))))
				(each (# (print %0 (cond ((eql %0 (. (storage-info svc) :find :member)) "  this one")
					((find %0 there) "  there") ("  not there")))) members))
			(opt_w (print (storage-sweep svc) " blocks let go of."))
			((or opt_c opt_C)
				(defq bad (storage-check opt_C svc))
				(each (const print) bad)
				(print (length bad) " blocks are not what their names say" (if opt_C ", let go of." ".")))
			((or opt_P opt_U)
				(each (const print) (storage-pin name opt_U svc))
				(print (length (storage-pins-missing svc)) " blocks of what is pinned are not here yet."))
			(:t (unless opt_l
					(defq info (storage-info svc))
					(print "Store:   " (. info :find :id))
					(print "Member:  " (. info :find :member) ", of " (length (. info :find :members)))
					(each (# (print "         with " %0)) (filter (# (nql %0 (. info :find :member))) (. info :find :members)))
					(print "Blocks:  " (. info :find :blocks) " of " (. info :find :block_size) " bytes or less")
					(print "Free:    " (/ (. info :find :free) 1048576) "MB")
					(print "Objects: " (. info :find :objects)))
				(each (const show) (storage-list name svc))))))

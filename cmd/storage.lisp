(import "lib/options/options.inc")
(import "service/storage/app.inc")
(import "lib/files/files.inc")

(defq usage `(
(("-h" "--help")
"Usage: storage [options] [path]

    options:
        -h --help: this help info.
        -j --join: make this machine a member of the store of
            its mesh, and start it. A store is made for it
            in storage/store.img, of 4096 MB at most, which
            takes only what is put in it, and its id is made
            from the mesh key, so every machine with that
            key is of the one store. From then on it starts
            by itself, whenever this machine is up. -m, -i
            and -k say otherwise.
        -f --forget: this machine no longer starts its
            store by itself. The store is left as it is.
        -s --start image: as -j, of the store in that file
            of the host, or disk.
        -m --make megabytes: with -s or -j, if there is no
            store there, make one that big.
        -i --id name: with -m, the id of the new store. The
            mesh's own if it is not said, or store.
        -F --format: with -s and -m, make a store of what is
            there even if it is something else, a disk.
        -k --copies num: with -m, how many members keep a
            block, default 3. With a path and no -s, how
            many are to keep the blocks of that object, or
            of every object whose path begins so: more for
            what must not be lost or is read a lot. 0 is as
            many as the store says.
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
        -T --tree dir: every file under that folder of the
            host is put, as a stream, at path and its own
            path, and then each is read back as a stream and
            compared with the file. obj, deps and folders
            whose names begin with a dot are not gone in to.
        -V --verify dir: the reading back and comparing of
            -T only, of what was put before, or elsewhere.
        -P --pin: this system holds all of what begins with
            path. -U no longer.

    The Storage service of this system, an object store. With
    no option, what the store is and what is in it.")
(("-j" "--join") ,(opt-flag 'opt_j))
(("-f" "--forget") ,(opt-flag 'opt_f))
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
(("-T" "--tree") ,(opt-str 'opt_T))
(("-V" "--verify") ,(opt-str 'opt_V))
))

(defun show (entry)
	;a line of an object: how long, how many times written, how many of
	;the members have it, who wrote it, its path
	(bind '(name hash bytes vv writer seen deleted copies) entry)
	(print (pad (str bytes) 12) " " (pad (str (store-vv-total vv)) 4) " "
		(pad (cat (str (length (filter (# (find %0 members)) seen))) "/" (str (length members))) 6) " "
		(slice writer 0 8) " " (if (> copies 0) (cat "x" (str copies)) "  ") " " name))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j :nil opt_f :nil opt_s :nil opt_m 0 opt_i :nil opt_F :nil opt_k -1 opt_q :nil opt_M :nil opt_x :nil opt_l :nil opt_p :nil opt_g :nil
				opt_d :nil opt_w :nil opt_c :nil opt_C :nil opt_P :nil opt_U :nil opt_T :nil opt_V :nil
				args (options stdio usage)))
		(defq name (if (> (length args) 1) (second args) ""))
		(if opt_j (setq opt_s (ifn opt_s +storage_image)))
		(when opt_f
			(storage-member-forget)
			(print "This machine does not start its store by itself now. The store is as it was."))
		(when opt_s
			;a store there already is as big as it is, and has its own id
			(defq size (if (> opt_m 0) opt_m +storage_size))
			(cond
				((storage-service) (print "The Storage service is running already."))
				((not (storage-claim opt_s))
					(print "Another session of this machine is serving the store at " opt_s "."))
				((storage-join opt_s size opt_i (if (> opt_k 0) opt_k) opt_F)
					;and by itself from now on
					(storage-member-save opt_s size opt_i (if (> opt_k 0) opt_k)))
				((print "No store at " opt_s ", and none made."))))
		(defq svc (storage-service) members (if svc (ifn (storage-members :nil svc) (list)) (list)))
		(cond
			((not svc) (unless opt_s (print "The Storage service is not running, storage -s image.")))
			(opt_x (defq said (storage-member))
				(print (if (storage-stop svc) "Stopped." "It did not say."))
				(if said (storage-unclaim (. said :find :image))))
			(opt_p
				(cond
					((eql name "") (print "A path to put it at."))
					(:t (bind '(what entry) (storage-put-file name opt_p :nil svc))
						(cond
							((eql what :ok) (show entry))
							((eql what :none) (print "No file " opt_p))
							((print "Not put: " what))))))
			(opt_g
				(cond
					((not (defq object (storage-stream name +file_open_read svc))) (print "No object " name))
					((eql opt_g "-")
						(while (defq part (read-blk object 65536)) (prin part))
						(stream-flush (io-stream 'stdout)))
					((defq host (file-stream opt_g +file_open_write) bytes 0)
						(while (defq part (read-blk object 65536))
							(write-blk host part)
							(setq bytes (+ bytes (length part))))
						(stream-flush host)
						(print bytes " bytes to " opt_g))
					((print "No file " opt_g " to write."))))
			((or opt_T opt_V)
				(defq files (filter (# (not (starts-with "./deps/" %0))) (files-all (ifn opt_T opt_V) '("")))
					put 0 same 0 bytes 0 t0 (pii-time))
				(defun at (file) (cat name (if (starts-with "./" file) (slice file 2 -1) file)))
				(when opt_T
					(each (lambda (file)
						(defq what (first (storage-put-file (at file) file :nil svc)))
						(if (eql what :ok) (++ put) (print "Not put: " what " " file)))
						files)
					(print put " of " (length files) " files put, in " (/ (- (pii-time) t0) 1000) "ms"))
				(defq t1 (pii-time))
				(each (lambda (file)
					(if (defq size (storage-same-file? (at file) file svc))
						(setq same (inc same) bytes (+ bytes size))
						(print "Not the same: " file)))
					files)
				(print same " of " (length files) " read back the same, " bytes " bytes, in " (/ (- (pii-time) t1) 1000) "ms"))
			(opt_d (print (first (storage-delete name :nil svc))))
			(opt_q
				;the function, written with or without a ' before it
				(defq form (catch (first (read (string-stream (trim-start opt_q "'")))) :nil)
					back (if (list? form) (storage-walk name :nil form :nil 60 svc)))
				(cond
					(back (print (first back))
						(print "by way of " (join (map (# (cat (slice (first %0) 0 8) "@" (str (second %0)))) (second back)) " ")))
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
			((and (>= opt_k 0) (not opt_s))
				;the one object, or every one whose path begins so
				(defq entries (if (storage-stat name svc) (list (storage-stat name svc)) (ifn (storage-list name svc) (list))))
				(each (lambda (entry)
					(bind '(what now) (storage-copies (first entry) opt_k svc))
					(if (eql what :ok) (show now) (print "Not set: " (first entry))))
					entries)
				(if (empty? entries) (print "No object at " name)))
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

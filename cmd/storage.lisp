(import "lib/options/options.inc")
(import "service/storage/app.inc")
(import "service/storage/scene.inc")
(import "lib/files/files.inc")
(import "service/net/app.inc")

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
            and -k say otherwise. It says what it joined
            and the other members, who keep what is put.
        -f --forget: this machine no longer starts its
            store by itself. The store is left as it is.
        -o --on machine: the store of another machine, by the
            start of its system id as mesh lists it, 90D57181:
            looked at, put to or got from as a client, this
            machine no member of it, and none of it kept here.
        -L --travels on|off: this machine is one that is
            taken away from the others, a laptop, or is not.
            With -j, or of a member. One that travels pins
            the area of its user, usr/<user>/, as its store
            starts, so it has all of it when it is away.
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
        -M --members: who the members are, which are
            there now, and what and where each is. With
            +id as path, that one is joined.
        -e --retire id: the member of that id retired, and
            the members said, as -M does. The start of the id
            is enough, if no other member's starts so.
        -x --stop: stop the service.
        -z --resize megabytes: the store of this member put on
            a volume of that many MB, bigger, or smaller if what
            it has fits: a copy on this machine, beside it, the
            size in its name, storage/store.8192MB.img, and the
            old one gone. This machine starts its store from the
            new one from now on.
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
            whose names begin with a dot are not gone in to,
            and files whose names begin with a dot are not put.
        -V --verify dir: the reading back and comparing of
            -T only, of what was put before, or elsewhere.
        -P --pin: this system holds all of what begins with
            path. -U no longer.
        -n --snapshot name: a snapshot of the objects whose
            paths begin with path, all if none is given:
            snapshots/name/ and their paths, no block copied.
        -N --snapshots: the snapshots there are.
        -R --restore name: the objects whose paths begin
            with path are as they were in that snapshot.
            Those made since are left, and said.
        -D --drop name: the snapshot is gone, its blocks let
            go of by a sweep if nothing else is made of them.
        -t --scene [name]: the scenes there are, in
            tests/storage/, and what each is for. With a
            name, that one is run: a store of its own is
            started, a thing done to it, what happens said
            and what came of it checked. -r file records it
            too.
        -r --record file: what every member of the store does,
            a line each, to that file of the host, - for
            stdout: the time, the member, what was done and
            what to. For -S seconds, 60 if not said.
        -S --secs secs: how long -r records.
        -G --group name: the group of the members -r
            records, *Storage if not said, a test's.

    The Storage service of this system, an object store. With
    no option, what the store is and what is in it.")
(("-j" "--join") ,(opt-flag 'opt_j))
(("-f" "--forget") ,(opt-flag 'opt_f))
(("-o" "--on") ,(opt-str 'opt_o))
(("-L" "--travels") ,(opt-str 'opt_L))
(("-s" "--start") ,(opt-str 'opt_s))
(("-m" "--make") ,(opt-num 'opt_m))
(("-i" "--id") ,(opt-str 'opt_i))
(("-F" "--format") ,(opt-flag 'opt_F))
(("-k" "--copies") ,(opt-num 'opt_k))
(("-q" "--query") ,(opt-str 'opt_q))
(("-M" "--members") ,(opt-flag 'opt_M))
(("-e" "--retire") ,(opt-str 'opt_e))
(("-x" "--stop") ,(opt-flag 'opt_x))
(("-z" "--resize") ,(opt-num 'opt_z))
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
(("-r" "--record") ,(opt-str 'opt_r))
(("-n" "--snapshot") ,(opt-str 'opt_n))
(("-N" "--snapshots") ,(opt-flag 'opt_N))
(("-R" "--restore") ,(opt-str 'opt_R))
(("-D" "--drop") ,(opt-str 'opt_D))
(("-S" "--secs") ,(opt-num 'opt_S))
(("-t" "--scene") ,(opt-flag 'opt_t))
(("-G" "--group") ,(opt-str 'opt_G))
))

(defun store-there (svc)
	;the members of the store that are up now, anywhere in the mesh: each
	;says the store and the member it is as it joins the group, the name,
	;the service, its machine, then those
	(defq id (cat (. (storage-info svc) :find :id) " "))
	(reduce (lambda (out entry)
			(and (> (length (defq parts (split entry ","))) 3) (starts-with id (elem-get parts 3))
				(push out (slice (elem-get parts 3) (length id) -1)))
			out)
		(mail-enquire (cat +storage_group ",")) (list)))

(defun store-known (svc)
	;the members of a store, as it and every other member up now know them:
	;one just joined has not met them yet
	(defq id (. (defq info (storage-info svc)) :find :id) me (. info :find :member)
		all (ifn (storage-members :nil svc) (list)))
	(each (lambda (entry)
			;the group's name, the service, its machine, and the store and member
			(defq parts (split entry ","))
			(and (> (length parts) 3) (starts-with (cat id " ") (elem-get parts 3))
				(nql (cat id " " me) (elem-get parts 3))
				(each (# (unless (find %0 all) (push all %0)))
					(ifn (storage-members :nil (hex-decode (second parts))) (list)))))
		(mail-enquire (cat +storage_group ",")))
	(sort all))

(defun store-say (svc members)
	;each member of the store, a line each: this machine, one that is
	;there or one that is not, and as far as the mesh knows, what it is
	;and where
	(defq me (. (storage-info svc) :find :member) there (store-there svc)
		peers (ifn (net-peers-rpc) (list)) systems (lisp-systems))
	(each (lambda (member)
			(defq sys (some (# (if (eql (hex-encode %0) (to-upper member)) %0)) systems)
				ip (some (lambda ((id ip &ignore)) (if (eql (to-upper id) (to-upper member)) ip)) peers)
				state (cond ((eql member me) "this machine") ((find member there) "there") ("not there")))
			(defq more (cat (if sys (cat "  " (net-what-is sys)) "") (if ip (cat "  " ip) "")))
			(print "    " (slice member 0 8) "  " state (if (nempty? more) (cat (pad "" (- 12 (length state))) more) "")))
		members))

(defun store-to (svc)
	;before a put, the store it goes to and who keeps it, on one line
	(defq info (storage-info svc) me (. info :find :member) there (store-there svc))
	(print "To " (. info :find :id) ", kept by its members: "
		(join (map (# (cat (slice %0 0 8) (cond ((eql %0 me) " this machine") ((find %0 there) " there") (" not there"))))
			(ifn (storage-members :nil svc) (list))) ", ")))

(defun got-all (object)
	;after the read of an object, an error if it ended of a fault, a block
	;not had, and not at its end
	(when (defq fault (stream-fault object))
		(print "Error: not all of it, a block of it could not be had " (second fault))))

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
			(defq opt_o :nil opt_j :nil opt_f :nil opt_s :nil opt_m 0 opt_i :nil opt_F :nil opt_k -1 opt_q :nil opt_M :nil opt_e :nil opt_x :nil opt_l :nil opt_p :nil opt_g :nil
				opt_d :nil opt_w :nil opt_c :nil opt_C :nil opt_P :nil opt_U :nil opt_T :nil opt_V :nil
				opt_r :nil opt_S 60 opt_G :nil opt_z 0 opt_L :nil opt_t :nil opt_n :nil opt_N :nil opt_R :nil opt_D :nil
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
				((defq joined (storage-join opt_s size opt_i (if (> opt_k 0) opt_k) opt_F))
					;and by itself from now on
					(storage-member-save opt_s size opt_i (if (> opt_k 0) opt_k))
					;what it joined, and who else is of it: a store of the mesh's
					;own id is every machine's that has this key and was joined
					(defq id (. (storage-info joined) :find :id) known (store-known joined))
					(print "Joined the store " id
						(if (eql id (storage-key-id)) ", the store of every machine with this mesh_key." "."))
					(cond
						((<= (length known) 1) (print "No other machine is a member of it yet."))
						(:t (print "Its members, who keep what is put in it:")
							(store-say joined known))))
				((print "No store at " opt_s ", and none made."))))
		(when opt_L
			(cond
				((not (find opt_L '("on" "off"))) (print "-L on, or -L off."))
				((not (storage-member-travels (eql opt_L "on")))
					(print "This machine is no member of a store, storage -j."))
				((eql opt_L "off")
					(print "This machine is not one that travels now. What it has pinned stays pinned, storage -U to let go."))
				((defq pinned (storage-user-pin))
					(print "This machine travels, and holds all of " pinned " from now on."))
				((print "This machine travels. Its user's area is pinned as its store starts."))))
		(defq svc (if opt_o (storage-service-on opt_o) (storage-service)) members (if svc (ifn (storage-members :nil svc) (list)) (list)))
		(cond
			((and opt_t (eql name ""))
				(each (lambda ((scene what)) (print scene (pad "" (max 1 (- 12 (length scene)))) what)) (scene-list)))
			(opt_t
				(defq out (if opt_r (if (eql opt_r "-") (io-stream 'stdout) (file-stream opt_r +file_open_write))) lines 0
					result (scene-run name (lambda (line) (print line) (stream-flush (io-stream 'stdout)))
						(if out (lambda (line) (when line (++ lines) (write-line out line))))))
				(if out (stream-flush out))
				(cond
					((not result) (print "No scene " name ", or it did not finish."))
					(:t (print (first result) " held, " (second result) " did not."
						(if (and out (nql opt_r "-")) (cat " " (str lines) " lines recorded to " opt_r) "")))))
			(opt_r
				;of the members of the group, whether or not this system has one
				(defq out (if (eql opt_r "-") (io-stream 'stdout) (file-stream opt_r +file_open_write)))
				(cond
					((not out) (print "No file " opt_r " to write."))
					(:t (defq lines (storage-record-run (lambda (line) (if line (write-line out line)))
							(+ (pii-time) (* opt_S 1000000)) opt_G))
						(stream-flush out)
						(unless (eql opt_r "-") (print lines " lines to " opt_r)))))			((not svc)
				(cond
					(opt_o (print "No store on a machine " opt_o " in the mesh, mesh lists them."))
					((not opt_s) (print "The Storage service is not running, storage -s image."))))
			((> opt_z 0)
				(print "Copying the store to a volume of " opt_z " MB ...")
				(if (defq moved_to (storage-resize opt_z svc))
					(print "The store is in " moved_to " now.")
					(print "Not done: it is that size, or what it has does not fit, or the file could not be made. It is as it was.")))
			(opt_x (defq said (storage-member))
				(print (if (storage-stop svc) "Stopped." "It did not say."))
				(if said (storage-unclaim (. said :find :image))))
			(opt_n
				(if (defq count (storage-snapshot opt_n name svc))
					(print "Snapshot " opt_n ", of " count " objects" (if (eql name "") "." (cat " under " name ".")))
					(print "Not a name a snapshot may have: " opt_n)))
			(opt_N
				(each (lambda ((snap count)) (print snap (pad "" (max 1 (- 20 (length snap)))) count " objects"))
					(ifn (storage-snapshots svc) (list))))
			(opt_R
				(if (defq got (storage-restore opt_R name svc))
					(print (first got) " set back as they were in " opt_R
						(if (> (second got) 0) (cat ", " (str (second got)) " made since are left.") "."))
					(print "No snapshot " opt_R (if (eql name "") "" (cat " of " name)))))
			(opt_D (print (ifn (storage-snapshot-drop opt_D svc) 0) " paths of " opt_D " gone."))
			(opt_p
				(cond
					((eql name "") (print "A path to put it at."))
					(:t (store-to svc)
						(bind '(what entry) (storage-put-file name opt_p :nil svc))
						(cond
							((eql what :ok) (show entry))
							((eql what :none) (print "No file " opt_p))
							((print "Not put: " what))))))
			(opt_g
				(cond
					((not (defq object (storage-stream name +file_open_read svc))) (print "No object " name))
					((eql opt_g "-")
						(while (defq part (read-blk object 65536)) (prin part))
						(stream-flush (io-stream 'stdout))
						(got-all object))
					((defq host (file-stream opt_g +file_open_write) bytes 0)
						(while (defq part (read-blk object 65536))
							(write-blk host part)
							(setq bytes (+ bytes (length part))))
						(stream-flush host)
						(print bytes " bytes to " opt_g)
						(got-all object))
					((print "No file " opt_g " to write."))))
			((or opt_T opt_V)
				;not the files of a folder of deps, nor one whose name begins with a dot
				(defq files (filter (# (not (or (starts-with "./deps/" %0) (starts-with "." (last (split %0 "/"))))))
					(files-all (ifn opt_T opt_V) '("")))
					put 0 same 0 bytes 0 t0 (pii-time))
				(defun at (file) (cat name (if (starts-with "./" file) (slice file 2 -1) file)))
				(when opt_T
					(store-to svc)
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
			((or opt_M opt_e)
				;a word of the command line that starts with - is an option, so
				;one is retired by -e, joined by +id
				(cond
					;by the start of its id, as -o takes one
					(opt_e (defq want (trim-start opt_e "-") got (storage-retire want svc))
						(case (first got)
							(:ok (setq members (rest got)))
							(:none (print "No member's id starts " want ", none retired."))
							(:many (print want " starts more than one member's id, none retired: " (join (rest got) " ")))))
					((starts-with "+" name) (setq members (storage-members name svc))))
				(print "Store:   " (. (storage-info svc) :find :id))
				(store-say svc members))
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

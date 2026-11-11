;;;;;;;;;;;;;;
; VP Assembler
;;;;;;;;;;;;;;

(import "././task/jobs.inc")
(import "././boot/image.inc")
(import "././boot/id.inc")
(import "././files/files.inc")

;module
(env-push)

;the build modes, 1 normal and 2 validate, a normal build and more checks
;as it runs
(defun vp-dir (mode)
	; (vp-dir mode) -> path
	;where a build mode's VP is kept, obj/vp/, or obj/vp_validate/ beside
	;it, a bundle for each source file of the VP of its functions, under the
	;file's own name
	(if (= mode 2) "obj/vp_validate/" "obj/vp/"))

(defun bundles-gone (dir)
	; (bundles-gone dir)
	;the bundles in dir, (vp-dir), of source files there are no more: a
	;file taken away leaves its bundle, and trace, lint and the loader go
	;by what is in dir. The code of its functions is left, a function of
	;that name may be another file's now
	(each (# (if (= (age %0) 0) (pii-remove (cat dir %0))))
		(files-all dir '(".vp") (length dir))))

(defun biggest-first (units sizes)
	;the units in the order the queue gives them out, the biggest first,
	;the queue goes from the end. With the small ones last the workers all
	;end at much the same time
	(map (const second) (sort (map (const list) sizes units) (# (- (first %0) (first %1))))))

(defun farm-jobs (units *abi* *cpu* plats &optional more herd)
	; (farm-jobs units abi cpu plats [more herd]) -> said
	;each unit a job for a herd of workers, lib/asm/asm.lisp, on the nodes
	;of this machine, a worker for each unit and no more than a node each,
	;herd of them if that is more: each has to set up to compile before its
	;first job. A unit is (kind what):
	;:file (file assemble targets), a source file, if it is to be assembled,
	;and the platforms to make code of it for, each (cpu abi prev), prev the
	;hash of the bundle that platform's code is of if it is known: one whose
	;bundle comes out that has no code made again.
	;:bundle (file), the bundle of the file's VP and no code.
	;:image (cpu abi funcs), a platform's boot image of those functions.
	;The VP is assembled as abi and cpu, it is the same for every platform.
	;plats are the platforms the workers place the translators of.
	;When the last unit is answered (more said) is asked, once, for more
	;units of what those made, and they are given to the same herd: the boot
	;images, which are made of code. What a worker says is printed, but a
	;line that starts with =, which is what it did: those are given back,
	;each split at its spaces. The errors are put on the caller's errors
	(defq said (list))
	(and more (empty? units) (setq units (more said) more :nil))
	(when (nempty? units)
		(defq job (# (cat (const (str-alloc +job_size))
				(str (list (first %0) (second %0) *abi* *cpu* plats *build_mode* *build_emit* *build_verb*
						(vp-dir *build_mode*))))))
		;a job is started again if its node goes. The timeout is only for a
		;worker that dies and its node does not, so it is long, a slow job
		;must not be taken for a dead one.
		(jobs-farm "lib/asm/asm.lisp" (map job units)
			(list (max (length units) (ifn herd 0)) 1 1) (task-timeout 60)
			(lambda (msg jobs)
				;the key it was given, then what it said. A job with an
				;error is not tried again.
				(defq error_flag :nil)
				(each (# (cond
							((starts-with "Error:" %0)
								(push errors %0)
								(setq error_flag :t))
							((starts-with "=" %0) (push said (split %0 " ")))
							((nempty? %0) (print %0)))) (split (slice msg +job_reply_size -1) "\n"))
				(stream-flush (io-stream 'stdout))
				(defq out (if error_flag (. jobs :failed msg) (. jobs :answered msg)))
				;the last of them is in: what more there is of what they made
				(when (and more (eql out 0))
					(defq next (more said))
					(setq more :nil)
					(when (nempty? next)
						(. jobs :add (map job next))
						(setq out (. jobs :out))))
				out)
			;a job that has gone out and not come back, again and again,
			;is not waited on for ever
			(lambda (tries)
				(cond
					((< tries 3)
						(print "No answer from a worker, trying again")
						(stream-flush (io-stream 'stdout)))
					(:t (push errors "Error: no answer from the workers, the build is stopped"))))))
	said)

(defun said-of (said what)
	; (said-of said what) -> lines
	;those of the lines a farm's workers said that start with what, less it
	(map (const rest) (filter (# (eql (first %0) what)) said)))

(defun file-units (files targets &optional assemble)
	; (file-units files targets [assemble]) -> units
	;a unit for each file, its code for the targets, in the order the queue
	;gives them out, the biggest first
	(biggest-first (map (# (list :file (list %0 assemble targets))) files)
		(map (# (second (pii-fstat %0))) files)))

(defun manifest-load (path)
	; (manifest-load path) -> env
	;what was made, and from what: a file to the keys noted of it
	(defq m (env 101))
	(catch (progn
			(and (defq data (load path)) (nempty? data)
				(each (lambda (e) (def m (sym (first e)) (rest e))) (first (read (string-stream data)))))
			:t) :t)
	m)

(defun manifest-save (m path)
	(save (str (map (lambda ((k v)) (cat (list (str k)) v)) (tolist m))) path))

(defun vp-manifest ()
	; (vp-manifest) -> path
	;of the bundles of the build mode: a source file to the key it was
	;assembled from, and the hash of its bundle
	(if (= *build_mode* 2) "obj/make_vp_validate" "obj/make_vp"))

(defun platform-manifest (cpu abi)
	; (platform-manifest cpu abi) -> path
	;of a platform's code: a source file to the hash of the bundle it was
	;translated from, the id of the translator, and a short hash of the
	;names of its functions. The names are beside it, (platform-names)
	(cat "obj/make_" cpu "_" abi))

(defun platform-names (cpu abi)
	; (platform-names cpu abi) -> path
	;of a platform's code: a source file to the names of the functions
	;there is code of. Only read when a file's are not what they were
	(cat (platform-manifest cpu abi) "_names"))

(defun compiled (made *abi* *cpu*)
	;a compile makes what it is told to, it does not go by the manifests.
	;Of each file whose bundle came out other than the one noted, the notes
	;are forgotten, the bundle's and this platform's, and the next make
	;makes them. In a mode that is not the one noted too, where the VP of
	;the two is not the same
	(defq vpman (manifest-load (vp-manifest)) platman (manifest-load (platform-manifest *cpu* *abi*))
		vp_dirty :nil plat_dirty :nil)
	(each (lambda ((file hash &rest _))
			(defq k (sym file))
			(when (and (defq v (get k vpman)) (nql (second v) hash))
				(undef vpman k) (setq vp_dirty :t))
			(when (and (defq n (get k platman)) (nql (first n) hash))
				(undef platman k) (setq plat_dirty :t))) made)
	(if vp_dirty (manifest-save vpman (vp-manifest)))
	(if plat_dirty (manifest-save platman (platform-manifest *cpu* *abi*))))

(defun compile (files &optional *abi* *cpu*)
	; (compile files [abi cpu]) -> (made errors)
	;the files, their VP and then this platform's code, whether or not
	;they have changed. made is each file done and the hash of its bundle
	(setd *abi* (abi) *cpu* (cpu))
	(unless (list?? files) (setq files (list files)))
	(defq errors (list) targets (list (list *cpu* *abi* :nil)))
	(list (said-of (farm-jobs (file-units files targets :t) *abi* *cpu* targets) "=key=") errors))

;;;;;;;;;;;;;
; make system
;;;;;;;;;;;;;

(defun all-vp-files ()
	;filter to only the .vp files, strip off the leading "./"
	(filter (# (not (starts-with "apps/" %0))) (files-all "." '(".vp") 2)))

(defun all-app-files ()
	;filter to only the app .vp files, strip off the leading "./"
	(filter (# (starts-with "apps/" %0)) (files-all "." '(".vp") 2)))

(defun all-class-files ()
	;filter to only the class.inc files, strip off the leading "./"
	(files-all "." '("class.inc") 2))

(defun all-build-files ()
	; (all-build-files) -> (vp_files class_files)
	;all-vp-files and all-class-files of one walk of the tree, each in the
	;order it has there. A walk was 4 to 5 ms on the M4
	(defq vps (list) classes (list))
	(each (# (cond
				((ends-with "class.inc" %0) (push classes %0))
				((not (starts-with "apps/" %0)) (push vps %0))))
		(files-all "." '(".vp" "class.inc") 2))
	(list vps classes))

(defun class-env (classes)
	;in a compile env, the classes declared, all that make-info needs to
	;know the function of a method. Not lib/asm/func.inc, the assembler
	;and the translator, a third of the time of a make with nothing to do.
	;But lib/asm/vp.inc: class.inc files take (const vp-push) and the like
	;as they are read, and an unbound symbol is not checked on a release
	;build, the VP64 of the snapshot that installs, it is a crash
	(include "lib/asm/class.inc")
	(include "lib/asm/vp.inc")
	(include "sys/statics/class.inc")
	(each include classes))

(defun make-scan (file &optional scan)
	;what make goes by of a file, read from it by (files-scan), or of the
	;scan given, made by a worker of (scans-farm): its includes and
	;imports, as paths, and the forms that name what it makes, kind and
	;names, made paths of by (make-products) with the classes declared
	(if (= (age file) 0) (throw "No such file !" file))
	(reduce (lambda ((d p) (kind & &rest names))
			(if (find kind '("include" "import"))
				(push d (path-to-absolute (first names) file))
				(push p (cat (list kind) names)))
			(list d p))
		(ifn scan (files-scan file))
		(list (list "lib/asm/asm.inc" "class/lisp/root.inc" "class/lisp/task.inc" file) (list))))

(defun make-products (p)
	;the paths of the functions the lines of (make-scan) name
	(map (lambda (line)
			(defq kind (first line) a (second line) b (if (> (length line) 2) (third line)))
			(case kind
				(("def-method" "gen-host-call") (f-path (sym a) (sym b)))
				("gen-vtable" (f-path (sym a) :vtable))
				("gen-type" (f-path (sym a) :type))
				("gen-create" (f-path (sym a) (if b (sym (cat :create_ b)) :create)))
				("def-func" (sym a)))) p))

;what make read of each file is kept, by its age, and read again only of
;a file that has changed. One that changed in the second the keep is
;written is not kept, the age is of a second; and none is kept of an
;asm.inc, files.inc or tree.inc not as these are, they say how a file is
;read. With each its hash, once that has been asked for, and beside them
;the key of each .vp file
(defq +make_scans "obj/make_scans"
	+scans_by ''("lib/asm/asm.inc" "lib/files/files.inc" "lib/text/tree.inc" "lib/text/tokenize.inc")
	;fewer files than this to scan are scanned here, a herd costs more
	+scans_farm_min 16
	;every translator is made of this as well as its own source
	+trans_parser "lib/trans/vp_parse.vp"
	;what each platform's boot image was made from
	+make_boot "obj/make_boot")

(defun scans-key ()
	;how a file is read, the ages of what says so, and the form of the keep
	(cat (list 2) (map (const age) +scans_by)))

(defun scans-load ()
	; (scans-load) -> (scans keys)
	;the scans kept, an env of file to (age scan hash), and the keys kept,
	;an env of .vp file to key
	(defq scans (env 101) keys (env 101))
	(catch (progn
			(when (and (defq data (load +make_scans)) (nempty? data)
					(eql (str (first (setq data (first (read (string-stream data)))))) (str (scans-key))))
				(each (lambda ((f a d p h)) (def scans (sym f) (list a (list d p) h))) (second data))
				(each (lambda ((f k)) (def keys (sym f) k)) (third data)))
			:t) :t)
	(list scans keys))

(defun scans-save (scans keys)
	;those of before this second kept, for the next make
	(defq now (/ (pii-time) 1000000) entries (list))
	(each (lambda ((k v))
			(if (< (first v) now)
				(push entries (list (str k) (first v) (first (second v)) (second (second v)) (third v)))))
		(tolist scans))
	(save (str (list (scans-key) entries (map (lambda ((k v)) (list (str k) v)) (tolist keys)))) +make_scans))

(defun file-age (%0)
	;modification time of a file, cached
	(or (get (setq %0 (sym %0)) ages) (def ages %0 (age %0))))

(defun make-info (%0)
	;lists of a file's immediate dependencies, and of the lines that name
	;what it makes, (make-products) gives their paths, of the scan kept if
	;the file has not changed. One that is read again is noted, changed
	(defq k (sym %0) a (file-age %0) c (get k scans))
	(unless (and c (= (first c) a))
		(setq scanned :t)
		(push changed %0)
		(def scans k (setq c (list a (make-scan %0) :nil))))
	(bind '(d p) (second c))
	(list (copy d) p))

(defun file-hash (f)
	;the hash of a file's text, kept with its scan till the file changes
	(defq c (get (sym f) scans))
	(unless (third c)
		(setq scanned :t)
		(elem-set c 2 (hex-encode (sha256 (ifn (load f) "")))))
	(third c))

(defun scans-stale (files)
	;those of the files with no scan kept, or one of an older file
	(filter (lambda (f)
			(not (and (defq c (get (sym f) scans)) (= (first c) (file-age f))))) files))

(defun scans-farm (files)
	;if many of the files have no scan kept, or one of an older file, as a
	;fresh tree has, as an install has, those of them and of every .inc and
	;.vp file of the tree, what they include and what make apps asks,
	;scanned by a herd of lib/asm/scan.lisp on the nodes of this machine,
	;all at once, and kept as (make-info) keeps one. One after another the
	;scan was the most of a make on one node. A file the herd does not
	;answer for is scanned when it is asked for. Of the caller's ages,
	;scans and scanned
	(when (and (> (length (scans-stale files)) +scans_farm_min)
			(defq todo (scans-stale (merge (cat files) (files-all "." '(".inc" ".vp") 2)))))
		;a herd that grows as the nodes come, an install starts them as it makes
		(setq scanned :t)
		(defq units (map (# (cat (const (str-alloc +job_size)) (str %0))) (partition todo 8)))
		(jobs-farm "lib/asm/scan.lisp" units
			(list (length units) 1 1) (task-timeout 60)
			(lambda (msg jobs)
				(when (. jobs :job msg)
					(each (lambda ((f scan h))
							(push changed f)
							(def scans (sym f) (list (file-age f) (make-scan f scan) h)))
						(first (read (string-stream (slice msg +job_reply_size -1)))))
					(. jobs :answered msg))))))

(defun tree-walk (files)
	; (tree-walk files) -> files
	;the files and all they include and import, merged into them, what
	;each includes and makes in info, and the .vp files of them given back.
	;Of the caller's ages, info, scans, scanned and changed. No class is
	;declared for it: what a file makes is only asked for a boot image,
	;(boot-prods), and the classes were 4 ms of a make's 15
	(scans-farm files)
	(each-mergeable (#
			(unless (get (sym %0) info)
				(bind '(d p) (make-info %0))
				(merge files d)
				(def info (sym %0) (list d p)))) files)
	(filter (# (ends-with ".vp" %0)) files))

(defun file-val (f)
	; (file-val f) -> num
	;a number of a file, of its name and of the hash of its text, for the
	;key of what includes it. Kept for this make, of the caller's vals
	(or (get (defq k (sym f)) vals)
		(def vals k (logxor (hash f) (get-long (hex-decode (file-hash f)) 0)))))

(defun tree-key (file)
	; (tree-key file) -> str
	;the key of a file, what its VP is made from: it and all it includes
	;and imports, and all they do. The assembler is among them, every .vp
	;file includes it. The includes have loops, so by a walk, not once a
	;file. Each has a number, (file-val), and the key is those of them all
	;folded two ways, and how many there are. There is no order to a fold,
	;so they are not sorted, and no text of them is made and hashed: a key
	;was the hash of their names and hashes in order, and under the
	;emulator, where an install makes every key, two thirds of 0.4 s was
	;the sort, the text and the hash
	(defq seen (env 101) todo (list file) x 0 s 0 n 0)
	(while (defq f (pop todo))
		(unless (get (defq k (sym f)) seen)
			(def seen k :t)
			(defq v (file-val f))
			(setq x (logxor x v) s (+ s v) n (inc n))
			(each (# (push todo %0)) (first (get k info)))))
	(cat (str x) " " (str s) " " (str n)))

(defun keys-fresh (vps known)
	;the keys of these .vp files, in keys: those kept that still stand, and
	;the rest worked out. One stands if no file it is made of was read
	;again, changed. Once any file has changed none is kept of a .vp file
	;that is not of these: what it is made of was not looked at. known is
	;every file there is info of
	(when (nempty? changed)
		(defq users (env 101) hit (env 101) todo (cat changed) old keys)
		(each (lambda (f)
				(each (lambda (d) (defq k (sym d)) (def users k (push (ifn (get k users) (list)) f)))
					(first (get (sym f) info)))) known)
		(while (defq f (pop todo))
			(unless (get (defq k (sym f)) hit)
				(def hit k :t)
				(each (# (push todo %0)) (ifn (get k users) '()))))
		(setq keys (env 101))
		(each (lambda (f) (and (not (get (defq k (sym f)) hit)) (defq v (get k old)) (def keys k v))) vps))
	(each (lambda (f)
			(unless (get (defq k (sym f)) keys)
				(setq scanned :t)
				(def keys k (tree-key f)))) vps))

(defun tree-facts (files classes)
	; (tree-facts files classes) -> facts
	;what a make goes by of the source, whatever platforms it makes, an env:
	;f_vps the .vp files of the files given, f_all all of them with what
	;they include and import, f_info what each includes and makes, f_keys
	;the key of each .vp file, f_vpman the manifest of the bundles of the
	;build mode, f_classes, f_scans what is kept of each file, f_read how
	;many of them were read again. The bundles of files that have gone are
	;taken away with it
	(bundles-gone (vp-dir *build_mode*))
	(bind '(scans keys) (scans-load))
	(defq ages (env 31) info (env 101) vals (env 101) scanned :nil changed (list)
		all (copy files) vps (tree-walk all))
	(keys-fresh vps all)
	(if scanned (scans-save scans keys))
	(defq facts (env 11))
	(def facts 'f_vps vps 'f_all all 'f_info info 'f_keys keys 'f_classes classes
		'f_vpman (manifest-load (vp-manifest)) 'f_scans scans 'f_read (length changed))
	facts)

(defun trans-id (cpu)
	; (trans-id cpu) -> :nil | str
	;the id of a CPU's translator, what its code is translated by: the
	;hashes of the bundles of its source and the parser's, :nil if either
	;is not made. Of the VP, not of the source: a change to the source
	;that leaves the VP as it was, a comment, a class it includes and does
	;not use, leaves the translator as it was. Of the caller's vpman
	(and (defq a (get (sym (cat "lib/trans/" cpu ".vp")) vpman)) (defq b (get (sym +trans_parser) vpman))
		(cat (second a) (second b))))

(defun vp-stale? (f)
	;is the bundle of the file not the one its key gives. Of the caller's
	;vpman and keys
	(not (and (defq v (get (defq k (sym f)) vpman)) (eql (first v) (get k keys)))))

(defun native-stale? (f man id)
	;is a platform's code of the file, by its manifest, not of the file's
	;bundle and the translator of that id
	(or (vp-stale? f)
		(not (and (defq n (get (sym f) man))
				(eql (first n) (second (get (sym f) vpman))) (eql (second n) id)))))

(defun note-vp (made)
	;the bundles a VP stage made, into vpman: each file to its key and the
	;hash of its bundle
	(each (lambda ((file hash &rest _)) (def vpman (defq k (sym file)) (list (get k keys) hash))) made))

(defun note-native (made man id cpu abi)
	;the code a stage made, into a platform's manifest: each file to the
	;hash of its bundle, the translator's id and the short hash of the
	;names of its functions. Where code was made of a file and the names
	;are not the ones noted, they are noted too, (platform-names), and a
	;name the file made the time before and does not now, nor does any
	;other, is code that is no longer made: a function taken out or named
	;again, one only a validate build has. Its code is taken away, a node
	;would go on binding it. Most makes change no name, and read no names
	(defq renamed (list))
	(each (lambda ((file &optional hash nh &rest names))
			(defq k (sym file) was (get k man) was_nh (and was (> (length was) 2) (third was)))
			(and nh (nql nh was_nh) (push renamed (list k names)))
			(def man k (list (second (get k vpman)) id (ifn nh was_nh)))) made)
	(when (nempty? renamed)
		(defq path (platform-names cpu abi) all (manifest-load path) gone (list))
		(each (lambda ((k names))
				(each (# (unless (find %0 names) (push gone %0))) (ifn (get k all) '()))
				(def all k names)) renamed)
		(when (nempty? gone)
			;not one that some file makes now, a function may have moved
			(defq now (env 1009))
			(each (lambda ((_ names)) (each (# (def now (sym %0) :t)) names)) (tolist all))
			(each (# (unless (get (sym %0) now) (pii-remove (cat "obj/" cpu "/" abi "/" %0)))) gone))
		(manifest-save all path)))

(defun plat-key (cpu abi)
	(sym (cat cpu "_" abi)))

(defun plat-man (cpu abi)
	; (plat-man cpu abi) -> manifest
	;a platform's manifest, read the first time it is asked for. A platform
	;with none was last made by age: beside its code is an empty file of
	;each source file's name, the age of which was when its code was made.
	;They go. Of the caller's mans
	(or (get (defq k (plat-key cpu abi)) mans)
		(progn
			(if (= (age (platform-manifest cpu abi)) 0)
				(each (const pii-remove) (files-all (cat "obj/" cpu "/" abi) '(".vp" ".inc"))))
			(def mans k (manifest-load (platform-manifest cpu abi))))))

(defun said-vp (said)
	;the bundles a farm said it made, noted. Of the caller's vp_made and
	;assembled
	(when (nempty? (defq made (said-of said "=key=")))
		(note-vp made)
		(setq vp_made :t assembled (+ assembled (length made)))))

(defun said-code (said)
	;the code a farm said it made, noted in the manifest of each platform it
	;was made for, by the translator there is now. Of the caller's mans,
	;dirty and did
	(defq plats (env 11))
	(each (lambda ((file cpu abi nh &rest names))
			(defq k (plat-key cpu abi))
			(unless (get k plats) (def plats k (list (sym cpu) (sym abi) (list))))
			(push (third (get k plats)) (cat (list file "-" nh) names))) (said-of said "=code="))
	(each (lambda ((k (cpu abi made)))
			(note-native made (plat-man cpu abi) (trans-id cpu) cpu abi)
			(def dirty k (list cpu abi))
			(defq d (ifn (get k did) (list cpu abi 0 0)))
			(def did k (list cpu abi (+ (third d) (length made))
				(reduce (# (+ %0 (- (length %1) 3))) made (last d)))))
		(tolist plats)))

(defun boot-id-kept (facts)
	; (boot-id-kept facts) -> hex
	;the id of the files of the facts, what (boot-id-of) gives of them, from
	;the hash kept of each: to read and hash them all again was 14 ms of an
	;image's 25. Kept with the facts
	(or (get 'f_id facts)
		(progn
			(defq scans (get 'f_scans facts) scanned :nil ctx (sha256-start))
			(each (lambda (file) (sha256-add ctx (cat file "\n" (hex-decode (file-hash file)) "\n")))
				(sort (cat (get 'f_all facts))))
			(def facts 'f_id (to-lower (hex-encode (sha256-end ctx)))))))

(defun boot-prods (facts)
	; (boot-prods facts) -> prods
	;the functions the files of the tree make, what the boot image of any
	;platform takes its own from. Of the source alone, so the same for
	;every platform, and kept with the facts it is read from
	(or (get 'f_prods facts)
		(progn
			(defq info (get 'f_info facts) prods (list))
			;the paths of what the files make, by the classes declared
			(within-compile-env (lambda ()
					(class-env (get 'f_classes facts))
					(each (# (merge prods (make-products (second (get (sym %0) info)))))
						(get 'f_all facts))))
			(def facts 'f_prods prods))))

(defun boot-units (targets force)
	; (boot-units targets force) -> units
	;the boot images to make, of the platforms: a unit for each whose image
	;is not what is there. An image is made of the platform's code of the
	;files of this make, the system's, and which functions it holds is said
	;by lib/boot/image.inc and this file. So it is made again if what is
	;noted of that code, each file's bundle and translator, or either of
	;those files, is not what the image was made from, if it has gone, or
	;with force. Not the whole of the platform's manifest: the apps are
	;noted there too, and no image holds an app, so making them leaves the
	;image as it is. Beside it are the id of the source it is of and
	;the names of those files, written again when the source is not what it
	;was: a comment makes no code, and a new id goes beside the image there
	;is. What each was made from is noted in +make_boot, here for one that
	;is not made again, when it says it is done for one that is. Of the
	;caller's facts, bootman, boot_dirty and booting
	(defq id (boot-id-kept facts) scans (get 'f_scans facts) scanned :nil units (list)
		tools (cat (file-hash "lib/boot/image.inc") (file-hash "lib/asm/asm.inc"))
		made_of (sort (cat (get 'f_vps facts))))
	(each (lambda ((cpu abi))
			(defq k (plat-key cpu abi) was (get k bootman) man (plat-man cpu abi)
				of (cat (hex-encode (sha256 (apply (const cat) (reduce (lambda (l f)
						(defq n (get (sym f) man))
						(push l f " " (if n (cat (str (first n)) " " (str (second n))) "-") "\n"))
					made_of (list ""))))) tools)
				image (and (not force) was (eql (second was) of)
					(> (age (cat "obj/" cpu "/" abi "/sys/boot_image")) 0)))
			(unless (and image (eql (first was) id))
				;what the image is built from, as one number, beside it
				(save id (boot-id-file cpu abi))
				(save (join (sort (cat (get 'f_all facts))) "\n") (boot-files-file cpu abi))
				(cond
					(image (def bootman k (list id of)) (setq boot_dirty :t))
					;not all the functions there are, lib/boot/image.inc says which
					(:t (push units (list :image (list cpu abi (image-funcs (boot-prods facts) cpu))))
						(def booting k (list id of))))))
		targets)
	units)

(defun build (files targets &optional boot force)
	; (build files targets [boot force])
	;the functions that have changed, of the files and all they include, for
	;each of the platforms, targets, each (cpu abi), by content and not by
	;age, and with boot their boot images.
	;A file's VP is made again if its key, the hash of all it is made of,
	;is not the one its bundle was made from, and each platform's code of
	;it with it, unless the bundle comes out the one that platform's code is
	;of. The code of the other files is made again, from their VP, for each
	;platform where it is not of the bundle there is now and that
	;platform's translator as it is now, or all of it with force. What was
	;made from what is noted in a manifest of the bundles and one of each
	;platform.
	;It is one farm: a job is a file, assembled once if it has to be, and
	;its code for every platform that wants it, so a herd is started once
	;and a file is read once. Two finer grains were tried and are not here,
	;docs/ai_digest/till_the_pips_squeak.md: a job for each platform of a
	;file was slower, and code made only of the functions of a file that
	;changed gained next to nothing, the code of a file is soon made. The boot images are jobs of the same herd,
	;given when the code is in and noted, each made if it is not of that.
	;With -v it says what it did: the source files read again and the
	;files assembled, then for each platform the files code was made of and
	;the functions
	(bind '(vps classes) (all-build-files))
	(setd files vps)
	(defq facts (tree-facts files classes) files (get 'f_vps facts)
		keys (get 'f_keys facts) vpman (get 'f_vpman facts)
		mans (env 11) dirty (env 11) did (env 11) errors (list) vp_made :nil assembled 0
		bootman (if boot (manifest-load +make_boot)) boot_dirty :nil booting (env 11)
		;the VP is the same for every platform, it is assembled as the first
		asm_cpu (first (first targets)) asm_abi (second (first targets)))
	;the translators first, this platform's, the one that runs this, of
	;each CPU: a build translates with what is made of lib/trans/<cpu>.vp
	;for it, so those that are not of their source as it is now are made
	;before the rest. If this platform's own then comes out other than it
	;was, by its id, the old one made them all, so all are made again by
	;the new one. Those made here are not made again for this platform by
	;the farm after; the others are made as any file is, a forced make
	;translates them from their VP, it does not make them twice. A platform
	;with none of its own, the VP64 as it installs, translates with those
	;of its boot image
	(bind '(& run_cpu run_abi) (map (const sym) (split (load-path) "/")))
	(defq run (list (list run_cpu run_abi :nil)) run_id (trans-id run_cpu)
		trans_files (filter (# (starts-with "lib/trans/" %0)) files)
		stale (filter (# (native-stale? %0 (plat-man run_cpu run_abi) run_id)) trans_files)
		built trans_files)
	(when (> (age (cat "obj/" run_cpu "/" run_abi "/" (trans-name run_cpu run_abi))) 0)
		(setq built stale)
		(when (nempty? stale)
			(defq said (farm-jobs (file-units stale run :t) run_abi run_cpu run))
			(said-vp said)
			(when (nql run_id (setq run_id (trans-id run_cpu)))
				(setq said (farm-jobs (file-units (setq built trans_files) run :t) run_abi run_cpu run))
				(said-vp said))
			(said-code said)))
	;where none were made, a platform with no translator of its own, the
	;translators' sources are files of the farm as any are. The id of a
	;translator is of the bundles of its source, so one that changes there
	;is known only when the farm has said: the code made in this make is
	;noted with it, and what was not made is of the id before, so the next
	;make makes it. An install has nothing noted and makes everything in
	;the one. To assemble them first, a farm of its own, was 0.23 s of an
	;install's 1.7 under the emulator
	;a unit for each file that is to be assembled, or whose code some
	;platform wants: with each platform, the hash of the bundle its code is
	;of, if it is of the translator there is now and this is not forced
	(defq ids (map (lambda ((cpu abi)) (trans-id cpu)) targets)
		tmans (map (lambda ((cpu abi)) (plat-man cpu abi)) targets)
		todo (list) wants (list))
	(each (lambda (file)
			(defq k (sym file) assemble (vp-stale? file) want (list))
			(each (lambda ((cpu abi) id man)
					(unless (and (eql cpu run_cpu) (eql abi run_abi) (find file built))
						(cond
							(assemble (push want (list cpu abi
								(and (not force) (defq n (get k man)) (eql (second n) id) (first n)))))
							((or force (native-stale? file man id)) (push want (list cpu abi :nil))))))
				targets ids tmans)
			(when (or assemble (nempty? want))
				(push todo file) (push wants (list :file (list file assemble want)))))
		files)
	(defq units (biggest-first wants (map (# (second (pii-fstat %0))) todo))
		;what the farm made is noted, and kept, once: as its last file is
		;answered if there are boot images to make of it, they go by what is
		;noted, or when it is over
		noted :nil
		note (lambda (said)
			(unless noted
				(setq noted :t)
				(said-vp said)
				(said-code said)
				(if vp_made (manifest-save vpman (vp-manifest)))
				(each (lambda ((k (cpu abi)))
						(manifest-save (get k mans) (platform-manifest cpu abi))) (tolist dirty))))
		said (farm-jobs units asm_abi asm_cpu targets
			(if boot (lambda (said) (note said) (boot-units targets force)))
			(if boot (length targets))))
	(note said)
	;the images that were made, noted
	(each (lambda ((cpu abi))
			(def bootman (defq k (plat-key cpu abi)) (get k booting))
			(setq boot_dirty :t)) (said-of said "=image="))
	(if boot_dirty (manifest-save bootman +make_boot))
	;what is built here is not what make install built at its waypoint any
	;more, service/source/app.inc (source-waypoint)
	(if (or (nempty? stale) (nempty? units)) (pii-remove "obj/installed"))
	(each (const print) errors)
	(when (> *build_verb* 0)
		(print "make: " (get 'f_read facts) " read, " assembled " assembled")
		(each (lambda ((_ (cpu abi files funcs)))
				(print "make " cpu " " abi ": " files " translated, " funcs " functions"))
			(tolist did)))
	(print "Done") :nil)

(defun make-bundles (&optional mode)
	; (make-bundles [mode])
	;the VP alone of every file, the apps' too, whose bundle in (vp-dir) of
	;the mode is not of its key, no platform's code of it: what trace
	;reads of a mode the platforms here are not built in
	(if mode (defq *build_mode* mode))
	(bind '(vps classes) (all-build-files))
	(defq facts (tree-facts (cat vps (all-app-files)) classes)
		keys (get 'f_keys facts) vpman (get 'f_vpman facts) errors (list)
		made (said-of (farm-jobs (map (# (list :bundle (list %0)))
			(filter (const vp-stale?) (get 'f_vps facts))) (abi) (cpu) '()) "=key="))
	(when (nempty? made) (note-vp made) (manifest-save vpman (vp-manifest)))
	(each (const print) errors)
	(print "Done") :nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this platform, and all of them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +supported_abi ''(AMD64 WIN64 ARM64 RISCV64 LA64)
	+supported_cpu ''(x86_64 x86_64 arm64 riscv64 la64))

(defun this-target (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(list (list *cpu* *abi*)))

(defun all-targets ()
	; (all-targets) -> targets
	;every platform. This one first, the VP is assembled as it, and the
	;VP64 last
	(bind '(& run_cpu run_abi) (map (const sym) (split (load-path) "/")))
	(defq own (list) others (list))
	(each (# (push (if (and (eql %0 run_abi) (eql %1 run_cpu)) own others) (list %1 %0)))
		+supported_abi +supported_cpu)
	(cat own others '((vp64 VP64))))

(defun make (&optional files *abi* *cpu*)
	; (make [files abi cpu])
	;what has changed, of this platform's code
	(build files (this-target *abi* *cpu*)))

(defun make-all (&optional files *abi* *cpu*)
	; (make-all [files abi cpu])
	;all of this platform's code of the files, whether or not it has
	;changed. The VP is made again only where it has, the rest of the code
	;is translated from the VP kept
	(build files (this-target *abi* *cpu*) :nil :t))

(defun remake (&optional files *abi* *cpu*)
	; (remake [files abi cpu])
	;what has changed, and the boot image
	(build files (this-target *abi* *cpu*) :t))

(defun remake-all (&optional files *abi* *cpu*)
	(build files (this-target *abi* *cpu*) :t :t))

(defun make-platforms (&optional mode)
	;every platform, in the mode given or the one there is
	(if mode (defq *build_mode* mode))
	(build :nil (all-targets)))

(defun make-all-platforms (&optional mode)
	(if mode (defq *build_mode* mode))
	(build :nil (all-targets) :nil :t))

(defun remake-platforms (&optional mode)
	(if mode (defq *build_mode* mode))
	(build :nil (all-targets) :t))

(defun remake-all-platforms (&optional mode)
	(if mode (defq *build_mode* mode))
	(build :nil (all-targets) :t :t))

(defun remake-all-vp (&optional mode)
	;only the VP64, in the mode given or the one there is
	(if mode (defq *build_mode* mode))
	(build :nil '((vp64 VP64)) :t :t))

(defun make-app-platforms (&optional mode force)
	;the apps alone, for every platform: those that have changed, or all of
	;them with force
	(if mode (defq *build_mode* mode))
	(build (all-app-files) (all-targets) :nil force))

;;;;;;;;;;;;;;;;;;;;;;;;
; compile and make tests
;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-time (&optional files *abi* *cpu*)
	; (make-time [files abi cpu]) -> us
	;how long a full build of the files takes, all of them compiled whether
	;or not they have changed: what make test and the Benchmark app time
	(setd files (all-vp-files) *abi* (abi) *cpu* (cpu))
	(defq start (pii-time))
	(bind '(made errors) (compile files *abi* *cpu*))
	(defq took (- (pii-time) start))
	(compiled made *abi* *cpu*)
	(each (const print) errors)
	(print "Done")
	took)

(defun make-test (&optional i *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu) i 10)
	(defq best +max_long worst 0 total 0 files (all-vp-files))
	(times i
		(defq _ (make-time files *abi* *cpu*))
		(setq total (+ total _) best (min best _) worst (max worst _)))
	(print "Mean time " (time-in-seconds (/ total i)) " seconds")
	(print "Best time " (time-in-seconds best) " seconds")
	(print "Worst time " (time-in-seconds worst) " seconds")
	:nil)

;module
(export-symbols
	'(all-vp-files all-class-files
		make make-all make-platforms make-all-platforms
		remake remake-all make-app-platforms
		remake-platforms remake-all-platforms remake-all-vp
		make-bundles make-time make-test))
(env-pop)

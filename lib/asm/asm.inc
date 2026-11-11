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

(defun farm-jobs (kind units *abi* *cpu*)
	; (farm-jobs kind units abi cpu) -> (made errors)
	;each unit a job for a herd of workers, lib/asm/asm.lisp, a worker for
	;each node of this machine: each has to set up to compile before its
	;first job, more of them than that only each pay for it. kind :vp is a
	;source file and the hash of the bundle this platform's code of it was
	;made from, its bundle of VP to (vp-dir) and this platform's code if
	;the bundle is another, :bundle the same with no code, :trans a list of
	;source files, their bundles to this platform's code. What a worker
	;says is printed. made is each file that was done, with the hash of its
	;bundle from a :vp or :bundle job, the errors are the rest
	(defq made (list) errors (list))
	(when (nempty? units)
		;a job is started again if its node goes. The timeout is only for a
		;worker that dies and its node does not, so it is long, a slow job
		;must not be taken for a dead one.
		(jobs-farm "lib/asm/asm.lisp"
			(map (# (cat (const (str-alloc +job_size))
						(str (list kind %0 *abi* *cpu* *build_mode* *build_emit* *build_verb*
								(vp-dir *build_mode*))))) units)
			(list (length units) 1 1) (task-timeout 60)
			(lambda (msg jobs)
				;the key it was given, then what it said. A job with an
				;error is not tried again.
				(defq error_flag :nil)
				(each (# (cond
							((starts-with "Error:" %0)
								(push errors %0)
								(setq error_flag :t))
							((starts-with "=key= " %0) (push made (rest (split %0 " "))))
							((nempty? %0) (print %0)))) (split (slice msg +job_reply_size -1) "\n"))
				(stream-flush (io-stream 'stdout))
				(if error_flag (. jobs :failed msg) (. jobs :answered msg)))
			;a job that has gone out and not come back, again and again,
			;is not waited on for ever
			(lambda (tries)
				(cond
					((< tries 3)
						(print "No answer from a worker, trying again")
						(stream-flush (io-stream 'stdout)))
					(:t (push errors "Error: no answer from the workers, the build is stopped"))))))
	(list made errors))

(defun vp-stage (files *abi* *cpu* &optional kind prevs)
	; (vp-stage files abi cpu [kind prevs]) -> (made errors)
	;the VP of the functions of the source files, the same for every
	;platform, a bundle of each file in (vp-dir) of the build mode, and this
	;platform's code of it at once, or with kind :bundle no code of any
	;platform. The bundle is what the other platforms translate, not a scan
	;of the source, a function in a (validatecase) is only made in some
	;modes. A file that goes wrong has none written. prevs is, for each
	;file, the hash of the bundle this platform's code was made from, if it
	;is known: a file whose bundle comes out the same has no code made
	;again. made is each file done and the hash of its bundle
	(setd kind :vp prevs (map (lambda (_) :nil) files))
	(farm-jobs kind (biggest-first (map (const list) files prevs) (map (# (second (pii-fstat %0))) files))
		*abi* *cpu*))

(defun translate-stage (files *abi* *cpu*)
	; (translate-stage files abi cpu) -> (made errors)
	;the bundles of the source files to this platform's code, a job a
	;file, the biggest first
	(defq dir (vp-dir *build_mode*))
	(farm-jobs :trans (map (const list)
			(biggest-first files (map (# (second (pii-fstat (cat dir %0)))) files))) *abi* *cpu*))

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
	;translated from, and the id of the translator
	(cat "obj/make_" cpu "_" abi))

(defun compiled (made *abi* *cpu*)
	;a compile makes what it is told to, it does not go by the manifests.
	;Of each file whose bundle came out other than the one noted, the notes
	;are forgotten, the bundle's and this platform's, and the next make
	;makes them. In a mode that is not the one noted too, where the VP of
	;the two is not the same
	(defq vpman (manifest-load (vp-manifest)) platman (manifest-load (platform-manifest *cpu* *abi*))
		vp_dirty :nil plat_dirty :nil)
	(each (lambda ((file hash))
			(defq k (sym file))
			(when (and (defq v (get k vpman)) (nql (second v) hash))
				(undef vpman k) (setq vp_dirty :t))
			(when (and (defq n (get k platman)) (nql (first n) hash))
				(undef platman k) (setq plat_dirty :t))) made)
	(if vp_dirty (manifest-save vpman (vp-manifest)))
	(if plat_dirty (manifest-save platman (platform-manifest *cpu* *abi*))))

(defun compile (files &optional *abi* *cpu*)
	; (compile files [abi cpu])
	;the files, their VP and then this platform's code, whether or not
	;they have changed
	(setd *abi* (abi) *cpu* (cpu))
	(unless (list?? files) (setq files (list files)))
	(bind '(made errors) (vp-stage files *abi* *cpu*))
	(compiled made *abi* *cpu*)
	(each (const print) errors)
	(print "Done") :nil)

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
	+trans_parser "lib/trans/vp_parse.vp")

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
					(each (lambda ((f scan))
							(push changed f)
							(def scans (sym f) (list (file-age f) (make-scan f scan) :nil)))
						(first (read (string-stream (slice msg +job_reply_size -1)))))
					(. jobs :answered msg))))))

(defun tree-walk (files)
	; (tree-walk files) -> files
	;the files and all they include and import, merged into them, what
	;each includes and makes in info, and the .vp files of them given back.
	;Of the caller's ages, info, scans, scanned and changed. No class is
	;declared for it: what a file makes is only asked for a boot image,
	;(boot-plan), and the classes were 4 ms of a make's 15
	(scans-farm files)
	(each-mergeable (#
			(unless (get (sym %0) info)
				(bind '(d p) (make-info %0))
				(merge files d)
				(def info (sym %0) (list d p)))) files)
	(filter (# (ends-with ".vp" %0)) files))

(defun tree-key (file)
	; (tree-key file) -> hex
	;the key of a file, what its VP is made from: the hash of the names and
	;hashes of it and of all it includes and imports, and all they do, in
	;the order of the names. The assembler is among them, every .vp file
	;includes it. The includes have loops, so by a walk, not once a file
	(defq seen (env 101) todo (list file) names (list))
	(while (defq f (pop todo))
		(unless (get (defq k (sym f)) seen)
			(def seen k :t)
			(push names f)
			(each (# (push todo %0)) (first (get k info)))))
	(hex-encode (sha256 (apply (const cat)
				(reduce (# (push %0 %1 " " (file-hash %1) "\n")) (sort names) (list ""))))))

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

(defun tree-facts (files classes kept)
	; (tree-facts files classes kept) -> facts
	;what a make goes by of the source, whatever platform it makes, an env:
	;f_vps the .vp files of the files given, f_all all of them with what
	;they include and import, f_info what each includes and makes, f_keys
	;the key of each .vp file, f_vpman the manifest of the bundles of the
	;build mode, f_classes. kept is a list
	;the platforms of one make share, empty till the first asks, or :nil:
	;working it out was 9 ms of each platform's 25 on the M4. The bundles of
	;files that have gone are taken away with it
	(cond
		((and kept (nempty? kept)) (first kept))
		(:t (bundles-gone (vp-dir *build_mode*))
			(bind '(scans keys) (scans-load))
			(defq ages (env 31) info (env 101) scanned :nil changed (list)
				all (copy files) vps (tree-walk all))
			(keys-fresh vps all)
			(if scanned (scans-save scans keys))
			(defq facts (env 11))
			(def facts 'f_vps vps 'f_all all 'f_info info 'f_keys keys 'f_classes classes
				'f_vpman (manifest-load (vp-manifest)))
			(if kept (push kept facts))
			facts)))

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
	(each (lambda ((file hash)) (def vpman (defq k (sym file)) (list (get k keys) hash))) made))

(defun note-native (made man id)
	;the code a stage made, into a platform's manifest: each file to the
	;hash of its bundle and the translator's id
	(each (lambda ((file &optional hash))
			(def man (defq k (sym file)) (list (second (get k vpman)) id))) made))

(defun make (&optional files *abi* *cpu* walk kept force)
	; (make [files abi cpu walk kept force])
	;the functions that have changed, of the files and all they include, in
	;two stages, by content and not by age. A file's VP is made again if
	;its key, the hash of all it is made of, is not the one its bundle was
	;made from, and this platform's code of it with it, unless the bundle
	;comes out as it was. The code of the other files is made again, from
	;their VP, if it is not of the bundle there is now and this platform's
	;translator as it is now, or of all of them with force. What was made
	;from what is noted in a manifest of the bundles and one of each
	;platform. walk is what (all-build-files) gave, if it has been asked
	;already, kept what (tree-facts) keeps
	(bind '(vps classes) (ifn walk (all-build-files)))
	(setd files vps *abi* (abi) *cpu* (cpu))
	(defq facts (tree-facts files classes kept) files (get 'f_vps facts)
		keys (get 'f_keys facts) vpman (get 'f_vpman facts)
		platman (manifest-load (platform-manifest *cpu* *abi*))
		vp_made :nil plat_made :nil)
	;a platform with no manifest was last made by age: beside its code is
	;an empty file of each source file's name, the age of which was when
	;its code was made. They go
	(if (= (age (platform-manifest *cpu* *abi*)) 0)
		(each (const pii-remove) (files-all (cat "obj/" *cpu* "/" *abi*) '(".vp" ".inc"))))
	;the translators first, this platform's, the one that runs this, of
	;each CPU: a build translates with what is made of lib/trans/<cpu>.vp
	;for it, so those that are not of their source as it is now are made
	;before the rest. If this platform's own then comes out other than it
	;was, by its id, the old one made them all, so all are made again by
	;the new one. Those made here are not made by the stages after; the
	;others are made as any file is, a forced make translates them from
	;their VP, it does not make them twice. A platform with none of its
	;own, the VP64 as it installs, translates with those of its boot image
	(bind '(& run_cpu run_abi) (map (const sym) (split (load-path) "/")))
	(defq own (and (eql *cpu* run_cpu) (eql *abi* run_abi))
		runman (if own platman (manifest-load (platform-manifest run_cpu run_abi)))
		run_id (trans-id run_cpu) run_made :nil
		trans_files (filter (# (starts-with "lib/trans/" %0)) files) errors (list)
		stale (filter (# (native-stale? %0 runman run_id)) trans_files)
		built trans_files)
	(cond
		((> (age (cat "obj/" run_cpu "/" run_abi "/" (trans-name run_cpu run_abi))) 0)
			(setq built stale)
			(when (nempty? stale)
				(bind '(made more) (vp-stage stale run_abi run_cpu))
				(note-vp made)
				(setq errors (cat errors more) vp_made :t run_made :t)
				(when (nql run_id (setq run_id (trans-id run_cpu)))
					(bind '(made more) (vp-stage (setq built trans_files) run_abi run_cpu))
					(note-vp made)
					(setq errors (cat errors more)))
				(note-native made runman run_id)))
		(:t ;none made above. This platform's translator is known by the
			;bundles of its source, so if they are not of their key their VP
			;is made before it is asked, the VP alone: the code of them is
			;translated with the rest
			(defq trans_vp (filter (# (and (vp-stale? %0)
					(or (eql %0 +trans_parser) (eql %0 (cat "lib/trans/" *cpu* ".vp"))))) files))
			(when (nempty? trans_vp)
				(bind '(made more) (vp-stage trans_vp *abi* *cpu* :bundle))
				(note-vp made)
				(setq errors (cat errors more) vp_made :t))))
	(if own (setq files (filter (# (not (find %0 built))) files)))
	(defq id (trans-id *cpu*))
	;the VP stage, the files whose bundle is not of their key: their VP,
	;and this platform's code at once if the bundle is not the one it has
	(defq vp_files (filter (const vp-stale?) files))
	(bind '(made more) (vp-stage vp_files *abi* *cpu* :vp
			(map (# (and (defq n (get (sym %0) platman)) (eql (second n) id) (first n))) vp_files)))
	(when (nempty? made) (note-vp made) (note-native made platman id) (setq vp_made :t plat_made :t))
	(setq errors (cat errors more))
	;the translate stage, the other files whose code is not of their
	;bundle and this platform's translator: their VP, from another
	;platform's VP stage or one before, to this platform's code
	(defq to_translate (filter (lambda (file)
				(and (not (find file vp_files)) (or force (native-stale? file platman id)))) files))
	(bind '(made more) (translate-stage to_translate *abi* *cpu*))
	(when (nempty? made) (note-native made platman id) (setq plat_made :t))
	;what was made, noted for the next make
	(if vp_made (manifest-save vpman (vp-manifest)))
	(if (or plat_made (and own run_made)) (manifest-save platman (platform-manifest *cpu* *abi*)))
	(and run_made (not own) (manifest-save runman (platform-manifest run_cpu run_abi)))
	;what is built here is not what make install built at its waypoint any
	;more, service/source/app.inc (source-waypoint)
	(if (or (nempty? stale) (nempty? vp_files) (nempty? to_translate)) (pii-remove "obj/installed"))
	(each (const print) (cat errors more))
	(print "Done") :nil)

(defun make-bundles (&optional mode)
	; (make-bundles [mode])
	;the VP alone of every file, the apps' too, whose bundle in (vp-dir) of
	;the mode is not of its key, no platform's code of it: what trace
	;reads of a mode the platforms here are not built in
	(if mode (defq *build_mode* mode))
	(bind '(vps classes) (all-build-files))
	(defq facts (tree-facts (cat vps (all-app-files)) classes :nil)
		keys (get 'f_keys facts) vpman (get 'f_vpman facts))
	(bind '(made errors) (vp-stage (filter (const vp-stale?) (get 'f_vps facts)) (abi) (cpu) :bundle))
	(when (nempty? made) (note-vp made) (manifest-save vpman (vp-manifest)))
	(each (const print) errors)
	(print "Done") :nil)

(defun boot-plan (walk kept)
	; (boot-plan walk kept) -> (prods id names)
	;what the boot image of any platform is made from: the functions the
	;files of the tree make, the id of those files and their names. Of the
	;source alone, so the same for every platform, and kept with the facts
	;it is read from: the hash of the files was 14 ms of an image's 25
	(bind '(vps classes) (ifn walk (all-build-files)))
	(defq facts (tree-facts vps classes kept))
	(cond
		((get 'f_plan facts))
		(:t (defq all (get 'f_all facts) info (get 'f_info facts) prods (list))
			;the paths of what the files make, by the classes declared
			(within-compile-env (lambda ()
					(class-env classes)
					(each (# (merge prods (make-products (second (get (sym %0) info))))) all)))
			(defq plan (list prods (boot-id-of all) (join (sort (cat all)) "\n")))
			(def facts 'f_plan plan)
			plan)))

(defun make-boot-all (&optional *abi* *cpu* walk kept)
	; (make-boot-all [abi cpu walk kept])
	(setd *abi* (abi) *cpu* (cpu))
	(bind '(prods id names) (boot-plan walk kept))
	;what the image is built from, as one number, beside it
	(save id (boot-id-file *cpu* *abi*))
	(save names (boot-files-file *cpu* *abi*))
	;not all of them, lib/boot/image.inc says which
	(boot-image (image-funcs prods *cpu*) *abi* *cpu*))

(defun make-all (&optional files *abi* *cpu* walk kept)
	; (make-all [files abi cpu walk kept])
	;all of this platform's code of the files, whether or not it has
	;changed. The VP is made again only where it has, the rest of the code
	;is translated from the VP kept
	(make files *abi* *cpu* walk kept :t))

(defun remake (&optional files *abi* *cpu* walk kept)
	;the facts of the tree are the make's and the boot image's both
	(setd *abi* (abi) *cpu* (cpu) walk (all-build-files) kept (list))
	(make files *abi* *cpu* walk kept)
	(make-boot-all *abi* *cpu* walk kept))

(defun remake-all (&optional files *abi* *cpu* walk kept)
	(setd *abi* (abi) *cpu* (cpu) walk (all-build-files) kept (list))
	(make-all files *abi* *cpu* walk kept)
	(make-boot-all *abi* *cpu* walk kept))

;;;;;;;;;;;;;;;;;;;;;
; cross platform make
;;;;;;;;;;;;;;;;;;;;;

(defq +supported_abi ''(AMD64 WIN64 ARM64 RISCV64 LA64)
	+supported_cpu ''(x86_64 x86_64 arm64 riscv64 la64))

(defun make-them (action &optional mode files)
	;every platform, in the mode given or the one there is. The tree is
	;walked once for all of them, and its facts worked out once, kept
	(defq walk (all-build-files) kept (list))
	(setd files (first walk))
	(if mode (defq *build_mode* mode))
	;this platform's first, its translators of each CPU are made with it
	(bind '(& run_cpu run_abi) (map (const sym) (split (load-path) "/")))
	(each (# (and (eql %0 run_abi) (eql %1 run_cpu) (action files %0 %1 walk kept)))
		+supported_abi +supported_cpu)
	(each (# (unless (and (eql %0 run_abi) (eql %1 run_cpu)) (action files %0 %1 walk kept)))
		+supported_abi +supported_cpu)
	(action files 'VP64 'vp64 walk kept))

(defun make-vp (action &optional mode files)
	;only the VP64, in the mode given or the one there is
	(defq walk (all-build-files))
	(setd files (copy (first walk)))
	(if mode (defq *build_mode* mode))
	(action files 'VP64 'vp64 walk))

(defun make-platforms (&optional mode) (make-them (const make) mode))
(defun make-all-platforms (&optional mode) (make-them (const make-all) mode))
(defun remake-platforms (&optional mode) (make-them (const remake) mode))
(defun remake-all-platforms (&optional mode) (make-them (const remake-all) mode))
(defun remake-all-vp (&optional mode) (make-vp (const remake-all) mode))
(defun make-app-platforms (&optional mode) (make-them (const make-all) mode (all-app-files)))

;;;;;;;;;;;;;;;;;;;;;;;;
; compile and make tests
;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-time (&optional files *abi* *cpu*)
	; (make-time [files abi cpu]) -> us
	;how long a full build of the files takes, all of them compiled whether
	;or not they have changed: what make test and the Benchmark app time
	(setd files (all-vp-files) *abi* (abi) *cpu* (cpu))
	(defq start (pii-time))
	(bind '(made errors) (vp-stage files *abi* *cpu*))
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

;;;;;;;;;;;;;;
; VP Assembler
;;;;;;;;;;;;;;

(import "././task/jobs.inc")
(import "././boot/image.inc")
(import "././boot/id.inc")
(import "././files/files.inc")

;module
(env-push)

(enums +select 0
	(enum reply launch timer))

(defq +vp_modes ''("release" "debug" "validate"))

(defun vp-dir (mode)
	; (vp-dir mode) -> path
	;where a build mode's VP is kept, obj/vp/<mode>/, a bundle for each
	;source file of the VP of its functions, under the file's own name
	(cat "obj/vp/" (elem-get +vp_modes mode) "/"))

(defun biggest-first (units sizes)
	;the units in the order the queue gives them out, the biggest first,
	;the queue goes from the end. With the small ones last the workers all
	;end at much the same time
	(map (const second) (sort (map (const list) sizes units) (# (- (first %0) (first %1))))))

(defun farm-jobs (kind units *abi* *cpu*)
	; (farm-jobs kind units abi cpu) -> (made errors), made is always ()
	;each unit a job for a herd of workers, lib/asm/asm.lisp, a worker for
	;each node of this machine: each has to set up to compile before its
	;first job, more of them than that only each pay for it. kind :vp is a
	;list of source files, their bundles of VP to (vp-dir) and this
	;platform's code, :trans a list of source files, their bundles to this
	;platform's code. The worker stamps each file for the platform as it
	;is done. What a worker says is printed, the errors are the result
	(defq made (list) errors (list))
	(when (nempty? units)
		;a job is started again if its node goes. The timeout is only for a
		;worker that dies and its node does not, so it is long, a slow job
		;must not be taken for a dead one.
		(defq timer_rate (/ 1000000 1) working :t retry_timeout (task-timeout 60) told 0
			select (list (mail-mbox) (mail-mbox) (mail-mbox))
			jobs (Jobs "lib/asm/asm.lisp" (elem-get select +select_launch)
				(elem-get select +select_reply) (list (length units) 1 1)))
		(. jobs :add (map (# (cat (const (str-alloc +job_size))
			(str (list kind %0 *abi* *cpu* *build_mode* *build_emit* *build_inst* *build_verb*
				(vp-dir *build_mode*))))) units))
		(mail-timeout (elem-get select +select_timer) timer_rate 0)
		(while working
			(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
			(cond
				((not msg)
					;task aborted
					(setq working :nil))
				((= idx +select_launch)
					;child launch response
					(. jobs :launched msg))
				((= idx +select_reply)
					;child worker response, the key it was given, then
					;what it said. A job with an error is not tried again.
					(defq error_flag :nil)
					(each (# (cond
						((starts-with "Error:" %0)
							(push errors %0)
							(setq error_flag :t))
						((nempty? %0) (print %0)))) (split (slice msg +job_reply_size -1) "\n"))
					(stream-flush (io-stream 'stdout))
					;all jobs done ?
					(if (eql 0 (if error_flag (. jobs :failed msg) (. jobs :answered msg)))
						(setq working :nil)))
				(:t ;timer event
					(mail-timeout (elem-get select +select_timer) timer_rate 0)
					(. jobs :refresh retry_timeout)
					;a job that has gone out and not come back, again and
					;again, is not waited on for ever
					(when (> (defq tries (. jobs :tries)) told)
						(setq told tries)
						(cond
							((< tries 3)
								(print "No answer from a worker, trying again")
								(stream-flush (io-stream 'stdout)))
							(:t (push errors "Error: no answer from the workers, the build is stopped")
								(setq working :nil)))))))
		(. jobs :close))
	(list made errors))

(defun vp-stage (files *abi* *cpu*)
	; (vp-stage files abi cpu) -> (made errors)
	;the VP of the functions of the source files, the same for every
	;platform, a bundle of each file in (vp-dir) of the build mode, and this
	;platform's code of it at once. The bundle is what the other platforms
	;translate, not a scan of the source, a function in an (errorcase) is
	;only made in some modes. A file that goes wrong has none written
	(farm-jobs :vp (map (const list) (biggest-first files (map (# (second (pii-fstat %0))) files)))
		*abi* *cpu*))

(defun translate-stage (files *abi* *cpu*)
	; (translate-stage files abi cpu) -> (made errors)
	;the bundles of the source files to this platform's code, a job a
	;file, the biggest first
	(defq dir (vp-dir *build_mode*))
	(farm-jobs :trans (map (const list)
		(biggest-first files (map (# (second (pii-fstat (cat dir %0)))) files))) *abi* *cpu*))

(defun compile (files &optional *abi* *cpu*)
	; (compile files [abi cpu])
	;the files, their VP and then this platform's code, whether or not
	;they have changed
	(setd *abi* (abi) *cpu* (cpu))
	(unless (list?? files) (setq files (list files)))
	(bind '(& errors) (vp-stage files *abi* *cpu*))
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

(defun make-scan (%0)
	;what make goes by of a file, read from it: its includes, as paths,
	;and the lines that name what it makes, kind and names, made paths of
	;by (make-products) with the classes declared
	(defq d (list "lib/asm/asm.inc" "class/lisp/root.inc" "class/lisp/task.inc" %0) p (list))
	(lines! (lambda (%0)
		(and (> (length %0) 10) (eql "(" (first %0))
				(<= 2 (length (defq s (split %0 (const (char-class " '()\q\r"))))) 4)
			(case (first s)
				(("include" "import")
					(push d (path-to-absolute (second s) file)))
				(("def-method" "gen-host-call" "gen-vtable" "gen-type" "gen-create")
					(push p (slice s 0 (min 3 (length s)))))
				("def-func"
					(push p (list "def-func" (if (eql (second s) "path-to-absolute")
						(path-to-absolute (third s) file) (second s)))))))
		:nil)
		(if (= (age %0) 0)
			(throw "No such file !" %0)
			(file-stream (defq file %0))))
	(list d p))

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
;asm.inc not as this one is, it says how a file is read
(defq +make_scans "obj/make_scans")

(defun scans-load ()
	;the scans kept, an env of file to (age scan)
	(defq scans (env 101))
	(catch (progn
		(when (and (defq data (load +make_scans)) (nempty? data))
			(bind '(asm_age entries) (first (read (string-stream data))))
			(when (= asm_age (age "lib/asm/asm.inc"))
				(each (lambda ((f a d p)) (def scans (sym f) (list a (list d p)))) entries)))
		:t) :t)
	scans)

(defun scans-save (scans)
	;those of before this second kept, for the next make
	(defq now (/ (pii-time) 1000000) entries (list))
	(each (lambda ((k v))
		(if (< (first v) now) (push entries (list (str k) (first v) (first (second v)) (second (second v))))))
		(tolist scans))
	(save (str (list (age "lib/asm/asm.inc") entries)) +make_scans))

(defun file-age (%0)
	;modification time of a file, cached
	(or (get (setq %0 (sym %0)) ages) (def ages %0 (age %0))))

(defun make-info (%0)
	;create lists of immediate dependencies and products, of the scan
	;kept if the file has not changed
	(defq k (sym %0) a (file-age %0) c (get k scans))
	(unless (and c (= (first c) a))
		(setq scanned :t)
		(def scans k (setq c (list a (make-scan %0)))))
	(bind '(d p) (second c))
	(list (copy d) (make-products p)))

(defun newest-ages (files)
	;the newest modification time of each file and all it includes and
	;imports, and all they do. Each starts as its own, and takes the
	;newest of what it includes, pass after pass, till a pass changes
	;none: the includes have loops, so not once a file. Asking the whole
	;list of each, merged, was 1.9 million compares for the system
	(each (# (def newest (sym %0) (file-age %0))) files)
	(defq changed :t)
	(while changed
		(setq changed :nil)
		(each (lambda (f)
			(defq k (sym f) v (get k newest)
				m (reduce (lambda (a d) (max a (get (sym d) newest))) (first (get k info)) v))
			(when (> m v) (def newest k m) (setq changed :t))) files)))

(defun make (&optional files *abi* *cpu* walk force)
	; (make [files abi cpu walk force])
	;the functions that have changed, of the files and all they include, in
	;two stages. A file's VP is made again if anything it is made of is
	;newer than the VP of any of its functions, and this platform's code of
	;it with it. The code of the other files is made again, from their VP,
	;if anything they are made of, or this platform's translator, is newer
	;than it, or of all of them with force. walk is what (all-build-files)
	;gave, if it has been asked already
	(bind '(vps classes) (ifn walk (all-build-files)))
	(setd files (copy vps) *abi* (abi) *cpu* (cpu))
	(defq ages (env 31) info (env 31) newest (env 31) scans (scans-load) scanned :nil
		dir (vp-dir *build_mode*))
	;list of all file imports while defining dependencies and functions
	(within-compile-env (lambda ()
		(class-env classes)
		(each-mergeable (#
			(bind '(d p) (make-info %0))
			(merge files d)
			(def info (sym %0) (list d p))) files)))
	(if scanned (scans-save scans))
	(newest-ages files)
	(setq files (filter (# (ends-with ".vp" %0)) files))
	;the translators first, this platform's, the one that runs this, of
	;each CPU: a build translates with what is made of lib/trans/<cpu>.vp
	;for it, so those with anything they are made of newer are made before
	;the rest. If this platform's own is one, it made them all, so then
	;all are made again by the new one. They are not made by the stages
	;after, as their own make would be newer than them. A platform with
	;none of its own, the VP64 as it installs, translates with those of
	;its boot image
	(bind '(& run_cpu run_abi) (map (const sym) (split (load-path) "/")))
	(defq trans_files (filter (# (starts-with "lib/trans/" %0)) files) errors (list)
		run_src (cat "lib/trans/" run_cpu ".vp")
		stale (if (and force (eql *cpu* run_cpu) (eql *abi* run_abi)) trans_files
			(filter (# (>= (get (sym %0) newest) (age (cat "obj/" run_cpu "/" run_abi "/" %0)))) trans_files)))
	(when (and (nempty? stale) (> (age (cat "obj/" run_cpu "/" run_abi "/" (trans-name run_cpu run_abi))) 0))
		(bind '(& more) (vp-stage stale run_abi run_cpu))
		(setq errors (cat errors more))
		(when (find run_src stale)
			(bind '(& more) (vp-stage trans_files run_abi run_cpu))
			(setq errors (cat errors more))))
	(and (eql *cpu* run_cpu) (eql *abi* run_abi)
		(setq files (filter (# (not (find %0 trans_files))) files)))
	;the VP stage, the files with anything newer than their bundle of VP:
	;their VP, and this platform's code at once
	(defq vp_files (filter (# (>= (get (sym %0) newest) (age (cat dir %0)))) files))
	(bind '(& more) (vp-stage vp_files *abi* *cpu*))
	(setq errors (cat errors more))
	;the translate stage, the other files whose VP, sources or this
	;platform's translator are newer than this platform's stamp of them:
	;their VP, from another platform's VP stage or one before, to this
	;platform's code. Newer, not as new, for the VP: the two stages of one
	;build may write both in the same second, and the age is of a second
	(defq trans_age (reduce (# (max %0 (age %1)))
		(list "lib/trans/vp.inc" "lib/trans/vpb.inc" "lib/trans/vpb_ops.inc" "lib/trans/vpb_trans.inc"
			(cat "lib/trans/" *cpu* ".vp")) 0))
	(bind '(& more) (translate-stage (filter (lambda (file)
		(and (not (find file vp_files))
			(or force
				(>= (get (sym file) newest) (defq done (age (cat "obj/" *cpu* "/" *abi* "/" file))))
				(> trans_age done) (> (age (cat dir file)) done)))) files) *abi* *cpu*))
	(each (const print) (cat errors more))
	(print "Done") :nil)

(defun make-boot-all (&optional *abi* *cpu* walk)
	(setd *abi* (abi) *cpu* (cpu))
	(boot-image
		(within-compile-env (lambda ()
			;its own list of the files, they are merged into
			(bind '(l classes) (ifn walk (all-build-files)))
			(setq l (copy l))
			(class-env classes)
			(defq *prods* (list) ages (env 31) scans (scans-load) scanned :nil)
			;lists of all file imports and products
			(each-mergeable (# (merge *prods* (bind '(d %0) (make-info %0))) (merge l d)) l)
			(if scanned (scans-save scans))
			;what the image is built from, as one number, beside it
			(save (boot-id-of l) (boot-id-file *cpu* *abi*))
			(save (join (sort (cat l)) (ascii-char 10)) (boot-files-file *cpu* *abi*))
			;not all of them, lib/boot/image.inc says which
			(image-funcs *prods* *cpu*))) *abi* *cpu*))

(defun make-all (&optional files *abi* *cpu* walk)
	; (make-all [files abi cpu walk])
	;all of this platform's code of the files, whether or not it has
	;changed. The VP is made again only where it has, the rest of the code
	;is translated from the VP kept
	(make files *abi* *cpu* walk :t))

(defun remake (&optional files *abi* *cpu* walk)
	(setd *abi* (abi) *cpu* (cpu) walk (all-build-files))
	(make files *abi* *cpu* walk)
	(make-boot-all *abi* *cpu* walk))

(defun remake-all (&optional files *abi* *cpu* walk)
	(setd *abi* (abi) *cpu* (cpu))
	(make-all files *abi* *cpu*)
	(make-boot-all *abi* *cpu* walk))

;;;;;;;;;;;;;;;;;;;;;
; cross platform make
;;;;;;;;;;;;;;;;;;;;;

(defq +supported_abi ''(AMD64 WIN64 ARM64 RISCV64 LA64)
	+supported_cpu ''(x86_64 x86_64 arm64 riscv64 la64))

(defun make-them (action &optional mode files)
	;every platform. With a mode all of them are built in it. With none
	;the native ones are debug and the VP64 is release. The tree is walked
	;once for all of them
	(defq walk (all-build-files))
	(setd files (copy (first walk)))
	(if mode (defq *build_mode* mode))
	;this platform's first, its translators of each CPU are made with it
	(bind '(& run_cpu run_abi) (map (const sym) (split (load-path) "/")))
	(each (# (and (eql %0 run_abi) (eql %1 run_cpu) (action files %0 %1 walk)))
		+supported_abi +supported_cpu)
	(each (# (unless (and (eql %0 run_abi) (eql %1 run_cpu)) (action files %0 %1 walk)))
		+supported_abi +supported_cpu)
	(ifn mode (defq *build_mode* 0))
	(action files 'VP64 'vp64 walk))

(defun make-vp (action &optional mode files)
	;only the VP64, in a mode, release if none is given, as it is with
	;none wherever it is built. The lint, which is only right on a debug
	;build, asks for debug
	(defq walk (all-build-files))
	(setd files (copy (first walk)))
	(defq *build_mode* (ifn mode 0))
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

(defun make-test (&optional i *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu) i 10)
	(defq best +max_long worst 0 total 0 files (all-vp-files))
	(times i
		(defq _ (pii-time))
		(compile files *abi* *cpu*)
		(setq _ (- (pii-time) _) total (+ total _)
			best (min best _) worst (max worst _)))
	(print "Mean time " (time-in-seconds (/ total i)) " seconds")
	(print "Best time " (time-in-seconds best) " seconds")
	(print "Worst time " (time-in-seconds worst) " seconds")
	:nil)

(defun compile-test (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(each (# (compile %0 *abi* *cpu*)) (defq files (all-vp-files)))
	(compile files *abi* *cpu*))

;module
(export-symbols
	'(all-vp-files all-class-files
	make make-all make-platforms make-all-platforms
	remake remake-all make-app-platforms
	remake-platforms remake-all-platforms remake-all-vp
	compile-test make-test))
(env-pop)

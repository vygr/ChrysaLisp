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

(defun compile (files &optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(unless (list?? files) (setq files (list files)))
	(when (nempty? files)
		;the biggest first, the queue goes from the end. With the small
		;ones last the workers all end at much the same time, shuffled
		;one of them could be left with a big one when the others are done
		(setq files (map (const second) (sort (map (# (list (second (pii-fstat %0)) %0)) files)
			(# (- (first %0) (first %1))))))
		;a job is started again if its node goes. The timeout is only for a
		;worker that dies and its node does not, so it is long, a slow job
		;must not be taken for a dead one.
		(defq timer_rate (/ 1000000 1) working :t errors (list) retry_timeout (task-timeout 60)
			told 0
			select (list (mail-mbox) (mail-mbox) (mail-mbox))
			max_workers (length files)
			;a worker for each node of this machine. Each has to set up to
			;compile before its first file, more of them than that only
			;each pay for it, and it was timed
			jobs (Jobs "lib/asm/asm.lisp" (elem-get select +select_launch)
				(elem-get select +select_reply) (list max_workers 1 1)))
		(. jobs :add (map (# (cat (const (str-alloc +job_size))
			(str (list (list %0) *abi* *cpu* *build_mode* *build_emit* *build_inst* *build_verb*)))) files))
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
						(:t (print %0)))) (split (slice msg +job_reply_size -1) "\n"))
					(stream-flush (io-stream 'stdout))
					;all jobs done ?
					(if (eql 0 (if error_flag (. jobs :failed msg) (. jobs :answered msg)))
						(setq working :nil)))
				(:t ;timer event
					(mail-timeout (elem-get select +select_timer) timer_rate 0)
					(. jobs :refresh retry_timeout)
					;a file that has gone out and not come back, again
					;and again, is not waited on for ever
					(when (> (defq tries (. jobs :tries)) told)
						(setq told tries)
						(cond
							((< tries 3)
								(print "No answer from a worker, trying again")
								(stream-flush (io-stream 'stdout)))
							(:t (push errors "Error: no answer from the workers, the build is stopped")
								(setq working :nil)))))))
		(. jobs :close)
		(each (const print) errors))
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
				(("def-method" "gen-vtable" "gen-type" "gen-create")
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
			("def-method" (f-path (sym a) (sym b)))
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

(defun make (&optional files *abi* *cpu* walk)
	;walk is what (all-build-files) gave, if it has been asked already
	(bind '(vps classes) (ifn walk (all-build-files)))
	(setd files (copy vps) *abi* (abi) *cpu* (cpu))
	(compile ((lambda ()
		(defq ages (env 31) info (env 31) newest (env 31) scans (scans-load) scanned :nil)
		;list of all file imports while defining dependencies and products
		(within-compile-env (lambda ()
			(class-env classes)
			(each-mergeable (#
				(bind '(d p) (make-info %0))
				(merge files d)
				(def info (sym %0) (list d (map (const func-obj-path) p)))) files)))
		(if scanned (scans-save scans))
		(newest-ages files)
		;filter to only the .vp files
		(setq files (filter (# (ends-with ".vp" %0)) files))
		;filter to only the files who's oldest product is older than any dependency
		(setq files (filter (#
			(>= (get (sym %0) newest) (reduce min (map (const file-age) (second (get (sym %0) info))) +max_long))) files))
		;return the list to compile
		files)) *abi* *cpu*))

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
			(boot-funcs *prods*))) *abi* *cpu*))

(defun make-all (&optional files *abi* *cpu* walk)
	(setd *abi* (abi) *cpu* (cpu))
	(compile (ifn files (all-vp-files)) *abi* *cpu*))

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
	(each (# (action files %0 %1 walk)) +supported_abi +supported_cpu)
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

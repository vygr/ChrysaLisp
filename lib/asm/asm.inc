;;;;;;;;;;;;;;
; VP Assembler
;;;;;;;;;;;;;;

;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")

;C++ ChrysaLisp ?
(unless (get 'nums)
	(defun mail-devices () '(0)))

(defun compile (files &optional *abi* *cpu* num_child)
	(setd *abi* (abi) *cpu* (cpu) num_child (max 1 (/ (* (length (mail-devices)) 80) 100)))
	(unless (list? files) (setq files (list files)))
	(setq files (shuffle files))
	(if (get 'nums)
		;Native ChrysaLisp
		(progn
			(defq data_in (list) select (list) num_child (min num_child (length files))
				child_mboxs (open-farm "class/lisp/compile.lisp" num_child kn_call_child)
				buckets (map (lambda (_) (list)) child_mboxs) err (list))
			(each (# (push (elem (% _ num_child) buckets) %0)) files)
			(each (# (push select (defq inbox (in-mbox (elem -2 (push data_in (in-stream))))))
					(mail-send (str (list (pop buckets) (get-long inbox 0) *abi* *cpu* *debug_mode* *debug_emit* *debug_inst*)) %0))
				child_mboxs)
			(while (/= (length select) 0)
				;read all child data
				(each-line (# (if (starts-with "Error:" %0) (push err %0) (print %0)))
					(elem (defq idx (mail-select select)) data_in))
				;child closed
				(setq data_in (erase data_in idx (inc idx)) select (erase select idx (inc idx))))
			(each print err))
		;C++ ChrysaLisp !
		(within-compile-env (# (each include files))))
	(print "Done") nil)

;;;;;;;;;;;;;
; make system
;;;;;;;;;;;;;

(defun make-merge (l sl)
	;merge string into string list
	(each (lambda (s)
		(unless (find s l) (push l s))) sl))

(defun all-files (root ext)
	;all files from root downwards with extention, none recursive
	;don't include "." folders
	(defq stack (list root) files (list))
	(while (setq root (pop stack))
		(each! 0 -1 (lambda (file type)
			(unless (starts-with "." file)
				(if (eql type "4")
					(push stack (cat root "/" file))
					(if (ends-with ext file)
						(push files (cat root "/" file))))))
			(unzip (split (pii-dirlist root) ",") (list (list) (list)))))
	files)

(defun all-vp-files ()
	;filter to only the .vp files, strip off the leading "./"
	(map (# (slice 2 -1 %0))
		(filter (# (not (starts-with "./apps/" %0))) (all-files "." ".vp"))))

(defun all-class-files ()
	;filter to only the class.inc files, stri off the leading "./"
	(map (# (slice 2 -1 %0)) (all-files "." "class.inc")))

(defun make-info (_)
	;create lists of immediate dependencies and products
	(defq d (list "lib/asm/asm.inc" "class/lisp/boot.inc" _) p (list))
	(each-line (lambda (_)
		(when (and (> (length _) 10) (eql "(" (elem 0 _))
				(<= 2 (length (defq s (split _ (const (cat " ')" (ascii-char 34) (ascii-char 13)))))) 4))
			(cond
				((eql (setq _ (elem 0 s)) "(include")
					(push d (elem 1 s)))
				((eql _ "(import")
					(push d (elem 1 s)))
				((eql _ "(def-method")
					(push p (f-path (sym (elem 1 s)) (sym (elem 2 s)))))
				((eql _ "(gen-vtable")
					(push p (f-path (sym (elem 1 s)) :vtable)))
				((eql _ "(gen-type")
					(push p (f-path (sym (elem 1 s)) :type)))
				((eql _ "(gen-create")
					(push p (f-path (sym (elem 1 s))
						(if (> (length s) 2) (sym (cat :create_ (elem 2 s))) :create))))
				((eql _ "(def-func")
					(push p (sym (elem 1 s))))))) (file-stream _))
	(list d p))

(defun func-obj (_)
	(sym (cat "obj/" *cpu* "/" *abi* "/" _)))

(defun make (&optional files *abi* *cpu*)
	(setd files (all-vp-files) *abi* (abi) *cpu* (cpu))
	(compile ((lambda ()
		(defq ages (env -31) info (env -31))
		(defun file-age (_)
			;modification time of a file, cached
			(or (get (setq _ (sym _)) ages) (def ages _ (age _))))
		;list of all file imports while defining dependencies and products
		(within-compile-env (lambda ()
			(include "sys/func.inc")
			(each include (all-class-files))
			(each-mergeable (#
				(bind '(d p) (make-info %0))
				(make-merge files d)
				(def info (sym %0) (list d (map func-obj p)))) files)))
		;filter to only the .vp files
		(setq files (filter (# (ends-with ".vp" %0)) files))
		;filter to only the files who's oldest product is older than any dependency
		(setq files (filter (#
			(bind '(d p) (get (sym %0) info))
			(setq p (reduce min (map file-age p)))
			(each-mergeable (# (make-merge d (elem 0 (get (sym %0) info)))) d)
			(some (# (>= %0 p)) (map file-age d))) files))
		;return the list to compile
		files)) *abi* *cpu*))

(defun make-boot (&optional r *funcs* *abi* *cpu*)
	(within-compile-env (lambda ()
		(setd *funcs* (list) *abi* (abi) *cpu* (cpu))
		(defq *fsyms* (env -31) z (cat (char 0 8) (char 0 4)))
		(include "sys/func.inc")
		(defun read-paths (_)
			(defq l (list) i (get-short _ fn_header_links) e (get-short _ fn_header_paths))
			(while (/= i e)
				(push l (sym (get-cstr _ (+ (get-long _ i) i))))
				(setq i (+ i 8))) l)
		(defun load-func (_)
			(or (get _ *fsyms*)
				(progn
					(unless (defq b (load (func-obj _))) (throw "No such file !" (func-obj _)))
					(defq h (slice fn_header_entry (defq l (get-short b fn_header_links)) b)
						l (slice l (defq p (get-short b fn_header_paths)) b))
					(def *fsyms* _ (list (cat (char -1 8) (char p 2) h) l (read-paths b))))))
		(unless (list? *funcs*) (setq *funcs* (list *funcs*)))
		(defq f (list
			;must be first function !
			'sys/load/init
			;must be second function !
			'sys/load/bind
			;must be third function !
			'sys/load/statics))
		;load all loader dependents
		(each-mergeable (# (merge-obj f (elem 2 (load-func %0)))) f)
		(defq fs (length f))
		;load up all extra functions requested
		(merge-obj f (map sym *funcs*))
		(each load-func f)
		;if recursive then load up all dependents
		(if r (each-mergeable (# (merge-obj f (elem 2 (load-func %0)))) f))
		;sort into order, leaving the loader dependents first !
		(sort cmp f fs)
		;list of all function bodies and links in order, list of offsets of header and link sections
		;and offset of new strings section
		(defq b (map (# (get %0 *fsyms*)) f) ns (list) nso (list) ho (list) lo (list)
			so (+ (length z) (reduce (#
				(push ho %0)
				(push lo (setq %0 (+ %0 (length (elem 0 %1)))))
				(+ %0 (length (elem 1 %1)))) b 0)))
		;list of all strings that will appear in new strings section, and list of all new string offsets
		(each (# (each (# (unless (find-rev %0 f) (merge-obj ns (list %0)))) (elem 2 (get %0 *fsyms*)))) f)
		(reduce (# (push nso %0) (+ %0 (length %1) 1)) ns 0)
		;create new link sections with offsets to header strings or new strings
		(each (# (defq u (elem _ lo))
			(elem-set 1 %0 (apply cat (push (map (#
				(char (- (if (defq i (find-rev %0 f))
					(+ (elem i ho) fn_header_pathname)
					(+ (elem (find-rev %0 ns) nso) so)) (+ u (* _ 8))) 8)) (elem 2 %0)) "")))) b)
		;build list of all sections of boot image
		;concatenate all sections and write out
		(defq stream (file-stream (func-obj 'sys/boot_image) file_open_write)
			f (reduce (# (push %0 (cat %1 (ascii-char 0)))) ns
				(push (reduce (# (push %0 (elem 0 %1) (elem 1 %1))) b (list)) z))
			l (reduce (# (+ %0 (length %1))) f 0))
		(each (# (write stream %0)) f)
		(print "-> " (func-obj 'sys/boot_image) " (" l ")") nil)))

(defun make-boot-all (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(make-boot nil
		(within-compile-env (lambda ()
			(include "sys/func.inc")
			(each include (all-class-files))
			(defq *prods* (list))
			;lists of all file imports and products
			(each-mergeable (# (make-merge *prods* (bind '(d %0) (make-info %0))) (make-merge _l d))
				(all-vp-files)) *prods*)) *abi* *cpu*))

(defun make-all (&optional *abi* *cpu* files)
	(setd *abi* (abi) *cpu* (cpu))
	(compile (opt files (all-vp-files)) *abi* *cpu*))

(defun remake (&optional *abi* *cpu* files)
	(setd *abi* (abi) *cpu* (cpu))
	(make files *abi* *cpu*)
	(make-boot-all *abi* *cpu*))

(defun remake-all (&optional *abi* *cpu* files)
	(setd *abi* (abi) *cpu* (cpu))
	(make-all *abi* *cpu* files)
	(make-boot-all *abi* *cpu*))

;;;;;;;;;;;;;;;;;;;;;
; cross platform make
;;;;;;;;;;;;;;;;;;;;;

(defun make-platforms ()
	(defq files (all-vp-files))
	(make files 'AMD64 'x86_64)
	(make files 'WIN64 'x86_64)
	(make files 'ARM64 'aarch64))

(defun make-all-platforms ()
	(defq files (all-vp-files))
	(make-all 'AMD64 'x86_64 files)
	(make-all 'WIN64 'x86_64 files)
	(make-all 'ARM64 'aarch64 files))

(defun remake-platforms ()
	(defq files (all-vp-files))
	(remake 'AMD64 'x86_64 files)
	(remake 'WIN64 'x86_64 files)
	(remake 'ARM64 'aarch64 files))

(defun remake-all-platforms ()
	(defq files (all-vp-files))
	(remake-all 'AMD64 'x86_64 files)
	(remake-all 'WIN64 'x86_64 files)
	(remake-all 'ARM64 'aarch64 files))

;;;;;;;;;;;;;;;;;;;;;;;;
; compile and make tests
;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-test (&optional i &optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(defun time-in-seconds (_)
		(str (/ _ 1000000) "." (pad (% _ 1000000) 6 "00000")))
	(defq b 1000000000 w 0 a 0 c 0 files (all-vp-files))
	(times (opt i 10)
		(defq _ (pii-time))
		(compile files *abi* *cpu*)
		(setq _ (- (pii-time) _) a (+ a _) c (inc c))
		(print "Time " (time-in-seconds _) " seconds")
		(print "Mean time " (time-in-seconds (/ a c)) " seconds")
		(print "Best time " (time-in-seconds (setq b (min b _))) " seconds")
		(print "Worst time " (time-in-seconds (setq w (max w _))) " seconds"))
	nil)

(defun compile-test (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(each (# (compile %0 *abi* *cpu*)) (defq f (all-vp-files)))
	(compile f *abi* *cpu* 1))

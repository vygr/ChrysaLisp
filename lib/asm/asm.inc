;;;;;;;;;;;;;;
; VP Assembler
;;;;;;;;;;;;;;

;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")

;C++ ChrysaLisp ?
(unless (get 'nums)
	(defun mail-devices () '(0)))

(defun-bind compile (files &optional *abi* *cpu* num_child)
	(setd *abi* (abi) *cpu* (cpu) num_child (max 1 (/ (* (length (mail-devices)) 80) 100)))
	(unless (lst? files) (setq files (list files)))
	(setq files (shuffle (map sym files)))
	(if (get 'nums)
		;Native ChrysaLisp
		(progn
			(defq data_in (list) select (array) num_child (min num_child (length files))
				child_mboxs (open-farm "class/lisp/compile.lisp" num_child kn_call_child)
				buckets (map (lambda (_) (list)) child_mboxs) err (list))
			(each (# (push (elem (% _ num_child) buckets) %0)) files)
			(each (# (push select (defq inbox (in-mbox (elem -2 (push data_in (in-stream))))))
					(mail-send (str (list (pop buckets) inbox *abi* *cpu* *debug_mode* *debug_emit* *debug_inst*)) %0))
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

(defun-bind make-merge (l sl)
	;merge string into string list
	(each (lambda (s)
		(unless (some (# (eql %0 s)) l) (push l s))) sl))

(defun-bind make-imports (_)
	;create list of immediate dependencies
	(defq d (list "lib/asm/asm.inc" "class/lisp/boot.inc" _))
	(each-line (# (when (starts-with "(include" %0)
			(push d (elem 1 (split %0 (const (cat " ')" (ascii-char 34) (ascii-char 13))))))))
		(file-stream _)) d)

(defun-bind make-info (_)
	;create lists of immediate dependencies and products
	(defq d (list "lib/asm/asm.inc" "class/lisp/boot.inc" _) p (list))
	(each-line (#
		(when (and (>= (length %0) 10) (eql "(" (elem 0 %0))
				(<= 2 (length (defq s (split %0 (const (cat " ')" (ascii-char 34) (ascii-char 13)))))) 4))
			(setq %0 (elem 0 s))
			(cond
				((eql %0 "(def-method")
					(push p (str (f-path (sym (elem 1 s)) (sym (elem 2 s))))))
				((eql %0 "(include")
					(push d (elem 1 s)))
				((eql %0 "(gen-vtable")
					(push p (str (f-path (sym (elem 1 s)) :vtable))))
				((eql %0 "(gen-create")
					(push p (str (f-path (sym (elem 1 s))
						(if (> (length s) 2) (sym (cat :create_ (sym (elem 2 s)))) :create)))))
				((eql %0 "(def-func")
					(push p (elem 1 s)))))) (file-stream _))
	(list d p))

(defun-bind func-obj (_)
	(cat "obj/" *cpu* "/" *abi* "/" _))

(defun-bind make (&optional root *abi* *cpu*)
	(setd root "make.inc" *abi* (abi) *cpu* (cpu))
	(compile ((lambda ()
		(defq *imports* (list root) *ages* (env -31) *deps* (env -31))
		(defun-bind file-age (_)
			;modification time of a file, cached
			(or (get (setq _ (sym _)) *ages*) (def *ages* _ (age _))))
		;list of all file imports while defining dependencies and products
		(within-compile-env (lambda ()
			(include "sys/func.inc")
			(each include (all-class-files))
			(each-mergeable (#
				(defq i (make-info %0))
				(bind '(d p) i)
				(make-merge *imports* d)
				(elem-set 1 i (map func-obj p))
				(def *deps* (sym %0) i)) *imports*)))
		;filter to only the .vp files
		(setq *imports* (filter (# (ends-with ".vp" %0)) *imports*))
		;filter to only the files who's oldest product is older than any dependency
		(setq *imports* (filter (#
			(defq d (eval (sym %0) *deps*) p (reduce min (map file-age (elem 1 d))) d (elem 0 d))
			(each-mergeable (# (make-merge d (elem 0 (eval %0 *deps*)))) d)
			(some (# (>= %0 p)) (map file-age d))) *imports*))
		;return the list to compile
		*imports*)) *abi* *cpu*))

(defun-bind make-boot (&optional r *funcs* *abi* *cpu*)
	(within-compile-env (lambda ()
		(setd *funcs* (list) *abi* (abi) *cpu* (cpu))
		(defq *fsyms* (env -31) z (cat (char 0 8) (char 0 4)))
		(include "sys/func.inc")
		(defun-bind read-paths (_)
			(defq l (list) i (get-short _ fn_header_links) e (get-short _ fn_header_paths))
			(while (/= i e)
				(push l (sym (get-cstr _ (+ (get-long _ i) i))))
				(setq i (+ i 8))) l)
		(defun-bind load-func (_)
			(or (get _ *fsyms*)
				(progn
					(unless (defq b (load (func-obj _))) (throw "No such file !" (func-obj _)))
					(defq h (slice fn_header_entry (defq l (get-short b fn_header_links)) b)
						l (slice l (defq p (get-short b fn_header_paths)) b))
					(def *fsyms* _ (list (cat (char -1 8) (char p 2) h) l (read-paths b))))))
		(unless (lst? *funcs*) (setq *funcs* (list *funcs*)))
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
		(print "image -> " (func-obj 'sys/boot_image) " (" l ")") nil)))

(defun-bind make-boot-all (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(make-boot nil
		(within-compile-env (lambda ()
			(include "sys/func.inc")
			(each include (all-class-files))
			(defq *prods* (list))
			;lists of all file imports and products
			(each-mergeable (# (make-merge *prods* (bind '(d %0) (make-info %0))) (make-merge _l d))
				(list "make.inc")) *prods*)) *abi* *cpu*))

(defun-bind all-vp-files ()
	;filter to only the .vp files
	(filter (# (ends-with ".vp" %0))
		;the list of all file imports
		(each-mergeable (# (make-merge _l (make-imports %0))) (list "make.inc"))))

(defun-bind all-class-files ()
	;filter to only the class.inc files
	(filter (# (ends-with "class.inc" %0))
		;the list of all file imports
		(each-mergeable (# (make-merge _l (make-imports %0))) (list "make.inc"))))

(defun-bind make-all (&optional *abi* *cpu* files)
	(setd *abi* (abi) *cpu* (cpu))
	(compile (opt files (all-vp-files)) *abi* *cpu*))

(defun-bind remake (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(make "make.inc" *abi* *cpu*)
	(make-boot-all *abi* *cpu*))

(defun-bind remake-all (&optional *abi* *cpu* files)
	(setd *abi* (abi) *cpu* (cpu))
	(make-all *abi* *cpu* files)
	(make-boot-all *abi* *cpu*))

;;;;;;;;;;;;;;;;;;;;;
; cross platform make
;;;;;;;;;;;;;;;;;;;;;

(defun-bind make-platforms ()
	(make "make.inc" 'AMD64 'x86_64)
	(make "make.inc" 'WIN64 'x86_64)
	(make "make.inc" 'ARM64 'aarch64))

(defun-bind make-all-platforms ()
	(defq files (all-vp-files))
	(make-all 'AMD64 'x86_64 files)
	(make-all 'WIN64 'x86_64 files)
	(make-all 'ARM64 'aarch64 files))

(defun-bind remake-platforms ()
	(remake 'AMD64 'x86_64)
	(remake 'WIN64 'x86_64)
	(remake 'ARM64 'aarch64))

(defun-bind remake-all-platforms ()
	(defq files (all-vp-files))
	(remake-all 'AMD64 'x86_64 files)
	(remake-all 'WIN64 'x86_64 files)
	(remake-all 'ARM64 'aarch64 files))

;;;;;;;;;;;;;;;;;;;;;;;;
; compile and make tests
;;;;;;;;;;;;;;;;;;;;;;;;

(defun-bind make-test (&optional i &optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(defun-bind time-in-seconds (_)
		(str (/ _ 1000000) "." (pad (% _ 1000000) 6 "00000")))
	(defq b 1000000000 w 0 a 0 c 0 files (all-vp-files))
	(times (opt i 10)
		(defq _ (time))
		(compile files *abi* *cpu*)
		(setq _ (- (time) _) a (+ a _) c (inc c))
		(print "Time " (time-in-seconds _) " seconds")
		(print "Mean time " (time-in-seconds (/ a c)) " seconds")
		(print "Best time " (time-in-seconds (setq b (min b _))) " seconds")
		(print "Worst time " (time-in-seconds (setq w (max w _))) " seconds"))
	nil)

(defun-bind compile-test (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(each (lambda (_)
		(compile _ *abi* *cpu*)) (defq f (all-vp-files)))
	(compile f *abi* *cpu* 1))

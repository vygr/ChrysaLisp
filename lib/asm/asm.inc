;;;;;;;;;;;;;;
; VP Assembler
;;;;;;;;;;;;;;

(import "././task/local.inc")
(import "././boot/image.inc")
(import "././files/files.inc")

;module
(env-push)

(enums +select 0
	(enum task reply timer))

(defun dispatch-job (key val)
	;send another job to child
	(cond
		((defq job (pop jobs))
			(def val :job job :timestamp (pii-time))
			(mail-send (get :child val) (cat
				(char key +long_size) (elem-get select +select_reply) job)))
		(:t ;no jobs in que
			(undef val :job :timestamp))))

(defun cancel-job (key val)
	;cancel this job
	(undef val :job :timestamp)
	(. farm :restart key val))

(defun create (key val nodes)
	; (create key val nodes)
	;function called when entry is created
	(open-task "lib/asm/asm.lisp" (elem-get nodes (random (length nodes)))
		+kn_call_child key (elem-get select +select_task)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (get :child val)) (mail-send child ""))
	(when (defq job (get :job val))
		(print "Restarting job ! -> " job)
		(push jobs job)
		(undef val :job :timestamp)))

(defun compile (files &optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(unless (list? files) (setq files (list files)))
	(when (nempty? files)
		(shuffle files)
		(defq timer_rate (/ 1000000 1) working :t errors (list) retry_timeout (task-timeout 2)
			select (list (mail-alloc-mbox) (mail-alloc-mbox) (mail-alloc-mbox))
			jobs (map (# (str (list (list %0) *abi* *cpu* *debug_mode* *debug_emit* *debug_inst*))) files)
			farm (Local (const create) (const destroy) (length files) (max 1 (min 4 (length (lisp-nodes))))))
		(mail-timeout (elem-get select +select_timer) timer_rate 0)
		(while working
			(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
			(cond
				((= idx +select_task)
					;child launch response
					(defq key (getf msg +kn_msg_key) child (getf msg +kn_msg_reply_id))
					(when (defq val (. farm :find key))
						(. farm :add_node (slice child +mailbox_id_size -1))
						(def val :child child)
						(dispatch-job key val)))
				((= idx +select_reply)
					;child worker response
					(defq key (str-as-num (pop (defq msg (split msg (ascii-char 10)))))
						error_flag :nil)
					(each (# (cond
						((starts-with "Error:" %0)
							(push errors %0)
							(setq error_flag :t))
						(:t (print %0)))) msg)
					(when (defq val (. farm :find key))
						(if error_flag
							(cancel-job key val)
							(dispatch-job key val)))
					(stream-flush (io-stream 'stdout))
					;all jobs done ?
					(when (= 0 (length jobs))
						(setq working :nil)
						(. farm :each (lambda (key val)
							(setq working (or working (get :job val)))))))
				(:t ;timer event
					(mail-timeout (elem-get select +select_timer) timer_rate 0)
					(. farm :refresh retry_timeout))))
		(. farm :close)
		(each mail-free-mbox select)
		(each (const print) errors))
	(print "Done") :nil)

;;;;;;;;;;;;;
; make system
;;;;;;;;;;;;;

(defun make-merge (l sl)
	;merge string into string list
	(each (lambda (s)
		(unless (find s l) (push l s))) sl))

(defun all-vp-files ()
	;filter to only the .vp files, strip off the leading "./"
	(filter (# (not (starts-with "apps/" %0))) (files-all "." '(".vp") 2)))

(defun all-class-files ()
	;filter to only the class.inc files, strip off the leading "./"
	(files-all "." '("class.inc") 2))

(defun make-info (_)
	;create lists of immediate dependencies and products
	(defq d (list "lib/asm/asm.inc" "class/lisp/root.inc" "class/lisp/task.inc" _) p (list))
	(lines! (lambda (_)
		(when (and (> (length _) 10) (eql "(" (first _))
				(<= 2 (length (defq s (split _ (const (char-class " '()\q\r"))))) 4))
			(case (first s)
				(("include" "import")
					(push d (abs-path (second s) file)))
				("def-method"
					(push p (f-path (sym (second s)) (sym (third s)))))
				("gen-vtable"
					(push p (f-path (sym (second s)) :vtable)))
				("gen-type"
					(push p (f-path (sym (second s)) :type)))
				("gen-create"
					(push p (f-path (sym (second s))
						(if (> (length s) 2) (sym (cat :create_ (third s))) :create))))
				("def-func"
					(push p (sym (second s)))))))
		(if (= (age _) 0)
			(throw "No such file !" _)
			(file-stream (defq file _))))
	(list d p))

(defun file-age (_)
	;modification time of a file, cached
	(or (get (setq _ (sym _)) ages) (def ages _ (age _))))

(defun make (&optional files *abi* *cpu*)
	(setd files (all-vp-files) *abi* (abi) *cpu* (cpu))
	(compile ((lambda ()
		(defq ages (env 31) info (env 31))
		;list of all file imports while defining dependencies and products
		(within-compile-env (lambda ()
			(include "lib/asm/func.inc")
			(each include (all-class-files))
			(each-mergeable (#
				(bind '(d p) (make-info %0))
				(make-merge files d)
				(def info (sym %0) (list d (map (const func-obj-path) p)))) files)))
		;filter to only the .vp files
		(setq files (filter (# (ends-with ".vp" %0)) files))
		;filter to only the files who's oldest product is older than any dependency
		(setq files (filter (#
			(bind '(d p) (get (sym %0) info))
			(setq p (reduce min (map (const file-age) p) +max_long))
			(each-mergeable (# (make-merge d (first (get (sym %0) info)))) d)
			(some (# (>= %0 p)) (map (const file-age) d))) files))
		;return the list to compile
		files)) *abi* *cpu*))

(defun make-boot-all (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(boot-image
		(within-compile-env (lambda ()
			(include "lib/asm/func.inc")
			(each include (all-class-files))
			(defq *prods* (list))
			;lists of all file imports and products
			(each-mergeable (# (make-merge *prods* (bind '(d %0) (make-info %0))) (make-merge l d))
				(defq l (all-vp-files))) *prods*)) *abi* *cpu*))

(defun make-all (&optional files *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(compile (ifn files (all-vp-files)) *abi* *cpu*))

(defun remake (&optional files *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(make files *abi* *cpu*)
	(make-boot-all *abi* *cpu*))

(defun remake-all (&optional files *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(make-all files *abi* *cpu*)
	(make-boot-all *abi* *cpu*))

;;;;;;;;;;;;;;;;;;;;;
; cross platform make
;;;;;;;;;;;;;;;;;;;;;

(defq +supported_abi ''(AMD64 WIN64 ARM64 RISCV64)
	+supported_cpu ''(x86_64 x86_64 arm64 riscv64))

(defun make-platforms ()
	(defq files (all-vp-files))
	(each (# (make files %0 %1)) +supported_abi +supported_cpu)
;	(defq *debug_mode* 0)
	(make files 'VP64 'vp64))

(defun make-all-platforms ()
	(defq files (all-vp-files))
	(each (# (make-all files %0 %1)) +supported_abi +supported_cpu)
;	(defq *debug_mode* 0)
	(make-all files 'VP64 'vp64))

(defun remake-platforms ()
	(defq files (all-vp-files))
	(each (# (remake files %0 %1)) +supported_abi +supported_cpu)
;	(defq *debug_mode* 0)
	(remake files 'VP64 'vp64))

(defun remake-all-platforms ()
	(defq files (all-vp-files))
	(each (# (remake-all files %0 %1)) +supported_abi +supported_cpu)
;	(defq *debug_mode* 0)
	(remake-all files 'VP64 'vp64))

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
	make make-all make-merge
	make-platforms make-all-platforms
	remake remake-all
	remake-platforms remake-all-platforms
	compile-test make-test))
(env-pop)

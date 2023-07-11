;;;;;;;;;;;;;;
; VP Assembler
;;;;;;;;;;;;;;

(import "././task/farm.inc")
(import "././boot/image.inc")

;module
(env-push)

(defun compile (files &optional *abi* *cpu* num_child)
	(unless (list? files) (setq files (list files)))
	(defq nxt_node (length (mail-nodes)) num_files (length (shuffle files)))
	(setd *abi* (abi) *cpu* (cpu) num_child (max 1 (* nxt_node 1)))
	(setq num_child (min num_child num_files))
	(enums +select 0 (enum task reply timer))
	(defun dispatch-job (key val)
		;send another job to child
		(cond
			((defq job (pop jobs))
				(def val :job job :timestamp (pii-time))
				(mail-send (get :child val) (cat
					(char key +long_size) (elem-get +select_reply select) job)))
			(:t	;no jobs in que
				(undef val :job :timestamp))))
	(defun create (key val nodes)
		; (create key val nodes)
		;function called when entry is created
		(if (= 0 (length nodes))
			(setq nodes (list (slice +long_size -1 (task-mailbox)))))
		(open-task "lib/asm/asm.lisp"
			(elem-get (setq nxt_node (% (inc nxt_node) (length nodes))) nodes)
			+kn_call_child key (elem-get +select_task select)))
	(defun destroy (key val)
		; (destroy key val)
		;function called when entry is destroyed
		(when (defq child (get :child val)) (mail-send child ""))
		(when (defq job (get :job val))
			(print "Job timed out -> " job)
			(push jobs job)
			(undef val :job :timestamp)))
	(when (/= num_child 0)
		(defq timer_rate (/ 1000000 1) working :t
			errors (list) nxt_node (random nxt_node)
			retry_timeout (if (starts-with "obj/vp64" (load-path)) 100000000 1000000)
			select (list (mail-alloc-mbox) (mail-alloc-mbox) (mail-alloc-mbox))
			jobs (map (# (str (list (list %0) *abi* *cpu* *debug_mode* *debug_emit* *debug_inst*))) files)
			farm (Farm create destroy num_child))
		(mail-timeout (elem-get +select_timer select) timer_rate 0)
		(while working
			(defq msg (mail-read (elem-get (defq idx (mail-select select)) select)))
			(cond
				((= idx +select_task)
					;child launch responce
					(defq key (getf msg +kn_msg_key) child (getf msg +kn_msg_reply_id))
					(when (defq val (. farm :find key))
						(def val :child child)
						(dispatch-job key val)))
				((= idx +select_reply)
					;child worker responce
					(defq key (str-as-num (pop (defq msg (split msg (ascii-char 10))))))
					(when (defq val (. farm :find key))
						(dispatch-job key val))
					(each (# (if (starts-with "Error:" %0) (push errors %0) (print %0))) msg)
					(stream-flush (io-stream 'stdout))
					;all jobs done ?
					(when (= 0 (length jobs))
						(setq working :nil)
						(. farm :each (lambda (key val)
							(setq working (or working (get :job val)))))))
				(:t	;timer event
					(mail-timeout (elem-get +select_timer select) timer_rate 0)
					(. farm :refresh retry_timeout))))
		(. farm :close)
		(each mail-free-mbox select)
		(each print errors))
	(print "Done") :nil)

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
		(unless (starts-with "./obj" root)
			(each! 0 -1 (lambda (file type)
				(unless (starts-with "." file)
					(if (eql type "4")
						(push stack (cat root "/" file))
						(if (ends-with ext file)
							(push files (cat root "/" file))))))
				(unzip (split (pii-dirlist root) ",") (lists2)))))
	files)

(defun all-vp-files ()
	;filter to only the .vp files, strip off the leading "./"
	(map (# (slice 2 -1 %0))
		(filter (# (not (starts-with "./apps/" %0))) (all-files "." ".vp"))))

(defun all-class-files ()
	;filter to only the class.inc files, strip off the leading "./"
	(map (# (slice 2 -1 %0)) (all-files "." "class.inc")))

(defun make-info (_)
	;create lists of immediate dependencies and products
	(defq d (list "lib/asm/asm.inc" "class/lisp/root.inc" _) p (list))
	(each-line (lambda (_)
		(when (and (> (length _) 10) (eql "(" (elem-get 0 _))
				(<= 2 (length (defq s (split _ (const (cat " ')" (ascii-char 34) (ascii-char 13)))))) 4))
			(cond
				((eql (setq _ (elem-get 0 s)) "(include")
					(push d (abs-path (elem-get 1 s) file)))
				((eql _ "(import")
					(push d (abs-path (elem-get 1 s) file)))
				((eql _ "(def-method")
					(push p (f-path (sym (elem-get 1 s)) (sym (elem-get 2 s)))))
				((eql _ "(gen-vtable")
					(push p (f-path (sym (elem-get 1 s)) :vtable)))
				((eql _ "(gen-type")
					(push p (f-path (sym (elem-get 1 s)) :type)))
				((eql _ "(gen-create")
					(push p (f-path (sym (elem-get 1 s))
						(if (> (length s) 2) (sym (cat :create_ (elem-get 2 s))) :create))))
				((eql _ "(def-func")
					(push p (sym (elem-get 1 s))))))) (file-stream (defq file _)))
	(list d p))

(defun make (&optional files *abi* *cpu*)
	(setd files (all-vp-files) *abi* (abi) *cpu* (cpu))
	(compile ((lambda ()
		(defq ages (env 31) info (env 31))
		(defun file-age (_)
			;modification time of a file, cached
			(or (get (setq _ (sym _)) ages) (def ages _ (age _))))
		;list of all file imports while defining dependencies and products
		(within-compile-env (lambda ()
			(include "lib/asm/func.inc")
			(each include (all-class-files))
			(each-mergeable (#
				(bind '(d p) (make-info %0))
				(make-merge files d)
				(def info (sym %0) (list d (map func-obj-path p)))) files)))
		;filter to only the .vp files
		(setq files (filter (# (ends-with ".vp" %0)) files))
		;filter to only the files who's oldest product is older than any dependency
		(setq files (filter (#
			(bind '(d p) (get (sym %0) info))
			(setq p (reduce min (map file-age p)))
			(each-mergeable (# (make-merge d (elem-get 0 (get (sym %0) info)))) d)
			(some (# (>= %0 p)) (map file-age d))) files))
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
			(each-mergeable (# (make-merge *prods* (bind '(d %0) (make-info %0))) (make-merge _l d))
				(all-vp-files)) *prods*)) *abi* *cpu*))

(defun make-all (&optional files *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(compile (opt files (all-vp-files)) *abi* *cpu*))

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
	(defq *debug_mode* 0)
	(make files 'VP64 'vp64))

(defun make-all-platforms ()
	(defq files (all-vp-files))
	(each (# (make-all files %0 %1)) +supported_abi +supported_cpu)
	(defq *debug_mode* 0)
	(make-all files 'VP64 'vp64))

(defun remake-platforms ()
	(defq files (all-vp-files))
	(each (# (remake files %0 %1)) +supported_abi +supported_cpu)
	(defq *debug_mode* 0)
	(remake files 'VP64 'vp64))

(defun remake-all-platforms ()
	(defq files (all-vp-files))
	(each (# (remake-all files %0 %1)) +supported_abi +supported_cpu)
	(defq *debug_mode* 0)
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
	(compile files *abi* *cpu* 1))

;module
(export-symbols
	all-files all-vp-files all-class-files
	make make-all make-merge
	make-platforms make-all-platforms
	remake remake-all
	remake-platforms remake-all-platforms
	compile-test make-test)
(env-pop)

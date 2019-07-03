;;;;;;;;;;;;;;
; VP Assembler
;;;;;;;;;;;;;;

(defq debug_mode t debug_trace nil debug_emit nil debug_inst nil)

(defun compile (*files* &optional *abi* *cpu* *pipes*)
	(setd *abi* (abi) *cpu* (cpu) *pipes* 16)
	(defq q (list) e (list))
	(unless (lst? *files*)
		(setq *files* (list *files*)))
	(setq *files* (shuffle (map sym *files*)))
	(if (def? 'pipe)
		(progn
			(while (gt *pipes* 0)
				(defq i (div (length *files*) *pipes*) s (slice 0 i *files*) *files* (slice i -1 *files*))
				(when (ne i 0)
					(push q (defq p (pipe "lisp cmd/asm.inc")))
					(pipe-write p (cat "(compile-pipe '" (str s) " '" *abi* " '" *cpu* ") ")))
				(setq *pipes* (dec *pipes*)))
			(when (ne 0 (length q))
				(print "Compiling with " (length q) " instances")
				(each (lambda (p)
					(each-pipe-line (lambda (l)
						(defq k (elem 0 (split l " ")))
						(cond
							((eql k "Done"))
							((eql k "Error:") (push e l) nil)
							(t (print l)))) p)) q)
				(each print e))
			(print "Done"))
		(compile-pipe *files* *abi* *cpu*))
	nil)

(defun compile-pipe (*files* *abi* *cpu*)
	(within-compile-env (lambda ()
		(each include *files*)
		(print "Done"))))

;;;;;;;;;;;;;
; make system
;;;;;;;;;;;;;

(defun make-imports (_)
	;create list of immediate dependencies
	(defq d (list 'cmd/asm.inc 'class/lisp/boot.inc _))
	(each-line (lambda (_)
		(setq _ (trim-end _ (char 13)))
		(when (and (ge (length _) 10) (eql "(" (elem 0 _))
				(eq 2 (length (defq s (split _ " ")))))
			(defq _ (elem 0 s))
			(cond
				((eql _ "(include")
					(push d (sym (trim-start (trim-end (elem 1 s) ")") "'"))))))) _) d)

(defun make-info (_)
	;create lists of immediate dependencies and products
	(defq d (list 'cmd/asm.inc 'class/lisp/boot.inc _) p (list))
	(defun trim-cruft (_)
		(sym (trim-start (trim-end _ ")") "'")))
	(each-line (lambda (_)
		(setq _ (trim-end _ (char 13)))
		(when (and (ge (length _) 10) (eql "(" (elem 0 _))
				(le 2 (length (defq s (split _ " "))) 4))
			(defq _ (elem 0 s))
			(cond
				((eql _ "(def-method")
					(push p (f-path (trim-cruft (elem 1 s)) (trim-cruft (elem 2 s)))))
				((eql _ "(include")
					(push d (trim-cruft (elem 1 s))))
				((eql _ "(gen-vtable")
					(push p (f-path (trim-cruft (elem 1 s)) 'vtable)))
				((eql _ "(gen-create")
					(push p (f-path (trim-cruft (elem 1 s))
						(if (gt (length s) 2) (sym-cat "create_" (trim-cruft (elem 2 s))) 'create))))
				((eql _ "(def-func")
					(push p (trim-cruft (elem 1 s))))))) _)
	(list d p))

(defun make (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(compile ((lambda ()
		(defq *env* (env 101) *imports* (list 'make.inc))
		(catch (progn
			(defun func-obj (_)
				(cat "obj/" *cpu* "/" *abi* "/" _))
			(defun make-sym (_)
				(sym-cat "_dep_" _))
			(defun make-time (_)
				;modification time of a file, cached
				(defq s (sym-cat "_age_" _))
				(or (def? s) (def *env* s (age _))))
			;list of all file imports while defining dependencies and products
			(within-compile-env (lambda ()
				(include 'sys/func.inc)
				(each include (all-class-files))
				(each-mergeable (lambda (_)
					(defq i (make-info _))
					(bind '(d p) i)
					(merge *imports* d)
					(elem-set 1 i (map func-obj p))
					(def *env* (make-sym _) i)) *imports*)))
			;filter to only the .vp files
			(setq *imports* (filter (lambda (_)
				(and (ge (length _) 3) (eql ".vp" (slice -4 -1 _)))) *imports*))
			;filter to only the files who's oldest product is older than any dependency
			(setq *imports* (filter (lambda (_)
				(defq d (eval (make-sym _)) p (reduce min (map make-time (elem 1 d))) d (elem 0 d))
				(each-mergeable (lambda (_)
					(merge d (elem 0 (eval (make-sym _))))) d)
				(some (lambda (_) (ge _ p)) (map make-time d))) *imports*))
			;drop the make environment, return the list to compile
			(setq *env* nil) *imports*) (setq *env* nil)))) *abi* *cpu*))

(defun make-boot (&optional r *funcs* *abi* *cpu*)
	(within-compile-env (lambda ()
		(setd *funcs* (list) *abi* (abi) *cpu* (cpu))
		(defq *env* (env 31) z (cat (char 0 8) (char 0 4)))
		(catch (progn
			(include 'sys/func.inc)
			(defun func-sym (_)
				(sym-cat 'f_ _))
			(defun func-obj (_)
				(cat "obj/" *cpu* "/" *abi* "/" _))
			(defun load-func (_)
				(or (def? (defq s (func-sym _)))
					(progn
						(unless (defq b (load (func-obj _)))
							(throw "No such file !" (func-obj _)))
						(defq h (slice fn_header_entry (defq l (get-short b fn_header_links)) b)
							l (slice l (defq p (get-short b fn_header_paths)) b))
						(def *env* s (list (cat (char -1 8) (char p 2) h) l (read-paths b))))))
			(defun read-paths (_)
				(defq l (list) i (get-short _ fn_header_links) e (get-short _ fn_header_paths))
				(while (ne i e)
					(push l (sym (get-cstr _ (add (get-long _ i) i))))
					(setq i (add i 8))) l)
			(unless (lst? *funcs*)
				(setq *funcs* (list *funcs*)))
			(defq f (list
				;must be first function !
				'sys/load/init
				;must be second function !
				'sys/load/bind
				;must be third function !
				'sys/load/statics))
			;load all loader dependents
			(each-mergeable (lambda (_)
				(merge f (elem 2 (load-func _)))) f)
			(defq fs (length f))
			;load up all extra functions requested
			(merge f (map sym *funcs*))
			(each load-func f)
			;if recursive then load up all dependents
			(when r
				(each-mergeable (lambda (_)
					(merge f (elem 2 (load-func _)))) f))
			;sort into order, leaving the loader dependents first !
			(sort cmp f fs)
			;list of all function bodies and links in order, list of offsets of header and link sections
			;and offset of new strings section
			(defq b (map (lambda (_) (eval (func-sym _))) f)
				ns (list) nso (list) ho (list) lo (list) so (add (length z) (reduce (lambda (x y)
				(push ho x)
				(push lo (setq x (add x (length (elem 0 y)))))
				(add x (length (elem 1 y)))) b 0)))
			;list of all strings that will appear in new strings section, and list of all new string offsets
			(each (lambda (_)
				(each (lambda (_)
					(unless (find _ f) (insert ns _))) (elem 2 (eval (func-sym _))))) f)
			(reduce (lambda (x y)
				(push nso x)
				(add x (length y) 1)) ns 0)
			;create new link sections with offsets to header strings or new strings
			(each (lambda (x)
				(defq u (elem _ lo))
				(elem-set 1 x (apply cat (push (map (lambda (y)
					(char (sub (if (defq i (find y f))
						(add (elem i ho) fn_header_pathname)
						(add (elem (find y ns) nso) so)) (add u (mul _ 8))) 8)) (elem 2 x)) "")))) b)
			;build list of all sections of boot image
			;concatenate all sections and save
			(save (setq f (apply cat (reduce (lambda (x y)
				(push x (cat y (char 0)))) ns (push (reduce (lambda (x y)
					(push x (elem 0 y) (elem 1 y))) b (list)) z)))) (func-obj 'sys/boot_image))
			(print "image -> " (func-obj 'sys/boot_image) " (" (length f) ")")
			(setq *env* nil)) (setq *env* nil)))))

(defun make-boot-all (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(make-boot nil
		(within-compile-env (lambda ()
			(include 'sys/func.inc)
			(each include (all-class-files))
			(defq *imports* (list 'make.inc) *prods* (list))
			;lists of all file imports and products
			(each-mergeable (lambda (_)
				(bind '(d p) (make-info _))
				(merge *imports* d)
				(merge *prods* p)) *imports*) *prods*)) *abi* *cpu*))

(defun all-vp-files ()
	(defq *imports* (list 'make.inc))
	;list of all file imports
	(each-mergeable (lambda (_)
		(merge *imports* (make-imports _))) *imports*)
	;filter to only the .vp files
	(filter (lambda (_)
		(and (ge (length _) 3) (eql ".vp" (slice -4 -1 _)))) *imports*))

(defun all-class-files ()
	(defq *imports* (list 'make.inc))
	;list of all file imports
	(each-mergeable (lambda (_)
		(merge *imports* (make-imports _))) *imports*)
	;filter to only the class.inc files
	(filter (lambda (_)
		(and (ge (length _) 9) (eql "class.inc" (slice -10 -1 _)))) *imports*))

(defun make-all (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(compile (all-vp-files) *abi* *cpu*))

(defun remake (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(make *abi* *cpu*)
	(make-boot-all *abi* *cpu*))

(defun remake-all (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(make-all *abi* *cpu*)
	(make-boot-all *abi* *cpu*))

;;;;;;;;;;;;;;;;;;;;;
; cross platform make
;;;;;;;;;;;;;;;;;;;;;

(defun make-platforms ()
	(make 'AMD64 'x86_64)
	(make 'WIN64 'x86_64)
	(make 'ARM64 'aarch64))

(defun make-all-platforms ()
	(make-all 'AMD64 'x86_64)
	(make-all 'WIN64 'x86_64)
	(make-all 'ARM64 'aarch64))

(defun remake-platforms ()
	(remake 'AMD64 'x86_64)
	(remake 'WIN64 'x86_64)
	(remake 'ARM64 'aarch64))

(defun remake-all-platforms ()
	(remake-all 'AMD64 'x86_64)
	(remake-all 'WIN64 'x86_64)
	(remake-all 'ARM64 'aarch64))

;;;;;;;;;;;;;;;;;;;;;;;;
; compile and make tests
;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-test (&optional i &optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(defun time-in-seconds (_)
		(defq f (str (mod _ 1000000)))
		(cat (str (div _ 1000000)) "." (slice 0 (sub 6 (length f)) "00000") f))
	(defq b 1000000000 a 0 c 0)
	(times (opt i 10)
		(defq _ (time))
		(compile (all-vp-files) *abi* *cpu*)
		(setq _ (sub (time) _) a (add a _) c (inc c))
		(print "Time " (time-in-seconds _) " seconds")
		(print "Mean time " (time-in-seconds (div a c)) " seconds")
		(print "Best time " (time-in-seconds (setq b (min b _))) " seconds"))
	nil)

(defun compile-test (&optional *abi* *cpu*)
	(setd *abi* (abi) *cpu* (cpu))
	(defq f (all-vp-files))
	(each (lambda (_)
		(compile _ *abi* *cpu*)) f)
	(compile f *abi* *cpu* 1))

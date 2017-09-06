;;;;;;;;;
; sign on
;;;;;;;;;

(print "ChrysaLisp 1.1")
(print "Press ESC/Enter to exit.")

;;;;;;;;;;;;;;
; VP Assembler
;;;;;;;;;;;;;;

(defq debug_mode t debug_emit nil debug_inst nil)

(defun compile (*files* &optional *os* *cpu* *pipes*)
	(setd *os* (platform) *cpu* (cpu) *pipes* 16)
	(defq q (list) e (list))
	(unless (lst? *files*)
		(setq *files* (list *files*)))
	(setq *files* (shuffle (map sym *files*)))
	(while (gt *pipes* 0)
		(defq i (div (length *files*) *pipes*) s (slice 0 i *files*) *files* (slice i -1 *files*))
		(when (ne i 0)
			(push q (defq p (pipe "lisp")))
			(pipe-write p (cat "(compile-pipe '" (str s) " '" *os* " '" *cpu* ") ")))
		(setq *pipes* (dec *pipes*)))
	(when (ne 0 (length q))
		(print "Compiling with " (length q) " instances")
		(each (lambda (p)
			(every-pipe-line (lambda (l)
				(defq k (elem 0 (split l " ")))
				(cond
					((eql k "Done"))
					((eql k "Error:") (push e l) nil)
					(t (print l)))) p)) q)
		(each print e))
	(print "Done") nil)

(defun compile-pipe (*files* *os* *cpu*)
	(within-compile-env (lambda ()
		(each import *files*)
		(print "Done"))))

(defun make-info (_)
	;create lists of immediate dependencies and products
	(defq d (list 'cmd/lisp.lisp _) p (list))
	(each-line _ (lambda (_)
		(when (le 2 (length (defq s (split _ " "))) 3)
			(defq _ (elem 0 s) o (sym (trim-start (trim-end (elem 1 s) ")") "'")))
			(cond
				((eql _ "(import")
					(push d o))
				((eql _ "(gen-class")
					(push p (sym-cat "class/class_" o)))
				((eql _ "(gen-new")
					(push p (sym-cat "class/" o "/new")))
				((eql _ "(gen-create")
					(push p (sym-cat "class/" o "/create")))
				((eql _ "(def-func")
					(push p o))))))
	(list d p))

(defun make-boot (&optional r *funcs*)
	(setd *funcs* (list))
	(defq *env* (env 101) z (cat (char 0 8) (char 0 8)))
	(defun func-obj (_)
		(cat "obj/" _))
	(defun load-func (_)
		(or (def? _)
			(progn
				(defq b (load (func-obj _))
					h (slice fn_header_entry (defq l (read-int fn_header_links b)) b)
					l (slice l (defq p (read-int fn_header_paths b)) b))
				(def *env* _ (list (cat (char -1 8) (char p 4) h) l (read-paths b))))))
	(defun read-byte (o f)
		(code (elem o f)))
	(defun read-short (o f)
		(add (read-byte o f) (bit-shl (read-byte (inc o) f) 8)))
	(defun read-int (o f)
		(add (read-short o f) (bit-shl (read-short (add o 2) f) 16)))
	(defun read-long (o f)
		(add (read-int o f) (bit-shl (read-int (add o 4) f) 32)))
	(defun read-paths (_)
		(defq l (list) i (read-int fn_header_links _))
		(while (ne 0 (defq p (read-long i _)))
			(defq j (add p i) k j)
			(while (ne 0 (read-byte j _))
				(setq j (inc j)))
			(push l (sym (slice k j _)))
			(setq i (add i 8))) l)
	(unless (lst? *funcs*)
		(setq *funcs* (list *funcs*)))
	(defq fn_header_length 8 fn_header_entry 12 fn_header_links 16 fn_header_paths 20 f (list
	;must be first function !
	'sys/load_init
	;must be second function !
	'sys/load_bind
	;must be third function !
	'sys/load_statics
	;must be included ! Because it unmaps all function blocks
	'sys/load_deinit
	;must be included ! Because load_deinit accesses them
	'sys/mem_statics
	;must be included !
	'sys/kernel))
	(merge-sym f (map sym *funcs*))
	;load up all functions requested
	(each load-func f)
	;if recursive then load up all dependents
	(when r
		(each-mergeable (lambda (_)
			(merge-sym f (elem 2 (load-func _)))) f))
	;sort into order
	(sort cmp f 6)
	;list of all function bodies and links in order, list of offsets of link sections, offset of new path section
	(defq b (map eval f) o (list) p (add (length z) (reduce (lambda (x y)
		(setq x (add x (length (elem 0 y))))
		(push o x)
		(add x (length (elem 1 y)))) b 0)))
	;list of all function names that will appear in new path section, and list of all new path offsets
	(each-mergeable-rev (lambda (_)
		(merge-sym f (elem 2 (eval _)))) f)
	(defq s (list))
	(reduce (lambda (x y)
		(push s x)
		(add x (length y) 1)) f 0)
	;create new link sections with offsets to new paths
	(each (lambda (x)
		(defq u (elem _ o))
		(elem-set 1 x (apply cat (push (map (lambda (y)
			(char (add (elem (find y f) s) (sub p u (mul _ 8))) 8)) (elem 2 x)) (char 0 8))))) b)
	;build list of all sections of boot image
	;concatenate all sections and save
	(save (setq f (apply cat (reduce (lambda (x y)
		(push x (cat y (char 0)))) f (push (reduce (lambda (x y)
			(push x (elem 0 y) (elem 1 y))) b (list)) z)))) (func-obj 'sys/boot_image))
	(setq *env* nil)
	(print "image -> " (func-obj 'sys/boot_image) " (" (length f) ")") nil)

(defun make-boot-all ()
	(make-boot nil ((lambda ()
		(defq *imports* (list 'make.inc) *products* (list))
		;lists of all file imports and products
		(each-mergeable (lambda (_)
			(bind '(d p) (make-info _))
			(merge-sym *imports* d)
			(merge-sym *products* p)) *imports*)
		*products*))))

(defun make (&optional *os* *cpu*)
	(compile ((lambda ()
		(defq *env* (env 101) *imports* (list 'make.inc))
		(defun func-obj (_)
			(cat "obj/" _))
		(defun make-sym (_)
			(sym-cat "_dep_" _))
		(defun make-time (_)
			;modification time of a file, cached
			(defq s (sym-cat "_age_" _))
			(or (def? s) (def *env* s (age _))))
		;list of all file imports while defining dependencies and products
		(each-mergeable (lambda (_)
			(defq i (make-info _))
			(bind '(d p) i)
			(merge-sym *imports* d)
			(elem-set 1 i (map func-obj p))
			(def *env* (make-sym _) i)) *imports*)
		;filter to only the .vp files
		(setq *imports* (filter (lambda (_)
			(and (ge (length _) 3) (eql ".vp" (slice -4 -1 _)))) *imports*))
		;filter to only the files who's oldest product is older than any dependency
		(setq *imports* (filter (lambda (_)
			(defq d (eval (make-sym _)) p (reduce min (map make-time (elem 1 d))) d (elem 0 d))
			(each-mergeable (lambda (_)
				(merge-sym d (elem 0 (eval (make-sym _))))) d)
			(some (lambda (_) (ge _ p)) (map make-time d))) *imports*))
		;drop the make environment and return the list to compile
		(setq *env* nil)
		*imports*)) *os* *cpu*))

(defun all-vp-files ()
	(defq *imports* (list 'make.inc))
	;list of all file imports
	(each-mergeable (lambda (_)
		(merge-sym *imports* (elem 0 (make-info _)))) *imports*)
	;filter to only the .vp files
	(filter (lambda (_)
		(and (ge (length _) 3) (eql ".vp" (slice -4 -1 _)))) *imports*))

(defun make-all (&optional *os* *cpu*)
	(compile (all-vp-files) *os* *cpu*)
	(make-boot-all))

(defun remake ()
	(make)
	(make-boot-all))

(defun make-test (&optional i)
	(defun time-in-seconds (_)
		(defq f (str (mod _ 1000000)))
		(cat (str (div _ 1000000)) "." (slice 0 (sub 6 (length f)) "00000") f))
	(defq b 1000000000 a 0 c 0)
	(times (opt i 10)
		(defq _ (time))
		(compile (all-vp-files))
		(setq _ (sub (time) _) a (add a _) c (inc c))
		(print "Time " (time-in-seconds _) " seconds")
		(print "Mean time " (time-in-seconds (div a c)) " seconds")
		(print "Best time " (time-in-seconds (setq b (min b _))) " seconds"))
	nil)

(defun compile-test ()
	(each compile (all-vp-files)))

(defun make-arm ()
	(make-all 'Linux 'ARM))

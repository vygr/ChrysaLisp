;;;;;;;;;
; sign on
;;;;;;;;;

(print "Asm-Kernel Lisp 1.1")
(print "Press ESC/Enter to exit.")

;;;;;;;;;;;;
; Primitives
;;;;;;;;;;;;

(defq list (lambda (&rest _) _))

(defmacro inc (_) `(add ,_ 1))
(defmacro dec (_) `(sub ,_ 1))

(defmacro obj? (_) `(inst-of 'class/class_obj ,_))
(defmacro list? (_) `(inst-of 'class/class_vector ,_))
(defmacro str? (_) `(inst-of 'class/class_string ,_))
(defmacro sym? (_) `(inst-of 'class/class_symbol ,_))
(defmacro num? (_) `(inst-of 'class/class_boxed_long ,_))

(defmacro opt (x y &optional z) (cond (z `(if ,x ,z ,y)) (t `(if ,x ,x ,y))))
(defmacro setd (&rest _)
	(defq i -2 l (list 'setq))
	(while (lt (setq i (add i 2)) (length _))
		(push l (elem i _) `(opt ,(elem i _) ,(elem (inc i) _)))) l)

;;;;;;;;;;;;;
; Definitions
;;;;;;;;;;;;;

(defmacro defun (n a &rest _)
	`(defq ,n (lambda ,a ~_)))

;;;;;;;;
; Scopes
;;;;;;;;

(defmacro let (l &rest _)
	`((lambda ,(map (lambda (_) (elem 0 _)) l) ~_) ~(map (lambda (_) (elem 1 _)) l)))

;;;;;;;;;;;;;;
; Control flow
;;;;;;;;;;;;;;

(defmacro if (x y &rest _)
	(cond ((eq 0 (length _)) `(cond (,x ,y)))
		(t `(cond (,x ,y) (t ~_)))))

(defmacro when (x &rest _)
	`(cond (,x ~_)))

(defmacro unless (x &rest _)
	`(cond ((not ,x) ~_)))

(defmacro until (x &rest _)
	`(while (not ,x) ~_))

(defmacro or (x &rest _)
	(if (eq 0 (length _)) x
		`(if (defq ,(defq _x (gensym)) ,x) ,_x (or ~_))))

(defmacro and (x &rest _)
	(if (eq 0 (length _)) x
		`(if ,x (and ~_) nil)))

(defmacro times (c &rest _)
	`(progn (defq ,(defq _c (gensym)) ,c)
		(while (le 0 (setq ,_c (dec ,_c))) ~_)))

;;;;;;;;;;;;
; Functional
;;;;;;;;;;;;

(defmacro curry (f &rest _)
	`(lambda (&rest _) (apply ,f (cat (list ~_) _))))

(defmacro rcurry (f &rest _)
	`(lambda (&rest _) (apply ,f (cat _ (list ~_)))))

(defmacro compose (&rest _)
	`(lambda (_) ,(reduce (lambda (x y)
		(list y x)) _ '_)))

(defun range (b e &optional s)
	(defq l (list) s (opt s 1 (abs s)))
	(if (le b e)
		(while (le b e)
			(push l b)
			(setq b (add b s)))
		(while (ge b e)
			(push l b)
			(setq b (sub b s)))) l)

(defun min-len (_)
	(defq m (length (elem 0 _)) i 0)
	(while (lt (setq i (inc i)) (length _))
		(defq e (length (elem i _)) m (if (lt m e) m e))) m)

(defun each-mergeable (_f _l)
	(defq _ -1)
	(while (lt (setq _ (inc _)) (length _l))
		(_f (elem _ _l))))

(defun each-mergeable-rev (_f _l)
	(defq _ (length _l))
	(while (ge (setq _ (dec _)) 0)
		(_f (elem _ _l))))

(defun each (_f &rest _b)
	(defq _ -1)
	(cond
		((eq 1 (length _b))
			(defq _b (elem 0 _b) _m (length _b))
			(while (lt (setq _ (inc _)) _m)
				(_f (elem _ _b))))
		((eq 2 (length _b))
			(defq _c (elem 0 _b) _b (elem 1 _b) _m (min (length _b) (length _c)))
			(while (lt (setq _ (inc _)) _m)
				(_f (elem _ _c) (elem _ _b))))
		(t
			(defq _m (min-len _b))
			(while (lt (setq _ (inc _)) _m)
				(defq _a (list) _i -1)
				(while (lt (setq _i (inc _i)) (length _b))
					(push _a (elem _ (elem _i _b))))
				(apply _f _a)))))

(defun each-rev (_f &rest _b)
	(cond
		((eq 1 (length _b))
			(defq _b (elem 0 _b) _ (length _b))
			(while (ge (setq _ (dec _)) 0)
				(_f (elem _ _b))))
		((eq 2 (length _b))
			(defq _c (elem 0 _b) _b (elem 1 _b) _ (min (length _b) (length _c)))
			(while (ge (setq _ (dec _)) 0)
				(_f (elem _ _c) (elem _ _b))))
		(t
			(defq _ (min-len _b))
			(while (ge (setq _ (dec _)) 0)
				(defq _a (list) _i -1)
				(while (lt (setq _i (inc _i)) (length _b))
					(push _a (elem _ (elem _i _b))))
				(apply _f _a)))))

(defun map (_f &rest _b)
	(defq _l (list) _ -1)
	(cond
		((eq 1 (length _b))
			(defq _b (elem 0 _b) _m (length _b))
			(while (lt (setq _ (inc _)) _m)
				(push _l (_f (elem _ _b)))))
		((eq 2 (length _b))
			(defq _c (elem 0 _b) _b (elem 1 _b) _m (min (length _b) (length _c)))
			(while (lt (setq _ (inc _)) _m)
				(push _l (_f (elem _ _c) (elem _ _b)))))
		(t
			(defq _m (min-len _b))
			(while (lt (setq _ (inc _)) _m)
				(defq _a (list) _i -1)
				(while (lt (setq _i (inc _i)) (length _b))
					(push _a (elem _ (elem _i _b))))
				(push _l (apply _f _a))))) _l)

(defun reduce (_f _l &rest _a)
	(if (eq 0 (length _a))
		(defq _ 0 _a (elem 0 _l))
		(defq _ -1 _a (elem 0 _a)))
	(while (lt (setq _ (inc _)) (length _l))
		(setq _a (_f _a (elem _ _l)))) _a)

(defmacro zip (&rest _)
 	`(map list ~_))

(defun filter (_f _l)
	(defq _ -1 _o (list))
	(while (lt (setq _ (inc _)) (length _l))
		(if (_f (defq _i (elem _ _l))) (push _o _i))) _o)

;;;;;;;;;;;;
; Predicates
;;;;;;;;;;;;

(defun some-impl (_f _b)
	(defq _ -1 _v nil)
	(cond
		((eq 1 (length _b))
			(defq _b (elem 0 _b) _m (length _b))
			(while (and (not _v) (lt (setq _ (inc _)) _m))
				(setq _v (_f (elem _ _b)))))
		((eq 2 (length _b))
			(defq _c (elem 0 _b) _b (elem 1 _b) _m (min (length _b) (length _c)))
			(while (and (not _v) (lt (setq _ (inc _)) _m))
				(setq _v (_f (elem _ _c) (elem _ _b)))))
		(t
			(defq _m (min-len _b))
			(while (and (not _v) (lt (setq _ (inc _)) _m))
				(defq _a (list) _i -1)
				(while (lt (setq _i (inc _i)) (length _b))
					(push _a (elem _ (elem _i _b))))
				(setq _v (apply _f _a))))) _v)

(defun every-impl (_f _b)
	(defq _ -1 _v t)
	(cond
		((eq 1 (length _b))
			(defq _b (elem 0 _b) _m (length _b))
			(while (and _v (lt (setq _ (inc _)) _m))
				(setq _v (_f (elem _ _b)))))
		((eq 2 (length _b))
			(defq _c (elem 0 _b) _b (elem 1 _b) _m (min (length _b) (length _c)))
			(while (and _v (lt (setq _ (inc _)) _m))
				(setq _v (_f (elem _ _c) (elem _ _b)))))
		(t
			(defq _m (min-len _b))
			(while (and _v (lt (setq _ (inc _)) _m))
				(defq _a (list) _i -1)
				(while (lt (setq _i (inc _i)) (length _b))
					(push _a (elem _ (elem _i _b))))
				(setq _v (apply _f _a))))) _v)

(defun some (_f &rest _b) (some-impl _f _b))
(defun every (_f &rest _b) (every-impl _f _b))
(defun notany (_f &rest _b) (not (some-impl _f _b)))
(defun notevery (_f &rest _b) (not (every-impl _f _b)))

;;;;;;;;;;;;
; Comparison
;;;;;;;;;;;;

(defun equalp (x y)
	(eql (str x) (str y)))

;;;;;;;;;;;;;;;;
; Math functions
;;;;;;;;;;;;;;;;

(defmacro minus (_)
	(neg _))

(defun neg (_)
	(sub 0 _))

(defun abs (_)
	(if (lt _ 0) (neg _) _))

(defun min (x y)
	(if (lt x y) x y))

(defun max (x y)
	(if (lt x y) y x))

(defun squared (_)
	(mul _ _))

(defun cubed (_)
	(mul _ _ _))

(defun divmod (x y)
	(list (div x y) (mod x y)))

(defun bit-not (_)
	(bit-xor _ -1))

;;;;;;;;;
; Streams
;;;;;;;;;

(defun each-line (_f _b)
	(defq _s (file-stream _f))
	(while (defq _l (read-line _s))
		(_b _l)))

(defun print-file (_)
	(each-line _ print))

;;;;;;;;;;;
; Utilities
;;;;;;;;;;;

(defun align (x a)
	(bit-and (add x (dec a)) (neg a)))

(defmacro ascii (_)
	(code _))

(defun to-base-char (_)
	(elem _ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defun from-base-char (_)
	(setq _ (code _))
	(cond
		((ge _ (ascii "a"))
			(sub _ (ascii "a") -10))
		((ge _ (ascii "A"))
			(sub _ (ascii "A") -10))
		(t (sub _ (ascii "0")))))

(defun prin-base (x b j)
	(defun prin-b (x j)
		(if (or (ne j 1) (ne 0 (div x b)))
			(prin-b (div x b) (sub j 1)))
		(prin (to-base-char (mod x b))))
	(prin-b x j))

(defun trim-start (s &optional c)
	(setd c " ")
	(while (and (ne 0 (length s)) (eql (elem 0 s) c))
		(setq s (slice 1 -1 s))) s)

(defun trim-end (s &optional c)
	(setd c " ")
	(while (and (ne 0 (length s)) (eql (elem -2 s) c))
		(setq s (slice 0 -2 s))) s)

(defun trim (s &optional c)
	(trim-start (trim-end s c) c))

(defun to-num (_)
	(defq n 0 b 10)
	(when (gt (length _) 1)
		(defq i (elem 1 _))
		(cond
			((eql i "x")
				(setq b 16 _ (slice 2 -1 _)))
			((eql i "o")
				(setq b 8 _ (slice 2 -1 _)))
			((eql i "b")
				(setq b 2 _ (slice 2 -1 _)))))
	(defq i -1)
	(while (lt (setq i (inc i)) (length _))
		(setq n (add (mul n b) (from-base-char (elem i _))))) n)

(defun match-list? (x y)
	(when (eq (defq i (length x)) (length y))
		(while (and (ge (setq i (dec i)) 0)
					(or (eql '_ (elem i y)) (eql (elem i x) (elem i y)))))
		(lt i 0)))

(defun pow2 (_)
	(defq i -1 b nil)
	(while (and (not b) (lt (setq i (inc i)) 64))
		(if (eq _ (bit-shl 1 i)) (setq b i))) b)

(defun insert (x y)
	(when (notany (lambda (x) (eql x y)) x)
		(push x y)))

(defun merge (x y)
	(each (lambda (y)
		(when (notany (lambda (x) (eql x y)) x)
			(push x y))) y))

(defun insert-sym (x y)
	(unless (find y x) (push x y)))

(defun merge-sym (x y)
	(each (lambda (y)
		(unless (find y x) (push x y))) y))

(defmacro sym-cat (&rest _)
	`(sym (cat ~_)))

(defun every-pipe-line (_f _p)
	(defq _d "" _v t)
	(while (and _p _v)
		(defq _d (cat _d (pipe-read _p)) _i (find (char 10) _d))
		(when _i
			(defq _i (inc _i) _l (slice 0 _i _d) _d (slice _i -1 _d)
				_v (every _f (split _l (char 10)))))))

(defun shuffle (l)
	(defq l (cat l) s (time))
	(defun get-next ()
		(setq s (abs (bit-xor 0xa5a5a5a5a5a5a5a5 (mul s 0x1574937f)))))
	(defun get-int (r)
		(if (gt r 0) (mod (get-next) r) 0))
	(each-rev (lambda (x)
		(defq i (get-int (inc _)) y (elem i l))
		(elem-set i l x)
		(elem-set _ l y)) l) l)

;;;;;;;;;;;;;;
; VP Assembler
;;;;;;;;;;;;;;

(defun platform ()
	(defq o 'Darwin)
	(when (defq f (file-stream 'platform))
		(setq o (sym (read-line f)))) o)

(defun cpu ()
	(defq o 'x86_64)
	(when (defq f (file-stream 'arch))
		(setq o (sym (read-line f)))) o)

(defun compile (*files* &optional *os* *cpu* *pipes*)
	(setd *os* (platform) *cpu* (cpu) *pipes* 1)
	(defq q (list) e (list))
	(unless (list? *files*)
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
	(defq *compile-env* (env 101) *imports* (list))
	(defmacro defcvar (&rest b)
		`(def *compile-env* ~b))
	(defmacro defcfun (n a &rest b)
		`(def *compile-env* ',n (lambda ,a ~b)))
	(defmacro defcmacro (n a &rest b)
		`(def *compile-env* ',n (macro ,a ~b)))
	(defun import (*file*)
		(unless (find *file* *imports*)
			(push *imports* *file*)
			(repl (file-stream *file*) *file*)))
	(each import *files*)
	(print "Done")
	(setq *compile-env* nil))

(defun make-info (f)
	;create lists of immediate dependencies and products
	(defq d (list 'cmd/lisp.lisp f) p (list))
	(each-line f (lambda (l)
		(when (le 2 (length (defq s (split l " "))) 3)
			(defq k (elem 0 s) o (sym (trim-start (trim-end (elem 1 s) ")") "'")))
			(cond
				((eql k "(import")
					(push d o))
				((eql k "(gen-class")
					(push p (sym-cat "class/class_" o)))
				((eql k "(gen-new")
					(push p (sym-cat "class/" o "/new")))
				((eql k "(gen-create")
					(push p (sym-cat "class/" o "/create")))
				((eql k "(def-func")
					(push p o))))))
	(list d p))

(defun make-boot (&optional r *funcs*)
	(setd *funcs* (list))
	(defq *env* (env 101) z (cat (char 0 8) (char 0 8)))
	(defun func-obj (f)
		(cat "obj/" f))
	(defun load-func (f)
		(or (val? f)
			(progn
				(defq b (load (func-obj f))
					h (slice fn_header_entry (defq l (read-int fn_header_links b)) b)
					l (slice l (defq p (read-int fn_header_paths b)) b))
				(def *env* f (list (cat (char -1 8) (char p 4) h) l (read-paths b))))))
	(defun read-byte (o f)
		(code (elem o f)))
	(defun read-short (o f)
		(add (read-byte o f) (bit-shl (read-byte (inc o) f) 8)))
	(defun read-int (o f)
		(add (read-short o f) (bit-shl (read-short (add o 2) f) 16)))
	(defun read-long (o f)
		(add (read-int o f) (bit-shl (read-int (add o 4) f) 32)))
	(defun read-paths (f)
		(defq l (list) i (read-int fn_header_links f))
		(while (ne 0 (defq p (read-long i f)))
			(defq j (add p i) k j)
			(while (ne 0 (read-byte j f))
				(setq j (inc j)))
			(push l (sym (slice k j f)))
			(setq i (add i 8))) l)
	(unless (list? *funcs*)
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
		(each-mergeable (lambda (i)
			(merge-sym f (elem 2 (load-func i)))) f))
	;list of all function bodies and links in order, list of offsets of link sections, offset of new path section
	(defq b (map eval f) o (list) p (add (length z) (reduce (lambda (x y)
		(setq x (add x (length (elem 0 y))))
		(push o x)
		(add x (length (elem 1 y)))) b 0)))
	;list of all function names that will appear in new path section, and list of all new path offsets
	(each-mergeable-rev (lambda (i)
		(merge-sym f (elem 2 (eval i)))) f)
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
		(each-mergeable (lambda (i)
			(defq d (make-info i))
			(merge-sym *imports* (elem 0 d))
			(merge-sym *products* (elem 1 d))) *imports*)
		*products*))))

(defun make (&optional *os* *cpu*)
	(compile ((lambda ()
		(defq *env* (env 101) *imports* (list 'make.inc))
		(defun func-obj (f)
			(cat "obj/" f))
		(defun make-sym (f)
			(sym-cat "_dep_" f))
		(defun make-time (f)
			;modification time of a file, cached
			(defq s (sym-cat "_age_" f))
			(or (val? s) (def *env* s (age f))))
		;list of all file imports while defining dependencies and products
		(each-mergeable (lambda (i)
			(defq d (make-info i))
			(merge-sym *imports* (elem 0 d))
			(elem-set 1 d (map func-obj (elem 1 d)))
			(def *env* (make-sym i) d)) *imports*)
		;filter to only the .vp files
		(setq *imports* (filter (lambda (f)
			(and (ge (length f) 3) (eql ".vp" (slice -4 -1 f)))) *imports*))
		;filter to only the files who's oldest product is older than any dependency
		(setq *imports* (filter (lambda (f)
			(defq d (eval (make-sym f)) p (reduce min (map make-time (elem 1 d))) d (elem 0 d))
			(each-mergeable (lambda (i)
				(merge-sym d (elem 0 (eval (make-sym i))))) d)
			(some (lambda (x) (ge x p)) (map make-time d))) *imports*))
		;drop the make environment and return the list to compile
		(setq *env* nil)
		*imports*)) *os* *cpu* 8))

(defun make-all (&optional *os* *cpu*)
	(defq n (time))
	(compile ((lambda ()
		(defq *imports* (list 'make.inc))
		;list of all file imports
		(each-mergeable (lambda (i)
			(defq d (make-info i))
			(merge-sym *imports* (elem 0 d))) *imports*)
		;filter to only the .vp files
		(setq *imports* (filter (lambda (f)
			(and (ge (length f) 3) (eql ".vp" (slice -4 -1 f)))) *imports*))
		*imports*)) *os* *cpu* 8)
	(make-boot-all)
	(setq n (div (sub (time) n) 10000))
	(print "Time " (div n 100) "." (mod n 100) " seconds") n)

(defun make-test (&optional i)
	(defq b 1000000 n 0)
	(times (opt i 10) (setq n (make-all) b (if (lt n b) n b)))
	(print "Best time " (div b 100) "." (mod b 100) " seconds") nil)

;test code for OOPS stuff
;issues with when to evaluate args to scope !

(defmacro constructor (n a &rest b)
	`(defun ,n ,a ((lambda () ~b (env)))))

(defmacro scope-progn (o &rest b)
	`(scope ,o ((lambda () ~b))))

(constructor make-button (x y w h)
	(defq m_x x m_y y m_w w m_h h)
	(defun get-pos ()
		(list m_x m_y))
	(defun set-pos (x y)
		(setq m_x x m_y y))
	(defun set-size (w h)
		(setq m_w w m_h h))
	(defun get-size ()
		(list m_w m_h)))

(defq button (make-button 0 0 256 32))
;(scope-progn button (print (get-pos)) (print (get-size)))
(scope-progn button (set-pos 50 100) (set-size 100 120))
;(scope-progn button (print (get-pos)) (print (get-size)))
(setq button nil)

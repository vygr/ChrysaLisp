;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/motion/iframe.inc - a scene written to a stream, and read from one
;
; (iframe-sent) -> sent
; (iframe-write stream rostrum sent) -> stream
; (iframe-outlines stream outlines) -> outlines
; (iframe-read stream outlines) -> rostrum
;
; An I-frame is a scene as it is at a step, to go on from: (. rostrum
; :copy) makes one in the task that has the scene. These make one a task
; on another node can have: the scene as bytes on a stream, any stream,
; and a scene made of them that goes on as the first would have. So a
; task can run a scene ahead, to find its length and to keep the places
; a seek starts from, and give them to the task that shows it.
;
; What is written is what a copy makes anew, each figure, component, map
; and list once, by a number, and what they hold: numbers, symbols,
; strs, arrays of numbers, each other. A function is written as its
; number among the functions of the task, (iframe-functions): the writer
; and the reader are tasks of the same code, so the numbers are the
; same. What a figure works out and keeps, its flattening, is not
; written, it is worked out again.
;
; An outline, the lines of a figure, is most of a scene's bytes, and is
; never changed in place: each is written once, the first time any
; I-frame of a writer has it, by its number after. sent is what a writer
; has written so, kept for all the I-frames of the one scene, and
; outlines is the reader's list of them, to be given every I-frame of
; that writer in the order they were written, (iframe-outlines), though
; the scene of one, (iframe-read), need only be read when it is wanted.
;
; On the stream an I-frame is a few arrays of numbers and a str: the
; outlines new with it, then the scene, its shape as a nums of tags and
; counts, its fixed numbers as one fixeds, and its arrays joined, each
; kind in one. So it is written and read in a few calls, by
; lib/streams/nums.inc, and no number is boxed to cross.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./rostrum.inc")
(import "././streams/nums.inc")

(enums +iframe_kind 0
	(enum rostrum figure comp fmap list))

(enums +iframe_tag 0
	(enum nil t int fixed str sym symref obj func array outline weak))

(enums +iframe_array 0
	(enum path fixeds nums))

;the env this is imported into, the task's: its functions are those a
;scene can hold. A weak link to it: an env that holds itself is never let
;go of, and its task does not end. And what of a figure or a component is
;not written: its class, what it works out and keeps, and its link up,
;made again
(defq *iframe_env* (weak-ref (env)) *iframe_funcs* :nil *iframe_news* :nil
	+iframe_skip ''(:vtable :fig_flat :fig_glow_flat :node_parent))

(defun iframe-lambda? (x)
	; (iframe-lambda? form) -> :t | :nil
	;a function of Lisp, a list that begins lambda
	(and (list? x) (not (env? x)) (nempty? x) (lambda? (first x))))

(defun iframe-functions ()
	; (iframe-functions) -> (map table)
	;every function of Lisp a scene of this task can hold, and its number:
	;those with a name in the task's env, in the order of their names,
	;each followed by those written inside it, in the order they are come
	;to. One named in another's code is that one, and is not gone into.
	;map is from a function, (weak-ref), to its number, table is them by
	;number. Made once
	(unless *iframe_funcs*
		(defq named (Fset 4093) seen (Fset 16381) map (Fmap 4093) table (list) names (list))
		(each (lambda (e)
				(each (lambda ((k v)) (if (iframe-lambda? v) (. named :insert (weak-ref v))))
					(tolist e)))
			(list *root_env* (obj-ref *iframe_env*)))
		(each (lambda ((k v)) (if (iframe-lambda? v) (push names (list (str k) v))))
			(tolist (obj-ref *iframe_env*)))
		(each (lambda ((& v))
				(unless (. map :find (weak-ref v))
					(. map :insert (weak-ref v) (length table))
					(push table v)
					(defq todo (list v))
					(while (defq x (pop todo))
						(each (lambda (e)
								(when (and (list? e) (not (env? e)) (nempty? e)
										(not (. named :find (defq key (weak-ref e))))
										(not (. seen :find key)))
									(. seen :insert key)
									(when (iframe-lambda? e)
										(. map :insert key (length table))
										(push table e))
									(push todo e)))
							x))))
			(sort names (lambda (a b) (cmp (first a) (first b)))))
		(setq *iframe_funcs* (list map table)))
	*iframe_funcs*)

(defun iframe-sent ()
	; (iframe-sent) -> sent
	;what a writer has written of a scene's outlines, none yet: each by
	;which it is, (weak-ref), those written, by number, those of a like
	;look, to find one of the same lines made again, and all that have been
	;met, held so that none is let go of and its place taken by another
	(list (Fmap 1021) (list) (Fmap 1021) (list)))

(defun iframe-news ()
	; (iframe-news) -> (rostrum figure)
	;one of each as it is made, to know what of a scene's is as it was
	;made and need not be written: a reader makes them so
	(unless *iframe_news* (setq *iframe_news* (list (Rostrum 1 1) (Figure))))
	*iframe_news*)

(defun iframe-as-made? (new k v)
	; (iframe-as-made? new k v) -> :t | :nil
	;is what a figure has for a key what a new one has: nothing, the same
	;number or symbol, or a list with nothing in it, the new one's own
	(defq was (def? k new))
	(cond
		((not v) (not was))
		((list? v) (and (list? was) (not (env? v)) (empty? v) (empty? was)))
		((or (num? v) (sym? v)) (and was (not (list? was)) (eql v was)))))

(defun iframe-kept (x)
	; (iframe-kept env) -> ((key val) ...)
	;what of a figure or a component is written: not what is made again or
	;found by the reader, and not what a new figure has as it is made
	(defq new (cond
		((def? :ros_ticks x) (first (iframe-news)))
		((def? :fig_comps x) (second (iframe-news)))))
	(filter (lambda ((k v))
			(not (or (find k +iframe_skip) (and new (iframe-as-made? new k v)))))
		(tolist x)))

(defun iframe-look (v)
	; (iframe-look outline) -> num
	;a number that two outlines of the same lines have the same, from how
	;many lines and points it has and where it begins and ends
	(defq n 0 a 0 b 0)
	(each (lambda ((closed p))
			(setq n (+ n (length p)))
			(when (nempty? p)
				(if (= a 0) (setq a (n2i (* 256.0 (first p)))))
				(setq b (n2i (* 256.0 (last p))))))
		v)
	(logxor (<< (length v) 40) (<< n 20) (<< a 8) b))

(defun iframe-same? (a b)
	; (iframe-same? outline outline) -> :t | :nil
	;the same lines, each closed or not as the other and of the same points
	(and (= (length a) (length b))
		(not (some (lambda ((ca pa) (cb pb)) (not (and (eql ca cb) (eql pa pb)))) a b))))

(defun iframe-kind (x)
	; (iframe-kind x) -> +iframe_kind
	;what a thing a scene holds is made as
	(case (rostrum-kind x)
		(:env (cond
			((def? :ros_ticks x) +iframe_kind_rostrum)
			((def? :fig_comps x) +iframe_kind_figure)
			(+iframe_kind_comp)))
		(:fmap +iframe_kind_fmap)
		(:t +iframe_kind_list)))

(defun iframe-morphs? (x)
	; (iframe-morphs? env) -> :t | :nil
	;a component that takes an outline to another: its 'to and 'from are
	;outlines
	(eql (def? 'key x) :fig_outline))

;;;;;;;;;;;;;;;
; the writer
;;;;;;;;;;;;;;;

;these write into what (iframe-write) has: ints, fixs, text, the arrays
;of each kind, syms, ids, funcs, and the outlines new with this I-frame

(defun iframe-put-sym (k)
	;a symbol, its name the first time, its number after
	(cond
		((defq n (. syms :find k)) (push ints +iframe_tag_symref n))
		(:t (. syms :insert k (. syms :size))
			(defq name (str k))
			(push ints +iframe_tag_sym (length name))
			(push text name))))

(defun iframe-put-outline (v)
	;an outline, (closed path) each line: all of it the first time it is
	;met, in any I-frame of this writer, its number after. One made again
	;of the same lines, the same words said twice, is the one written
	(unless (defq id (. sent_map :find (defq key (weak-ref v))))
		(defq look (iframe-look v) like (ifn (. sent_like :find look) (list)))
		(unless (setq id (some (# (if (iframe-same? v (elem-get sent_keep %0)) %0)) like))
			(setq id (length sent_keep))
			(push sent_keep v)
			(. sent_like :insert look (push like id))
			(push defs id (length v))
			(each (lambda ((closed p))
					(push defs (if closed 1 0) (length p))
					(push def_paths p))
				v))
		(. sent_map :insert key id)
		(push sent_held v))
	(push ints +iframe_tag_outline id))

(defun iframe-put (v)
	;a value: nothing, a number, a symbol, a str, an array of numbers, a
	;function, or one of the things of the scene by its number
	(cond
		((not v) (push ints +iframe_tag_nil))
		((eql v :t) (push ints +iframe_tag_t))
		((rostrum-kind v) (push ints +iframe_tag_obj (. ids :find (weak-ref v))))
		((or (func? v) (list? v))
			(unless (defq n (. funcs :find (weak-ref v)))
				(throw "iframe-write: a function with no number !" v))
			(push ints +iframe_tag_func n))
		((sym? v) (iframe-put-sym v))
		((str? v) (push ints +iframe_tag_str (length v)) (push text v))
		((array? v)
			(cond
				((or (doubles? v) (floats? v)) (throw "iframe-write: an array it has no kind for !" v))
				((find :path (type-of v)) (push ints +iframe_tag_array +iframe_array_path (length v)) (push paths v))
				((fixeds? v) (push ints +iframe_tag_array +iframe_array_fixeds (length v)) (push fixeds_all v))
				(:t (push ints +iframe_tag_array +iframe_array_nums (length v)) (push nums_all v))))
		((or (double? v) (float? v)) (throw "iframe-write: a number it has no kind for !" v))
		((fixed? v) (push ints +iframe_tag_fixed) (push fixs v))
		((num? v) (push ints +iframe_tag_int v))
		((throw "iframe-write: a thing it can not write !" v))))

(defun iframe-put-env (x kind)
	;a figure's or a component's keys and what each is. An outline by its
	;number, a component's weak link to a figure by the figure's
	(defq pairs (iframe-kept x) morphs (iframe-morphs? x))
	(push ints (length pairs))
	(each (lambda ((k v))
			(iframe-put-sym k)
			(cond
				((and (list? v) (or (eql k :fig_outline) (and morphs (find k '(to from)))))
					(iframe-put-outline v))
				((and (= kind +iframe_kind_comp) (eql k 'other) (num? v))
					(push ints +iframe_tag_weak (ifn (. ids :find v) -1)))
				((iframe-put v))))
		pairs))

(defun iframe-join (parts empty)
	; (iframe-join parts empty) -> array
	;arrays of one kind made one
	(if (empty? parts) empty (apply (const cat) parts)))

(defun iframe-out (stream array)
	;an array, how many elements then the elements
	(write-long stream (length array))
	(write-nums stream array))

(defun iframe-write (stream this sent)
	; (iframe-write stream rostrum sent) -> stream
	;the scene as it is now, on the stream, with the outlines no I-frame of
	;this writer has had, sent, before it
	(bind '(funcs &ignore) (iframe-functions))
	(bind '(sent_map sent_keep sent_like sent_held) sent)
	;each thing it holds, once, in the order a copy comes to them
	(defq ids (Fmap 1021) olds (list) todo (list this))
	(while (defq x (pop todo))
		(unless (. ids :find (defq key (weak-ref x)))
			(. ids :insert key (length olds))
			(push olds x)
			(case (rostrum-kind x)
				(:env (defq morphs (iframe-morphs? x))
					(each (lambda ((k v))
							(and (not (eql k :fig_outline))
								(not (and morphs (find k '(to from))))
								(rostrum-kind v) (push todo v)))
						(iframe-kept x)))
				(:fmap (. x :each (lambda (k v) (if (rostrum-kind v) (push todo v)))))
				(:t (each (# (if (rostrum-kind %0) (push todo %0))) x)))
			;a scene of many figures is long to go through
			(task-slice)))
	(defq ints (nums) fixs (fixeds) text (list) paths (list) fixeds_all (list) nums_all (list)
		syms (Fmap 101) defs (nums) def_paths (list))
	;how many things, what each is made as, a map with how many buckets
	(push ints (length olds))
	(each (lambda (x)
			(defq kind (iframe-kind x))
			(push ints kind (if (= kind +iframe_kind_fmap) (get :num_buckets x) 0)))
		olds)
	;then what each holds
	(each (lambda (x)
			(defq kind (iframe-kind x))
			(cond
				((<= kind +iframe_kind_comp) (iframe-put-env x kind))
				((= kind +iframe_kind_fmap)
					(defq pairs (list))
					(. x :each (lambda (k v) (push pairs k v)))
					(push ints (/ (length pairs) 2))
					(each (const iframe-put) pairs))
				(:t (push ints (length x))
					(each (const iframe-put) x)))
			(task-slice))
		olds)
	(defq all_text (apply (const cat) (cat (list "") text)))
	(iframe-out stream defs)
	(iframe-out stream (iframe-join def_paths (path)))
	(iframe-out stream ints)
	(iframe-out stream fixs)
	(iframe-out stream (iframe-join paths (path)))
	(iframe-out stream (iframe-join fixeds_all (fixeds)))
	(iframe-out stream (iframe-join nums_all (nums)))
	(write-long stream (length all_text))
	(write-blk stream all_text)
	stream)

;;;;;;;;;;;;;;;
; the reader
;;;;;;;;;;;;;;;

(defun iframe-in (stream array)
	; (iframe-in stream array) -> array
	;an array (iframe-out) wrote, onto the end of the one given
	(read-nums stream (* (read-long stream) (if (floats? array) +int_size +long_size)) array))

(defun iframe-outlines (stream outlines)
	; (iframe-outlines stream outlines) -> outlines
	;the outlines new with the I-frame the stream is at, each put in the
	;list at its number. The stream is left at its scene, (iframe-read)
	(defq defs (iframe-in stream (nums)) data (iframe-in stream (path)) i 0 at 0)
	(while (< i (length defs))
		(defq id (elem-get defs i) parts (elem-get defs (inc i)) outline (list))
		(setq i (+ i 2))
		(times parts
			(defq n (elem-get defs (inc i)))
			(push outline (list (/= 0 (elem-get defs i)) (slice data at (+ at n))))
			(setq i (+ i 2) at (+ at n)))
		(while (<= (length outlines) id) (push outlines :nil))
		(elem-set outlines id outline))
	outlines)

;these read from what (iframe-read) has: ints and where it is in them, i,
;fixs and f, the arrays of each kind and how far each is read, text and
;tp, syms, objs, funcs and outlines

(defun iframe-int ()
	; (iframe-int) -> num
	(setq i (inc i))
	(elem-get ints (dec i)))

(defun iframe-take ()
	; (iframe-take) -> value
	;the next value, as (iframe-put) wrote it
	(defq tag (iframe-int))
	(cond
		((= tag +iframe_tag_nil) :nil)
		((= tag +iframe_tag_t) :t)
		((= tag +iframe_tag_int) (iframe-int))
		((= tag +iframe_tag_fixed) (setq f (inc f)) (elem-get fixs (dec f)))
		((= tag +iframe_tag_str) (defq n (iframe-int))
			(setq tp (+ tp n))
			(slice text (- tp n) tp))
		((= tag +iframe_tag_sym) (defq n (iframe-int))
			(setq tp (+ tp n))
			(push syms (defq s (sym (slice text (- tp n) tp))))
			s)
		((= tag +iframe_tag_symref) (elem-get syms (iframe-int)))
		((= tag +iframe_tag_obj) (elem-get objs (iframe-int)))
		((= tag +iframe_tag_func) (elem-get funcs (iframe-int)))
		((= tag +iframe_tag_array) (defq kind (iframe-int) n (iframe-int))
			(cond
				((= kind +iframe_array_path) (setq pp (+ pp n)) (slice paths (- pp n) pp))
				((= kind +iframe_array_fixeds) (setq fp (+ fp n)) (slice fixeds_all (- fp n) fp))
				(:t (setq np (+ np n)) (slice nums_all (- np n) np))))
		((= tag +iframe_tag_outline) (elem-get outlines (iframe-int)))
		((= tag +iframe_tag_weak)
			(if (>= (defq id (iframe-int)) 0) (weak-ref (elem-get objs id)) 0))
		((throw "iframe-read: no such tag !" tag))))

(defun iframe-read (stream outlines)
	; (iframe-read stream outlines) -> rostrum
	;the scene of the I-frame the stream is at, its outlines read already,
	;(iframe-outlines), and those of every I-frame before it
	(bind '(& funcs) (iframe-functions))
	(defq ints (iframe-in stream (nums)) fixs (iframe-in stream (fixeds))
		paths (iframe-in stream (path)) fixeds_all (iframe-in stream (fixeds))
		nums_all (iframe-in stream (nums))
		text (if (> (defq n (read-long stream)) 0) (read-blk stream n) "")
		i 0 f 0 tp 0 pp 0 fp 0 np 0 syms (list) objs (list) kinds (list))
	;each thing made, as what it is, then what each holds
	(times (iframe-int)
		(defq kind (iframe-int) buckets (iframe-int))
		(push kinds kind)
		(push objs (cond
			((= kind +iframe_kind_rostrum) (Rostrum 1 1))
			((= kind +iframe_kind_figure) (Figure))
			((= kind +iframe_kind_comp) (env 1))
			((= kind +iframe_kind_fmap) (Fmap buckets))
			((list)))))
	(each (lambda (x kind)
			(cond
				((<= kind +iframe_kind_comp)
					(times (iframe-int)
						(defq k (iframe-take))
						(def x k (iframe-take))))
				((= kind +iframe_kind_fmap)
					(times (iframe-int)
						(defq k (iframe-take))
						(. x :insert k (iframe-take))))
				(:t (times (iframe-int) (push x (iframe-take)))))
			(task-slice))
		objs kinds)
	;each child's weak link up is to its parent
	(each (lambda (x kind)
			(if (<= kind +iframe_kind_figure)
				(each (# (. %0 :set_parent x)) (get :node_children x))))
		objs kinds)
	(first objs))

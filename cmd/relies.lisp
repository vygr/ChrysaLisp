(import "lib/options/options.inc")
(import "lib/files/files.inc")
(import "lib/files/info.inc")
(import "lib/asm/assembled.inc")

(defq usage `(
(("-h" "--help")
"Usage: relies [options] [function] ...

    options:
        -h --help: this help info.
        -v --verbose: every place, not only the sums by
            what is called.
        -q --quiet: every place and no more, nothing if
            there is none. For lint.
        -s --saves: the other way round: where a register is
            put by before a call and brought back after it,
            pushed and popped, or written to the stack and
            read back, and the call is not declared to trash
            it. Such a save was needed when what is called
            trashed more, and is for nothing now.

    Where a function goes by more than is declared of what it
    calls: a register it reads after a call that is declared
    to trash it, and does not give it.

    What a method trashes is in its declaration, and lint
    says that is what the code may do. It is all of the ways
    through the method. Code that calls it may know which way
    it will go, a push to an array that was just given the
    room, and count on a register the short way leaves. That
    works, and nothing checks it: what is called can be
    changed inside what it declares, and what calls it is then
    wrong with nothing to say so.

    This reads each function as it was assembled, the debug
    VP a build keeps, obj/vp/debug/, and works out which registers are still wanted
    after each instruction, backwards from where it ends, by
    what each instruction reads and writes. A call reads what
    its method takes and writes what it gives. A register
    wanted after a call, that the call is declared to trash
    and does not give, is such a place.

    Two things called are left out. What does not come back,
    :sys_pii :exit. And a walk that calls back to what called
    it, :view :forward_tree: it is declared to trash every
    register, it can not know what it is given to call, and
    what gives it the callbacks keeps in registers what they
    work on. That is its own affair and not the walk's.

    To be rid of one, the long way through what is called
    puts by the registers it uses and brings them back, and
    then it is declared to trash less, 'trace -w'.

    With no function named, all there are.")
(("-v" "--verbose") ,(opt-flag 'opt_v))
(("-q" "--quiet") ,(opt-flag 'opt_q))
(("-s" "--saves") ,(opt-flag 'opt_s))
))

(defq +regs ''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14)
	;an instruction of two registers that reads both and writes the second
	+both_ops ''(emit-add-rr emit-sub-rr emit-mul-rr emit-and-rr emit-or-rr emit-xor-rr emit-shl-rr emit-shr-rr
		emit-asr-rr emit-min-rr emit-max-rr emit-seq-rr emit-sne-rr emit-slt-rr emit-sgt-rr emit-sle-rr emit-sge-rr)
	;that reads both and writes both
	+swap_ops ''(emit-swp-rr emit-lnot-rr emit-land-rr)
	;that reads the first and writes the second
	+move_ops ''(emit-cpy-rr emit-ext-rr emit-abs-rr)
	;of a constant and a register, read and written
	+const_ops ''(emit-add-cr emit-sub-cr emit-mul-cr emit-and-cr emit-or-cr emit-xor-cr emit-shl-cr emit-shr-cr
		emit-asr-cr emit-min-cr emit-max-cr emit-seq-cr emit-sne-cr emit-slt-cr emit-sgt-cr emit-sle-cr emit-sge-cr)
	+branch_cr_ops ''(emit-beq-cr emit-bne-cr emit-blt-cr emit-bgt-cr emit-ble-cr emit-bge-cr)
	+branch_rr_ops ''(emit-beq-rr emit-bne-rr emit-blt-rr emit-bgt-rr emit-ble-rr emit-bge-rr)
	+get_ops ''(emit-cpy-ir emit-cpy-ir-b emit-cpy-ir-s emit-cpy-ir-i emit-cpy-ir-ub emit-cpy-ir-us emit-cpy-ir-ui emit-lea-i)
	+put_ops ''(emit-cpy-ri emit-cpy-ri-b emit-cpy-ri-s emit-cpy-ri-i)
	+get2_ops ''(emit-cpy-dr emit-cpy-dr-b emit-cpy-dr-s emit-cpy-dr-i emit-cpy-dr-ub emit-cpy-dr-us emit-cpy-dr-ui emit-lea-d)
	+put2_ops ''(emit-cpy-rd emit-cpy-rd-b emit-cpy-rd-s emit-cpy-rd-i)
	;nothing of a general register in them
	+none_ops ''(emit-label emit-tlabel emit-align emit-string emit-byte emit-short emit-int emit-long
		emit-alloc emit-free emit-ret emit-sync emit-brk emit-nop emit-vp-nop emit-jmp
		emit-cpy-ff emit-add-ff emit-sub-ff emit-mul-ff emit-div-ff emit-min-ff emit-max-ff emit-sqrt-ff
		emit-abs-ff emit-neg-ff emit-beq-ff emit-bne-ff emit-blt-ff emit-bgt-ff emit-ble-ff emit-bge-ff))

(defun bit-of (reg)
	; (bit-of reg) -> num
	;a general register as a bit, 0 if it is not one
	(if (and (sym? reg) (defq i (find reg +regs))) (<< 1 i) 0))

(defun bits-of (regs)
	; (bits-of regs) -> num
	;registers, or their numbers, as bits
	(reduce (# (logior %0 (if (num? %1) (<< 1 %1) (bit-of %1)))) regs 0))

(defun regs-in (mask)
	; (regs-in mask) -> (reg ...)
	(filter (# (/= 0 (logand mask (bit-of %0)))) +regs))

(defun flow-of (inst)
	; (flow-of inst) -> (reads writes)
	;the general registers an instruction reads and those it writes, as
	;bits. Not of a call or a jump out, those are by what is called. One
	;that is not known here is taken to read every register in it and
	;write none: more is then wanted than is, never less
	(defq op (first inst) a (bit-of (second inst)) n (length inst)
		b (if (> n 2) (bit-of (third inst)) 0) c (if (> n 3) (bit-of (elem-get inst 3)) 0))
	(cond
		((find op +none_ops) (list 0 0))
		;a register taken from itself, or xor with itself, is made 0: nothing is read
		((and (find op '(emit-xor-rr emit-sub-rr)) (= a b)) (list 0 b))
		((find op +both_ops) (list (logior a b) b))
		((find op +swap_ops) (list (logior a b) (logior a b)))
		((find op +move_ops) (list a b))
		((find op +const_ops) (list b b))
		((eql op 'emit-cpy-cr) (list 0 b))
		((find op '(emit-div-rrr emit-div-rrr-u)) (list (logior a b c) (logior b c)))
		((find op +branch_cr_ops) (list b 0))
		((find op +branch_rr_ops) (list (logior a b) 0))
		((find op +get_ops) (list a c))
		((find op +put_ops) (list (logior a b) 0))
		((find op +get2_ops) (list (logior a b) c))
		((find op +put2_ops) (list (logior a b c) 0))
		((find op '(emit-cpy-pr emit-lea-p)) (list 0 b))
		;to and from the float registers, and floats in memory
		((find op '(emit-cpy-rf emit-cvt-rf emit-cpy-if)) (list a 0))
		((find op '(emit-cpy-fr emit-cvt-fr)) (list 0 b))
		((eql op 'emit-cpy-fi) (list b 0))
		((eql op 'emit-cpy-df) (list (logior a b) 0))
		((eql op 'emit-cpy-fd) (list (logior b c) 0))
		((eql op 'emit-push) (list (bits-of (rest inst)) 0))
		((find op '(emit-pop emit-trash)) (list 0 (bits-of (rest inst))))
		((list (bits-of (rest inst)) 0))))

(defun declared-of (class_db class method key)
	; (declared-of class_db class method key) -> num
	;what a method takes, :dec_inputs, or gives, :dec_outputs, as bits:
	;as its class declares it, or the nearest class above that says
	;
	;If none of them says, what the comment of the function that is the
	;method says: a create is declared with neither, and its comment has
	;what it gives
	(defq said "none" from class static :nil)
	;a static method of a class is not the one of that name in the class
	;above, a create: only a virtual one takes and gives what that does
	(while (and (eql said "none") (not static) class (nql class :nil) (defq entry (. class_db :find class)))
		(when (defq m (.-> entry (:find :methods) (:find method)))
			(setq said (ifn (. m :find key) "none") static (eql (. m :find :type) :static)))
		(setq class (. entry :find :parent)))
	(and (eql said "none") (defq entry (. class_db :find from))
		(defq m (.-> entry (:find :methods) (:find method)))
		(setq said (ifn (. m :find (if (eql key :dec_inputs) :inputs :outputs)) "none")))
	;a create that says nothing gives the object, as they all do
	(and (eql said "none") (eql key :dec_outputs) (starts-with ":create" (str method)) (setq said ":r0"))
	(bits-of (regs-of said)))

(defun methods-of (class_db)
	; (methods-of class_db) -> (by_function class_db)
	;what is declared of each method, by the function it is: (trashes
	;takes gives), as bits. trashes is what the function itself does if
	;that is said, a virtual method's, and :nil if nothing is said
	(defq by_function (Fmap 1021))
	(. class_db :each (lambda (class entry)
		(.-> entry (:find :methods) (:each (lambda (method m)
			(when (. m :find :dec_line)
				(defq said (ifn (. m :find :declared_own) (. m :find :declared)) path (str (. m :find :function))
					now (list (if said (bits-of (regs-of said)))
						(declared-of class_db class method :dec_inputs)
						(declared-of class_db class method :dec_outputs))
					was (. by_function :find path))
				;a function may be more than one method, a create that is a
				;vcreate too, and a call bound to it does not say which. What
				;it trashes is what any of them says, what it gives what any
				;does, and what it takes only what they all do: so less is
				;said to be gone by here than may be, never more
				(. by_function :insert path (if was
					(list (if (or (first was) (first now)) (logior (ifn (first was) 0) (ifn (first now) 0)))
						(logand (second was) (second now)) (logior (third was) (third now)))
					now))))))))
	(list by_function class_db))

(defun called-of (inst all labels methods)
	; (called-of inst all labels methods) -> :nil | (name trashes takes gives)
	;what a call or a jump out is of, and what is declared of it: one
	;bound to its function, through a link to its path, and what that
	;function trashes; or one through a vtable, which has its class and
	;name with it, and what any class called so may. :nil if not known
	(bind '(by_function class_db) methods)
	(case (first inst)
		((emit-call-p emit-jmp-p)
			(and (defq at (. labels :find (second inst))) (< (inc at) (length all))
				(list? (defq to (second (elem-get all (inc at))))) (> (length to) 1)
				(defq at (. labels :find (second to))) (< (inc at) (length all))
				(str? (defq path (second (elem-get all (inc at)))))
				(defq m (. by_function :find path)) (first m)
				(cat (list path) m)))
		((emit-call-i emit-jmp-i)
			(when (> (length inst) 4)
				(defq class (elem-get inst 3) method (elem-get inst 4) found :nil of class
					name (cat (str class) " " (str method) ", by its vtable"))
				;said by the class, or the nearest above it that does
				(while (and (not found) class (nql class :nil) (defq entry (. class_db :find class)))
					(and (defq m (.-> entry (:find :methods) (:find method))) (. m :find :declared) (setq found m))
					(setq class (. entry :find :parent)))
				(if found (list name (bits-of (regs-of (. found :find :declared)))
					(declared-of class_db of method :dec_inputs)
					(declared-of class_db of method :dec_outputs)))))))

(defun relies-of (function methods)
	; (relies-of function methods) -> :nil | ((at name regs) ...)
	;the places in a function where a register is read after a call that
	;is declared to trash it: which instruction, what was called, and
	;the registers, as bits. :nil if the function is not there
	(when (defq got (insts-of function))
		(bind '(insts labels start frames all) got)
		(defq count (length insts) next (next-of insts labels start)
			me (. (first methods) :find function) gives (if me (third me) 0)
			;what each instruction reads, writes, and, of a call, what is
			;called. A jump out is a call and then the end
			flows (map (lambda (inst)
				(defq op (first inst))
				(cond
					((find op '(emit-call-p emit-call-i emit-jmp-p emit-jmp-i))
						(defq called (called-of inst all labels methods)
							base (if (find op '(emit-call-i emit-jmp-i)) (bit-of (second inst)) 0))
						(if called
							(list (logior base (third called)) (logior (second called) (elem-get called 3)) called)
							(list (logior base (bits-of (rest inst))) 0 :nil)))
					;a call of the host: the first register is one it uses to
					;get there, and is written. The rest it reads
					;and as many of :r0 on as it has arguments. What it gives is in :r0
					((eql op 'emit-call-abi)
						(list (logior (bit-of (third inst)) (dec (<< 1 (min 15 (ifn (elem-get inst 4) 0)))))
							(logior (bit-of (second inst)) 1) :nil))
					;a call of a part of itself: what that takes and gives is not
					;declared, so it is taken to give every register. Nothing is
					;said to lean on what it does not
					((eql op 'emit-call) (list 0 0x7fff :nil))
					((find op '(emit-call-r emit-jmp-r emit-stack-init))
						(list (bits-of (rest inst)) 0 :nil))
					((cat (flow-of inst) (list :nil)))))
				insts)
			live (map (lambda (n) 0) insts) changed :t found (list)
			;a part of the function that it calls, by a label, or gives the
			;address of to be called, a callback: where that ends is not
			;where the function does, and what it gives is not declared.
			;All that can be got to from where such a call goes
			inner (map (lambda (n) :nil) insts)
			todo (reduce (# (if (and (find (first %1) '(emit-call emit-lea-p))
					(defq to (. labels :find (if (eql (first %1) 'emit-call) (last %1) (second %1))))
					(<= 0 (setq to (- to start))) (< to count)) (push %0 to) %0))
				insts (list)))
		(while (defq at (pop todo))
			(unless (elem-get inner at)
				(elem-set inner at :t)
				(each (# (push todo %0)) (elem-get next at))))
		;wanted before an instruction: what it reads, and what is wanted
		;after it that it does not write. After the end, what the
		;function gives. Backwards, till nothing changes
		(defq after-of (lambda (at)
			(if (find (first (elem-get insts at)) '(emit-ret emit-jmp-p emit-jmp-i emit-jmp-r))
				(if (elem-get inner at) 0 gives) (reduce (# (logior %0 (elem-get live %1))) (elem-get next at) 0))))
		(while changed
			(setq changed :nil)
			(defq at count)
			(while (>= (setq at (dec at)) 0)
				(bind '(reads writes &ignore) (elem-get flows at))
				(defq in (logior reads (logand (after-of at) (lognot writes))))
				(unless (= in (elem-get live at))
					(elem-set live at in)
					(setq changed :t))))
		;wanted after a call, trashed by it as declared, and not given by it
		(each (lambda (flow)
			(when (defq called (third flow))
				;what does not come back has nothing read after it
				(defq at (!) bad (if (find (first called) '("sys/pii/exit" "gui/view/forward_tree")) 0
					(logand (after-of at) (second called) (lognot (elem-get called 3)))))
				(when (/= bad 0)
					;the first instruction after it, by where control goes, that
					;reads one of them: where to look
					;reads one of them: where to look. Of the lowest of them,
					;and not past where it is written
					(defq one (logand bad (neg bad)) seen (list) todo (cat (elem-get next at)) reader :nil)
					(while (and (not reader) (defq to (pop todo)))
						(unless (find to seen)
							(push seen to)
							(cond
								((/= 0 (logand one (first (elem-get flows to)))) (setq reader to))
								((/= 0 (logand one (second (elem-get flows to)))))
								(:t (each (# (push todo %0)) (elem-get next to))))))
					(push found (list at (first called) bad reader)))))
			flows)
		found))

;how far each way from a call a save and its bringing back are looked for
(defq +saves_near 8)

(defun saves-of (function methods)
	; (saves-of function methods) -> :nil | ((at name regs reader) ...)
	;where a function puts a register by before a call and brings it
	;back after, and need not: the call is not declared to trash it or
	;to give it, and nothing between the two writes it. Pushed and
	;popped, or written to a place on the stack and read back from it.
	;reader is where it is brought back. What is between the save and
	;the call, and the call and the bringing back, has to be plain: a
	;label, a branch or another call ends the looking
	(when (defq got (insts-of function))
		(bind '(insts labels start frames all) got)
		(defq found (list) count (length insts)
			plain (lambda (inst) (not (or (find (first inst) +call_ops) (find (first inst) +leave_ops)
				(find (first inst) '(emit-label emit-tlabel emit-jmp emit-alloc emit-free))
				(starts-with "emit-b" (first inst))))))
		(each (lambda (inst)
			(when (and (find (first inst) '(emit-call-p emit-call-i))
					(defq called (called-of inst all labels methods)))
				(bind '(name trashes takes gives) called)
				(defq at (!) changed (logior trashes gives) before (list) after (list) i at going :t)
				;back to what was put by, and all that is written on the way
				(while (and going (>= (setq i (dec i)) 0) (> i (- at +saves_near)))
					(defq was (elem-get insts i))
					(cond
						((not (plain was)) (setq going :nil))
						((eql (first was) 'emit-pop) (setq going :nil))
						((eql (first was) 'emit-push) (push before (list :push i (bits-of (rest was)) 0)) (setq going :nil))
						((and (eql (first was) 'emit-cpy-ri) (eql (third was) :rsp))
							(push before (list :slot i (bit-of (second was)) (elem-get was 3))))
						((setq changed (logior changed (second (flow-of was)))))))
				(setq i at going :t)
				(defq changed_after changed)
				(while (and going (< (setq i (inc i)) count) (< i (+ at +saves_near)))
					(defq was (elem-get insts i))
					(cond
						((not (plain was)) (setq going :nil))
						((eql (first was) 'emit-push) (setq going :nil))
						((eql (first was) 'emit-pop) (push after (list :push i (bits-of (rest was)) 0)) (setq going :nil))
						((and (eql (first was) 'emit-cpy-ir) (eql (second was) :rsp))
							(push after (list :slot i (bit-of (elem-get was 3)) (third was) changed_after))
							(setq changed_after (logior changed_after (bit-of (elem-get was 3)))))
						((setq changed_after (logior changed_after (second (flow-of was)))))))
				;a push with its pop, a place written with the same place read
				;back into the register it was written from
				(each (lambda ((kind i regs slot))
					(each (lambda ((kind2 j regs2 slot2 &optional seen))
						(when (and (eql kind kind2) (= slot slot2))
							(defq idle (logand regs regs2 (lognot (if (eql kind :push) changed_after seen))))
							(if (/= idle 0) (push found (list (+ at start) name idle (+ j start))))))
						after))
					before)))
			insts)
		found))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_v :nil opt_q :nil opt_s :nil args (options stdio usage)))
		(defq functions (rest args))
		(setq functions (filter (# (not (starts-with +jit_dir %0))) (vp-functions-of functions)))
		(cond
			((not (defq class_db (files-classes-info)))
				(print "The classes could not be read."))
			(:t (defq methods (methods-of class_db) by_called (Fmap 101) places 0 callers 0)
				(each (lambda (function)
					(task-slice)
					(when (nempty? (defq found (ifn (if opt_s (saves-of function methods) (relies-of function methods)) (list))))
						(++ callers)
						(each (lambda ((at name bad reader))
							(++ places)
							(if (or opt_v opt_q) (print function " (" at "): " (if opt_s "round " "after ") name ", " (join (map (const str) (regs-in bad)) " ")
								(cond (opt_s (cat ", put by and brought back at " (str reader) " for nothing"))
									(reader (cat ", read at " (str reader))) (", wanted where it ends"))))
							(defq entry (. by_called :find name))
							(unless entry (. by_called :insert name (setq entry (list 0 0 (list)))))
							(elem-set entry 0 (inc (first entry)))
							(elem-set entry 1 (logior (second entry) bad))
							(unless (find function (third entry)) (push (third entry) function)))
							found)))
					functions)
				(defq rows (list))
				(unless opt_q (. by_called :each (lambda (name entry) (push rows (cat (list name) entry)))))
				(each (lambda ((name sites bad who))
					(print (pad sites 4) "  " name ": " (join (map (const str) (regs-in bad)) " "))
					(print "      by " (join (slice who 0 (min 6 (length who))) ", ")
						(if (> (length who) 6) (cat " and " (str (- (length who) 6)) " more") "")))
					(sort rows (# (- (second %1) (second %0)))))
				(unless opt_q (print "Functions: " (length functions) (if opt_s ". Places a register is put by round a call that leaves it: "
						". Places a register is read after a call declared to trash it: ")
					places ", in " callers " functions, of " (length rows) " things called."))))))

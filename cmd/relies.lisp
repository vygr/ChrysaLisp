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

    This reads each function as it was assembled, the VP a
    normal build keeps, obj/vp/, and works out which registers are still wanted
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

(defq
	;an instruction of two registers that reads both and writes the second
	+both_ops ''(add-rr sub-rr mul-rr and-rr or-rr xor-rr shl-rr shr-rr
		asr-rr min-rr max-rr seq-rr sne-rr slt-rr sgt-rr sle-rr sge-rr
		div-rr div-rr-u rem-rr rem-rr-u)
	;that reads both and writes both
	+swap_ops ''(swp-rr land-rr)
	;that reads the first and writes the second
	+move_ops ''(cpy-rr ext-rr abs-rr)
	;of a constant and a register, read and written
	+const_ops ''(add-cr sub-cr mul-cr and-cr or-cr xor-cr shl-cr shr-cr
		asr-cr min-cr max-cr seq-cr sne-cr slt-cr sgt-cr sle-cr sge-cr)
	+branch_cr_ops ''(beq-cr bne-cr blt-cr bgt-cr ble-cr bge-cr)
	+branch_rr_ops ''(beq-rr bne-rr blt-rr bgt-rr ble-rr bge-rr)
	+get_ops ''(load-rcr load-rcr-b load-rcr-s load-rcr-i load-rcr-ub load-rcr-us load-rcr-ui lea-rcr)
	+put_ops ''(store-rrc store-rrc-b store-rrc-s store-rrc-i)
	+get2_ops ''(load-rrr load-rrr-b load-rrr-s load-rrr-i load-rrr-ub load-rrr-us load-rrr-ui lea-rrr)
	+put2_ops ''(store-rrr store-rrr-b store-rrr-s store-rrr-i)
	;nothing of a general register in them
	+none_ops ''(label tlabel align string byte short int long
		alloc free ret sync nop vp-nop jmp
		cpy-dd add-dd sub-dd mul-dd div-dd min-dd max-dd sqrt-dd
		abs-dd neg-dd beq-dd bne-dd blt-dd bgt-dd ble-dd bge-dd
		add-ff sub-ff mul-ff div-ff min-ff max-ff sqrt-ff
		abs-ff neg-ff beq-ff bne-ff blt-ff bgt-ff ble-ff bge-ff
		cvt-fd cvt-df))

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
		((and (find op '(xor-rr sub-rr)) (= a b)) (list 0 b))
		((find op +both_ops) (list (logior a b) b))
		((find op +swap_ops) (list (logior a b) (logior a b)))
		((find op +move_ops) (list a b))
		((find op +const_ops) (list b b))
		;one register, read and written
		((eql op 'lnot-rr) (list a a))
		((eql op 'cpy-cr) (list 0 b))
		((find op +branch_cr_ops) (list b 0))
		((find op +branch_rr_ops) (list (logior a b) 0))
		((find op +get_ops) (list a c))
		((find op +put_ops) (list (logior a b) 0))
		((find op +get2_ops) (list (logior a b) c))
		((find op +put2_ops) (list (logior a b c) 0))
		((find op '(load-pr lea-pr)) (list 0 b))
		;to and from the float registers, and floats in memory
		((find op '(cpy-rd cvt-rd cpy-rf cvt-rf load-rcd load-rcf)) (list a 0))
		((find op '(cpy-dr cvt-dr cpy-fr cvt-fr)) (list 0 b))
		((find op '(store-drc store-frc)) (list b 0))
		((find op '(load-rrd load-rrf)) (list (logior a b) 0))
		((find op '(store-drr store-frr)) (list (logior b c) 0))
		((eql op 'push) (list (bits-of (rest inst)) 0))
		((find op '(pop trash)) (list 0 (bits-of (rest inst))))
		((list (bits-of (rest inst)) 0))))

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
					((find op '(call-p call-rc jmp-p jmp-rc))
						(defq called (called-of inst all labels methods)
							base (if (find op '(call-rc jmp-rc)) (bit-of (second inst)) 0))
						(if called
							(list (logior base (third called)) (logior (second called) (elem-get called 3)) called)
							(list (logior base (bits-of (rest inst))) 0 :nil)))
					;a call of the host: the first register is one it uses to
					;get there, and is written. The rest it reads
					;and as many of :r0 on as it has arguments. What it gives is in :r0
					((eql op 'call-abi)
						(list (logior (bit-of (third inst)) (dec (<< 1 (min 15 (ifn (elem-get inst 4) 0)))))
							(logior (bit-of (second inst)) 1) :nil))
					;a call of a part of itself: what that takes and gives is not
					;declared, so it is taken to give every register. Nothing is
					;said to lean on what it does not
					((eql op 'call) (list 0 0x7fff :nil))
					((find op '(call-r jmp-r stack-init))
						(list (bits-of (rest inst)) 0 :nil))
					((cat (flow-of inst) (list :nil)))))
				insts)
			live (map (lambda (n) 0) insts) changed :t found (list)
			;a part of the function that it calls, by a label, or gives the
			;address of to be called, a callback: where that ends is not
			;where the function does, and what it gives is not declared.
			;All that can be got to from where such a call goes
			inner (map (lambda (n) :nil) insts)
			todo (reduce (# (if (and (find (first %1) '(call lea-pr))
					(defq to (. labels :find (if (eql (first %1) 'call) (last %1) (second %1))))
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
			(if (find (first (elem-get insts at)) '(ret jmp-p jmp-rc jmp-r))
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
				(find (first inst) '(label tlabel jmp alloc free))
				(and (starts-with "b" (first inst)) (nql (first inst) 'byte))))))
		(each (lambda (inst)
			(when (and (find (first inst) '(call-p call-rc))
					(defq called (called-of inst all labels methods)))
				(bind '(name trashes takes gives) called)
				(defq at (!) changed (logior trashes gives) before (list) after (list) i at going :t)
				;back to what was put by, and all that is written on the way
				(while (and going (>= (setq i (dec i)) 0) (> i (- at +saves_near)))
					(defq was (elem-get insts i))
					(cond
						((not (plain was)) (setq going :nil))
						((eql (first was) 'pop) (setq going :nil))
						((eql (first was) 'push) (push before (list :push i (bits-of (rest was)) 0)) (setq going :nil))
						((and (eql (first was) 'store-rrc) (eql (third was) :rsp))
							(push before (list :slot i (bit-of (second was)) (elem-get was 3))))
						((setq changed (logior changed (second (flow-of was)))))))
				(setq i at going :t)
				(defq changed_after changed)
				(while (and going (< (setq i (inc i)) count) (< i (+ at +saves_near)))
					(defq was (elem-get insts i))
					(cond
						((not (plain was)) (setq going :nil))
						((eql (first was) 'push) (setq going :nil))
						((eql (first was) 'pop) (push after (list :push i (bits-of (rest was)) 0)) (setq going :nil))
						((and (eql (first was) 'load-rcr) (eql (second was) :rsp))
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

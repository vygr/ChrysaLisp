(import "lib/options/options.inc")
(import "lib/files/files.inc")

(defq usage `(
(("-h" "--help")
"Usage: locals [options] [function] ...

    options:
        -h --help: this help info.
        -v --verbose: a line for every function that has
            locals, not only the sum.
        -t --top num: the functions that move most to and
            from the stack, that many of them, default 10.

    What the named locals of each VP function cost, and what
    of that a compiler that kept them in registers could save.
    It reads the instructions of a function as they were
    assembled, obj/vp/, which a build of the VP64 leaves,
    'make vp', and sees the function whole: where its frame
    is made and let go of, each place in the frame that is
    read or written, a local, which of them have their
    address taken, and, by where control goes, which are
    still wanted after each call.

    A local that never has its address taken could be kept
    in a register. One that is not wanted after any call
    could be in any. One that is has to be in a register the
    call leaves alone, or be put by for the call.

    With no function named, all there are.")
(("-v" "--verbose") ,(opt-flag 'opt_v))
(("-t" "--top") ,(opt-num 'opt_t))
))

(defq +obj_dir "obj/vp/" +jit_dir "lib/gpu/jit/"
	;what reads a place of the frame, what writes one, and what takes
	;the address of one: each has the stack pointer as its base
	+load_ops ''(emit-cpy-ir emit-cpy-ir-b emit-cpy-ir-s emit-cpy-ir-i
		emit-cpy-ir-ub emit-cpy-ir-us emit-cpy-ir-ui emit-cpy-if)
	+store_ops ''(emit-cpy-ri emit-cpy-ri-b emit-cpy-ri-s emit-cpy-ri-i emit-cpy-fi)
	;what goes somewhere and does not come back here, and what calls
	+leave_ops ''(emit-ret emit-jmp-p emit-jmp-r emit-jmp-i)
	+call_ops ''(emit-call emit-call-p emit-call-r emit-call-i emit-call-abi)
	;what is not an instruction of the function at all
	+data_ops ''(emit-label emit-tlabel emit-align emit-string emit-byte emit-short emit-int emit-long))

(defun insts-of (function)
	; (insts-of function) -> :nil | (insts labels start frames)
	;the instructions of a function, from where it is entered to where
	;its table of links begins. The fourth thing of the file is the
	;header, which has the two labels. And what its frames of named
	;locals were, the form after the instructions, if it has any: each
	;(which_alloc size ((name offset type) ...))
	(when (and (defq stream (file-stream (cat +obj_dir function)))
			(defq all (first (read stream))) (> (length all) 4))
		(defq head (elem-get all 3) labels (Fmap 31) more (first (read stream)))
		(each (# (if (find (first %0) '(emit-label emit-tlabel)) (. labels :insert (last (second %0)) (!)))) all)
		(and (eql (first head) 'emit-short) (>= (length head) 4)
			(defq start (. labels :find (third head))) (defq end (. labels :find (elem-get head 3)))
			(list (slice all start end) labels start
				(if (and (list? more) (nempty? more) (eql (first more) 'locals)) (rest more) (list))))))

(defun target-of (inst labels start count)
	; (target-of inst labels start count) -> :nil | index
	;where a branch or a jump goes, if that is in the function
	(and (sym? (defq label (last inst))) (defq at (. labels :find label))
		(<= 0 (setq at (- at start))) (< at count) at))

(defun next-of (insts labels start)
	; (next-of insts labels start) -> ((index ...) ...)
	;for each instruction, where control may go from it
	(defq count (length insts))
	(map (lambda (inst)
		(defq op (first inst) at (!) on (if (< (inc at) count) (list (inc at)) (list)))
		(cond
			((find op +leave_ops) (list))
			((eql op 'emit-jmp) (if (defq to (target-of inst labels start count)) (list to) (list)))
			((starts-with "emit-b" op)
				(if (defq to (target-of inst labels start count)) (push on to) on))
			(on)))
		insts))

(defun place-of (stack sizes offset)
	; (place-of stack sizes offset) -> :nil | (which offset)
	;a place so far up from the stack pointer: which frame it is in, of
	;those there are, the last made first, and where in that frame.
	;:nil if it is above them all, in what was pushed, or the caller's
	(defq at (length stack) found :nil)
	(while (and (not found) (>= (setq at (dec at)) 0))
		(defq which (elem-get stack at) size (elem-get sizes which))
		(if (< offset size) (setq found (list which offset)) (setq offset (- offset size))))
	found)

(defun local-of (frames which offset)
	; (local-of frames which offset) -> (name pinned?)
	;the local a place of a frame is of, if the frame is one of named
	;locals, and if it is a struct, which is always in memory. One of
	;a frame made by hand is known by where it is
	(defq frame (some (# (if (= (first %0) which) %0)) frames) found :nil)
	(when frame
		;the one that begins at or under it and is nearest: a struct has
		;places all through it, and they are all the struct
		(each (lambda ((name at type))
			(and (<= at offset) (or (not found) (> at (second found))) (setq found (list name at type))))
			(third frame)))
	(if found
		(list (cat (str which) ":" (first found)) (eql (third found) "") :t)
		(list (cat (str which) "+" (str offset)) :nil :nil)))

(defun look-at (function)
	; (look-at function) -> :nil | (function frame slots loads stores kept_loads kept_stores
	;	over_loads over_stores pinned hand_loads hand_stores note)
	;a function seen whole. :nil if it has no frame
	(when (defq got (insts-of function))
		(bind '(insts labels start frames) got)
		(defq count (length insts) next (next-of insts labels start)
			;which of the function's allocs each alloc is, from 1, and how
			;big each is
			which_of (list) sizes (list 0) allocs 0)
		(each (lambda (inst)
			(cond
				((eql (first inst) 'emit-alloc) (push which_of (++ allocs)) (push sizes (second inst)))
				((push which_of 0))))
			insts)
		;the frames there are as each instruction is come to, the last made
		;last, by where control goes, not by where the instruction is: a
		;return in the middle lets go of them, and what is after it has them
		(defq stacks (cap count (list)) todo (list 0) odd :nil)
		(times count (push stacks :nil))
		(when (> count 0) (elem-set stacks 0 (list)))
		(while (defq at (pop todo))
			(defq inst (elem-get insts at) op (first inst) stack (elem-get stacks at))
			(cond
				((eql op 'emit-alloc) (setq stack (cat stack (list (elem-get which_of at)))))
				((eql op 'emit-free)
					;so many bytes of them, whole frames from the last made
					(defq bytes (second inst) stack (cat stack))
					(while (and (> bytes 0) (nempty? stack))
						(setq bytes (- bytes (elem-get sizes (pop stack)))))
					(if (/= bytes 0) (setq odd :t))))
			(each (lambda (to)
				(cond
					((not (elem-get stacks to)) (elem-set stacks to stack) (push todo to))
					((nql (str (elem-get stacks to)) (str stack)) (setq odd :t))))
				(elem-get next at)))
		(when (> allocs 0)
			;each local that is read or written, and what is done to it at
			;each instruction. One whose address is taken, or that is a
			;struct, is in memory and stays there
			(defq slots (list) named (list) events (cap count (list)) pinned (list) escapes :nil)
			(each (lambda (inst)
				(defq op (first inst) stack (ifn (elem-get stacks (!)) (list)) event :nil place :nil)
				(cond
					((find op +data_ops))
					((and (find op +load_ops) (eql (second inst) :rsp))
						(if (setq place (place-of stack sizes (third inst))) (setq event :load)))
					((and (find op +store_ops) (eql (third inst) :rsp))
						(if (setq place (place-of stack sizes (elem-get inst 3))) (setq event :store)))
					((and (eql op 'emit-lea-i) (eql (second inst) :rsp))
						(if (setq place (place-of stack sizes (third inst))) (setq event :addr)))
					;the stack pointer used as a number: where the frame is
					;is known to something this can not follow
					((and (not (find op '(emit-alloc emit-free emit-push emit-pop))) (find :rsp inst))
						(setq escapes :t)))
				(cond
					(event
						(bind '(name struct of_named) (local-of frames (first place) (second place)))
						(unless (find name slots) (push slots name) (push named of_named))
						(and (or struct (eql event :addr)) (not (find name pinned)) (push pinned name))
						(push events (list event name)))
					((push events :nil))))
				insts)
			;which locals are still wanted after each instruction: one is
			;wanted before an instruction that reads it, and before one that
			;does not write it if it is wanted after. Gone round till
			;nothing changes. A set of them is a number, a bit a local
			(defq bits (map (lambda (slot) (<< 1 (!))) slots) live (cap count (list)) changed :t
				big (> (length slots) 62))
			(times count (push live 0))
			(unless big
				(while changed
					(setq changed :nil)
					(defq at count)
					(while (>= (setq at (dec at)) 0)
						(defq out (reduce (# (logior %0 (elem-get live %1))) (elem-get next at) 0)
							event (elem-get events at)
							bit (if event (elem-get bits (find (second event) slots)) 0)
							in (cond
								((not event) out)
								((eql (first event) :store) (logand out (lognot bit)))
								((logior out bit))))
						(unless (= in (elem-get live at))
							(elem-set live at in)
							(setq changed :t)))))
			;what is wanted after a call, any call
			(defq over 0)
			(each (lambda (inst)
				(if (find (first inst) +call_ops)
					(setq over (logior over (reduce (# (logior %0 (elem-get live %1))) (elem-get next (!)) 0)))))
				insts)
			;what keeping them in registers would come to, if a call leaves
			;no register as it was. A local that could be in a register
			;has one that is its own. It is in it from when it is first
			;read or written till a call, so a read is of the stack only
			;where, by some way to it, there was a call since it was last
			;read or written: had. And it is written to the stack only
			;before a call, if it has been written since the last by some
			;way, dirty, and is wanted after. Both gone round till nothing
			;changes, forwards
			(defq keeps (if (or escapes big) 0
					(reduce (# (if (and (elem-get named (!)) (not (find %1 pinned))) (logior %0 (elem-get bits (!))) %0)) slots 0))
				prev (map (lambda (n) (list)) next) had (map (lambda (n) -1) next) dirty (map (lambda (n) 0) next)
				calls (map (# (if (find (first %0) +call_ops) :t)) insts) changed :t
				ones (lambda (n) (defq c 0) (while (/= n 0) (setq n (logand n (dec n))) (++ c)) c))
			(each (lambda (on) (defq from (!)) (each (# (push (elem-get prev %0) from)) on)) next)
			(when (> count 0) (elem-set had 0 0))
			(defq out-of (lambda (at in may)
				;what is had, or dirty, after an instruction, of what was before it
				(defq event (elem-get events at))
				(cond
					((elem-get calls at) 0)
					((and event (nql (first event) :addr) (or (not may) (eql (first event) :store)))
						(logior in (elem-get bits (find (second event) slots))))
					(in))))
			(while changed
				(setq changed :nil)
				(each (lambda (from)
					(defq at (!))
					(when (or (> at 0) (nempty? from))
						(defq in_had (if (= at 0) 0 (reduce (# (logand %0 (out-of %1 (elem-get had %1) :nil))) from -1))
							in_dirty (reduce (# (logior %0 (out-of %1 (elem-get dirty %1) :t))) from 0))
						(unless (and (= in_had (elem-get had at)) (= in_dirty (elem-get dirty at)))
							(elem-set had at in_had) (elem-set dirty at in_dirty)
							(setq changed :t))))
					prev))
			(defq saved_loads 0 spills 0 most 0 used (list))
			(each (lambda (inst)
				(defq at (!) event (elem-get events at)
					after (reduce (# (logior %0 (elem-get live %1))) (elem-get next at) 0))
				(each (# (and (sym? %0) (starts-with ":r" %0) (nql %0 :rsp) (not (find %0 used)) (push used %0))) inst)
				(setq most (max most (ones (logand keeps (elem-get live at)))))
				(and event (eql (first event) :load) (/= -1 (elem-get had at))
					(/= 0 (logand keeps (elem-get had at) (elem-get bits (find (second event) slots))))
					(++ saved_loads))
				(if (elem-get calls at)
					(setq spills (+ spills (ones (logand keeps (elem-get dirty at) after))))))
				insts)
			(defq free (- 15 (length used)) fits (if (and (/= keeps 0) (<= most free)) 1 0))
			;the sums. Of named locals: all that is read and written, of
			;that what is of one that could be kept in a register, and of
			;that what is of one wanted after a call. And what is read and
			;written of frames made by hand, which are not this compiler's
			(defq loads 0 stores 0 kept_loads 0 kept_stores 0 over_loads 0 over_stores 0 hand_loads 0 hand_stores 0)
			(each (lambda (event)
				(when (and event (nql (first event) :addr))
					(defq load (eql (first event) :load) slot (second event) index (find slot slots)
						keep (not (or escapes big (find slot pinned)))
						across (/= 0 (logand over (elem-get bits index))))
					(cond
						((not (elem-get named index)) (if load (++ hand_loads) (++ hand_stores)))
						(:t (if load (++ loads) (++ stores))
							(when keep
								(if load (++ kept_loads) (++ kept_stores))
								(when across (if load (++ over_loads) (++ over_stores))))))))
				events)
			(list function (reduce (const +) sizes 0) (length (filter (const identity) named))
				loads stores kept_loads kept_stores over_loads over_stores
				(length (filter (# (elem-get named (find %0 slots))) pinned)) hand_loads hand_stores
				saved_loads spills fits (if (/= keeps 0) 1 0) most free
				(cond (escapes "the stack pointer is used as a number") (big "more than 62 locals")
					(odd "the frames are not the same by two ways") (""))))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_v :nil opt_t 10 args (options stdio usage)))
		(defq functions (rest args))
		(if (empty? functions)
			(setq functions (filter (# (not (starts-with +jit_dir %0)))
				(sort (map (# (slice %0 (length +obj_dir) -1)) (files-all (slice +obj_dir 0 -2) '(""))))))
			(setq functions (map (# (if (starts-with +obj_dir %0) (slice %0 (length +obj_dir) -1) %0)) functions)))
		(defq seen (list) sum (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) noted 0 of_named 0)
		(each (lambda (function)
			(task-slice)
			(when (defq row (look-at function))
				(push seen row)
				(each (# (elem-set sum (!) (+ (elem-get sum (!)) %0))) (slice row 1 16))
				(if (nempty? (last row)) (++ noted))
				(if (> (third row) 0) (++ of_named))
				(if opt_v (print (first row) ": frames " (second row) " bytes, named locals " (third row)
					", read " (elem-get row 3) " written " (elem-get row 4)
					", could be registers: read " (elem-get row 5) " written " (elem-get row 6)
					", of those wanted after a call: read " (elem-get row 7) " written " (elem-get row 8)
					(if (> (elem-get row 9) 0) (cat ", " (str (elem-get row 9)) " are in memory, a struct or their address taken") "")
					(if (> (+ (elem-get row 10) (elem-get row 11)) 0)
						(cat ", of frames made by hand: read " (str (elem-get row 10)) " written " (str (elem-get row 11))) "")
					", in registers a call does not keep: " (elem-get row 12) " reads less, " (elem-get row 13) " writes before calls"
					", " (elem-get row 16) " wanted at once, " (elem-get row 17) " registers it does not use"
					(if (nempty? (last row)) (cat ". " (last row)) "")))))
			functions)
		(bind '(frame slots loads stores kept_loads kept_stores over_loads over_stores pinned hand_loads hand_stores
			saved_loads spills fits keeping) sum)
		(print "Functions: " (length functions) ", with a frame: " (length seen) ", with named locals: " of_named)
		(print "Named locals: " slots ", in memory, a struct or their address taken: " pinned)
		(print "Read from the stack: " loads ", written to it: " stores)
		(print "Of locals that could be registers: read " kept_loads ", written " kept_stores)
		(print "  not wanted after any call: read " (- kept_loads over_loads) ", written " (- kept_stores over_stores))
		(print "  wanted after a call: read " over_loads ", written " over_stores)
		(print "Kept in registers that no call leaves as it was:")
		(print "  read " (- kept_loads saved_loads) ", it was " kept_loads ", written before a call " spills ", it was " kept_stores)
		(print "  functions with such locals " keeping ", of them with a register for each wanted at once " fits)
		(print "Of frames made by hand, not named locals: read " hand_loads ", written " hand_stores)
		(print "Functions it can not follow: " noted)
		(when (> opt_t 0)
			(print "Most to and from the stack that could be registers:")
			(each (lambda (row)
				(print "  " (pad (+ (elem-get row 5) (elem-get row 6)) 5) " " (first row)
					", " (third row) " locals" (if (nempty? (last row)) (cat ", " (last row)) "")))
				(slice (sort seen (# (- (+ (elem-get %1 5) (elem-get %1 6)) (+ (elem-get %0 5) (elem-get %0 6)))))
					0 (min opt_t (length seen)))))))

(import "lib/options/options.inc")
(import "lib/files/files.inc")
(import "lib/files/info.inc")
(import "lib/asm/assembled.inc")

(defq usage `(
(("-h" "--help")
"Usage: locals [options] [function] ...

    options:
        -h --help: this help info.
        -v --verbose: a line for every function that has
            locals, not only the sum.
        -t --top num: the functions that move most to and
            from the stack, that many of them, default 10.
        -c --cost num: what the methods called cost their
            callers, the worst that many of them: for each,
            the calls of it from functions with locals, the
            locals wanted after those calls, how many of
            them there is no register for that the call
            leaves alone and the caller does not use, and
            how many of those it is the call that keeps
            out: there would be a register if it trashed
            less. Then the registers it trashes that would
            each be a home for one more local, and at how
            many of those calls. To trash less is safe for
            every caller, and lint says if the code does.

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
(("-c" "--cost") ,(opt-num 'opt_c))
))

(defun trashes-of (class_db)
	; (trashes-of class_db) -> (by_function class_db ledger)
	;what the methods are declared to trash, by the function a method
	;is, for a call that is bound to it: what the function itself does,
	;if that is said, a virtual method's. And a ledger, of what each
	;method called costs those that call it, to be filled in
	(defq by_function (Fmap 1021))
	(. class_db :each (lambda (class entry)
		(.-> entry (:find :methods) (:each (lambda (method m)
			(if (defq said (ifn (. m :find :declared_own) (. m :find :declared)))
				(. by_function :insert (str (. m :find :function)) (regs-of said))))))))
	(list by_function class_db (Fmap 101)))

(defun call-trashes (inst all labels trashes)
	; (call-trashes inst all labels trashes) -> :nil | (name (num ...))
	;what a call is of, and the general registers it may leave not as
	;they were, if what it calls is a method that says: one bound to its
	;function, through a link to its path, or a virtual one, which has
	;its class and name with it. :nil if it is not known
	(bind '(by_function class_db &ignore) trashes)
	(case (first inst)
		(emit-call-p
			(and (defq at (. labels :find (second inst))) (< (inc at) (length all))
				(list? (defq to (second (elem-get all (inc at))))) (> (length to) 1)
				(defq at (. labels :find (second to))) (< (inc at) (length all))
				(str? (defq path (second (elem-get all (inc at)))))
				(defq regs (. by_function :find path))
				(list path regs)))
		(emit-call-i
			(when (> (length inst) 4)
				(defq class (elem-get inst 3) method (elem-get inst 4) said :nil
					name (cat (str class) " " (str method) ", by its vtable"))
				;said by the class, or the nearest above it that does
				(while (and (not said) class (nql class :nil) (defq entry (. class_db :find class)))
					(if (defq m (.-> entry (:find :methods) (:find method))) (setq said (. m :find :declared)))
					(setq class (. entry :find :parent)))
				(if said (list name (regs-of said)))))))

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

(defun look-at (function &optional trashes)
	; (look-at function) -> :nil | (function frame slots loads stores kept_loads kept_stores
	;	over_loads over_stores pinned hand_loads hand_stores note)
	;a function seen whole. :nil if it has no frame
	(when (defq got (insts-of function))
		(bind '(insts labels start frames all) got)
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
			;and if a call leaves alone what its method is declared not to
			;trash. A call that leaves as many registers as there are
			;locals wanted after it, of those the function does not use
			;itself, or of them all, moves nothing: the rest are as
			;before. The same two sums, twice, for the two
			(defq told (list 0 0 0 0) known 0 quiet (list 0 0))
			(when (and trashes (/= keeps 0))
				(defq numbers (map (# (str-as-num (slice (str %0) 2 -1))) used))
				(each (lambda (all_free)
					(defq which (!) calls (map (lambda (inst)
						(defq at (!))
						(when (find (first inst) +call_ops)
							(defq wanted (ones (logand keeps (reduce (# (logior %0 (elem-get live %1))) (elem-get next at) 0)))
								called (call-trashes inst all labels trashes) trashed (if called (second called))
								room (if trashed (length (filter (# (not (or (find %0 trashed) (and (not all_free) (find %0 numbers)))))
									(range 0 15))) 0))
							(when (and trashed (= which 0))
								(++ known)
								;what this call costs, put to what is called: the locals
								;wanted after it, those of them there is no register
								;for, and each register it trashes that the caller
								;does not use, which would be a home for one more
								(defq ledger (third trashes) entry (. ledger :find (first called)))
								(unless entry (. ledger :insert (first called)
									(setq entry (list 0 0 0 (map (lambda (n) 0) (range 0 15)) (cat trashed) 0))))
								(elem-set entry 0 (inc (first entry)))
								(elem-set entry 1 (+ (second entry) wanted))
								(when (> wanted room)
									(elem-set entry 2 (+ (third entry) (- wanted room)))
									;and how many of those it is that keeps out, not the
									;caller's own use of registers: one for each register
									;it trashes that the caller does not use
									(elem-set entry 5 (+ (elem-get entry 5)
										(min (- wanted room) (length (filter (# (not (find %0 numbers))) trashed)))))
									(each (# (unless (find %0 numbers)
										(elem-set (elem-get entry 3) %0 (inc (elem-get (elem-get entry 3) %0))))) trashed)))
							(cond
								((not trashed) :t)
								((<= wanted room)
									(elem-set quiet which (inc (elem-get quiet which))) :nil)
								(:t))))
						insts) changed :t)
					(each (lambda (inst) (elem-set had (!) (if (= (!) 0) 0 -1)) (elem-set dirty (!) 0)) insts)
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
					(each (lambda (inst)
						(defq at (!) event (elem-get events at)
							after (reduce (# (logior %0 (elem-get live %1))) (elem-get next at) 0))
						(and event (eql (first event) :load) (/= -1 (elem-get had at))
							(/= 0 (logand keeps (elem-get had at) (elem-get bits (find (second event) slots))))
							(elem-set told (* which 2) (inc (elem-get told (* which 2)))))
						(if (elem-get calls at)
							(elem-set told (inc (* which 2)) (+ (elem-get told (inc (* which 2))) (ones (logand keeps (elem-get dirty at) after))))))
						insts))
					'(:nil :t)))
			(defq all_calls (length (filter (# (find (first %0) +call_ops)) insts)))
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
				saved_loads spills fits (if (/= keeps 0) 1 0)
				(first told) (second told) (third told) (elem-get told 3)
				(if (/= keeps 0) all_calls 0) known (first quiet) (second quiet) most free
				(cond (escapes "the stack pointer is used as a number") (big "more than 62 locals")
					(odd "the frames are not the same by two ways") (""))))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_v :nil opt_t 10 opt_c 0 args (options stdio usage)))
		(defq functions (rest args))
		(if (empty? functions)
			(setq functions (filter (# (not (starts-with +jit_dir %0)))
				(sort (map (# (slice %0 (length +obj_dir) -1)) (files-all (slice +obj_dir 0 -2) '(""))))))
			(setq functions (map (# (if (starts-with +obj_dir %0) (slice %0 (length +obj_dir) -1) %0)) functions)))
		(defq seen (list) sum (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) noted 0 of_named 0
			trashes (if (defq class_db (files-classes-info)) (trashes-of class_db)))
		(each (lambda (function)
			(task-slice)
			(when (defq row (look-at function trashes))
				(push seen row)
				(each! (# (elem-set sum (dec (!)) (+ (elem-get sum (dec (!))) %0))) (list row) 1 24)
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
					", " (elem-get row 24) " wanted at once, " (elem-get row 25) " registers it does not use"
					", calls " (elem-get row 20) ", that say what they trash " (elem-get row 21)
					(if (nempty? (last row)) (cat ". " (last row)) "")))))
			functions)
		(bind '(frame slots loads stores kept_loads kept_stores over_loads over_stores pinned hand_loads hand_stores
			saved_loads spills fits keeping told_loads told_spills told_loads_all told_spills_all
			all_calls known quiet quiet_all) sum)
		(print "Functions: " (length functions) ", with a frame: " (length seen) ", with named locals: " of_named)
		(print "Named locals: " slots ", in memory, a struct or their address taken: " pinned)
		(print "Read from the stack: " loads ", written to it: " stores)
		(print "Of locals that could be registers: read " kept_loads ", written " kept_stores)
		(print "  not wanted after any call: read " (- kept_loads over_loads) ", written " (- kept_stores over_stores))
		(print "  wanted after a call: read " over_loads ", written " over_stores)
		(print "Kept in registers that no call leaves as it was:")
		(print "  read " (- kept_loads saved_loads) ", it was " kept_loads ", written before a call " spills ", it was " kept_stores)
		(print "  functions with such locals " keeping ", of them with a register for each wanted at once " fits)
		(print "Kept in registers a call leaves as they were, if its method is declared not to trash them:")
		(print "  calls in those functions " all_calls ", of methods that say what they trash " known)
		(print "  in the registers the function does not use: calls that move nothing " quiet
			", read " (- kept_loads told_loads) ", written before a call " told_spills)
		(print "  in any register: calls that move nothing " quiet_all
			", read " (- kept_loads told_loads_all) ", written before a call " told_spills_all)
		(print "Of frames made by hand, not named locals: read " hand_loads ", written " hand_stores)
		(print "Functions it can not follow: " noted)
		;the classes are read under a lock, and it may not be had in the time
		(and (> opt_c 0) (not trashes)
			(print "What is called, and what it costs those that call it: not known, the classes could not be read."))
		(when (and (> opt_c 0) trashes)
			(defq costs (list))
			(. (third trashes) :each (lambda (name entry) (push costs (cat (list name) entry))))
			(print "What is called, and what it costs those that call it. Calls, locals wanted after them,")
			(print "locals with no register the call leaves that the caller does not use, and of those")
			(print "the ones it is the call that keeps out:")
			(each! (lambda ((name sites wanted short helps trashed its))
				;the registers that would each be a home for one more, the most calls first
				(defq best (sort (filter (# (> (second %0) 0)) (map (# (list %0 (elem-get helps %0))) (range 0 15)))
					(# (- (second %1) (second %0)))))
				(print "  " (pad sites 4) " " (pad wanted 5) " " (pad short 5) " " (pad its 5) "  " name)
				(print "                          trashes " (length trashed) " of 15"
					(if (nempty? best) (cat ", would help: " (join (map! (# (cat ":r" (str (first %0)) " at " (str (second %0))))
						(list best) 0 (min 6 (length best))) ", ")) "")))
				(list (sort costs (# (- (elem-get %1 6) (elem-get %0 6))))) 0 (min opt_c (length costs))))
		(when (> opt_t 0)
			(print "Most to and from the stack that could be registers:")
			(each! (lambda (row)
				(print "  " (pad (+ (elem-get row 5) (elem-get row 6)) 5) " " (first row)
					", " (third row) " locals" (if (nempty? (last row)) (cat ", " (last row)) "")))
				(list (sort seen (# (- (+ (elem-get %1 5) (elem-get %1 6)) (+ (elem-get %0 5) (elem-get %0 6))))))
					0 (min opt_t (length seen))))))

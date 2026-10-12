;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; named locals kept in registers
;
; A function that asks, (vpx-regs), is seen whole when it has all been
; assembled, as instructions and before they are translated, and its named
; locals that can be are kept in registers of their own: a read of the
; stack becomes a copy from a register, and a write to the stack that
; nothing then reads is gone.
;
; A local has a register, its home, for the whole function: one the
; function may trash, as its declaration says, that none of its own
; instructions name, and that nothing it calls takes or gives a value in.
; So what calls it sees no register changed that was not before, and
; nothing of its own is in the way.
;
; The stack is still where a local is, whenever it has to be. A write goes
; to the stack as it did, and to the home as well if a read may come
; before a call that trashes the home. A read is from the home if the
; value is in it by every way to the read, had. If not it is from the
; stack, by way of the home if another read may follow. Then a write to
; the stack that no read of the stack can come after is taken out.
; Nothing is ever only in a register a call may trash: what a call trashes
; is what its method is declared to, (method-trashes), and all of them if
; it does not say.
;
; What it will not touch: a local that is a struct, or has its address
; taken, or is above one in its frame that has, or is read or written as
; less than a long. And a function it can
; not follow all of: one that uses the stack pointer as a number, has a
; label nothing is seen to go to, calls or jumps by a register, calls a
; part of itself, or calls what is not a method. Such a function is as it
; was, every instruction.
;
; Nothing here calls itself.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +keep_loads ''(emit-cpy-ir emit-cpy-ir-b emit-cpy-ir-s emit-cpy-ir-i
		emit-cpy-ir-ub emit-cpy-ir-us emit-cpy-ir-ui emit-cpy-if)
	+keep_stores ''(emit-cpy-ri emit-cpy-ri-b emit-cpy-ri-s emit-cpy-ri-i emit-cpy-fi)
	;what goes somewhere and does not come back here, and what calls
	+keep_leaves ''(emit-ret emit-jmp-p emit-jmp-r emit-jmp-i)
	+keep_calls ''(emit-call emit-call-p emit-call-r emit-call-i emit-call-abi)
	;what it does not follow
	+keep_never ''(emit-call emit-call-r emit-jmp-r emit-stack-init)
	+keep_regs ''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14))

(defun keep-next (insts labels)
	; (keep-next insts labels) -> :nil | ((index ...) ...)
	;for each instruction, where control may go from it. :nil if a branch
	;or a jump goes to a label that is not in the function
	(defq count (length insts) lost :nil)
	(defq next (map (lambda (inst)
		(defq op (first inst) at (!) on (if (< (inc at) count) (list (inc at)) (list)))
		(cond
			((find op +keep_leaves) (list))
			((or (eql op 'emit-jmp) (starts-with "emit-b" op))
				(cond
					((find op '(emit-brk emit-byte)) on)
					((defq to (. labels :find (last inst))) (if (eql op 'emit-jmp) (list to) (push on to)))
					(:t (setq lost :t) on)))
			(on)))
		insts))
	(unless lost next))

(defun keep-stacks (insts next)
	; (keep-stacks insts next) -> :nil | (stacks sizes)
	;the frames there are as each instruction is come to, the last made
	;last, by where control goes, each the number of the alloc that made
	;it, from 1, and how big each alloc is. :nil if they are not the same
	;by two ways to an instruction, or a free is not of whole frames
	(defq count (length insts) which_of (list) sizes (list 0) allocs 0
		stacks (map (lambda (inst) :nil) insts) todo (list 0) odd :nil)
	(each (lambda (inst)
		(cond
			((eql (first inst) 'emit-alloc) (push which_of (++ allocs)) (push sizes (second inst)))
			((push which_of 0))))
		insts)
	(when (> count 0) (elem-set stacks 0 (list)))
	(while (defq at (pop todo))
		(defq inst (elem-get insts at) op (first inst) stack (elem-get stacks at))
		(cond
			((eql op 'emit-alloc) (setq stack (cat stack (list (elem-get which_of at)))))
			((eql op 'emit-free)
				(defq bytes (second inst) stack (cat stack))
				(while (and (> bytes 0) (nempty? stack))
					(setq bytes (- bytes (elem-get sizes (pop stack)))))
				(if (/= bytes 0) (setq odd :t))))
		(each (lambda (to)
			(cond
				((not (elem-get stacks to)) (elem-set stacks to stack) (push todo to))
				((nql (str (elem-get stacks to)) (str stack)) (setq odd :t))))
			(elem-get next at)))
	(unless odd (list stacks sizes)))

(defun keep-place (stack sizes offset)
	; (keep-place stack sizes offset) -> :nil | (which offset)
	;a place so far up from the stack pointer: which frame it is in, the
	;last made first, and where in that frame. :nil if it is above them all
	(defq at (length stack) found :nil)
	(while (and (not found) (>= (setq at (dec at)) 0))
		(defq which (elem-get stack at) size (elem-get sizes which))
		(if (< offset size) (setq found (list which offset)) (setq offset (- offset size))))
	found)

(defun keep-local (frames which offset)
	; (keep-local frames which offset) -> (name whole)
	;the local a place of a frame is of, and if the place is all of a
	;local that is not a struct: the local begins there. A place of a
	;frame that has no named locals, or is part way in to one, is not
	(defq frame (some (# (if (= (first %0) which) %0)) frames) found :nil)
	(when frame
		(each (lambda ((name at type))
			(and (<= at offset) (or (not found) (> at (second found))) (setq found (list name at type))))
			(third frame)))
	(if found
		(list (cat (str which) ":" (first found)) (and (= (second found) offset) (nql (third found) "")))
		(list (cat (str which) "+" (str offset)) :nil)))

(defun keep-flow (next prev events slot home_dead)
	; (keep-flow next prev events slot home_dead) -> (wanted had memory)
	;for one local, at each instruction: is its value wanted in its home
	;after this, is it had there before this, by every way, and is what
	;is on the stack wanted after this. home_dead is, for each
	;instruction, if it is a call that trashes the home
	(defq count (length next) wanted (map (lambda (n) :nil) next) wanted_in (map (lambda (n) :nil) next)
		had (map (lambda (n) :t) next) memory (map (lambda (n) :nil) next) memory_in (map (lambda (n) :nil) next)
		changed :t)
	;wanted in the home: a read may come, before a write of it or a call
	;that trashes the home. Backwards, till nothing changes
	(while changed
		(setq changed :nil)
		(defq at count)
		(while (>= (setq at (dec at)) 0)
			(defq out (if (some (# (elem-get wanted_in %0)) (elem-get next at)) :t) event (elem-get events at)
				in (cond
					((and event (eql (second event) slot)) (eql (first event) :load))
					((elem-get home_dead at) :nil)
					(out)))
			(unless (and (eql out (elem-get wanted at)) (eql in (elem-get wanted_in at)))
				(elem-set wanted at out) (elem-set wanted_in at in)
				(setq changed :t))))
	;had in the home, by every way here. After a write that is copied to
	;the home, or a read that goes by it, it is. After a call that
	;trashes the home, or a write that is not copied, it is not
	(defq had_out (lambda (at)
		(defq event (elem-get events at) in (elem-get had at))
		(cond
			((and event (eql (second event) slot))
				(if (eql (first event) :store) (elem-get wanted at) (if (or in (elem-get wanted at)) :t)))
			((elem-get home_dead at) :nil)
			(in))))
	(when (> count 0) (elem-set had 0 :nil))
	(setq changed :t)
	(while changed
		(setq changed :nil)
		(each (lambda (from)
			(defq at (!))
			(when (and (> at 0) (nempty? from))
				(defq in (if (every (# (had_out %0)) from) :t))
				(unless (eql in (elem-get had at))
					(elem-set had at in)
					(setq changed :t))))
			prev))
	;what is on the stack is wanted: a read that is not from the home may
	;come before a write. Backwards
	(setq changed :t)
	(while changed
		(setq changed :nil)
		(defq at count)
		(while (>= (setq at (dec at)) 0)
			(defq out (if (some (# (elem-get memory_in %0)) (elem-get next at)) :t) event (elem-get events at)
				in (cond
					;a write, and what was there is not wanted. A read that is
					;of the stack wants it; one that is of the home does not,
					;and what comes after it may
					((and event (eql (second event) slot))
						(if (and (eql (first event) :load) (or out (not (elem-get had at)))) :t))
					(out)))
			(unless (and (eql out (elem-get memory at)) (eql in (elem-get memory_in at)))
				(elem-set memory at out) (elem-set memory_in at in)
				(setq changed :t))))
	(list wanted had memory))

(defun keep-locals (insts frames mine fixed callee)
	; (keep-locals insts frames mine fixed callee) -> :nil | (insts kept)
	;the instructions of a function, from where it is entered to its
	;end, with its named locals kept in registers where they can be, and
	;what was done, ((local home reads writes) ...): for each local kept,
	;its home, and how many reads and writes of the stack are gone.
	;:nil if nothing is done.
	;
	;frames is what its frames of named locals are, each (which size
	;((name offset type) ...)), (scope-note). mine is the registers the
	;function may trash, fixed those it takes or gives a value in.
	;callee is asked of each call and jump out, the instruction, and
	;gives (trashes takes_and_gives), trashes :nil if it is not known,
	;or :nil if what is called is not known at all
	(defq count (length insts) labels (Fmap 31) never :nil used (list))
	(each (lambda (inst)
		(defq op (first inst))
		(cond
			((eql op 'emit-label) (. labels :insert (last (second inst)) (!)))
			((find op +keep_never) (setq never :t)))
		(each (# (and (sym? %0) (find %0 +keep_regs) (not (find %0 used)) (push used %0))) inst))
		insts)
	;an address taken of a label of the function: where it goes is not seen
	(each (lambda (inst)
		(and (find (first inst) '(emit-lea-p emit-cpy-pr)) (. labels :find (second inst)) (setq never :t)))
		insts)
	(when (and (not never) (> count 0) (nempty? frames) (defq next (keep-next insts labels)))
		(defq prev (map (lambda (n) (list)) next))
		(each (lambda (on) (defq from (!)) (each (# (push (elem-get prev %0) from)) on)) next)
		;a label nothing is seen to go to is gone to by a way not seen
		(each (lambda (inst)
			(and (eql (first inst) 'emit-label) (> (!) 0) (empty? (elem-get prev (!))) (setq never :t)))
			insts)
		(when (and (not never) (defq framed (keep-stacks insts next)))
			(bind '(stacks sizes) framed)
			;what is done to which local at each instruction, and what each
			;call trashes. A local that can not be kept is put in pinned
			(defq slots (list) places (list) taken_at (list) pinned (list) events (list) home_calls (list) taken (cat fixed))
			(each (lambda (inst)
				(defq op (first inst) stack (ifn (elem-get stacks (!)) (list)) event :nil place :nil whole :t called :nil)
				(cond
					((and (find op +keep_loads) (eql (second inst) :rsp))
						(if (setq place (keep-place stack sizes (third inst))) (setq event :load whole (eql op 'emit-cpy-ir))))
					((and (find op +keep_stores) (eql (third inst) :rsp))
						(if (setq place (keep-place stack sizes (elem-get inst 3))) (setq event :store whole (eql op 'emit-cpy-ri))))
					((and (eql op 'emit-lea-i) (eql (second inst) :rsp))
						(if (setq place (keep-place stack sizes (third inst))) (setq event :addr whole :nil)))
					((find op '(emit-alloc emit-free)))
					;a push or a pop moves the stack pointer: with a frame
					;there, what is at a place from it is not followed
					((find op '(emit-push emit-pop)) (if (nempty? stack) (setq never :t)))
					((find :rsp inst) (setq never :t))
					((or (find op +keep_calls) (find op '(emit-jmp-p emit-jmp-i)))
						(cond
							((eql op 'emit-call-abi) (setq called (list :nil (list))))
							((setq called (callee inst)))
							((setq never :t)))
						(when called
							(each (# (unless (find %0 taken) (push taken %0))) (second called))
							;a jump out does not come back: nothing is wanted after it
							(unless (find op +keep_calls) (setq called :nil)))))
				(push home_calls (if called (list (first called))))
				(cond
					(event
						(bind '(name named) (keep-local frames (first place) (second place)))
						(unless (find name slots) (push slots name) (push places place))
						(if (eql event :addr) (push taken_at place))
						(and (not (and named whole)) (not (find name pinned)) (push pinned name))
						(push events (list event name)))
					((push events :nil))))
				insts)
			;an address taken is of all that is above it in the frame, for all
			;that is known here: a call given it may fill more than the one
			(each (lambda (slot)
				(defq place (elem-get places (!)))
				(and (some (# (and (= (first %0) (first place)) (<= (second %0) (second place)))) taken_at)
					(not (find slot pinned)) (push pinned slot)))
				slots)
			(unless never
				;the registers a local may have: the most wanted local first,
				;and for it the register the fewest calls trash
				(defq homes (filter (# (not (or (find %0 used) (find %0 taken)))) mine)
					keeping (sort (filter (# (not (find %0 pinned))) slots)
						(# (- (length (filter (lambda (e) (and e (eql (second e) %1))) events))
							(length (filter (lambda (e) (and e (eql (second e) %0))) events)))))
					trashed_by (lambda (home) (length (filter (# (and %0 (or (not (first %0)) (find home (first %0))))) home_calls)))
					;of two that as many trash, the lower: it is the same every time
					homes (sort homes (# (if (= (defq a (trashed_by %0)) (defq b (trashed_by %1)))
						(- (find %0 +keep_regs) (find %1 +keep_regs)) (- a b))))
					edits (map (lambda (n) :nil) next) kept (list))
				(each (lambda (slot)
					(when (nempty? homes)
						(defq home (first homes)
							home_dead (map (# (and %0 (or (not (first %0)) (find home (first %0))) :t)) home_calls))
						(bind '(wanted had memory) (keep-flow next prev events slot home_dead))
						;what each read and write of it becomes, and what that saves
						(defq mine_edits (list) reads 0 writes 0)
						(each (lambda (event)
							(when (and event (eql (second event) slot))
								(defq at (!) inst (elem-get insts at))
								(cond
									((eql (first event) :store)
										(defq src (second inst) to_stack (elem-get memory at) to_home (elem-get wanted at))
										(unless to_stack (++ writes))
										(push mine_edits (list at (cat (if to_stack (list inst) (list))
											(if to_home (list (list 'emit-cpy-rr src home)) (list))))))
									((elem-get had at)
										(++ reads)
										(push mine_edits (list at (list (list 'emit-cpy-rr home (last inst))))))
									((elem-get wanted at)
										(push mine_edits (list at (list (list 'emit-cpy-ir :rsp (third inst) home)
											(list 'emit-cpy-rr home (last inst)))))))))
							events)
						;only if it is worth a register: something of the stack is gone
						(when (> (+ reads writes) 0)
							(each (lambda ((at with)) (elem-set edits at with)) mine_edits)
							(push kept (list slot home reads writes))
							(setq homes (rest homes)))))
					keeping)
				(when (nempty? kept)
					(defq out (list))
					(each (lambda (inst)
						(if (defq with (elem-get edits (!)))
							(each (# (push out %0)) with)
							(push out inst)))
						insts)
					(list out kept))))))

;module
(export-symbols '(keep-locals))
(env-pop)

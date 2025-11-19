;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WASM Control Flow Graph (CFG) Analysis
; Converts unstructured VP jumps to WASM structured blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Structures for CFG Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A basic block is a sequence of instructions with:
; - One entry point (label or fall-through)
; - One or more exit points (branches or fall-through)
(defun make-block (id start_pc end_pc label)
	; (make-block id start_pc end_pc label) -> block
	(scatter (Emap)
		:id id
		:start_pc start_pc
		:end_pc end_pc
		:label label
		:successors (list)
		:predecessors (list)
		:loop_header :nil
		:loop_depth 0
		:block_type :nil))  ; :nil, :loop, :if, :block

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Build CFG from VP Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-labels (emit_code)
	; (find-labels emit_code) -> label_map
	; Build map of label -> instruction index
	(defq label_map (Emap) pc 0)
	(each (lambda (inst)
		(when (eql (first inst) 'emit-label)
			(. label_map :insert (last (last inst)) pc))
		(setq pc (inc pc)))
		emit_code)
	label_map)

(defun find-branches (emit_code)
	; (find-branches emit_code) -> (list of (pc op target_label))
	; Find all branch/jump instructions
	(defq branches (list) pc 0)
	(each (lambda (inst)
		(defq op (first inst))
		(when (find op '(emit-jmp emit-beq-cr emit-bne-cr emit-bne-rr emit-beq-rr
						emit-bge-rr emit-ble-rr emit-bge-cr emit-ble-cr
						emit-blt-cr emit-blt-rr emit-bgt-rr emit-bgt-cr
						emit-jmp-p emit-call emit-call-p))
			; Extract target label (usually last or second-to-last element)
			(defq target (if (or (eql op 'emit-call) (eql op 'emit-call-p) (eql op 'emit-jmp))
							(last inst)
							(last (most inst))))  ; For conditional branches, target is before depth
			(push branches (list pc op target)))
		(setq pc (inc pc)))
		emit_code)
	branches)

(defun build-basic-blocks (emit_code label_map branches)
	; (build-basic-blocks emit_code label_map branches) -> blocks
	; Identify basic block boundaries
	(defq leaders (Fset 31)  ; Set of instruction indices that start blocks
		block_id 0)

	; Entry is always a leader
	(. leaders :insert 0)

	; Labels are leaders
	(. label_map :each (lambda (label pc)
		(. leaders :insert pc)))

	; Instructions after branches are leaders
	(each (lambda ((pc op target))
		(when (< (inc pc) (length emit_code))
			(. leaders :insert (inc pc))))
		branches)

	; Build blocks from leaders
	(defq sorted_leaders (sort (map (const identity) (. leaders :each (const identity))))
		blocks (list))

	(each! (lambda (start)
		(defq end (if (< (! +1) (length sorted_leaders))
					(dec (elem-get sorted_leaders (inc (!))))
					(dec (length emit_code))))
		(defq label :nil)
		; Find if this block has a label
		(. label_map :each (lambda (lbl pc)
			(when (= pc start) (setq label lbl))))
		(push blocks (make-block block_id start end label))
		(setq block_id (inc block_id)))
		sorted_leaders)

	blocks)

(defun connect-blocks (blocks emit_code branches)
	; (connect-blocks blocks emit_code branches)
	; Add successor/predecessor edges between blocks
	(defq pc_to_block (Emap))

	; Build index: pc -> block
	(each (lambda (block)
		(defq start (. block :find :start_pc)
			end (. block :find :end_pc))
		(for (list pc start (<= pc end) (inc pc))
			(. pc_to_block :insert pc block)))
		blocks)

	; Add edges for branches
	(each (lambda ((pc op target))
		(defq src_block (. pc_to_block :find pc))
		; Find target block by label
		(defq tgt_block (some (lambda (b)
			(when (eql (. b :find :label) target) b))
			blocks))
		(when (and src_block tgt_block)
			(defq src_succs (. src_block :find :successors)
				tgt_preds (. tgt_block :find :predecessors))
			(unless (find tgt_block src_succs)
				(push src_succs tgt_block)
				(def src_block :successors src_succs))
			(unless (find src_block tgt_preds)
				(push tgt_preds src_block)
				(def tgt_block :predecessors tgt_preds))))
		branches)

	; Add edges for fall-through
	(each! (lambda (block)
		(when (< (!) (dec (length blocks)))
			(defq next_block (elem-get blocks (inc (!))))
			; Check if last instruction is unconditional jump or return
			(defq last_inst (elem-get emit_code (. block :find :end_pc))
				last_op (first last_inst))
			(unless (or (eql last_op 'emit-jmp) (eql last_op 'emit-jmp-p)
					   (eql last_op 'emit-ret) (eql last_op 'emit-return))
				; Add fall-through edge
				(defq succs (. block :find :successors)
					preds (. next_block :find :predecessors))
				(unless (find next_block succs)
					(push succs next_block)
					(def block :successors succs))
				(unless (find block preds)
					(push preds block)
					(def next_block :predecessors preds)))))
		blocks))

(defun detect-loops (blocks)
	; (detect-loops blocks)
	; Detect natural loops using dominance analysis
	; A loop exists when there's a back edge (edge to a dominator)

	; Simple loop detection: look for edges to earlier blocks
	(each (lambda (block)
		(defq block_id (. block :find :id)
			succs (. block :find :successors))
		(each (lambda (succ)
			(defq succ_id (. succ :find :id))
			; Back edge if successor has lower id (earlier in code)
			(when (<= succ_id block_id)
				(def succ :loop_header :t)
				(def succ :block_type :loop)))
			succs))
		blocks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WASM Block Depth Calculation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calculate-block-depths (blocks emit_code)
	; (calculate-block-depths blocks emit_code) -> depth_map
	; Calculate WASM block nesting depth for each label
	; Returns map: label -> depth from that block

	(defq depth_map (Emap)
		block_stack (list))  ; Stack of (block label depth)

	; For now, simple strategy: each label gets its own block
	; Depth is calculated based on how many enclosing blocks
	(each (lambda (block)
		(defq label (. block :find :label))
		(when label
			(. depth_map :insert label (length block_stack))
			; Push this block onto stack
			(push block_stack (list block label (length block_stack)))))
		blocks)

	depth_map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Convert to WASM Structured Form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-wasm-block-start (label block_type)
	; Emit WASM block/loop/if start
	(case block_type
		(:loop
			(emit-byte +wasm_loop)
			(emit-byte 0x40))  ; void type
		(:block
			(emit-byte +wasm_block)
			(emit-byte 0x40))
		(:if
			(emit-byte +wasm_if)
			(emit-byte 0x40))
		(:t :nil)))

(defun emit-wasm-block-end ()
	; Emit WASM block end
	(emit-byte +wasm_end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main CFG Analysis Entry Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun analyze-control-flow (emit_code)
	; (analyze-control-flow emit_code) -> cfg_info
	; Analyze control flow and return CFG information
	; Returns: (scatter cfg_info
	;            :blocks blocks
	;            :label_map label_map
	;            :depth_map depth_map
	;            :branches branches)

	(defq label_map (find-labels emit_code)
		branches (find-branches emit_code)
		blocks (build-basic-blocks emit_code label_map branches))

	(connect-blocks blocks emit_code branches)
	(detect-loops blocks)

	(defq depth_map (calculate-block-depths blocks emit_code))

	(scatter (Emap)
		:blocks blocks
		:label_map label_map
		:depth_map depth_map
		:branches branches))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper: Get Branch Depth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-branch-depth (target_label depth_map current_depth)
	; (get-branch-depth target_label depth_map current_depth) -> depth
	; Calculate relative depth for br/br_if instruction
	; In WASM, depth 0 is current block, 1 is parent, etc.

	(defq target_depth (. depth_map :find target_label))
	(if target_depth
		(- current_depth target_depth)
		0))  ; Default to 0 if not found

;module
(export-symbols '(analyze-control-flow get-branch-depth
	emit-wasm-block-start emit-wasm-block-end
	make-block find-labels find-branches build-basic-blocks
	connect-blocks detect-loops calculate-block-depths))
(env-pop)

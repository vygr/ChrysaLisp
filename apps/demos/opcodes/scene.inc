;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The scene of the Opcodes demo, and the message for a slice of it.
; The app moves the opcodes about, and draws them, and so do its
; children, each a slice of the rows, straight onto the shared pixmap
; of the app's canvas. Where every opcode is goes out with each slice.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/vector.inc")
(import "lib/asm/ops.inc")
(import "lib/task/jobs.inc")

;a slice for a child to draw, the key of the shared pixels and the rows,
;then a +bouncer for every opcode
(structure +slice +job_size
	(long shared)
	(uint y y1)
	(offset bouncers))

;its answer, the first row, and how many opcodes it drew, -1 if it could
;not reach the pixels
(structure +slice_reply +job_reply_size
	(uint y drawn))

;an opcode as it is for one frame, the numbers are fixeds
(structure +bouncer 0
	(long model x y angle scale phase sq_x sq_y))

(defq +scene_width 800 +scene_height 600 +scene_back 0xff0a0e14
	f_width (n2f +scene_width) f_height (n2f +scene_height) f_scale 1.0
	font (create-font "fonts/Hack-Regular.ctf" 34))

; Assign chromatic color palettes based on opcode category
(defun opcode-colors (name)
	(cond
		; Floating point: Electric Cyan / Ice Blue
		((or (ends-with "-ff" name) (ends-with "-rf" name) (ends-with "-fr" name)
			 (ends-with "-if" name) (ends-with "-fi" name) (ends-with "-df" name)
			 (ends-with "-fd" name) (starts-with "sqrt" name) (starts-with "cvt" name))
			(list 0xff00e5ff 0xffe0ffff 0x4000e5ff))

		; Control Flow: Electric Violet / Magenta
		((or (starts-with "call" name) (starts-with "jmp" name) (starts-with "ret" name)
			 (starts-with "beq" name) (starts-with "bne" name) (starts-with "bge" name)
			 (starts-with "ble" name) (starts-with "blt" name) (starts-with "bgt" name)
			 (starts-with "sync" name) (starts-with "brk" name))
			(list 0xffd500f9 0xfffce4ec 0x40d500f9))

		; Conditionals & Sets: Hot Pink / Ruby
		((or (starts-with "seq" name) (starts-with "sne" name) (starts-with "slt" name)
			 (starts-with "sle" name) (starts-with "sgt" name) (starts-with "sge" name))
			(list 0xffff007f 0xfffce4ec 0x40ff007f))

		; Data Movement: Radiant Gold / Amber
		((or (starts-with "cpy" name) (starts-with "lea" name) (starts-with "swp" name)
			 (starts-with "ext" name))
			(list 0xffffab00 0xfffff8e1 0x40ffab00))

		; Stack & Memory: Sunset Orange
		((or (starts-with "push" name) (starts-with "pop" name) (starts-with "alloc" name)
			 (starts-with "free" name) (starts-with "stack" name))
			(list 0xffff3d00 0xffffebe7 0x40ff3d00))

		; Bitwise Shifts & Logic: Sun Yellow
		((or (starts-with "shl" name) (starts-with "shr" name) (starts-with "asr" name)
			 (starts-with "and" name) (starts-with "or" name) (starts-with "xor" name)
			 (starts-with "lnot" name) (starts-with "land" name))
			(list 0xffffd600 0xfffffde7 0x40ffd600))

		; Arithmetic: Radioactive Lime / Mint
		((or (starts-with "add" name) (starts-with "sub" name) (starts-with "mul" name)
			 (starts-with "div" name) (starts-with "min" name) (starts-with "max" name)
			 (starts-with "abs" name) (starts-with "neg" name))
			(list 0xff00e676 0xffe8f5e9 0x4000e676))

		; Directives & Architecture: Sky Blue
		(:t
			(list 0xff2979ff 0xffe3f2fd 0x402979ff))))

; Build pre-centered glyph and stroke models for a VP opcode symbol
(defun make-opcode-model (op_sym)
	(defq name (slice (str op_sym) 5 -1))
	(bind '(col_outline col_core col_glow) (opcode-colors name))
	(defq raw_paths (font-glyph-paths font name))
	(bind '((min_x min_y) (max_x max_y)) (vector-bounds-2d raw_paths))
	(defq cx (* (+ min_x max_x) 0.5)
		cy (* (+ min_y max_y) 0.5)
		hw (+ (* (- max_x min_x) 0.5) (* f_scale 4.0))
		hh (+ (* (- max_y min_y) 0.5) (* f_scale 4.0)))
	; Center all glyph paths symmetrically around (0, 0)
	(defq core_paths (map (#
			(path-transform (fixeds
				1.0 0.0 (neg cx)
				0.0 1.0 (neg cy))
				%0 (cat %0))) raw_paths))
	; Generate multi-layered stroked outlines
	(defq outline_paths (path-stroke-polygons (list) (* f_scale 2.2) +join_round core_paths)
		glow_paths (path-stroke-polygons (list) (* f_scale 5.5) +join_round core_paths))
	(list name core_paths outline_paths glow_paths col_outline col_core col_glow hw hh))

;a model is made the first time its opcode is drawn, and kept. There are
;a good many opcodes, and a child that made them all before its first
;slice would keep the app waiting
(defq +opcode_count (length +vp_emit_ops) opcode_models (map (lambda (&) :nil) +vp_emit_ops))

(defun opcode-model (index)
	; (opcode-model index) -> (name core outline glow col_outline col_core col_glow hw hh)
	(unless (defq model (elem-get opcode_models index))
		(elem-set opcode_models index
			(setq model (make-opcode-model (elem-get +vp_emit_ops index)))))
	model)

(defun opcode-index (op_sym)
	(ifn (find op_sym +vp_emit_ops) 0))

(defun scene-pack (bouncers)
	; (scene-pack bouncers) -> str
	;where every opcode is, to go out with a slice. A bouncer is
	;(model pos vel angle omega scale phase sq_x sq_y)
	(apply (const cat) (cat (list "") (map (lambda ((model pos vel angle omega scale phase sq_x sq_y))
		(apply (const cat) (map (# (char (n2i (* %0 65536.0)) +long_size))
			(list (/ (n2f model) 65536.0) (first pos) (second pos) angle scale phase sq_x sq_y))))
		bouncers))))

(defun scene-draw (canvas packed y y1)
	; (scene-draw canvas packed y y1) -> drawn
	;the rows of the scene from y up to y1, how many opcodes were drawn is
	;the result. One that does not reach the rows is not placed or drawn,
	;and the clip of the canvas cuts those that do to the rows.
	(defq fy (n2f y) fy1 (n2f y1) drawn 0)
	(.-> canvas (:set_clip 0 y +scene_width y1)
		(:set_color +scene_back) (:fbox 0 y +scene_width (- y1 y)))
	(each (lambda (at)
		(bind '(model x y angle scale phase sq_x sq_y)
			(map (# (/ (n2f %0) 65536.0)) (getf-> (slice packed at (+ at +bouncer_size))
				+bouncer_model +bouncer_x +bouncer_y +bouncer_angle +bouncer_scale
				+bouncer_phase +bouncer_sq_x +bouncer_sq_y)))
		(bind '(name core_paths outline_paths glow_paths col_outline col_core col_glow hw hh)
			(opcode-model (n2i (* model 65536.0))))
		;the broad phase, it is squashed and stretched by a quarter at most
		(defq breath (+ 1.0 (* 0.06 (sin phase)))
			reach (+ (* (+ hw hh) scale breath 1.3) 8.0))
		(when (and (> (+ y reach) fy) (< (- y reach) fy1))
			; Compute animated breathing scale and squash/stretch matrix
			(defq final_sx (* scale breath sq_x)
				final_sy (* scale breath sq_y)
				sa (sin angle) ca (cos angle)
				tx_matrix (fixeds (* final_sx ca) (* final_sx (* sa -1.0)) x
					(* final_sy sa) (* final_sy ca) y))
			;a stroked outline laps over itself wherever the glyph is thinner
			;than the stroke is wide, and where two letters are close. So it
			;is filled by the none zero rule, odd even would leave holes there
			; 1. Outer Translucent Neon Glow
			(.-> canvas (:set_color col_glow) (:fpoly 0.0 0.0 +winding_none_zero
				(map (# (path-transform tx_matrix %0 (cat %0))) glow_paths)))
			; 2. Crisp Neon Outline Hull
			(.-> canvas (:set_color col_outline) (:fpoly 0.0 0.0 +winding_none_zero
				(map (# (path-transform tx_matrix %0 (cat %0))) outline_paths)))
			; 3. High-Contrast Luminescent Core
			(.-> canvas (:set_color col_core) (:fpoly 0.0 0.0 +winding_odd_even
				(map (# (path-transform tx_matrix %0 (cat %0))) core_paths)))
			(setq drawn (inc drawn))))
		(range 0 (length packed) +bouncer_size))
	drawn)

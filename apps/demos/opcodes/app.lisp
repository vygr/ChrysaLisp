(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/math/vector.inc")
(import "lib/asm/ops.inc")

(enums +select 0
	(enum main timer))

(enums +event 0
	(enum close))

(defq canvas_width 800 canvas_height 700 canvas_scale 1
	f_width (n2f canvas_width) f_height (n2f canvas_height) f_scale (n2f canvas_scale)
	rate (/ 1000000 30) global_tick 0.0
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

; Pre-compile models for all canonical VP opcodes from lib/asm/ops.inc
(defq *opcode_models* (map (const make-opcode-model) +vp_emit_ops))

(defun find-opcode-model (sym)
	(defq name (slice (str sym) 5 -1))
	(or (some (# (if (eql (first %0) name) %0)) *opcode_models*)
		(first *opcode_models*)))

; Bouncer Particle: (model pos_v vel_v angle omega scale scale_phase squash_x squash_y)
(defun create-bouncer (op_sym x y vx vy &optional scale)
	(defq model (find-opcode-model op_sym)
		ang (* (n2f (random 628)) 0.01)
		om (- (* (n2f (random 200)) 0.00015) 0.015)
		phase (* (n2f (random 628)) 0.01)
		s (ifn scale (+ 0.95 (* (n2f (random 25)) 0.01))))
	(list model
		(Vec2-f x y)
		(Vec2-f vx vy)
		ang om s phase 1.0 1.0))

; Spawn an active flock of bouncers covering diverse opcode families
(defq *bouncers* (list
	(create-bouncer 'emit-call    160.0 160.0  2.1  1.7 1.05)   ; Violet (Control)
	(create-bouncer 'emit-add-rr  560.0 180.0 -2.3  1.8 0.95)   ; Lime (Arithmetic)
	(create-bouncer 'emit-cpy-rr  380.0 320.0  1.7 -2.0 1.10)   ; Amber (Data)
	(create-bouncer 'emit-sqrt-ff 220.0 480.0 -1.9  2.2 1.00)   ; Cyan (Float)
	(create-bouncer 'emit-shl-rr  620.0 520.0 -2.0 -1.6 0.90)   ; Yellow (Bitwise)
	(create-bouncer 'emit-push    420.0 150.0  2.3 -1.5 1.00)   ; Orange (Stack)
	(create-bouncer 'emit-slt-rr  260.0 300.0 -1.6 -2.1 0.95)   ; Hot Pink (Compare)
	(create-bouncer 'emit-alloc   520.0 380.0  1.9  1.9 0.90)   ; Orange (Memory)
	(create-bouncer 'emit-div-rrr 180.0 580.0  2.2 -1.7 1.05))) ; Lime (Arithmetic)

(ui-window *window* ()
	(ui-title-bar _ "Kinetic VP Opcodes" (0xea19) +event_close)
	(ui-canvas *canvas* canvas_width canvas_height canvas_scale))

(defun fpoly (col mode %2)
	(.-> *canvas* (:set_color col) (:fpoly 0.0 0.0 mode %2)))

(defun update-physics ()
	(++ global_tick 0.02)
	(each (lambda (b)
		(bind '(model pos vel angle omega scale phase sq_x sq_y) b)
		(bind '(name core_paths outline_paths glow_paths col_outline col_core col_glow hw hh) model)

		; Update position, rotation, and phase
		(vector-add pos vel pos)
		(setq angle (+ angle omega))
		(setq phase (+ phase 0.045))

		; Relax squash and stretch smoothly back to 1.0
		(setq sq_x (+ sq_x (* (- 1.0 sq_x) 0.12)))
		(setq sq_y (+ sq_y (* (- 1.0 sq_y) 0.12)))

		; Dynamic oriented bounding box extents in world space
		(defq ca (abs (cos angle)) sa (abs (sin angle))
			ext_x (* scale (+ (* hw ca) (* hh sa)))
			ext_y (* scale (+ (* hw sa) (* hh ca))))

		(bind '(x y) pos)
		(bind '(vx vy) vel)

		; Smooth wall reflection with occasional opcode morph on bounce
		; Left wall
		(when (and (< x ext_x) (< vx 0.0))
			(setq vx (abs vx)
				sq_x 0.75 sq_y 1.25)
			(when (= 0 (random 2))
				(elem-set b 0 (elem-get *opcode_models* (random (length *opcode_models*))))))

		; Right wall
		(when (and (> x (- f_width ext_x)) (> vx 0.0))
			(setq vx (neg (abs vx))
				sq_x 0.75 sq_y 1.25)
			(when (= 0 (random 2))
				(elem-set b 0 (elem-get *opcode_models* (random (length *opcode_models*))))))

		; Top wall
		(when (and (< y ext_y) (< vy 0.0))
			(setq vy (abs vy)
				sq_x 1.25 sq_y 0.75)
			(when (= 0 (random 2))
				(elem-set b 0 (elem-get *opcode_models* (random (length *opcode_models*))))))

		; Bottom wall
		(when (and (> y (- f_height ext_y)) (> vy 0.0))
			(setq vy (neg (abs vy))
				sq_x 1.25 sq_y 0.75)
			(when (= 0 (random 2))
				(elem-set b 0 (elem-get *opcode_models* (random (length *opcode_models*))))))

		; Write back updated values
		(elem-set vel +vec2_x vx)
		(elem-set vel +vec2_y vy)
		(elem-set b 3 angle)
		(elem-set b 6 phase)
		(elem-set b 7 sq_x)
		(elem-set b 8 sq_y)) *bouncers*))

(defun draw-backdrop ()
	; Clean deep space black/slate canvas
	(. *canvas* :fill 0xff0b0e14))

(defun redraw ()
	(draw-backdrop)

	; Draw each bouncing opcode with depth shadow, neon outline, and illuminated core
	(each (lambda (b)
		(bind '(model pos vel angle omega scale phase sq_x sq_y) b)
		(bind '(name core_paths outline_paths glow_paths col_outline col_core col_glow hw hh) model)
		(bind '(x y) pos)

		; Compute animated breathing scale and squash/stretch matrix
		(defq breath (+ 1.0 (* 0.06 (sin phase)))
			final_sx (* scale breath sq_x)
			final_sy (* scale breath sq_y)
			sa (sin angle) ca (cos angle)
			ma (* final_sx ca)
			mb (* final_sx (* sa -1.0))
			mc (* final_sy sa)
			md (* final_sy ca))

		; 1. Ambient Drop Shadow (offset proportional to scale)
		(defq sh_off (* scale 8.0)
			sh_matrix (fixeds ma mb (+ x sh_off) mc md (+ y sh_off)))
		(fpoly 0x48000000 +winding_odd_even
			(map (# (path-transform sh_matrix %0 (cat %0))) glow_paths))

		; Main transformation matrix centered at (x, y)
		(defq tx_matrix (fixeds ma mb x mc md y))

		; 2. Outer Translucent Neon Glow
		(fpoly col_glow +winding_odd_even
			(map (# (path-transform tx_matrix %0 (cat %0))) glow_paths))

		; 3. Crisp Neon Outline Hull
		(fpoly col_outline +winding_odd_even
			(map (# (path-transform tx_matrix %0 (cat %0))) outline_paths))

		; 4. High-Contrast Luminescent Core
		(fpoly col_core +winding_odd_even
			(map (# (path-transform tx_matrix %0 (cat %0))) core_paths)))
		*bouncers*)

	(. *canvas* :swap +pixmap_mode_normal))

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t)
	(.-> *canvas* (:fill 0) (:set_canvas_flags +canvas_flag_antialias))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(mail-timeout (elem-get select +select_timer) rate 0)
	(while *running*
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_main)
				(if (= (getf msg +ev_msg_target_id) +event_close)
					(setq *running* :nil)
					(. *window* :event msg)))
			((= idx +select_timer)
				(mail-timeout (elem-get select +select_timer) rate 0)
				(update-physics)
				(redraw))
			(:t (. *window* :event msg))))
	(gui-sub-rpc *window*))

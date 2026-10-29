(report-header "Spectrum: the Z80 starts the ROM, and a game loads from its tape")
(defq *app_root* "apps/games/spectrum/")
(import "lib/consts/scodes.inc")
(import "apps/games/spectrum/machine.inc")

;the Z80 is held in VP registers for a frame, apps/games/spectrum/lisp.vp.
;Each opcode was checked against FUSE's tests and the whole of zexall; what
;this keeps is that the machine as a whole still works: the ROM tests the
;memory, clears the screen and says who made it, each frame's interrupt
;counted in FRAMES, and a game's tape is read to its end by the ROM's
;loader, typed in as a user would

(defun sp-line (m y)
	;the 32 bytes of pixel line y of the screen, summed
	(defq at (logior 0x4000 (<< (logand y 0xc0) 5) (<< (logand y 7) 8) (<< (logand y 0x38) 2)))
	(reduce (lambda (sum i) (+ sum (zx-peek m (+ at i)))) (range 0 32) 0))

(defun sp-frames (m tape n &optional script)
	; (sp-frames machine tape n [script]) -> script
	;n frames, the keys of the script held as it says, the tape read when
	;the ROM asks for it
	(setd script '())
	(defq keys (zx-key-map) joys (zx-joy-map))
	(times n
		(when (nempty? script)
			(defq step (first script))
			(zx-keys m (second step) keys joys)
			(elem-set step 0 (dec (first step)))
			(when (<= (first step) 0) (setq script (rest script))))
		(while (= (zx-run m) +zx_stop_tape) (zx-tape-trap m tape)))
	script)

(defq sp_rom (load "apps/games/spectrum/data/48.rom"))
(assert-eq "the ROM is there, 16K" 0x4000 (length sp_rom))

(defq sp_m (zx-machine sp_rom))
(sp-frames sp_m :nil 100)
(assert-true "the ROM has the Z80 in its interrupt mode 1" (= 1 (getf sp_m +zx_im)))
(defq sp_frames (zx-peek16 sp_m 0x5c78))
(sp-frames sp_m :nil 10)
(assert-eq "each frame's interrupt counts in FRAMES" 10 (- (zx-peek16 sp_m 0x5c78) sp_frames))
(assert-true "the screen is clear but for its last line"
	(every (# (= 0 (sp-line sp_m %0))) (range 0 184)))
(assert-true "and that has the words of who made it" (> (sp-line sp_m 188) 0))

;a game, LOAD "" typed in, the ROM's loader trapped and its blocks put
;in memory at once, the code it then runs is in RAM
(defq sp_tape (list (zx-tap-blocks (load "apps/games/spectrum/data/Pyjamarama.tap")) 0))
(assert-eq "the tape has its blocks, headers and data" 6 (length (first sp_tape)))
(zx-reset sp_m)
(setf sp_m +zx_trap 1)
(sp-frames sp_m sp_tape 100)
(defq sp_script (list))
(each (lambda (codes) (push sp_script (list 4 codes) (list 8 '())))
	(list (list +sc_j) (list +sc_lctrl +sc_p) (list +sc_lctrl +sc_p) (list +sc_return)))
(sp-frames sp_m sp_tape 200 sp_script)
(assert-eq "every block of it was read" 6 (second sp_tape))
(assert-true "and the game runs, from RAM" (>= (getf sp_m +zx_pc) 0x4000))

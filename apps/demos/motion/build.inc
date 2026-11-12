;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The build: one function, :list :cat, from its source to a call of it. Its
; VP, its native code, the boot image it is in and the binding of that, then
; Lisp binding the function that calls it, and the call. Every name, number
; and byte is the tree's, read from obj/ as it was when this was written:
; the bundle of class/list/class.vp, obj/arm64/ARM64/class/list/cat, and the
; arm64 boot image walked as sys/load/init walks it.
;
; As in the story a caption's end brings on the next thing. Each part is
; written above the part that brings it on, so a name is bound as it is read.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +build_paper 0xff0a0e1a +build_rules 0xff141c2e +build_panel 0xff1a2440 +build_panel2 0xff090d18
	+build_cyan 0xff00e5ff +build_violet 0xffd500f9 +build_gold 0xffffab00 +build_lime 0xff00e676
	+build_pink 0xffff4081 +build_blue 0xff448aff +build_orange 0xffff6d00 +build_red 0xffff1744
	+build_dim 0xff6a7a90 +build_shade 0xe00a0e1a
	;the boot image of arm64, 238 functions, as a grid 17 wide, and the grid
	;made small for the end
	+build_funcs 238 +build_cols 17 +build_small 0.45
	;the source of :list :cat, class/list/class.vp
	+build_source ''("(def-method :list :cat)" "  (entry :list :cat '(:r0 :r1))" "  (class/list/cat)"
		"  (exit :list :cat '(:r0))" "  (vp-ret)" "(def-func-end)")
	+build_source_ink ''(0xffd500f9 0xff6a7a90 0xffffab00 0xff6a7a90 0xffff4081 0xffd500f9)
	;what it includes, and with what they include and import 88 files
	+build_includes ''("lib/asm/func.inc" "lib/consts/chars.inc" "class/list/class.inc"
		"class/stream/class.inc" "88 files in all")
	;its entry of the bundle, obj/vp/class/list/class.vp
	+build_vp ''("(\qclass/list/cat\q" "((label '_0)" "(long -1)" "(short _7 _2 _3 _5 6144)" "(label '_1)"
		"(string \qclass/list/cat\q)" "(byte 0)" "(byte (- _2 _1))" "(align 8 (- _2 _1))" "(label '_2)"
		"(call-p _3)" "(jmp-p _4)" "(align 8)" "(label '_3)" "(long (- _5 *pc*))" "(label '_4)"
		"(long (- _6 *pc*))" "(label '_5)" "(string \qclass/array/cat\q)" "(byte 0)" "(label '_6)"
		"(string \qclass/list/ref_all\q)" "(byte 0)" "(align 8)" "(label '_7)))")
	+build_vp_ink ''(0xffd8e4f0 0xff6a7a90 0xff448aff 0xff448aff 0xff6a7a90 0xffffab00 0xff448aff 0xff448aff
		0xff6a7a90 0xff6a7a90 0xff00e5ff 0xffff4081 0xff6a7a90 0xff6a7a90 0xffd500f9 0xff6a7a90 0xffd500f9
		0xff6a7a90 0xffffab00 0xff448aff 0xff6a7a90 0xffffab00 0xff448aff 0xff6a7a90 0xff6a7a90)
	;the translators, and how long :list :cat is in the code of each
	+build_lanes ''(("arm64" 0xff00e676) ("x86_64 AMD64" 0xff448aff) ("x86_64 WIN64" 0xff00e5ff)
		("riscv64" 0xffffab00) ("la64" 0xffff6d00) ("vp64" 0xffd500f9))
	+build_sizes ''("arm64  120" "AMD64  112" "WIN64  112" "riscv64  136" "la64  136" "vp64  104")
	;the parts of its arm64 code, bytes from and to
	+build_parts ''((0 18 "header" 0xff448aff) (18 33 "name" 0xffffab00) (33 40 "back" 0xff6a7a90)
		(40 64 "code" 0xff00e676) (64 80 "links" 0xffd500f9) (80 120 "paths" 0xffffab00))
	+build_arm ''("580000dd  ldr  x29, link 0" "f81f8f9e  str  x30, [x28, #-8]!" "d63f03a0  blr  x29"
		"f840879e  ldr  x30, [x28], #8" "5800009d  ldr  x29, link 1" "d61f03a0  br   x29")
	;the slots of the list vtable, the parent first
	+build_vtable ''("0   class/array/vtable" "8   class/list/deinit" "16  class/list/type" "24  class/obj/hash"
		"32  class/array/eql" "40  class/list/print" "48  class/array/get_length" "56  class/list/ref_elem"
		"64  class/list/slice" "72  class/list/cat"))

;;;;;;;;;;;;;;;;
; things to make
;;;;;;;;;;;;;;;;

(defun build-glow (colour)
	;a halo of a colour, seen through
	(logior (logand colour 0xffffff) 0x50000000))

(defun build-add (fig &optional parent)
	;on the world, the group the camera shakes, or in a group of it
	(scene-add fig (ifn parent (get :layer stage))))

(defun build-text (text size x y colour &optional parent)
	;code from its left, in cells, on a line at y, not there till it is typed
	(defq fig (Figure "text"))
	(. fig :set_outline (scene-mono text size))
	(def fig :fig_fill colour :fig_x x :fig_y y :fig_reveal 0.0)
	(build-add fig parent))

(defun build-type (figs secs &optional wait then)
	;typed a letter at a time, one after another, so long each, then what
	;follows is fired
	(defq at (ifn wait 0.0) last_one (dec (length figs)))
	(each (lambda (fig)
			(. fig :add (Tween :fig_reveal 1.0 secs (const ease-linear) (if (= (!) last_one) then) at))
			(setq at (+ at secs)))
		figs))

(defun build-code (lines colours x y step size &optional parent)
	; (build-code lines colours x y step size [parent]) -> figs
	;lines of code, one under another, each its own colour
	(map (lambda (line colour) (build-text line size x (+ y (* step (n2f (!)))) colour parent))
		lines colours))

(defun build-lines (lines size step)
	; (build-lines lines size step) -> outline
	;lines of code from their left, one under another, the first on 0
	(defq out (list))
	(each (lambda (line)
			(defq m (fixeds 1.0 0.0 0.0 0.0 1.0 (* step (n2f (!)))))
			(each (lambda ((closed p)) (push out (list closed (mat3x2-vec2-mul m p p))))
				(scene-mono line size)))
		lines)
	out)

(defun build-panel (x y w h colour &optional parent)
	;glass, its edge drawn round it, its fill darker down it, and a halo
	(defq fig (Figure "panel"))
	(. fig :set_outline (list (list :t (path-gen-rect 0.0 0.0 (n2f w) (n2f h) 8.0 8.0 (path)))))
	(def fig :fig_x (n2f x) :fig_y (n2f y) :fig_fill +build_panel :fig_fill2 +build_panel2
		:fig_stroke colour :fig_width 1.5 :fig_trim 0.0 :fig_glow (build-glow colour) :fig_glow_size 5.0
		:fig_alpha 0.0)
	(. fig :add (Tween :fig_trim 1.0 0.8 (const ease-in-out)) (Tween :fig_alpha 1.0 0.4))
	(build-add fig parent))

(defun build-flow (points colour &optional parent)
	;a line that draws itself, of dashes that march along it
	(defq fig (Figure "flow"))
	(. fig :set_outline (list (list :nil points)))
	(def fig :fig_stroke colour :fig_width 2.0 :fig_dash '(7.0 6.0) :fig_trim 0.0
		:fig_glow (build-glow colour) :fig_glow_size 4.0)
	(. fig :add (Tween :fig_trim 1.0 0.6 (const ease-out)) (Rate :fig_dash_phase -40.0))
	(build-add fig parent))

(defun build-arc (x y x1 y1 h)
	; (build-arc x y x1 y1 h) -> path
	;a line from one place to another bowed up by h in its middle, or down
	;by a negative h
	(defq out (path))
	(each! (lambda () (defq u (/ (n2f (!)) 16.0))
			(push out (+ x (* u (- x1 x))) (- (+ y (* u (- y1 y))) (* h 4.0 u (- 1.0 u))))) '() 0 17)
	out)

(defun build-dot-end (f c)
	;a dot has come to the end of its way, it goes and says so
	(. f :kill)
	(if (defq then (get :after f)) (then f c)))

(defun build-dot (points secs colour &optional after parent)
	;a bright dot that goes along a way and leaves a trail
	(defq fig (Figure "dot"))
	(. fig :set_outline (list (list :t (path-gen-ellipse 0.0 0.0 4.0 4.0 (path)))))
	(def fig :fig_fill colour :fig_x (first points) :fig_y (second points) :fig_trail 6
		:fig_glow (build-glow colour) :fig_glow_size 6.0 :after after)
	(. fig :add (Follow points secs (const ease-in-out) (const build-dot-end)))
	(build-add fig parent))

(defun build-label (text size x y colour &optional wait parent)
	;code in the middle of a place, that comes into view
	(defq fig (Figure "label"))
	(. fig :set_outline (outline-centred (scene-mono text size)))
	(def fig :fig_fill colour :fig_x x :fig_y y :fig_alpha 0.0)
	(scene-show fig 0.4 (ifn wait 0.0))
	(build-add fig parent))

(defun build-tag (text x y colour)
	;a name typed over the picture, a dark halo behind it so it reads, on
	;the top layer so nothing that comes later is drawn over it
	(defq fig (build-text text 9 x y colour (get :labels stage)))
	(def fig :fig_glow +build_shade :fig_glow_size 3.0)
	(build-type (list fig) 0.5)
	fig)

(defun build-burst (x y colour n)
	(times n (spark-at x y colour)))

(defun build-cell (i)
	; (build-cell i) -> (x y)
	;where a function of the boot image is, in the grid of them
	(list (+ 48.0 (* 33.0 (n2f (% i +build_cols)))) (+ 112.0 (* 20.0 (n2f (/ i +build_cols))))))

(defun build-cell-small (i)
	; (build-cell-small i) -> (x y)
	;where it is once the grid is made small, about its corner, at the left
	(bind '(x y) (build-cell i))
	(list (+ 20.0 (* +build_small (- x 28.0))) (+ 110.0 (* +build_small (- y 100.0)))))

(defun build-colour (i)
	;a function of the image by its folder: init and the statics, class/,
	;lib/, the loader's translator, and sys/
	(cond ((< i 2) +build_gold) ((< i 160) +build_blue) ((< i 162) +build_violet) (+build_lime)))

(defun build-byte (fig)
	;a piece of code on its way to one of the translators
	(defq lanes (get :lanes stage))
	(bind '(& colour y) (elem-get lanes (. stage :random (length lanes))))
	(build-dot (path 430.0 258.0 470.0 y) 0.45 colour))

(defun build-flash (fig)
	;a link of a function bound, somewhere in the image
	(defq i (. stage :random +build_funcs))
	(scene-pulse (elem-get (get :blocks stage) i))
	(bind '(x y) (build-cell i))
	(spark-at x y +build_gold))

(defun build-strip-one (fig)
	;a path table taken out of a function of the image
	(bind '(x y) (build-cell (. stage :random +build_funcs)))
	(spark-at x y +build_dim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the end, then each part above the one that brings it on
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-end (f c)
	;all goes, and a moment later it all begins again
	(scene-go (get :title stage) 1.0)
	(scene-go (get :caption stage) 1.0)
	(scene-go (get :world stage) 1.0)
	(defq clock (Figure "clock"))
	(. clock :add (Wait 2.0 (lambda (f c) (setq restart :t))))
	(scene-add clock))

;;;;;;;;;
; running
;;;;;;;;;

(defun build-result (f c)
	;what comes back
	(defq answer (build-text "(1 2 3 4)" 13 314.0 160.0 +build_gold))
	(def answer :fig_glow (build-glow +build_gold) :fig_glow_size 6.0)
	(build-type (list answer) 0.6)
	(build-burst 360.0 155.0 +build_gold 30)
	(. (get :title stage) :add (Cues (Tween :fig_glow_size 16.0 0.3 (const ease-out))
		(Tween :fig_glow_size 7.0 0.8 (const ease-in-out))))
	(scene-say '("The function we followed from its source: a super call of :array :cat,"
		"then a jump to :list :ref_all, and back comes (1 2 3 4)") 6.0 (const build-end)))

(defun build-call (f c)
	;the call goes from the line typed, by cat's Func to lisp_cat, through
	;the vtable's slot to class/list/cat, to :array :cat and :list :ref_all,
	;and back
	(. f :kill)
	(bind '(ax ay) (build-cell-small 51))
	(bind '(bx by) (build-cell-small 3))
	(bind '(cx cy) (build-cell-small 62))
	(build-dot (path 470.0 122.0 560.0 210.0 400.0 393.0 ax ay bx by cx cy 470.0 150.0)
		5.0 +build_cyan (const build-result)))

(defun build-repl (f c)
	;the call typed, and the vtable it goes through
	(each (# (scene-go %0 0.4)) (get :load stage))
	(defq figs (list) panel (build-panel 300 92 320 80 +build_pink)
		line (build-text "> (cat '(1 2) '(3 4))" 13 314.0 126.0 +build_pink)
		vt (build-panel 300 194 210 224 +build_cyan)
		head (build-text "class/list/vtable" 10 312.0 212.0 +build_cyan)
		slots (build-code +build_vtable (map (lambda (_) +build_dim) +build_vtable) 312.0 232.0 18.0 10)
		fun (scene-chip "lisp_cat" 565.0 210.0 100 26 +build_gold (get :layer stage) 12)
		rc (build-text "call-rc :r2 72" 10 518.0 246.0 +build_gold))
	(def (last slots) :fig_fill +build_cyan :fig_glow (build-glow +build_cyan) :fig_glow_size 4.0)
	(build-type (list line) 1.0 0.4)
	(build-type (list head) 0.4 1.0)
	(build-type slots 0.12 1.2)
	(build-type (list rc) 0.6 2.4)
	(defq clock (Figure "clock"))
	(. clock :add (Wait 3.2 (const build-call)))
	(build-add clock)
	(scene-say '("cat is a Func: lisp_cat checks its args, then calls :seq :cat through"
		"the vtable of the first list, 72 bytes in, the slot bound to class/list/cat")))

(defun build-func (f c)
	;the address in a Func, and that in the root env
	(defq figs (get :load stage) layer (get :layer stage)
		fun (scene-chip "Func" 380.0 334.0 80 26 +build_gold layer 12)
		sym (scene-chip "cat" 545.0 334.0 70 26 +build_pink layer 13))
	(push figs fun sym (build-flow (path 360.0 266.0 375.0 320.0) +build_gold)
		(build-flow (path 422.0 334.0 508.0 334.0) +build_pink)
		(build-label "the root env" 9 545.0 362.0 +build_pink 0.4))
	(scene-say '("The address is wrapped in a Func, and the Func put in the root env as cat") 4.5 (const build-repl)))

(defun build-load (f c)
	;a new block, the function read into it, its links bound, and it sealed
	;its file read in, 328 bytes: the function, 216 of them, up to the end
	;of its links, and its paths section, 112, side by side
	(defq figs (get :load stage) layer (get :layer stage)
		block (build-panel 300 196 170 86 +build_violet)
		head (build-tag "a new 128 KB block" 312.0 214.0 +build_violet)
		fun (scene-chip "lisp_cat  216" 760.0 252.0 100 24 +build_gold layer 11)
		paths (scene-chip "paths" 820.0 252.0 52 24 +build_dim layer 9))
	(def (get :box paths) :fig_dash '(4.0 3.0))
	(scene-to fun 360.0 252.0 0.8 (const ease-out3) :nil 0.4)
	(scene-to paths 440.0 252.0 0.8 (const ease-out3) :nil 0.4)
	(push figs block head fun paths)
	;three of its links are to functions of the image
	(each (lambda (i)
			(bind '(x y) (build-cell-small i))
			(push figs (build-flow (build-arc 310.0 252.0 x y 24.0) +build_violet)))
		'(140 111 49))
	(push figs (build-tag "class/str/vtable" 150.0 254.0 +build_blue)
		(build-tag "class/seq/vtable" 150.0 266.0 +build_blue)
		(build-tag "class/lisp/repl_error" 150.0 278.0 +build_blue))
	;two are not bound yet, they come in with it
	(each (lambda (name)
			(defq y (+ 238.0 (* 34.0 (n2f (!))))
				chip (scene-chip name 760.0 y 124 22 +build_blue layer 10))
			(scene-to chip 560.0 y 0.8 (const ease-back) :nil (+ 1.0 (* 0.3 (n2f (!)))))
			(push figs chip (build-flow (build-arc 408.0 242.0 498.0 y 16.0) +build_blue)))
		'("env_args_type" "env_args_match"))
	;its paths are what its links were bound by, and once they are they go,
	;the block keeps only the function
	(. paths :add (Tween :fig_alpha 0.0 0.8 (const ease-in) (lambda (f c) (. f :kill)) 2.6))
	(defq clock (Figure "clock"))
	(. clock :add (Wait 2.6 (lambda (f c)
		(. f :kill)
		(build-burst 440.0 252.0 +build_dim 14)
		(push (get :load stage) (build-tag "paths let go, once linked" 396.0 298.0 +build_dim)))))
	(build-add clock)
	;sealed, runnable
	(. block :add (Tween :fig_stroke +build_lime 0.6 (const ease-in-out) :nil 3.4)
		(Tween :fig_glow (build-glow +build_lime) 0.6 (const ease-in-out) :nil 3.4))
	(scene-say '("Read from its file into a 128 KB block, its paths bind its links,"
		"then the paths section is let go: 216 of its 328 bytes stay") 6.0 (const build-func)))

(defun build-miss (f c)
	;it was not found
	(push (get :load stage) (build-tag "not on the list" 20.0 262.0 +build_red))
	(build-burst 150.0 240.0 +build_red 12)
	(scene-say '("It is not on the list: the image did not hold it") 3.0 (const build-load)))

(defun build-search (f c)
	;down the function list, looking for it by name
	(build-dot (get :chain_points stage) 1.8 +build_red (const build-miss) (get :image stage)))

(defun build-lisp (f c)
	;the image made small, and Lisp starting
	(scene-title "Running" +build_pink 40)
	(each (# (scene-go %0 0.5)) (cat (get :host stage) (get :tags stage)))
	(defq image (get :image stage))
	(. image :add (Tween :fig_sx +build_small 1.2) (Tween :fig_sy +build_small 1.2)
		(Tween :fig_x 20.0 1.2) (Tween :fig_y 110.0 1.2))
	(defq panel (build-panel 300 92 320 80 +build_pink)
		head (build-text "class/lisp/root.inc" 10 312.0 110.0 +build_pink)
		line (build-text "(ffi \qclass/seq/lisp_cat\q cat)" 12 312.0 142.0 +scene_ink))
	(build-type (list head) 0.5 1.0)
	(build-type (list line) 1.0 1.6)
	(def stage :load (list panel head line))
	(scene-say '("Lisp starts, and reads class/lisp/root.inc: ffi asks :sys_load :bind"
		"for class/seq/lisp_cat, by its name") 4.5 (const build-search)))

;;;;;;;
; bound
;;;;;;;

(defun build-kernel (f c)
	;where later functions are, and the kernel called
	(defq world (get :world stage) kernel (elem-get (get :blocks stage) 167)
		prefix (build-text "obj/arm64/ARM64/" 11 160.0 80.0 +build_orange))
	(build-type (list prefix) 0.8)
	(push (get :host stage) prefix)
	(def kernel :fig_glow (build-glow +build_gold) :fig_glow_size 8.0)
	(. kernel :add (Tween :fig_fill +build_gold 0.4 (const ease-out) :nil 1.0))
	(scene-size kernel 2.4 0.4 (const ease-out) (lambda (f c) (scene-size f 1.0 0.6 (const ease-back))) 1.0)
	(push (get :tags stage) (build-tag "sys/kernel/kernel" 404.0 296.0 +build_gold))
	(. world :add (Cues (Wait 1.0) (Shake :fig_x 7.0 0.6)) (Cues (Wait 1.0) (Shake :fig_y 7.0 0.6)))
	(defq clock (Figure "clock"))
	(. clock :add (Wait 1.0 (lambda (f c)
		(. f :kill)
		(bind '(x y) (build-cell 167))
		(build-burst x y +build_gold 24))))
	(build-add clock)
	(scene-say '("The folder it was read from, obj/arm64/ARM64/, is kept for later binds,"
		"and sys/kernel/kernel is bound and called") 5.0 (const build-lisp)))

(defun build-bind (f c)
	;each link of each made an address, and all of it runnable
	(defq timer (Figure "timer"))
	(. timer :add (Emit 90.0 (const build-flash) 2.4))
	(push (get :host stage) (build-add timer))
	(each (# (. %0 :add (Tween :fig_stroke +build_lime 1.0 (const ease-in-out) :nil 1.0)
			(Tween :fig_glow (build-glow +build_lime) 1.0 (const ease-in-out) :nil 1.0)))
		(get :our_links stage))
	(. (get :frame stage) :add (Tween :fig_stroke +build_lime 0.8 (const ease-in-out) :nil 2.6)
		(Tween :fig_glow (build-glow +build_lime) 0.8 (const ease-in-out) :nil 2.6))
	(scene-say '("Each link's offset becomes the address of the code of the function it"
		"names, found on the list by name, then all of it is made runnable") 5.0 (const build-kernel)))

(defun build-chain (f c)
	;the statics and all after them, each put on the function list
	(defq points (path) chain (Figure "list"))
	(each! (lambda () (bind '(x y) (build-cell (!))) (push points x y)) '() 1 +build_funcs)
	(. chain :set_outline (list (list :nil points)))
	(def chain :fig_stroke +build_lime :fig_width 1.2 :fig_trim 0.0
		:fig_glow (build-glow +build_lime) :fig_glow_size 3.0)
	(. chain :add (Tween :fig_trim 1.0 2.5 (const ease-in-out)))
	(scene-add chain (get :image stage))
	(def stage :chain_points points)
	(scene-say '("init copies the statics and all after them to memory of their own,"
		"and puts each on the function list") 5.0 (const build-bind)))

(defun build-host (f c)
	;the host, and the image read into memory it can run
	(scene-title "Bound" +build_orange 40)
	(defq layer (get :layer stage) host (scene-chip "chrysalisp" 90.0 74.0 120 22 +build_orange layer 11)
		frame (Figure "frame"))
	(. frame :set_outline (list (list :t (path-gen-rect 26.0 98.0 614.0 412.0 6.0 6.0 (path)))))
	(def frame :fig_stroke +build_orange :fig_width 1.5 :fig_dash '(10.0 6.0) :fig_trim 0.0
		:fig_glow (build-glow +build_orange) :fig_glow_size 4.0)
	(. frame :add (Tween :fig_trim 1.0 1.2 (const ease-in-out)) (Rate :fig_dash_phase -30.0))
	(def stage :host (list host (build-add frame) (build-flow (build-arc 90.0 86.0 48.0 105.0 10.0) +build_orange)
			(build-tag "the image, in memory and runnable" 34.0 402.0 +build_orange))
		:frame frame)
	(scene-say '("The host reads the image into memory, makes it runnable, and calls"
		"init's entry, the short at byte 10, with argv and its tables of calls") 5.5 (const build-chain)))

;;;;;;;;;;;;
; boot image
;;;;;;;;;;;;

(defun build-vp64 (f c)
	(scene-say '("The VP64 image, the one an install runs, holds what an install binds"
		"before it has built anything: 803 functions in 243,884 bytes") 5.0 (const build-host)))

(defun build-ends (f c)
	;its length first, and a length of 0 at the end
	(defq tags (get :tags stage))
	(push tags (build-tag "first long: 71,500" 300.0 96.0 +build_gold)
		(build-tag "length 0, the end" 492.0 402.0 +build_gold))
	;the end, after the last function: a long and an int of 0, a block of
	;its own in the gap before the frame, a line down from it to its name
	(bind '(x y) (build-cell (dec +build_funcs)))
	(defq end (Figure "end"))
	(. end :set_outline (list (list :t (path-gen-rect -3.0 -7.0 3.0 7.0 1.0 1.0 (path)))))
	(def end :fig_x (+ x 20.0) :fig_y y :fig_fill +build_gold :fig_glow (build-glow +build_gold)
		:fig_glow_size 4.0 :fig_sx 0.0 :fig_sy 0.0)
	(scene-size end 1.0 0.5 (const ease-back))
	(push tags (build-add end (get :image stage))
		(build-flow (path (+ x 20.0) (+ y 9.0) (+ x 20.0) 392.0) +build_gold))
	(scene-say '("The first long is the length of the image, and a length of 0 ends it:"
		"for arm64, 238 functions in 71,500 bytes") 5.0 (const build-vp64)))

(defun build-strip (f c)
	;the path tables go, and the links of ours are to the names in the image
	(defq timer (Figure "timer") image (get :image stage))
	(. timer :add (Emit 70.0 (const build-strip-one) 1.6))
	(push (get :tags stage) (build-add timer))
	(bind '(x y) (build-cell 51))
	(bind '(x1 y1) (build-cell 3))
	(bind '(x2 y2) (build-cell 62))
	(def stage :our_links (list (build-flow (build-arc x y x1 y1 30.0) +build_violet image)
		(build-flow (build-arc x y x2 y2 40.0) +build_violet image)))
	(push (get :tags stage) (build-tag "class/array/cat" 166.0 116.0 +build_violet)
		(build-tag "class/list/ref_all" 380.0 158.0 +build_violet)
		(build-tag "120 bytes in its file, 80 here" 66.0 188.0 +build_cyan))
	(scene-say '("Each is kept without its path table: a link now holds the way to the name"
		"in the header of the function it is to, in the image") 5.5 (const build-ends)))

(defun build-image (f c)
	;the functions of the boot image, from all over, into their order
	(scene-title "Boot image" +build_gold 40)
	(each (# (scene-go %0 0.4)) (get :native stage))
	(defq image (Figure "image") blocks (list)
		shape (list (list :t (path-gen-rect -14.0 -7.0 14.0 7.0 2.0 2.0 (path)))))
	;made smaller later, about its corner
	(def image :fig_x 28.0 :fig_y 100.0 :fig_px 28.0 :fig_py 100.0)
	(build-add image)
	(each! (lambda () (bind '(x y) (build-cell (!)))
			(defq block (Figure "function") wait (+ 0.4 (* 0.006 (n2f (!)))))
			(. block :set_outline shape)
			(def block :fig_fill (logior (logand (build-colour (!)) 0xffffff) 0xb0000000)
				:fig_x (random-fixed -100.0 740.0) :fig_y (random-fixed 500.0 700.0) :fig_alpha 0.0)
			(. block :add (Tween :fig_alpha 1.0 0.3 (const ease-out) :nil wait))
			(scene-to block x y 0.9 (const ease-out3) :nil wait)
			(push blocks (scene-add block image))) '() 0 +build_funcs)
	(defq ours (elem-get blocks 51))
	(def ours :fig_fill +build_cyan :fig_glow (build-glow +build_cyan) :fig_glow_size 5.0)
	(def stage :image image :blocks blocks
		:tags (list (build-tag "sys/load/init, sys/statics/statics" 34.0 96.0 +build_gold)
			(build-tag "class/list/cat" 66.0 176.0 +build_cyan)))
	(scene-say '("The boot image: the loader, the statics, the kernel and all they call,"
		"sys/load/init first, sys/statics/statics second, the rest by name") 5.5 (const build-strip)))

;;;;;;;;
; native
;;;;;;;;

(defun build-unpath (f c)
	;in the boot image the paths section is not kept: each link is made to
	;hold the way to the name in the header of the function it is to
	(each (# (. %0 :add (Tween :fig_alpha 0.0 1.0 (const ease-in) :nil 0.4)))
		(cat (get :paths_part stage) (get :path_names stage) (list (get :paths_arc stage))))
	(build-burst 520.0 134.0 +build_dim 20)
	;each link slot, 8 bytes, holds something fixed now, not a way to a path
	(each (lambda (x)
			(defq slot (Figure "slot"))
			(. slot :set_outline (list (list :t (path-gen-rect -15.0 -6.0 15.0 6.0 2.0 2.0 (path)))))
			(def slot :fig_x x :fig_y 144.0 :fig_fill +build_violet :fig_glow (build-glow +build_violet)
				:fig_glow_size 4.0 :fig_sx 0.0 :fig_sy 0.0)
			(scene-size slot 1.0 0.5 (const ease-back) :nil 1.2)
			(push (get :native stage) (build-add slot)))
		'(360.0 400.0))
	(push (get :native stage) (build-tag "the image keeps 80 of its 120 bytes" 430.0 244.0 +build_dim)
		(build-tag "links now to names in the image" 430.0 256.0 +build_violet))
	(scene-say '("In the boot image its paths section is not kept: its links are made to"
		"hold the way to the names in the headers of the image instead") 5.0 (const build-image)))

(defun build-links (f c)
	;each link to the path it names
	(defq figs (get :native stage)
		a (build-text "class/array/cat" 8 422.0 147.0 +scene_ink)
		b (build-text "class/list/ref_all" 8 502.0 147.0 +scene_ink))
	(build-type (list a b) 0.6)
	(defq ways (list (build-flow (build-arc 360.0 154.0 422.0 154.0 -26.0) +build_violet)
		(build-flow (build-arc 400.0 154.0 502.0 154.0 -40.0) +build_violet)
		(build-label "+16" 9 391.0 194.0 +build_violet 0.4) (build-label "+24" 9 451.0 208.0 +build_violet 0.6)))
	;the paths and the ways to them, what the image does not keep
	(def stage :path_names (cat (list a b) ways))
	(push figs a b)
	(each (# (push figs %0)) ways)
	(scene-say '("A link holds the way from itself to its path, 16 and 24 bytes on."
		"Nothing else in a function is moved, the rest of it is relative") 5.5 (const build-unpath)))

(defun build-arm (f c)
	;the six instructions of its code
	(defq lines (build-code +build_arm (map (lambda (_) +build_lime) +build_arm) 196.0 256.0 19.0 11))
	(build-type lines 0.35)
	(each (# (push (get :native stage) %0)) lines)
	(scene-say '("The call keeps the link register on the VP stack, x28, round a blr;"
		"the tail call is an ldr of the second link and a plain br") 6.0 (const build-links)))

(defun build-fields (f c)
	;the header says where each part starts
	(defq figs (get :native stage)
		text (build-text "ln_fnode -1  length 120  entry 40  links 64  paths 80  stack 6144" 11 28.0 228.0 +build_blue)
		name (build-text "class/list/cat" 8 113.0 147.0 +scene_ink))
	(build-type (list text name) 0.9)
	(each (lambda ((x h colour))
			(push figs (defq arc (build-flow (build-arc 65.0 112.0 x 112.0 h) colour)))
			;the one to the paths goes with them in the image
			(if (= x 420.0) (def stage :paths_arc arc)))
		'((220.0 22.0 0xff00e676) (340.0 32.0 0xffd500f9) (420.0 42.0 0xffffab00)))
	(push figs text name)
	(scene-say '("The header says where each part starts: the code at 40, the links at 64,"
		"the paths at 80. ln_fnode is -1, it is on no list yet") 5.5 (const build-arm)))

(defun build-native (f c)
	;its arm64 code, a byte to every 5 pixels across the picture
	(each (# (scene-go %0 0.5)) (get :lane_figs stage))
	(scene-go (get :vp stage) 0.5)
	(defq figs (list) ticks (Figure "longs") lines (list))
	(each (lambda ((from to name colour))
			(defq x (+ 20.0 (* 5.0 (n2f from))) w (* 5.0 (n2f (- to from))) box (Figure "part")
				wait (+ 0.4 (* 0.15 (n2f (!)))))
			(. box :set_outline (list (list :t (path-gen-rect 2.5 0.0 (- w 2.5) 36.0 3.0 3.0 (path)))))
			(def box :fig_x x :fig_y 116.0 :fig_fill (logior (logand colour 0xffffff) 0x70000000)
				:fig_fill2 (logior (logand colour 0xffffff) 0x18000000) :fig_stroke colour :fig_width 1.5
				:fig_glow (build-glow colour) :fig_glow_size 2.5 :fig_alpha 0.0 :fig_sy 0.0)
			(. box :add (Tween :fig_alpha 1.0 0.4 (const ease-out) :nil wait)
				(Tween :fig_sy 1.0 0.6 (const ease-back) :nil wait))
			(defq label (build-label name 8 (+ x (* 0.5 w)) 127.0 colour wait))
			(push figs (build-add box) label)
			(if (eql name "paths") (def stage :paths_part (list box label))))
		+build_parts)
	;a ruler under it, a tick at each long, and the byte each part starts at:
	;the parts are not of longs, only the code, links and paths start on one
	(each! (lambda () (defq x (+ 20.0 (* 40.0 (n2f (!)))))
			(push lines (list :nil (path x 156.0 x 161.0)))) '() 0 16)
	(. ticks :set_outline lines)
	(def ticks :fig_stroke 0x90ffffff :fig_width 1.0 :fig_alpha 0.0)
	(scene-show ticks 0.6 1.4)
	(push figs (build-add ticks))
	(each (lambda (at) (push figs (build-label (str at) 7 (+ 20.0 (* 5.0 (n2f at))) 170.0 +build_dim 1.4)))
		'(0 18 33 40 64 80 120))
	(def stage :native figs)
	(scene-say '("Its native code, obj/arm64/ARM64/class/list/cat, 120 bytes, a tick each long:"
		"a header, its name, its code, its links and the paths they name") 5.5 (const build-fields)))

(defun build-sizes (f c)
	;each comes out its own length
	(each (lambda (lane size)
			(defq chip (first lane))
			(. (get :label chip) :add (Tween :fig_outline (outline-centred (scene-text size 12)) 0.8
				(const ease-in-out) :nil (* 0.15 (n2f (!)))))
			(scene-pulse chip))
		(get :lanes stage) +build_sizes)
	(scene-say '("Pass after pass, till no label moves: 120 bytes for arm64, 112 for x86_64,"
		"136 for RISC-V and LoongArch, 104 for the VP64 emulator") 5.5 (const build-native)))

(defun build-translate (f c)
	;the text to the parser, and to a translator for each CPU
	(scene-title "Native" +build_lime 40)
	(each (# (scene-go %0 0.5)) (get :src stage))
	(scene-to (get :vp stage) -332.0 0.0 1.0 (const ease-in-out))
	(defq layer (get :layer stage) parser (scene-chip "vp_parse" 380.0 258.0 100 30 +build_cyan layer 13)
		lanes (list) figs (list parser (build-flow (path 290.0 258.0 330.0 258.0) +build_cyan)))
	(def parser :fig_sx 0.0 :fig_sy 0.0)
	(scene-size parser 1.0 0.6 (const ease-elastic) :nil 0.8)
	(each (lambda ((name colour))
			(defq y (+ 128.0 (* 52.0 (n2f (!))))
				chip (scene-chip name 760.0 y 150 30 colour layer 12))
			(scene-to chip 545.0 y 0.8 (const ease-back) :nil (+ 0.6 (* 0.12 (n2f (!)))))
			(push figs chip (build-flow (path 430.0 258.0 470.0 y) colour))
			(push lanes (list chip colour y)))
		+build_lanes)
	(. parser :add (Cues (Wait 1.6) (Emit 24.0 (const build-byte) 4.0)))
	(def stage :lanes lanes :lane_figs figs)
	(scene-say '("lib/trans/vp_parse reads the text into records for the translators,"
		"one VP function for each CPU: parse, passes and output, all VP") 5.5 (const build-sizes)))

;;;;
; vp
;;;;

(defun build-hash (f c)
	;the hash of the bundle, kept with the key
	(defq hash (build-text "bundle FA7CFD77BC7D0BC56FF91601DCB864AE ..." 10 34.0 270.0 +build_gold))
	(build-type (list hash) 1.0)
	(push (get :src stage) hash)
	(scene-say '("The bundle's hash is kept beside the key: each CPU's code of it is made"
		"again only when that hash, or that CPU's translator, is new") 5.0 (const build-translate)))

(defun build-entry (f c)
	;the text of the VP, as the bundle has it
	(each (# (scene-go %0 0.4)) (get :keys stage))
	(defq group (Figure "vptext"))
	(build-add group)
	(defq panel (build-panel 352 88 268 334 +build_violet group)
		head (build-text "obj/vp/class/list/class.vp" 10 364.0 106.0 +build_violet group)
		lines (build-code +build_vp +build_vp_ink 366.0 124.0 12.0 10 group))
	(build-type (list head) 0.5)
	(build-type lines 0.14 0.5)
	(def stage :vp group)
	(scene-say '("VP text, the function's entry in its file's bundle, under obj/vp/:"
		"the one form a function has before there is any CPU") 5.5 (const build-hash)))

(defun build-tail (f c)
	;a call just before a ret is a jump
	(defq code (get :code stage))
	(build-burst 70.0 204.0 +build_pink 16)
	(scene-go (elem-get code 4) 0.3)
	(. (elem-get code 2) :add (Tween :fig_outline (build-lines '("  (call-p _3)" "  (jmp-p _4)") 11 22.0) 0.8
		(const ease-in-out) :nil 0.2))
	(. (elem-get code 5) :add (Tween :fig_y 208.0 0.8 (const ease-in-out) :nil 0.4))
	(scene-say '("vpopt, the one optimiser: a call just before a ret is a jump"
		"of the same kind, and the ret goes") 4.5 (const build-entry)))

(defun build-emit (f c)
	;to VP: entry and exit move nothing here, each call goes through a link
	(defq code (get :code stage))
	(each (lambda (fig)
			(build-burst (+ (get :fig_x fig) 80.0) (- (get :fig_y fig) 5.0) +build_dim 8)
			(scene-go fig 0.4))
		(list (elem-get code 1) (elem-get code 3)))
	(. (elem-get code 2) :add (Tween :fig_outline (build-lines '("  (call-p _3)" "  (call-p _4)") 11 22.0) 0.9)
		(Tween :fig_fill +build_cyan 0.9) (Tween :fig_glow (logand (build-glow +build_cyan) 0x40ffffff) 0.9)
		(Tween :fig_y 164.0 0.8 (const ease-in-out) :nil 0.5))
	(. (elem-get code 4) :add (Tween :fig_outline (build-lines '("  (ret)") 11 22.0) 0.9)
		(Tween :fig_y 208.0 0.8 (const ease-in-out) :nil 0.5))
	(. (elem-get code 5) :add (Tween :fig_y 230.0 0.8 (const ease-in-out) :nil 0.5))
	(scene-say '("entry and exit move nothing, the args are already where they are wanted;"
		"each call is a call-p through a link, _3 and _4") 5.0 (const build-tail)))

(defun build-macro (f c)
	;the macro of the class opens out into the two calls it is
	(scene-title "VP" +build_violet 40)
	(defq code (get :code stage) line (elem-get code 2))
	(def line :fig_glow (build-glow +build_gold) :fig_glow_size 4.0)
	(. line :add (Tween :fig_outline (build-lines '("  (s-call :list :cat '(:r0 :r1) '(:r0))"
			"  (call :list :ref_all '(:r0) '(:r0))") 11 22.0) 1.0 (const ease-in-out) :nil 0.4))
	(each! (lambda (fig) (. fig :add (Tween :fig_y (+ (get :fig_y fig) 22.0) 0.8 (const ease-in-out) :nil 0.4)))
		(list code) 3 6)
	(scene-say '("(class/list/cat) is a macro of the class: a super call of :array :cat,"
		"and then a call of :list :ref_all") 4.5 (const build-emit)))

;;;;;;;;
; source
;;;;;;;;

(defun build-key (f c)
	;what the file is made of folds into its key
	(each (lambda (chip)
			(defq wait (* 0.08 (n2f (!))))
			(scene-to chip 500.0 300.0 0.7 (const ease-in3) :nil wait)
			(scene-size chip 0.1 0.7 (const ease-in3) (lambda (f c) (. f :kill)) wait))
		(get :includes stage))
	(defq key (scene-chip "key" 500.0 300.0 120 30 +build_gold (get :layer stage) 14)
		num (build-text "7934804169052796838 -7018691077404665736 88" 9 360.0 338.0 +build_gold))
	(def key :fig_sx 0.0 :fig_sy 0.0)
	(def (get :box key) :fig_glow (build-glow +build_gold) :fig_glow_size 6.0)
	(scene-size key 1.0 0.7 (const ease-elastic) :nil 0.9)
	(build-type (list num) 1.0 1.4)
	(def stage :keys (list key num))
	(scene-say '("Two numbers, the names and hashes of the 88 files folded two ways,"
		"and how many: with the same key, nothing is made") 5.0 (const build-macro)))

(defun build-includes (f c)
	;what the file is made of comes to it
	(defq chips (list))
	(each (lambda (name)
			(defq y (+ 112.0 (* 34.0 (n2f (!))))
				chip (scene-chip name 760.0 y 210 24 +build_blue (get :layer stage) 11))
			(scene-to chip 500.0 y 0.8 (const ease-back) :nil (* 0.2 (n2f (!))))
			(push chips chip))
		+build_includes)
	(def stage :includes chips)
	(scene-say '("Its key is of its own text and of all it includes and imports,"
		"88 files: its VP is made again only when one of them is") 5.0 (const build-key)))

(defun build-new ()
	(def stage :ros_by_depth :nil :paper_is +build_paper)
	;the world, what the camera shakes, in three layers: a faint ruling at
	;the back, all that comes and goes, and the labels over it all, so what
	;comes later is drawn under them and they can be read
	(defq world (Figure "world") back (Figure "rules"))
	(scene-add world)
	(. back :set_outline (cat (map! (lambda () (list :nil (path (n2f (* (!) 40)) 0.0 (n2f (* (!) 40)) 480.0))) '() 0 17)
		(map! (lambda () (list :nil (path 0.0 (n2f (* (!) 40)) 640.0 (n2f (* (!) 40))))) '() 0 13)))
	(def back :fig_stroke +build_rules :fig_width 1.0 :fig_alpha 0.0)
	(scene-show back 1.5)
	(scene-add back world)
	(def stage :world world :layer (scene-add (Figure "layer") world) :labels (scene-add (Figure "labels") world))
	(defq title (Figure "title"))
	(. title :set_outline (outline-centred (scene-text "Source" 40 +font)))
	(def title :fig_fill +build_cyan :fig_x 320.0 :fig_y 46.0 :fig_alpha 0.0
		:fig_glow (build-glow +build_cyan) :fig_glow_size 7.0)
	(. title :add (Wave :fig_ry 0.18 9.0))
	(scene-show title 0.8)
	(scene-add title)
	(def stage :title title)
	(defq panel (build-panel 20 92 310 200 +build_cyan)
		head (build-text "class/list/class.vp" 11 34.0 112.0 +build_cyan)
		code (build-code +build_source +build_source_ink 36.0 142.0 22.0 11))
	(build-type (list head) 0.6 0.4)
	(build-type code 0.45 1.0)
	(def stage :src (cat (list panel head) code) :code code)
	(scene-say '("The source, Lisp that writes VP: class/list/class.vp,"
		"and :list :cat one of the 24 functions it makes") 5.0 (const build-includes)))

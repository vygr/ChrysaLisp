;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The story, from docs/history/history.md: Games, VP, Taos, ChrysaLisp.
;
; There is no list of what happens when. A caption's end brings on the next
; thing, the last of the bars to reach the middle does, and so does the last
; tile of the picture. Each dot of mail, when it arrives, picks where it goes
; next, from the chips that are still there.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bits +story 0
	(bit bar zone))

(defq +story_paper 0xff14181c +story_ink 0xffe8ecf0 +story_gold 0xffe0a040
	+story_blue 0xff40a0e0 +story_green 0xff60c060 +story_pink 0xffe06080
	+story_violet 0xffa070e0 +story_dark 0xff283038
	+story_regular "fonts/OpenSans-Regular.ctf"
	;the shape of a chrysalis, a body, and a wing, of a butterfly of no kind
	;in particular
	+story_chrysalis "M0 -46 C16 -44 22 -22 18 0 C15 22 8 40 0 48 C-8 40 -15 22 -18 0 C-22 -22 -16 -44 0 -46 Z"
	+story_body "M0 -30 C4 -28 5 -14 4 0 C3 14 2 26 0 30 C-2 26 -3 14 -4 0 C-5 -14 -4 -28 0 -30 Z"
	+story_wing "M2 -8 C24 -62 84 -76 98 -40 C106 -16 72 2 40 6 C72 12 86 40 66 60 C46 76 14 42 2 8 Z"
	+story_wing_mark "M34 -24 C44 -38 66 -44 76 -32 C82 -22 62 -10 44 -8 C38 -8 30 -16 34 -24 Z")

;;;;;;;;;;;;;;;;
; things to make
;;;;;;;;;;;;;;;;

(defun story-text (text size &optional font)
	; (story-text text size [font]) -> outline
	;the letters made big and brought down, so that they stay smooth drawn
	;at more than the picture's size
	(map (lambda ((closed p))
			(list closed (path-transform (const (fixeds 0.5 0.0 0.0 0.0 0.5 0.0)) p p)))
		(outline-text text (ifn font +story_regular) (* size 2))))

(defun story-lines (lines size)
	; (story-lines lines size) -> outline
	;lines of words, each in the middle, one under another, about 0 0
	(defq out (list) step (n2f (+ size (/ size 4))) y (* step (* -0.5 (n2f (dec (length lines))))))
	(each (lambda (line)
			(defq m (fixeds 1.0 0.0 0.0 0.0 1.0 y))
			(each (lambda ((closed p)) (push out (list closed (path-transform m p p))))
				(outline-centred (story-text line size)))
			(setq y (+ y step)))
		lines)
	out)

(defun story-add (fig &optional parent)
	;a figure on the stage, or in a group
	(. (ifn parent stage) :add_node fig)
	fig)

(defun story-word (text size x y colour &optional parent font)
	;words in the middle of a place, seen through to start with
	(defq fig (Figure "word"))
	(. fig :set_outline (outline-centred (story-text text size font)))
	(def fig :fill colour :x x :y y :alpha 0.0)
	(story-add fig parent))

(defun story-show (fig &optional secs wait)
	;it comes into view
	(. fig :add (Tween :alpha 1.0 (ifn secs 0.5) (const ease-out) :nil (ifn wait 0.0))))

(defun story-go (fig &optional secs)
	;it goes out of view, and goes
	(. fig :add (Tween :alpha 0.0 (ifn secs 0.5) (const ease-in) (lambda (f c) (. f :kill)))))

(defun story-to (fig x y secs &optional fnc then wait)
	;it goes to a place, by two tweens, what is then fired comes of the one
	(. fig :add (Tween :x x secs (ifn fnc (const ease-in-out)) :nil (ifn wait 0.0))
		(Tween :y y secs (ifn fnc (const ease-in-out)) then (ifn wait 0.0))))

(defun story-size (fig to secs &optional fnc then wait)
	;it goes to a size, both ways
	(. fig :add (Tween :sx to secs (ifn fnc (const ease-in-out)) :nil (ifn wait 0.0))
		(Tween :sy to secs (ifn fnc (const ease-in-out)) then (ifn wait 0.0))))

(defun story-net (name)
	;a group on the stage with a layer in it for each sort of thing, the later
	;ones drawn over the earlier. What arrives late goes in with its own sort,
	;the mail stays on top of it whenever it came
	(defq net (Figure name))
	(each (lambda (layer) (def net layer (story-add (Figure "layer") net)))
		'(:lines :picture :chips :mail))
	(story-add net))

(defun story-chip (name x y w h colour &optional parent size)
	;a box with a name in it, a group of the two. Its line draws round it
	;and the rest comes up
	(defq fig (Figure "chip") box (Figure "box") label (Figure "label")
		hw (* 0.5 (n2f w)) hh (* 0.5 (n2f h)))
	(. box :set_outline (list (list :t (path-gen-rect (neg hw) (neg hh) hw hh 5.0 5.0 (path)))))
	(def box :fill +story_dark :stroke colour :width 2.0 :trim 0.0)
	(. box :add (Tween :trim 1.0 0.7 (const ease-in-out)))
	(. label :set_outline (outline-centred (story-text name (ifn size 13))))
	(def label :fill +story_ink :alpha 0.0)
	(story-show label 0.4 0.3)
	(. fig :add_node box)
	(. fig :add_node label)
	(def fig :x x :y y :box box :label label :name_is name)
	(story-add fig parent))

(defun story-pulse (fig)
	;a chip that has been given something swells a little
	(unless (get :pulsing fig)
		(def fig :pulsing :t)
		(. fig :add (Cues (Tween :sx 1.25 0.08 (const ease-out)) (Tween :sx 1.0 0.25 (const ease-in-out)))
			(Cues (Tween :sy 1.25 0.08 (const ease-out))
				(Tween :sy 1.0 0.25 (const ease-in-out) (lambda (f c) (def f :pulsing :nil)))))))

(defun story-arrow (x y x1 y1 colour secs &optional parent)
	;a line that draws itself, with a head on it
	(defq fig (Figure "arrow"))
	(. fig :set_outline (list (list :nil (path x y x1 y1))))
	(def fig :stroke colour :width 2.5 :trim 0.0 :cap2 +cap_arrow)
	(. fig :add (Tween :trim 1.0 secs (const ease-in-out)))
	(story-add fig parent))

(defun story-link (ax ay bx by parent)
	;a line that draws itself between two chips, behind them
	(defq fig (Figure "link"))
	(. fig :set_outline (list (list :nil (path ax ay bx by))))
	(def fig :stroke 0xff485058 :width 1.5 :trim 0.0)
	(. fig :add (Tween :trim 1.0 0.8 (const ease-in-out)))
	(story-add fig parent))

(defun story-say (lines &optional secs then)
	;what is said at the foot of the picture, what was said goes. So long,
	;then what follows is fired. With no secs it stays till the next is said
	(if (defq old (get :caption stage)) (story-go old 0.3))
	(defq fig (Figure "caption"))
	(. fig :set_outline (story-lines lines 17))
	(def fig :fill +story_ink :x 320.0 :y 448.0 :alpha 0.0)
	(. fig :add (if secs
		(Cues (Tween :alpha 1.0 0.4 (const ease-out) :nil 0.3) (Wait secs then))
		(Tween :alpha 1.0 0.4 (const ease-out) :nil 0.3)))
	(def stage :caption fig)
	(story-add fig))

(defun story-title (text colour)
	;the big word morphs on to the next, and its colour
	(defq fig (get :title stage))
	(. fig :add (Tween :outline (outline-centred (story-text text 54 +font)) 1.4 (const ease-in-out))
		(Tween :fill colour 1.4)))

;;;;;;;
; games
;;;;;;;

(defun story-new ()
	(def stage :by_depth :nil :paper_is +story_paper)
	(defq title (Figure "title"))
	(. title :set_outline (outline-centred (story-text "Games" 54 +font)))
	(def title :fill +story_gold :x 320.0 :y 66.0 :alpha 0.0)
	(. title :add (Wave :ry 0.22 8.0) (Wave :rx 0.1 11.0))
	(story-show title 0.8)
	(story-add title)
	(def stage :title title)
	(def stage :machine (story-chip "ZX81" 320.0 240.0 120 64 +story_gold :nil 22))
	(story-say '("A ZX81, 1K of memory," "and machine code typed in by hand") 3.5 story-games))

(defun story-games (f c)
	(defq names '("Automania" "Pyjamarama" "Everyone's a Wally" "Frost Byte" "Battle of the Planets")
		machine (get :machine stage) words (list))
	(. (get :label machine) :add (Tween :outline (outline-centred (story-text "ZX Spectrum" 15)) 0.8))
	;each comes in fast and settles slowly, one after another
	(each (lambda (name)
			(defq at (!) word (story-word name 19 (+ 760.0 (* 40.0 (n2f at)))
				(+ 140.0 (* 42.0 (n2f at)))
				(elem-get (list +story_blue +story_green +story_pink +story_gold +story_violet) at)))
			(story-show word 0.4 (* 0.5 (n2f at)))
			(. word :add (Tween :x (if (odd? at) 500.0 140.0) 1.0 (const ease-out3) :nil (* 0.5 (n2f at)))
				(Wave :y 3.0 (+ 2.0 (* 0.4 (n2f at)))))
			(push words word))
		names)
	(def stage :words words)
	(story-say '("Mikro-Gen, 1983: games for the ZX Spectrum") 4.5 story-twice))

;;;;;;;;;;;;;;
; written twice
;;;;;;;;;;;;;;

(defun story-twice (f c)
	(each (lambda (word) (. word :add (Tween :y -40.0 0.6 (const ease-in))) (story-go word 0.6)) (get :words stage))
	(story-go (get :machine stage) 0.5)
	(defq st (story-chip "Atari ST" -100.0 150.0 170 40 +story_blue :nil 17)
		amiga (story-chip "Amiga" 740.0 150.0 170 40 +story_pink :nil 17) bars (list))
	(story-to st 150.0 150.0 0.9 (const ease-back))
	(story-to amiga 490.0 150.0 0.9 (const ease-back))
	(def stage :st st :amiga amiga)
	;the same code on each side, a bar at a time
	(each (lambda (x colour)
			(times 7 (defq i (dec (length bars)) row (% (length bars) 7) bar (Figure "bar")
					w (+ 60.0 (* 12.0 (n2f (% (* (inc row) 5) 7)))))
				(. bar :set_outline (list (list :t (path-gen-rect (neg w) -5.0 w 5.0 3.0 3.0 (path)))))
				(def bar :fill colour :x x :y (+ 196.0 (* 24.0 (n2f row))) :sx 0.0 :meets +story_zone
					:on_enter (lambda (this other) (story-bar-in this)))
				(. bar :add (Tween :sx 1.0 0.5 (const ease-out3) :nil (+ 0.8 (* 0.3 (n2f row)))))
				(push bars (story-add bar))))
		'(150.0 490.0) (list +story_blue +story_pink))
	(def stage :bars bars :bars_left (length bars))
	(story-say '("Then the Atari ST and the Amiga:" "Custodian, Verminator, and everything written twice")
		6.0 story-gather))

(defun story-gather (f c)
	;the zone they are making for is only there to be met
	(defq zone (Figure "zone"))
	(. zone :set_outline (list (list :t (path -24.0 -24.0 24.0 -24.0 24.0 24.0 -24.0 24.0))))
	(def zone :fill 0x00ffffff :x 320.0 :y 270.0 :kinds +story_zone)
	(story-add zone)
	(def stage :zone zone)
	(each (lambda (bar)
			(defq wait (* 0.18 (n2f (!))))
			(story-to bar 320.0 270.0 1.1 (const ease-in3) :nil wait))
		(get :bars stage))
	(story-say '("So he stopped writing for either: a machine of his own," "registers r0 upward, and instructions like copy")))

(defun story-bar-in (this)
	;a bar has reached the middle, it goes in a burst. The last brings on VP
	(defq x (get :x this) y (get :y this))
	(def this :meets 0)
	(times 3 (spark-at x y (get :fill this)))
	(. this :kill)
	(def stage :bars_left (dec (get :bars_left stage)))
	(if (= 0 (get :bars_left stage)) (story-vp)))

;;;;
; vp
;;;;

(defun story-vp ()
	(. (get :zone stage) :kill)
	(story-title "VP" +story_green)
	(defq vp (story-chip "VP" 320.0 270.0 110 60 +story_green :nil 26))
	(def vp :sx 0.0 :sy 0.0)
	(story-size vp 1.0 0.7 (const ease-elastic))
	(times 30 (spark-at 320.0 270.0 +story_green))
	(def stage :vp vp)
	(story-say '("A way to write one game for two computers," "before it was anything else: the Virtual Processor") 4.5 story-arrows))

(defun story-arrows (f c)
	(def stage :arrows (list (story-arrow 262.0 250.0 180.0 176.0 +story_green 0.8)
		(story-arrow 378.0 250.0 460.0 176.0 +story_green 0.8)))
	;what came of it, the first thing written that way, for both machines
	(defq word (story-word "Onslaught" 30 320.0 170.0 +story_gold :nil +font))
	(def word :sx 0.0 :sy 0.0)
	(story-show word 0.2)
	(story-size word 1.0 0.7 (const ease-back))
	(times 16 (spark-at 320.0 170.0 +story_gold))
	(def stage :onslaught word)
	(story-say '("Onslaught, the first thing written in VP, assembled for"
		"the ST and the Amiga at the press of a button") 5.5 story-transputers))

(defun story-transputers (f c)
	(story-go (get :onslaught stage) 0.5)
	;the first of the things that are not an ST or an Amiga, a row of them
	(defq extras (list) net (story-net "row"))
	(def stage :net net)
	(each (lambda (x)
			(defq chip (story-chip "T800" x 560.0 62 30 +story_violet (get :chips net)))
			(story-to chip x 372.0 0.8 (const ease-back) :nil (* 0.15 (n2f (!))))
			(push extras chip))
		'(60.0 130.0 200.0 270.0))
	(def stage :extras extras :transputers (cat extras))
	(story-say '("Tim Moore's ray tracer, and the wish to speed it up:"
		"plug in more processors, Inmos transputers") 5.0 story-kernel))

(defun story-kernel-mark (chip wait x)
	;the kernel coming up on a chip
	(defq mark (Figure "kernel"))
	(. mark :set_outline (list (list :t (path-gen-ellipse x -9.0 3.5 3.5 (path)))))
	(def mark :fill +story_gold :alpha 0.0)
	(story-show mark 0.3 wait)
	(. chip :add_node mark))

(defun story-kernel (f c)
	;a kernel comes up on each, they are joined, and work goes between them.
	;It is the mail of the network to come, with this row for its chips, in
	;a group of its own that has not moved
	(defq row (get :transputers stage) lines (get :lines (get :net stage)))
	(def stage :chips row :mailing :t)
	(each! (lambda (chip)
			(defq next (elem-get row (inc (!))))
			(story-link (get :x chip) 372.0 (get :x next) 372.0 lines))
		(list row) 0 -2)
	(each (lambda (chip) (story-kernel-mark chip (* 0.2 (n2f (!))) 22.0)) row)
	(times 4 (story-dot))
	(story-say '("A small kernel on each, and work and mail between them:"
		"what the research institutes had been trying to do") 6.0 story-translate))

(defun story-translate (f c)
	;a letter of the code goes to them, and a line stays
	(push (get :extras stage) (story-arrow 285.0 302.0 190.0 350.0 +story_violet 0.8))
	(story-say '("Then the VP code itself went to disc, translated as it loads:"
		"Andy Henson's translator for the T800") 5.0 story-pc))

(defun story-pc-joins (pc from_x)
	;it has come, it has a kernel, it is joined to the last, and it is one of
	;the chips now. The letters will find it for themselves
	(story-kernel-mark pc 0.0 36.0)
	(story-link from_x 372.0 (get :x pc) 372.0 (get :lines (get :net stage)))
	(push (get :chips stage) pc)
	(story-dot))

(defun story-pc (f c)
	;the PCs come in beside the transputers and start taking the work
	(defq chips (get :chips (get :net stage))
		one (story-chip "PC 386" 760.0 372.0 92 30 +story_gold chips 14)
		two (story-chip "PC 386" 900.0 372.0 92 30 +story_gold chips 14))
	(story-to one 400.0 372.0 0.9 (const ease-back) (lambda (f c) (story-pc-joins f 270.0)))
	(story-to two 530.0 372.0 0.9 (const ease-back) (lambda (f c) (story-pc-joins f 400.0)) 0.7)
	(push (get :extras stage) one two (story-arrow 355.0 302.0 430.0 350.0 +story_gold 0.8))
	(story-say '("Andy Thomason's for the 386, and the same VP code"
		"that ran on the transputers ran on the PC") 6.5 story-taos))

;;;;;;
; taos
;;;;;;

(defq +story_chips ''("T800" "386" "68000" "ARM" "PowerPC" "R3000" "Alpha" "PgC7600"))

(defun story-taos (f c)
	(each (lambda (arrow) (. arrow :kill)) (get :arrows stage))
	(each (lambda (extra) (story-go extra 0.6)) (get :extras stage))
	;the letters of the row stop going on, and what is left of it goes
	(def stage :mailing :nil :chips (list))
	(story-go (get :net stage) 0.6)
	(story-go (get :st stage) 0.6)
	(story-go (get :amiga stage) 0.6)
	(story-go (get :vp stage) 0.8)
	(story-title "Taos" +story_blue)
	;the network, a group, it floats in space, and all of it with it
	(defq net (story-net "net") n (length +story_chips) chips (list))
	(def net :x 320.0 :y 262.0)
	(. net :add (Wave :ry 0.34 9.0) (Wave :rx 0.2 13.0))
	(def stage :net net)
	(each (lambda (name)
			(defq a (* +fp_2pi (/ (n2f (!)) (n2f n))) b (* +fp_2pi (/ (n2f (inc (!))) (n2f n))))
			(story-link (* 215.0 (cos a)) (* 128.0 (sin a)) (* 215.0 (cos b)) (* 128.0 (sin b)) (get :lines net)))
		+story_chips)
	(each (lambda (name)
			(defq a (* +fp_2pi (/ (n2f (!)) (n2f n)))
				chip (story-chip name 0.0 0.0 78 26 +story_blue (get :chips net)))
			(story-to chip (* 215.0 (cos a)) (* 128.0 (sin a)) 0.9 (const ease-back) :nil (* 0.1 (n2f (!))))
			(push chips chip))
		+story_chips)
	(def stage :chips chips)
	(story-say '("Taos: one file on disc, on any processor," "any mix of them in the one network") 5.0 story-mail))

(defun story-trace (cx cy)
	;a tile of the picture, by the way a ray tracer would have it: a ball, a
	;checked floor and a sky. cx and cy are from the middle, in tiles
	(defq nx (/ (+ (n2f cx) 0.5) 3.4) ny (/ (+ (n2f cy) 0.5) 3.4) d (+ (* nx nx) (* ny ny)))
	(cond
		((< d 1.0)
			(defq nz (sqrt (- 1.0 d))
				lit (max 0.12 (+ (* nx -0.5) (* ny -0.6) (* nz 0.62)))
				spec (max 0.0 (- lit 0.86)))
			(+ 0xff000000 (<< (min 255 (n2i (+ (* 230.0 lit) (* 900.0 spec)))) 16)
				(<< (min 255 (n2i (+ (* 60.0 lit) (* 900.0 spec)))) 8)
				(min 255 (n2i (+ (* 50.0 lit) (* 900.0 spec))))))
		((> cy 1)
			(if (odd? (+ cx cy 100)) 0xfff0f0f0 0xff303848))
		(:t (+ 0xff000000 (<< (+ 30 (* 8 (+ cy 5))) 16) (<< (+ 70 (* 10 (+ cy 5))) 8) 200))))

(defun story-mail (f c)
	;the picture is worked out a tile at a time, in any order, a tile for
	;each letter that arrives. Eight letters go round
	(defq cells (list))
	(each (lambda (cy) (each (lambda (cx) (push cells (list cx cy))) (range -7 7))) (range -5 5))
	(each (lambda (i)
			(defq j (random (length cells)) t0 (elem-get cells i))
			(elem-set cells i (elem-get cells j))
			(elem-set cells j t0))
		(range 0 (length cells)))
	(def stage :cells cells :mailing :t :tiles (list))
	(times 8 (story-dot))
	(story-say '("Mail, never shared memory: work spreads over the processors" "as a liquid spreads out over a surface")))

(defun story-dot ()
	;a letter, at a chip that is there
	(when (and (get :mailing stage) (defq chip (story-any-chip :nil)))
		(defq dot (Figure "letter"))
		(. dot :set_outline (list (list :t (path-gen-ellipse 0.0 0.0 5.0 5.0 (path)))))
		(def dot :fill +story_gold :x (get :x chip) :y (get :y chip))
		(story-add dot (get :mail (get :net stage)))
		(story-hop dot chip)))

(defun story-any-chip (not_this)
	;any of the chips that are there, and not that one
	(defq live (filter (lambda (chip) (not (or (get :dead chip) (eql chip not_this))))
		(ifn (get :chips stage) (list))))
	(if (nempty? live) (elem-get live (random (length live)))))

(defun story-hop (dot from)
	;on to another chip. When it arrives it is that chip's to send on
	(when (defq to (story-any-chip from))
		(def dot :to to)
		(. dot :add (Follow (path (get :x dot) (get :y dot) (get :x to) (get :y to))
			(+ 0.4 (/ (n2f (random 400)) 1000.0)) (const ease-in-out)
			(lambda (dot c) (story-arrive dot))))))

(defun story-arrive (dot)
	;it has come to a chip, which swells, a tile of the picture is done if
	;there is one to do, and it goes on. If there are no more letters to go
	;it goes. The chip it was going to may have gone: it finds another
	(defq to (get :to dot))
	(cond
		((not (get :mailing stage)) (. dot :kill))
		(:t (unless (get :dead to) (story-pulse to))
			(story-tile)
			(story-hop dot to))))

(defun story-tile ()
	(when (nempty? (defq cells (ifn (get :cells stage) (list))))
		(bind '(cx cy) (pop cells))
		(defq tile (Figure "tile") s 7.0)
		(. tile :set_outline (list (list :t (path 0.0 0.0 s 0.0 s s 0.0 s))))
		(def tile :fill (story-trace cx cy) :x (* s (n2f cx)) :y (* s (n2f cy)) :alpha 0.0)
		(story-show tile 0.25)
		(story-add tile (get :picture (get :net stage)))
		(push (get :tiles stage) tile)
		;the last tile of the picture is what brings on the next
		(if (empty? cells)
			(story-say '("A ray tracer, in parallel on a 486," "four transputers and four R3000s at once") 4.5 story-tao))))

(defun story-tao (f c)
	(story-say '("Tao Group, 1991 to 2007: Elate, then intent,"
			"in tens of millions of devices") 4.0 story-idea))

;;;;;;;;;;;;
; chrysalisp
;;;;;;;;;;;;

(defq +story_now ''("x86-64" "ARM64" "RISC-V 64" "LoongArch 64" "VP64"))

(defun story-idea (f c)
	;the picture fades, the title morphs, and the chips are those of today.
	;There are fewer, the rest go, and the letters find their own way round them
	(each (lambda (tile) (story-go tile (+ 0.2 (/ (n2f (random 800)) 1000.0)))) (get :tiles stage))
	(def stage :tiles (list))
	(story-title "ChrysaLisp" +story_gold)
	(defq chips (get :chips stage) n (length +story_now) keep (slice chips 0 n))
	(each! (lambda (chip)
			(times 4 (spark-at 320.0 262.0 +story_blue))
			(. chip :kill))
		(list chips) n)
	(def stage :chips keep)
	;the lines between the old ones go
	(story-go (get :lines (get :net stage)) 0.5)
	(each (lambda (chip name)
			(defq a (* +fp_2pi (/ (n2f (!)) (n2f n))))
			(. (get :label chip) :add (Tween :outline (outline-centred (story-text name 12)) 1.0))
			(. (get :box chip) :add (Tween :stroke +story_gold 1.0))
			(story-to chip (* 215.0 (cos a)) (* 128.0 (sin a)) 1.2 (const ease-in-out)))
		keep +story_now)
	(story-say '("9 February 2015: the first commit of ChrysaLisp," "a Virtual Processor, and a translator for each") 5.0 story-lisp))

(defun story-lisp (f c)
	(story-say '("A Lisp on the VP, Darren Pegg's doing," "and a system that assembles itself") 4.5 story-vanish))

(defun story-vanish (f c)
	;any of them may go. The letters are not told, they find another
	(defq chip (elem-get (get :chips stage) 3))
	(story-size chip 1.8 0.3 (const ease-out))
	(. chip :add (Tween :alpha 0.0 0.3 (const ease-in) (lambda (f c)
		(defq x (+ 320.0 (get :x f)) y (+ 262.0 (get :y f)))
		(times 18 (spark-at x y +story_pink))
		;a while on, another of the same is started in its place
		(defq timer (Figure "timer"))
		(def timer :back (list (get :x f) (get :y f)))
		(. timer :add (Wait 2.5 (lambda (t c) (. t :kill) (story-return (get :back t)))))
		(story-add timer)
		(. f :kill))))
	(def stage :chips (filter (lambda (c) (nql c chip)) (get :chips stage)))
	(story-say '("And what Taos could not do: any node may vanish," "and the work finds its way round") 5.5 story-gather2))

(defun story-return (at)
	;it comes up where the other was, and the letters find it for themselves
	(bind '(x y) at)
	(defq chip (story-chip "LoongArch 64" x y 78 26 +story_gold (get :chips (get :net stage)) 12))
	(def chip :sx 0.0 :sy 0.0)
	(story-size chip 1.0 0.6 (const ease-back))
	(push (get :chips stage) chip))

(defun story-gather2 (f c)
	;the letters stop going on, the chips come together
	(def stage :mailing :nil)
	(defq net (get :net stage))
	(each (lambda (chip)
			(story-to chip 0.0 0.0 1.3 (const ease-anticipate) (lambda (f c) (. f :kill)) (* 0.1 (n2f (!))))
			(story-size chip 0.2 1.3 (const ease-in)))
		(get :chips stage))
	(defq shell (Figure "chrysalis"))
	(. shell :set_outline (outline-svg +story_chrysalis))
	(def shell :fill 0xff9a7030 :stroke 0xffe0c080 :width 1.5 :x 320.0 :y 250.0 :sx 0.0 :sy 0.0)
	(story-size shell 1.0 0.9 (const ease-back)
		(lambda (f c) (story-hatch)) 1.0)
	(story-add shell)
	(def stage :shell shell)
	(story-say '("The same idea, again and again, since the early 1980s") 6.0 story-fly))

(defun story-wing (fly side colour)
	;a wing and its mark, a group that flaps on its y axis about the body
	(defq wing (Figure "wing") mark (Figure "mark"))
	(. wing :set_outline (outline-svg +story_wing))
	(def wing :fill colour :stroke 0xff201810 :width 2.0)
	(. mark :set_outline (outline-svg +story_wing_mark))
	(def mark :fill 0xfff0e0a0)
	(defq group (Figure "side"))
	(. group :add_node wing)
	(. group :add_node mark)
	(def group :sx 0.0 :sy 0.0 :scale_to side)
	(. group :add (Tween :sx side 0.9 (const ease-back)) (Tween :sy 1.0 0.9 (const ease-back))
		(Wave :ry (* side 0.85) 0.55))
	(story-add group fly))

(defun story-hatch ()
	;the chrysalis morphs to a body, and the wings open from it
	(defq shell (get :shell stage))
	(. shell :add (Tween :outline (outline-svg +story_body) 0.9 (const ease-in-out))
		(Tween :fill 0xff302418 0.9))
	(def shell :x 0.0 :y 0.0)
	(defq fly (Figure "butterfly"))
	(def fly :x 320.0 :y 250.0)
	(story-add fly)
	(def stage :fly fly)
	(story-wing fly 1.0 +story_gold)
	(story-wing fly -1.0 +story_gold)
	;the body goes on top of them
	(. fly :add_node shell)
	(. fly :add (Wave :rz 0.08 2.3)))

(defun story-fly (f c)
	;off it goes, in a long curve, and when it has gone it starts again
	(defq fly (get :fly stage) points (path))
	(each (lambda (i)
			(defq u (/ (n2f i) 40.0))
			(push points (+ 320.0 (* 420.0 u u) (* 60.0 (sin (* u 9.0))))
				(- 250.0 (* 380.0 u) (* 60.0 (sin (* u 5.0))))))
		(range 0 41))
	(. fly :add (Follow points 5.0 (const ease-in) (lambda (f c) (story-end))))
	(story-size fly 0.5 5.0 (const ease-in))
	(story-say '("The whole story is in docs/history/history.md")))

(defun story-end ()
	;all goes, and a moment later it all begins again
	(story-go (get :title stage) 1.0)
	(story-go (get :caption stage) 1.0)
	(defq clock (Figure "clock"))
	(. clock :add (Wait 2.0 (lambda (f c) (setq restart :t))))
	(story-add clock))

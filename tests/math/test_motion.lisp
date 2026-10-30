(report-header "Motion: figures, what changes them, what they meet")

(import "lib/motion/rostrum.inc")

(defun mo-near (a b &optional tol)
	(<= (abs (- a b)) (ifn tol 0.01)))

;the eases, each from 0 to 1
(each (lambda (e)
		(assert-true "an ease starts at 0" (mo-near (e 0.0) 0.0))
		(assert-true "and ends at 1" (mo-near (e 1.0) 1.0)))
	(list ease-linear ease-in ease-out ease-in-out ease-in3 ease-out3 ease-back
		ease-anticipate ease-bounce ease-elastic))
(assert-true "slow in is behind half way" (< (ease-in 0.5) 0.5))
(assert-true "slow out is ahead" (> (ease-out 0.5) 0.5))
(assert-true "back goes past the end" (> (ease-back 0.8) 1.0))
(assert-true "anticipate goes back first" (< (ease-anticipate 0.2) 0.0))

;a line evenly along its length
(defq mo_square (path 0.0 0.0 100.0 0.0 100.0 100.0 0.0 100.0))
(assert-true "a closed square is 400 round" (mo-near (morph-length mo_square :t) 400.0))
(assert-true "open it is 300" (mo-near (morph-length mo_square :nil) 300.0))
(defq mo_r (morph-resample mo_square :t 8))
(assert-eq "8 points" 16 (length mo_r))
(assert-true "the second half way along a side" (mo-near (elem-get mo_r 2) 50.0))
(assert-true "the third at a corner" (and (mo-near (elem-get mo_r 4) 100.0) (mo-near (elem-get mo_r 5) 0.0)))
(assert-true "the last half way up the last side" (and (mo-near (elem-get mo_r 14) 0.0) (mo-near (elem-get mo_r 15) 50.0)))
(defq mo_t (morph-trim mo_square :t 0.25))
(assert-true "a quarter of the way round is the first corner"
	(and (mo-near (elem-get mo_t (- (length mo_t) 2)) 100.0) (mo-near (last mo_t) 0.0)))

;a morph, at each end the shape it should be
(defq mo_a (list (list :t mo_square))
	mo_b (list (list :t (path 200.0 200.0 300.0 200.0 300.0 300.0 200.0 300.0)))
	mo_m (Morph mo_a mo_b) mo_0 (morph-at mo_m 0.0) mo_1 (morph-at mo_m 1.0) mo_h (morph-at mo_m 0.5))
(assert-eq "one line for one line" 1 (length mo_m))
(bind '(mx my mx1 my1) (outline-box mo_0))
(assert-true "at 0 it is the first" (and (mo-near mx 0.0 1.0) (mo-near mx1 100.0 1.0)))
(bind '(mx my mx1 my1) (outline-box mo_1))
(assert-true "at 1 it is the second" (and (mo-near mx 200.0 1.0) (mo-near my1 300.0 1.0)))
(bind '(mx my mx1 my1) (outline-box mo_h))
(assert-true "half way it is half way" (and (mo-near mx 100.0 1.0) (mo-near mx1 200.0 1.0)))

;more lines one side than the other: what has no partner grows from a point
(defq mo_two (list (list :t mo_square) (list :t (path 400.0 0.0 500.0 0.0 500.0 100.0 400.0 100.0)))
	mo_g (Morph mo_a mo_two) mo_g0 (morph-at mo_g 0.0))
(assert-eq "two lines" 2 (length mo_g))
;one line is the square as it was, the other all at the one point
(defq mo_sizes (map (lambda (line)
		(bind '(x y x1 y1) (outline-box (list line)))
		(n2i (- x1 x)))
	mo_g0))
(assert-true "the one with no partner starts as a point" (= (reduce (const min) mo_sizes 1000) 0))
(assert-true "the one with a partner is as it was" (= (reduce (const max) mo_sizes 0) 100))

;letters, words of differing lines
(defq mo_vp (outline-text "VP" "fonts/OpenSans-Regular.ctf" 48) mo_taos (outline-text "Taos" "fonts/OpenSans-Regular.ctf" 48))
(assert-true "letters are lines" (> (length mo_vp) 1))
(assert-eq "VP to Taos has as many lines as the larger" (max (length mo_vp) (length mo_taos))
	(length (Morph mo_vp mo_taos)))
(bind '(mx my mx1 my1) (outline-box (outline-centred mo_vp)))
(assert-true "centred is about 0" (mo-near (+ mx mx1) 0.0 1.0))

;a tween, of a number, by the time given and not the ticks
(defq mo_stage (Rostrum 640 480) mo_f (Figure "f") mo_over 0)
(. mo_stage :add_node mo_f)
(. mo_f :add (Tween :x 100.0 1.0 (const ease-linear) (lambda (fig comp) (setq mo_over (inc mo_over)))))
(. mo_stage :tick 0.5)
(assert-true "half the time is half way" (mo-near (get :x mo_f) 50.0))
(assert-eq "and it is not over" 0 mo_over)
(times 6 (. mo_stage :tick 0.1))
(assert-true "at the end it is there" (mo-near (get :x mo_f) 100.0))
(assert-eq "and what it fires is fired once" 1 mo_over)
(assert-eq "and it is gone" 0 (length (get :comps mo_f)))

;a colour by its channels
(assert-eq "half way between two colours" 0xff808080 (motion-mix 0xff000000 0xffffffff 0.5))
(assert-eq "none to red is red seen through" 0x80ff0000 (motion-mix 0 0xffff0000 0.5))

;rate and wave
(defq mo_f (Figure))
(. mo_stage :add_node mo_f)
(. mo_f :add (Rate :rz 2.0) (Wave :y 10.0 4.0))
(. mo_stage :tick 1.0)
(assert-true "a rate goes by so much a second" (mo-near (get :rz mo_f) 2.0))
(assert-true "a wave is at its top a quarter of the way" (mo-near (get :y mo_f) 10.0))

;cues, each in turn. What is over can add what is next
(defq mo_f (Figure) mo_log (list))
(. mo_stage :add_node mo_f)
(. mo_f :add (Cues (Tween :x 10.0 1.0 (const ease-linear) (lambda (f c) (push mo_log :a)))
	(Wait 1.0 (lambda (f c) (push mo_log :b)))
	(Tween :x 20.0 1.0 (const ease-linear) (lambda (f c) (push mo_log :c)))))
(. mo_stage :tick 1.0)
(assert-eq "the first" '(:a) mo_log)
(. mo_stage :tick 1.0)
(assert-eq "the second" '(:a :b) mo_log)
(. mo_stage :tick 0.5)
(assert-true "and the third is half way, from where it had got" (mo-near (get :x mo_f) 15.0))

;a tie
(defq mo_l (Figure))
(. mo_stage :add_node mo_l)
(. mo_l :add (Tie mo_f '(:x) 5.0))
(. mo_stage :tick 0.0)
(assert-true "a tie has what the other has" (mo-near (get :x mo_l) (+ (get :x mo_f) 5.0)))

;follow a line
(defq mo_f (Figure))
(. mo_stage :add_node mo_f)
(. mo_f :add (Follow (path 0.0 0.0 100.0 0.0 100.0 100.0) 2.0))
(. mo_stage :tick 1.0)
(assert-true "half the time is the corner" (and (mo-near (get :x mo_f) 100.0) (mo-near (get :y mo_f) 0.0)))

;the tree, a child goes with its parent
(defq mo_stage (Rostrum 640 480) mo_p (Figure "parent") mo_c (Figure "child"))
(. mo_p :add_node mo_c)
(. mo_stage :add_node mo_p)
(def mo_p :x 100.0 :rz (* +fp_2pi 0.25))
(def mo_c :x 10.0)
(. mo_stage :tick 0.0)
(defq mo_w (get :world mo_c))
(assert-true "a child is placed by its parent, turned" (and (mo-near (elem-get mo_w 3) 100.0 0.1) (mo-near (elem-get mo_w 7) 10.0 0.1)))
(assert-eq "it finds its root" mo_stage (. mo_c :root))
(assert-eq "nodes by name" 1 (length (. mo_stage :find_nodes "child")))

;a card tilted, with no focus it is only narrower, with one it tapers
(defq mo_q (path -100.0 -100.0 100.0 -100.0 100.0 100.0 -100.0 100.0) mo_stage (Rostrum 640 480) mo_f (Figure))
(. mo_f :set_outline (list (list :t mo_q)))
(def mo_f :fill 0xffffffff :x 320.0 :y 240.0 :ry (* +fp_2pi 0.125))
(. mo_stage :add_node mo_f)
(. mo_stage :tick 0.0)
(bind '(bx by bx1 by1) (. mo_stage :box mo_f))
(assert-true "tilted with no focus its height stays" (mo-near (- by1 by) 200.0 0.5))
(assert-true "and it is narrower" (mo-near (- bx1 bx) (* 200.0 (cos (* +fp_2pi 0.125))) 0.5))
(def mo_stage :focus 500.0)
(bind '(bx by bx1 by1) (. mo_stage :box mo_f))
(assert-true "with a focus the near side is taller" (> (- by1 by) 200.0))
(def mo_f :ry 0.0 :z 500.0)
(. mo_stage :tick 0.0)
(bind '(bx by bx1 by1) (. mo_stage :box mo_f))
(assert-true "facing it, twice the focus away, it is half the size" (mo-near (- bx1 bx) 100.0 0.5))

;what they meet: enter once, stay, leave once
(defq +mo_a 1 +mo_b 2 mo_stage (Rostrum 640 480) mo_log (list))
(defun mo-card (x kinds meets)
	(defq f (Figure))
	(. f :set_outline (list (list :t (path 0.0 0.0 10.0 0.0 10.0 10.0 0.0 10.0))))
	(def f :fill 0xffffffff :x x :kinds kinds :meets meets)
	(. mo_stage :add_node f)
	f)
(defq mo_1 (mo-card 0.0 +mo_a +mo_b) mo_2 (mo-card 100.0 +mo_b 0))
(def mo_1 :on_enter (lambda (a b) (push mo_log :in)) :on_stay (lambda (a b) (push mo_log :on))
	:on_leave (lambda (a b) (push mo_log :out)))
(. mo_stage :tick 0.0)
(assert-eq "apart nothing is fired" '() mo_log)
(def mo_2 :x 5.0)
(. mo_stage :tick 0.0)
(assert-eq "they meet" '(:in) mo_log)
(. mo_stage :tick 0.0)
(assert-eq "they still do" '(:in :on) mo_log)
(def mo_2 :x 100.0)
(. mo_stage :tick 0.0)
(assert-eq "they part" '(:in :on :out) mo_log)

;kinds it does not look out for are not met
(defq mo_log (list) mo_3 (mo-card 0.0 +mo_a 0))
(. mo_stage :tick 0.0)
(assert-eq "mo_1 does not look out for its own kind" '() mo_log)
(. mo_3 :kill)

;boxes that overlap, shapes that do not. A triangle in each far corner
(defq mo_stage (Rostrum 640 480) mo_log (list) mo_t1 (Figure) mo_t2 (Figure))
(. mo_t1 :set_outline (list (list :t (path 0.0 0.0 100.0 0.0 0.0 100.0))))
(. mo_t2 :set_outline (list (list :t (path 100.0 100.0 100.0 10.0 10.0 100.0))))
(def mo_t1 :fill 0xffffffff :kinds 1 :meets 1 :on_enter (lambda (a b) (push mo_log :in)))
(def mo_t2 :fill 0xffffffff :kinds 1 :meets 1)
(. mo_stage :add_node mo_t1)
(. mo_stage :add_node mo_t2)
(. mo_stage :tick 0.0)
(assert-eq "their boxes meet but they do not" '() mo_log)
(def mo_t2 :x -30.0 :y -30.0)
(. mo_stage :tick 0.0)
(assert-eq "moved over, they do" '(:in) mo_log)

;a group meets by what is under it
(defq mo_stage (Rostrum 640 480) mo_log (list) mo_g (Figure) mo_k (Figure) mo_o (Figure))
(. mo_k :set_outline (list (list :t (path 0.0 0.0 10.0 0.0 10.0 10.0 0.0 10.0))))
(def mo_k :fill 0xffffffff :x 50.0)
(. mo_g :add_node mo_k)
(def mo_g :kinds 1 :meets 2 :on_enter (lambda (a b) (push mo_log :in)))
(. mo_o :set_outline (list (list :t (path 0.0 0.0 10.0 0.0 10.0 10.0 0.0 10.0))))
(def mo_o :fill 0xffffffff :x 55.0 :kinds 2)
(. mo_stage :add_node mo_g)
(. mo_stage :add_node mo_o)
(. mo_stage :tick 0.0)
(assert-eq "a group has what is under it" '(:in) mo_log)

;a collision adds a figure and kills one, a death hook adds another. The
;count a tick after
(defq mo_stage (Rostrum 640 480) mo_a1 (Figure) mo_b1 (Figure) mo_died 0)
(. mo_a1 :set_outline (list (list :t (path 0.0 0.0 10.0 0.0 10.0 10.0 0.0 10.0))))
(. mo_b1 :set_outline (list (list :t (path 0.0 0.0 10.0 0.0 10.0 10.0 0.0 10.0))))
(def mo_a1 :fill 0xffffffff :kinds 1 :meets 2
	:on_enter (lambda (a b) (. b :kill) (. (. a :root) :add_node (Figure "spark")))
	:on_death (lambda (a) (setq mo_died (inc mo_died))))
(def mo_b1 :fill 0xffffffff :kinds 2 :on_death (lambda (a) (setq mo_died (inc mo_died))))
(. mo_stage :add_node mo_a1)
(. mo_stage :add_node mo_b1)
(. mo_stage :tick 0.0)
(. mo_stage :tick 0.0)
(assert-eq "the one killed has gone" 2 (length (. mo_stage :children)))
(assert-eq "and its death was fired" 1 mo_died)
(assert-eq "and the one made is there" 1 (length (. mo_stage :find_nodes "spark")))

;moved to a group made that moment, in its own turn, and it is not lost.
;What was under the stage is now under the group, which is placed
(defq mo_stage (Rostrum 640 480) mo_f (Figure "moved") mo_ran 0)
(. mo_stage :add_node mo_f)
(def mo_f :x 5.0)
(. mo_f :add (Wait 0.0 (lambda (f c)
	(defq group (Figure "group"))
	(def group :x 100.0)
	(. (. f :root) :add_node group)
	(. group :add_node f)
	(setq mo_ran (inc mo_ran)))))
(. mo_stage :tick 0.1)
(assert-eq "what moved it ran" 1 mo_ran)
(assert-true "it has its place under the group at once" (mo-near (elem-get (get :world mo_f) 3) 105.0))
(. mo_stage :tick 0.1)
(assert-true "and has it the next tick" (mo-near (elem-get (get :world mo_f) 3) 105.0))
(assert-eq "it is the one child of the one group" 1 (length (. mo_stage :children)))

;moved to a group yet to come this tick, it is not done twice
(defq mo_stage (Rostrum 640 480) mo_f (Figure) mo_late (Figure) mo_turns 0)
(. mo_stage :add_node mo_f)
(. mo_stage :add_node mo_late)
(. mo_f :add (Custom (lambda (f dt)
	(setq mo_turns (inc mo_turns))
	(if (= mo_turns 1) (. mo_late :add_node f))
	:t)))
(. mo_stage :tick 0.1)
(assert-eq "one turn a tick" 1 mo_turns)
(. mo_stage :tick 0.1)
(assert-eq "and one the next" 2 mo_turns)

;the same pairs as asking every pair, for a good many that move
(defq mo_stage (Rostrum 640 480) mo_many (list) mo_seen 0)
(each (lambda (i)
		(defq f (Figure))
		(. f :set_outline (list (list :t (path 0.0 0.0 20.0 0.0 20.0 20.0 0.0 20.0))))
		(def f :fill 0xffffffff :kinds 1 :meets 1 :x (n2f (random 600)) :y (n2f (random 440)))
		(def f :on_enter (lambda (a b) (setq mo_seen (inc mo_seen))))
		(push mo_many f)
		(. mo_stage :add_node f))
	(range 0 200))
(. mo_stage :tick 0.0)
(defq mo_all 0)
(each! (lambda (a)
		(each! (lambda (b)
				(defq ax (get :x a) ay (get :y a) bx (get :x b) by (get :y b))
				(and (< (abs (- ax bx)) 20.0) (< (abs (- ay by)) 20.0) (setq mo_all (inc mo_all))))
			(list mo_many) (inc (!))))
	(list mo_many))
;the edges touching count in the one and not the other, none of these are
;that close but a pair at exactly 20 apart would differ
(assert-true "the sweep finds every pair, each fires both" (mo-near (n2f mo_seen) (n2f (* 2 mo_all)) 4.0))

;a window that keeps a view's shape, 4 to 3, with 10 by 40 of its own. The
;right side held, the top stays and the bottom goes
(import "gui/lisp.inc")
(assert-eq "the right side says how big" (str '(100 100 510 440))
	(str (window-aspect 100 100 510 300 4 10 40 640 480 100 100)))
(assert-eq "the left side held, the right stays" (str '(0 100 410 440))
	(str (window-aspect 0 100 410 300 1 10 40 640 480 100 100)))
(assert-eq "the bottom held and no side" (str '(100 100 323 300))
	(str (window-aspect 100 100 400 300 8 10 40 640 480 100 100)))
(assert-eq "the top held, the bottom stays" (str '(100 340 456 640))
	(str (window-aspect 100 340 300 640 2 10 40 640 480 100 100)))
(assert-eq "a corner, the side says" (str '(50 50 460 390))
	(str (window-aspect 50 200 460 390 3 10 40 640 480 100 100)))
(assert-eq "never smaller than it can be" (str '(100 100 363 329))
	(str (window-aspect 100 100 120 120 12 10 40 640 480 100 230)))

;the demo's story, all of it, in simulated time, on a canvas no one sees. Any
;fault in it stops the test. It is over when it says to begin again, and
;nothing it was doing is left over the end
(import "gui/canvas/lisp.inc")
(defq stage (Rostrum 640 480 700.0) restart :nil mo_time 0.0 mo_most 0 mo_ticks 0)
(import "apps/demos/motion/scene.inc")
(import "apps/demos/motion/story.inc")
(defq mo_canvas (. (Canvas 320 240 1) :set_canvas_flags +canvas_flag_antialias) mo_view (fixeds 0.5 0.0 0.0 0.0 0.5 0.0))
(story-new)
(while (and (not restart) (< mo_time 400.0))
	(. stage :tick 0.033)
	(setq mo_time (+ mo_time 0.033) mo_most (max mo_most (length (. stage :children))))
	;a frame in ten is drawn, all of it on a Pi is over a minute
	(if (= 0 (% (setq mo_ticks (inc mo_ticks)) 10)) (. stage :draw mo_canvas mo_view))
	(task-slice))
(assert-true "the story comes to its end" restart)
(assert-true "in a minute or two" (and (> mo_time 60.0) (< mo_time 180.0)))
(print "the story took " (n2i mo_time) "s of its own time, at most " mo_most " figures at the top")

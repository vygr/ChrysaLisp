(report-header "Motion app: min and max step the picture a size down or up")

;from the size it is, the next of the steps of its shape, in quarters of its
;width: 160 is 640 by 480, 200 is 800 by 600, 256 is 1024 by 768. The most
;is what the screen has room for, the least 640 by 480. A size between steps,
;as a window sized by hand is, goes to the next step that way
(defq mp_out (split (test-output (cat
		"(catch (import {apps/demos/motion/app.lisp}) :t)"
		" (print (list (picture-next 160 400 1) (picture-next 200 400 1) (picture-next 210 400 1) (picture-next 320 400 1) (picture-next 400 400 1)))"
		" (print (list (picture-next 400 400 -1) (picture-next 210 400 -1) (picture-next 200 400 -1) (picture-next 160 400 -1) (picture-next 100 400 -1)))"
		" (print (list (picture-next 160 300 1) (picture-next 256 300 1) (picture-next 300 300 1) (picture-next 400 300 -1)))")) "\n"))
(assert-eq "up: a step at a time, from between steps to the next, and no further than the most" "(200 256 256 400 400)" (first mp_out))
(assert-eq "down: a step at a time, and no less than 640 by 480" "(320 200 160 160 160)" (second mp_out))
(assert-eq "the most is what the screen has room for, not a step of its own" "(200 300 300 300)" (third mp_out))

;what it was showing when closed is what it opens on: the scene, its seed, the
;step it was at, playing or not, where the window was and the picture's size.
;Into a file of the test's own, not the user's
(defq mp_cfg (split (test-output (cat
		"(catch (import {apps/demos/motion/app.lisp}) :t)"
		" (setq *config_file* {tests/scratch/test_motion_app.tre}) (config-load)"
		" (. *scene_toolbar* :set_selected 2) (scene-new 12345) (setq playing :nil target 300)"
		" (. *window* :change 40 50 700 600) (. *picture* :change 0 0 800 600)"
		" (config-save) (setq *config* :nil) (config-load)"
		" (print (gather *config* :scene :seed :step :playing :x :y :width :height))"
		" (pii-remove *config_file*)")) "\n"))
(assert-eq "saved and loaded, it is as it was" "(2 12345 300 :nil 40 50 800 600)" (first mp_cfg))

;the scene is its child's, ./child.lisp, and the child's is a cache: a seek
;forward from where it is, and one back from an I-frame, give the scene that
;plain steps from its seed give, every figure where it is
(defq mc_out (split (test-output (cat
		"(catch (import {apps/demos/motion/child.lisp}) :t)"
		" (defun mc-sum () (defq s 0) (. stage :each_node (lambda (n)"
		" (setq s (+ s (n2i (* 100.0 (ifn (get :fig_x n) 0.0))) (n2i (* 100.0 (ifn (get :fig_y n) 0.0)))))))"
		" (list (get :ros_ticks stage) s (length (. stage :children))))"
		" (defq mbox (mail-mbox))"
		" (scene-have 2 12345 :t) (seek-to 900 mbox) (defq ahead (mc-sum) copies (length iframes))"
		" (seek-to 450 mbox) (defq back (mc-sum))"
		" (scene-make 2 12345) (times 450 (. stage :tick +ros_step)) (defq plain_back (mc-sum))"
		" (times 450 (. stage :tick +ros_step))"
		" (print ahead) (print (mc-sum)) (print back) (print plain_back) (print copies)")) "\n"))
(assert-eq "a seek forward is the scene its steps make" (second mc_out) (first mc_out))
(assert-eq "and one back, from an I-frame" (elem-get mc_out 3) (third mc_out))
(assert-true "the seek forward kept I-frames" (> (str-to-num (elem-get mc_out 4)) 1))

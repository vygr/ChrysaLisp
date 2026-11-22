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

;how long each scene was last found to be is kept with the config, what the
;bar of the next scan of it is of: a config kept before there was one has
;about what each was, and the bar shows the steps run of that, no more than all
(defq mp_len (split (test-output (cat
		"(catch (import {apps/demos/motion/app.lisp}) :t)"
		" (setq *config_file* {tests/scratch/test_motion_len.tre}) (config-load)"
		" (print lengths)"
		" (elem-set lengths 2 777) (config-save) (setq *config* :nil lengths :nil) (config-load)"
		" (print lengths)"
		" (setq scene 2) (scan-show 300) (print (list (get :value *scan*) (get :maximum *scan*)))"
		" (scan-show 5000) (print (list (get :value *scan*) (get :maximum *scan*)))"
		" (pii-remove *config_file*)")) "\n"))
(assert-eq "no length kept yet, about what each scene was" "(6824 0 10566)" (first mp_len))
(assert-eq "a length found is kept" "(6824 0 777)" (second mp_len))
(assert-eq "the scan's bar, the steps run of the length last found" "(300 777)" (third mp_len))
(assert-eq "and no more than all of it" "(777 777)" (elem-get mp_len 3))

;a scout's I-frames, sent to the child that draws: the scene written at
;two steps, mailed the later first. Mail comes in any order, and the later
;may speak of outlines the earlier brought, so it is sought from only when
;the earlier is here. A seek from it is the scene plain steps give
(defq ms_out (split (test-output (cat
		"(catch (import {apps/demos/motion/child.lisp}) :t)"
		" (defun mc-sum () (defq s 0) (. stage :each_node (lambda (n)"
		" (setq s (+ s (n2i (* 100.0 (ifn (get :fig_x n) 0.0))) (n2i (* 100.0 (ifn (get :fig_y n) 0.0)))))))"
		" (list (get :ros_ticks stage) s (length (. stage :children))))"
		" (defq mbox (mail-mbox) got (mail-mbox) sent (iframe-sent) msgs (list))"
		" (scene-make 2 12345)"
		" (each (lambda (which) (times 300 (. stage :tick +ros_step))"
		" (scout-send got 2 12345 which sent) (push msgs (mail-read got))) '(0 1))"
		" (times 50 (. stage :tick +ros_step)) (defq plain (mc-sum))"
		" (setq have :nil) (scene-have 2 12345 :t)"
		" (mail-send ibox (second msgs)) (sent-take) (print sent_whole)"
		" (mail-send ibox (first msgs)) (sent-take) (print sent_whole)"
		" (mail-send ibox (cat (char 0 8) (char 999 8) (char 2 8) (char 900 8))) (sent-take) (print sent_whole)"
		" (seek-to 650 mbox) (print (mc-sum)) (print plain)"
		" (print (list (length iframes) (first (first iframes))))")) "\n"))
(assert-eq "the later I-frame alone is not sought from" "0" (first ms_out))
(assert-eq "with the earlier here both are" "2" (second ms_out))
(assert-eq "one of another scene is left" "2" (third ms_out))
(assert-eq "a seek from a scout's I-frame is the scene its steps make" (elem-get ms_out 4) (elem-get ms_out 3))
;it went on from step 600: the one copy of its own it kept on the way is of 601
(assert-eq "and was from it, not from the scene's start" "(1 601)" (elem-get ms_out 5))

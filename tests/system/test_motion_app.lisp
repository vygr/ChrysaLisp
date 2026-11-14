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

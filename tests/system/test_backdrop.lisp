(report-header "Backdrop: where the lines of a ruling fall")

(import "gui/lisp.inc")

;as it always was: a grid about the middle, lines from the top
(assert-eq "a grid of 32 about the middle of 100" "(18 50 82)" (str (backdrop-lines 50 32 100)))
(assert-eq "lines of 32 from the top of 100" "(0 32 64 96)" (str (backdrop-lines 0 32 100)))
;moved about
(assert-eq "an origin off the left" "(22 54 86)" (str (backdrop-lines -10 32 100)))
(assert-eq "and off the right" "(8 40 72)" (str (backdrop-lines 200 32 100)))
(assert-eq "an origin of 0 has a line at 0" 0 (first (backdrop-lines 0 10 50)))
;parts of a pixel: a line is on the pixel nearest where it falls, and they do not drift
(assert-eq "a gap of 12.5" "(0 13 25 38 50 63 75 88)" (str (backdrop-lines 0.0 12.5 100)))
(assert-eq "an origin of half a pixel" "(1 11 21)" (str (backdrop-lines 0.5 10.0 30)))
;too fine to see
(assert-eq "a ruling less than 2 apart is none" 0 (length (backdrop-lines 0 1.5 100)))
(assert-eq "nothing across nothing" 0 (length (backdrop-lines 5 10 0)))
;a backdrop is made, and takes what it is given
(defq bd (Backdrop))
(def bd :style :grid :origin_x 12.5 :origin_y 3 :rules (list (list 40.0 0xff000000) (list 20.0 0x80000000)) :turned :t)
(assert-eq "its rulings are kept" 2 (length (get :rules bd)))

(report-header "Reader: a minus sign, as a symbol, the start of one, and of a number")

(defun rm-read (text)
	;the first form the reader gives for the text
	(first (read (string-stream text) (ascii-code " "))))

(assert-eq "a minus alone is the symbol" :t (eql (rm-read "-") (sym "-")))
(assert-eq "a minus then a space" "(- ax 24)" (str (rm-read "(- ax 24)")))
(assert-eq "a minus then a close bracket" "(-)" (str (rm-read "(-)")))
(assert-eq "a minus then an open bracket" "(- (a) b)" (str (rm-read "(-(a) b)")))
(assert-eq "two is the one symbol" :t (eql (rm-read "--") (sym "--")))
(assert-eq "and in a form" "(-- i)" (str (rm-read "(-- i)")))
(assert-eq "a longer symbol" :t (eql (rm-read "-+-") (sym "-+-")))
(assert-eq "a minus at the end of the text" "(a -)" (str (rm-read "(a -)")))
(assert-eq "a negative number" -12 (rm-read "-12"))
(assert-eq "a negative fixed point number" -0.5 (rm-read "-0.5"))
(assert-eq "a subtraction still works" 3 (eval (rm-read "(- 5 2)")))

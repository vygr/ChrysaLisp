(import "lib/calc/core.inc")

(print "=== Core Calculator Tests ===")
(print "")

; Test arithmetic
(print "Arithmetic Tests:")
(print "  5 + 3 = " (calc-add 5 3))
(print "  10 - 4 = " (calc-sub 10 4))
(print "  7 * 6 = " (calc-mul 7 6))
(print "  20 / 4 = " (calc-div 20 4))
(print "  17 % 5 = " (calc-mod 17 5))
(print "  10 / 0 = " (calc-div 10 0) " (should be :error)")
(print "")

; Test bitwise
(print "Bitwise Tests:")
(print "  12 AND 10 = " (calc-and 12 10) " (binary: 1100 AND 1010 = 1000 = 8)")
(print "  12 OR 10 = " (calc-or 12 10) " (binary: 1100 OR 1010 = 1110 = 14)")
(print "  12 XOR 10 = " (calc-xor 12 10) " (binary: 1100 XOR 1010 = 0110 = 6)")
(print "  NOT 5 = " (calc-not 5))
(print "  8 << 2 = " (calc-shl 8 2) " (shift left)")
(print "  32 >> 2 = " (calc-shr 32 2) " (shift right)")
(print "")

; Test base conversion
(print "Base Conversion Tests:")
(print "  255 in hex = " (calc-to-base 255 16))
(print "  255 in binary = " (calc-to-base 255 2))
(print "  255 in octal = " (calc-to-base 255 8))
(print "  'FF' from hex = " (calc-from-base "FF" 16))
(print "  '1010' from binary = " (calc-from-base "1010" 2))
(print "  '377' from octal = " (calc-from-base "377" 8))
(print "")

; Test simple evaluation
(print "Simple Evaluation Tests:")
(print "  calc-simple-eval 15 + 7 = " (calc-simple-eval 15 "+" 7))
(print "  calc-simple-eval 100 - 58 = " (calc-simple-eval 100 "-" 58))
(print "  calc-simple-eval 9 * 8 = " (calc-simple-eval 9 "*" 8))
(print "  calc-simple-eval 64 AND 127 = " (calc-simple-eval 64 "AND" 127))
(print "")

; Test RPN evaluation
(print "RPN Evaluation Tests:")
(print "  RPN: 3 4 + = " (calc-rpn-eval '(3 4 "+") 10) " (should be 7)")
(print "  RPN: 10 5 - 2 * = " (calc-rpn-eval '(10 5 "-" 2 "*") 10) " (should be 10)")
(print "  RPN: 15 7 1 1 + - / 3 * 2 1 1 + + - = "
	(calc-rpn-eval '(15 7 1 1 "+" "-" "/" 3 "*" 2 1 1 "+" "+" "-") 10)
	" (should be 5)")
(print "  RPN: FF A0 XOR (hex) = "
	(calc-to-base (calc-rpn-eval '("FF" "A0" "XOR") 16) 16))
(print "")

(print "=== All Calculator Core Tests Complete ===")

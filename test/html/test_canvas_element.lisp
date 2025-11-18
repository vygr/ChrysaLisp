#!/usr/bin/env lsp

;; Canvas Element Tests
;; TDD approach - tests first!

(import "lib/test/unittest.inc")
(import "lib/html/parser.inc")
(import "lib/html/script.inc")

(deftest-suite "Canvas Element Tests")

; Test 1: Parse canvas tag
(deftest "Parse Canvas Tag"
	(defq html "<canvas id=\"myCanvas\" width=\"300\" height=\"200\"></canvas>")
	(defq doc (parse-html html))
	(defq canvases (. doc :get-elements-by-tag-name "canvas"))

	(assert-eq 1 (length canvases))
	(defq canvas (elem 0 canvases))
	(assert-eq "myCanvas" (. canvas :get-attribute "id"))
	(assert-eq "300" (. canvas :get-attribute "width"))
	(assert-eq "200" (. canvas :get-attribute "height")))

; Test 2: Default canvas dimensions
(deftest "Default Canvas Dimensions"
	(defq html "<canvas></canvas>")
	(defq doc (parse-html html))
	(defq canvases (. doc :get-elements-by-tag-name "canvas"))

	(assert-eq 1 (length canvases)))

; Test 3: Get 2D context from script
(deftest "Get 2D Context From Script"
	(defq html "
		<canvas id=\"c\" width=\"100\" height=\"100\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(defq has-ctx (if ctx :t :nil))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq :t (. script-ctx :get-global "has-ctx")))

; Test 4: fillRect drawing
(deftest "FillRect Drawing"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :fillRect 10 10 50 50)
			(defq success :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq :t (. script-ctx :get-global "success")))

; Test 5: strokeRect drawing
(deftest "StrokeRect Drawing"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :strokeRect 20 20 60 60)
			(defq success :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq :t (. script-ctx :get-global "success")))

; Test 6: clearRect drawing
(deftest "ClearRect Drawing"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :fillRect 0 0 200 200)
			(. ctx :clearRect 50 50 100 100)
			(defq success :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq :t (. script-ctx :get-global "success")))

; Test 7: Set fillStyle color
(deftest "Set FillStyle Color"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :set-fill-style \"#FF0000\")
			(. ctx :fillRect 10 10 50 50)
			(defq style (. ctx :get-fill-style))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq "#FF0000" (. script-ctx :get-global "style")))

; Test 8: Set strokeStyle color
(deftest "Set StrokeStyle Color"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :set-stroke-style \"#00FF00\")
			(defq style (. ctx :get-stroke-style))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq "#00FF00" (. script-ctx :get-global "style")))

; Test 9: Set lineWidth
(deftest "Set LineWidth"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :set-line-width 5)
			(defq width (. ctx :get-line-width))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq 5 (. script-ctx :get-global "width")))

; Test 10: Path drawing with beginPath, moveTo, lineTo, stroke
(deftest "Path Drawing"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :beginPath)
			(. ctx :moveTo 10 10)
			(. ctx :lineTo 100 10)
			(. ctx :lineTo 100 100)
			(. ctx :stroke)
			(defq success :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq :t (. script-ctx :get-global "success")))

; Test 11: Path with closePath
(deftest "Path With ClosePath"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :beginPath)
			(. ctx :moveTo 50 50)
			(. ctx :lineTo 100 50)
			(. ctx :lineTo 100 100)
			(. ctx :closePath)
			(. ctx :fill)
			(defq success :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq :t (. script-ctx :get-global "success")))

; Test 12: Draw circle with arc
(deftest "Draw Circle With Arc"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :beginPath)
			(. ctx :arc 100 100 50 0 6.28)  ; 2*PI
			(. ctx :stroke)
			(defq success :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq :t (. script-ctx :get-global "success")))

; Test 13: fillText
(deftest "FillText Drawing"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :fillText \"Hello Canvas\" 10 50)
			(defq success :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq :t (. script-ctx :get-global "success")))

; Test 14: strokeText
(deftest "StrokeText Drawing"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :strokeText \"Outlined Text\" 10 50)
			(defq success :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq :t (. script-ctx :get-global "success")))

; Test 15: Set font
(deftest "Set Font"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :set-font \"24px Arial\")
			(defq font (. ctx :get-font))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq "24px Arial" (. script-ctx :get-global "font")))

; Test 16: Save and restore state
(deftest "Save And Restore State"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :set-fill-style \"#FF0000\")
			(. ctx :save)
			(. ctx :set-fill-style \"#00FF00\")
			(defq green (. ctx :get-fill-style))
			(. ctx :restore)
			(defq red (. ctx :get-fill-style))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq "#00FF00" (. script-ctx :get-global "green"))
	(assert-eq "#FF0000" (. script-ctx :get-global "red")))

; Test 17: Translate transform
(deftest "Translate Transform"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :translate 50 50)
			(. ctx :fillRect 0 0 20 20)
			(defq success :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq :t (. script-ctx :get-global "success")))

; Test 18: Rotate transform
(deftest "Rotate Transform"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :translate 100 100)
			(. ctx :rotate 0.785)  ; 45 degrees
			(. ctx :fillRect -25 -25 50 50)
			(defq success :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq :t (. script-ctx :get-global "success")))

; Test 19: Scale transform
(deftest "Scale Transform"
	(defq html "
		<canvas id=\"c\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas (. document :get-element-by-id \"c\"))
			(defq ctx (. canvas :get-context \"2d\"))
			(. ctx :scale 2 2)
			(. ctx :fillRect 10 10 20 20)
			(defq success :t)
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq :t (. script-ctx :get-global "success")))

; Test 20: Multiple canvas elements
(deftest "Multiple Canvas Elements"
	(defq html "
		<canvas id=\"c1\" width=\"100\" height=\"100\"></canvas>
		<canvas id=\"c2\" width=\"200\" height=\"200\"></canvas>
		<script>
			(defq canvas1 (. document :get-element-by-id \"c1\"))
			(defq canvas2 (. document :get-element-by-id \"c2\"))
			(defq ctx1 (. canvas1 :get-context \"2d\"))
			(defq ctx2 (. canvas2 :get-context \"2d\"))
			(. ctx1 :set-fill-style \"#FF0000\")
			(. ctx2 :set-fill-style \"#0000FF\")
			(defq color1 (. ctx1 :get-fill-style))
			(defq color2 (. ctx2 :get-fill-style))
		</script>")

	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq script-ctx (. executor :get-context))

	(assert-eq "#FF0000" (. script-ctx :get-global "color1"))
	(assert-eq "#0000FF" (. script-ctx :get-global "color2")))


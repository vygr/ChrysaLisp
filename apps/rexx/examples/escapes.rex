/* escapes.rex - Demonstrate escape sequences (modern enhancement) */

SAY "=== REXX Escape Sequences Demo ==="
SAY ""

SAY "Unlike original REXX, this implementation supports escape sequences!"
SAY ""

SAY "1. Newline with \\n:"
SAY "Line 1\nLine 2\nLine 3"
SAY ""

SAY "2. Tab with \\t:"
SAY "Name:\tJohn\nAge:\t25\nCity:\tNew York"
SAY ""

SAY "3. Multiple lines in one SAY:"
SAY "First line\nSecond line\nThird line"
SAY ""

SAY "4. Combining escape sequences:"
msg = "Header\n------\nItem 1:\tValue A\nItem 2:\tValue B\nItem 3:\tValue C"
SAY msg
SAY ""

SAY "5. Backslash and quotes:"
SAY "Path: C:\\Users\\John\\Documents"
SAY "Quote: \"Hello, World!\""
SAY ""

SAY "6. Building formatted output:"
title = "REXX Report"
line = "============"
SAY title
SAY line
SAY "Name:\tChrysaLisp REXX"
SAY "Version:\t1.0"
SAY "Feature:\tEscape Sequences"
SAY line
SAY ""

SAY "=== Demo Complete ==="

EXIT 0

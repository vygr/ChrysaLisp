/* variables.rex - Variable manipulation and PARSE */

SAY "=== REXX Variables Demo ==="
SAY ""

name = "REXX"
platform = "ChrysaLisp"
year = "2025"

SAY "Language: " name
SAY "Platform: " platform
SAY "Year: " year
SAY ""

fullname = name " on " platform
SAY "Full: " fullname
SAY ""

count = "5"
SAY "Count: " count
SAY "Count length: " LENGTH(count)
SAY ""

text = "one two three four five"
SAY "Parsing: " text
word1 = WORD(text, 1)
word2 = WORD(text, 2)
word3 = WORD(text, 3)
SAY "First: " word1
SAY "Second: " word2
SAY "Third: " word3

EXIT 0

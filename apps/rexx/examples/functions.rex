/* functions.rex - Demonstrate REXX built-in functions */

SAY "=== REXX Built-in Functions Demo ==="
SAY ""

text = "hello world"
SAY "Original: " text
SAY "UPPER: " UPPER(text)
SAY "LENGTH: " LENGTH(text)
SAY ""

phrase = "The quick brown fox"
SAY "Phrase: " phrase
SAY "WORDS: " WORDS(phrase)
SAY "WORD 1: " WORD(phrase, 1)
SAY "WORD 3: " WORD(phrase, 3)
SAY ""

test = "ChrysaLisp"
SAY "String: " test
SAY "SUBSTR(test,1,6): " SUBSTR(test, 1, 6)
SAY "REVERSE: " REVERSE(test)
SAY ""

needle = "Lisp"
haystack = "ChrysaLisp REXX"
SAY "Finding '" needle "' in '" haystack "'"
SAY "POS: " POS(needle, haystack)
SAY ""

SAY "LEFT('test',10,'*'): " LEFT("test", 10, "*")
SAY "RIGHT('test',10,'*'): " RIGHT("test", 10, "*")

EXIT 0

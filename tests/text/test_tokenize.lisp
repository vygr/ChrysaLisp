(import "lib/text/tokenize.inc")
(import "lib/text/syntax.inc")

(report-header "Tokenize: a line of source as its tokens, with no highlighter made")

(bind '(tk_toks tk_states tk_state) (tokenize-line "(defq a 12 k :key) ;after"))
(assert-eq "the tokens of a line, every character of it in one of them"
	"(defq a 12 k :key) ;after" (apply (const cat) tk_toks))
(assert-eq "what each of them is"
	"(:text :symbol :text :symbol :text :number :text :symbol :text :keysym :text :comment)" (str tk_states))
(assert-eq "and it ends in text, with no state given it began in it" ":text" (str tk_state))

;a string in braces that is not closed is the state the line ends in, and
;given with the next line that one begins inside it
(bind '(tk_toks tk_states tk_state) (tokenize-line "(f {one"))
(assert-eq "a string left open is the state that comes back" ":string2" (str tk_state))
(bind '(tk_toks tk_states tk_state) (tokenize-line "two} 3)" tk_state))
(assert-eq "the next line begins in it, and ends it" "(:string2 :text :number :text)" (str tk_states))
(assert-eq "the first token is the rest of the string" "two}" (first tk_toks))
(assert-eq "and the line ends in text" ":text" (str tk_state))
(bind '(tk_toks tk_states tk_state) (tokenize-line "(f \qone"))
(assert-eq "a string in quotes left open too" ":string1" (str tk_state))
(bind '(tk_toks tk_states tk_state) (tokenize-line ""))
(assert-eq "an empty line is no tokens" "(() () :text)" (str (list tk_toks tk_states tk_state)))

;the highlighter reads a line with the same, and keeps the state itself
(defq tk_syntax (Syntax))
(assert-eq "(Syntax) :tokenize gives the tokens and what they are, as it did"
	(str (slice (tokenize-line "(f {one") 0 2)) (str (. tk_syntax :tokenize "(f {one")))
(assert-eq "and keeps the state for the next line" ":string2" (str (. tk_syntax :get_state)))
(assert-eq "which it then begins in"
	(str (slice (tokenize-line "two} 3)" :string2) 0 2)) (str (. tk_syntax :tokenize "two} 3)")))
(assert-eq "and ends" ":text" (str (. tk_syntax :get_state)))

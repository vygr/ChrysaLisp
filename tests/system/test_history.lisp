(report-header "History: the search of a terminal's history, Ctrl-R in the Terminal and the TUI")

(defq +state_filename "tmp_test_history.tre")
(import "apps/system/terminal/state.inc")

(defq *meta_map* (scatter (Emap) :history (list "make" "make docs" "tests -a" "nodes -s ring -n 8" "make it" "echo hi"))
	*history_idx* 6)
(assert-eq "the latest with what is typed in it" "make it" (history-find "mak"))
(assert-eq "and where the history is now" 4 *history_idx*)
(assert-eq "asked again with what it gave, the one before" "make docs" (history-find "make it"))
(assert-eq "and before that" "make" (history-find "make docs"))
(assert-eq "after the oldest, the latest again" "make it" (history-find "make"))
(assert-eq "it is anywhere in a command, not only the start" "nodes -s ring -n 8" (history-find "ring"))
(assert-eq "a line that was changed is a new search" "tests -a" (history-find "tests"))
(assert-eq "one that none has gives none" :nil (history-find "zzz"))
(assert-eq "and leaves the place in the history where it was" 2 (progn (history-find "tests") (history-find "zzz") *history_idx*))
(assert-eq "nothing typed is every command, the latest first" "echo hi" (history-find ""))
(assert-eq "and the one before it" "make it" (history-find "echo hi"))
(scatter *meta_map* :history (list))
(assert-eq "an empty history has nothing to find" :nil (history-find "make"))

;the Terminal app loads this as part of a module, apps/system/terminal/
;actions.inc, and what a module does not send out is gone when it has
;loaded. What the search keeps from one press to the next has to be sent
;out, a function is found without, a variable is not. It was not, and
;the first Ctrl-R in the Terminal app was an error
(defq hs_said (test-output (cat
	"(import {usr/env.inc}) (import {gui/lisp.inc})"
	" (defq +state_filename {tmp_test_history.tre})"
	" (import {apps/system/terminal/widgets.inc}) (import {apps/system/terminal/actions.inc})"
	" (print (if (and (def? (quote *history_find*)) (def? (quote *history_found*))) {kept} {gone})"
	" (if (. *key_map_control* :find (ascii-code {r})) { and bound to a key} { and no key}))")))
(assert-eq "as the Terminal app loads it, what the search keeps is there, and Ctrl-R is a key"
	"kept and bound to a key\n" hs_said)

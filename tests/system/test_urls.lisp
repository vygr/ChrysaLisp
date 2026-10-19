(report-header "Files: a path completed as a textfield does it on tab, of the tree and of the host")

(import "lib/files/urls.inc")

;the first word of a command line is a command, of cmd/
(assert-eq "a command's name" "ts " (url-ext "tes" 3))
;a word after it is a file of the tree
(assert-eq "a file of the tree after a command" "ia/" (url-ext "ls apps/med" 11))
;a word with a / in it is a path, the first word as well: a field of one
;path, the Films app's, had only commands offered
(assert-eq "a path the only word" "ms/" (url-ext "apps/media/fil" 14))
(assert-eq "a file in it" "dle" (url-ext "apps/media/films/data/cra" 25))
;a path from / of the host's, not a folder of the tree with ./ before it
(if (and (defq ur_root (pii-dirlist "/")) (found? ur_root "usr,"))
	(assert-eq "a folder from / of the host's" "r/" (url-ext "/us" 3))
	(test-skip "a folder from / of the host's" "this host has no /usr"))

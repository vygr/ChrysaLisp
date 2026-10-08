;the cache of results of the suite itself, tests/suite.inc
(report-header "System: the cache of test results")

;what a file names. An import beside it, an import by its whole path, a
;file a string has the path of, and a command a string starts with or has
;after a |. A file that is not there is not named, nor is the file itself
(defq sc_names (test-file-scan "tests/system/test_suite_cache.lisp" (join (list
	"(import \q./test_pipe.lisp\q)"
	"(import \qlib/files/files.inc\q)"
	"(import \qlib/no/such/file.inc\q)"
	"(defq a (load \qlib/gpu/shaders/raymarch.shader\q) b {cmd/tests.lisp})"
	"(pipe-run \qsort -r | head -n 2\q print)"
	"(defq me \qtests/system/test_suite_cache.lisp\q)"
	"(defq no_string (+ 1 2) path lib/task/pipe.inc)") (ascii-char 10))))
(assert-list-eq "what a file names"
	'("cmd/head.lisp" "cmd/sort.lisp" "cmd/tests.lisp" "lib/files/files.inc"
		"lib/gpu/shaders/raymarch.shader" "tests/system/test_pipe.lisp")
	(sort (cat sc_names)))

;what is known of a file, when it was changed, a hash, and what it names
(defq sc_info (test-file-info "lib/crypto/poly1305.inc"))
(assert-eq "three things known of a file" 3 (length sc_info))
(assert-eq "when it was changed" (age "lib/crypto/poly1305.inc") (first sc_info))
(assert-eq "a hash of 32 hex digits" 32 (length (second sc_info)))
(assert-true "it has no imports, it names nothing" (empty? (third sc_info)))
(assert-eq "a file that is not Lisp names nothing" 0
	(length (third (test-file-info "lib/gpu/shaders/raymarch.shader"))))
(assert-eq "a file that is not there has an age of 0" 0 (first (test-file-info "lib/no/such/file.inc")))

;what a module stands on is all it names, and they name, sorted
(defq sc_on (test-stands-on (list "tests/crypto/test_aead.lisp")))
(each (# (assert-true (cat "the tests of sealing stand on " %0) (find %0 sc_on)))
	'("tests/crypto/test_aead.lisp" "lib/crypto/aead.inc" "lib/crypto/chacha20.inc"
		"lib/crypto/poly1305.inc" "lib/crypto/sha256.inc"))
(assert-true "and not on the shaders" (not (find "lib/gpu/vp.inc" sc_on)))
(assert-list-eq "sorted" (sort (cat sc_on)) sc_on)

;the key of a module is the same while nothing under it changes, another
;module has another, and so has another base
(defq sc_base (test-base) sc_key (test-key "tests/crypto/test_aead.lisp" sc_base))
(assert-eq "a key of 32 hex digits" 32 (length sc_key))
(assert-eq "the same key again" sc_key (test-key "tests/crypto/test_aead.lisp" sc_base))
(assert-true "another module, another key"
	(not (eql sc_key (test-key "tests/crypto/test_poly1305.lisp" sc_base))))
(assert-true "another base, another key"
	(not (eql sc_key (test-key "tests/crypto/test_aead.lisp" (cat sc_base "x")))))

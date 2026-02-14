; Run with: ./run_tui.sh -e -n 1 -f -s tests/run_all.lisp

(import "lib/task/pipe.inc")
(import "tests/utils.inc")

(defun run-suite ()
	(print "Starting ChrysaLisp Functional Test Suite...")

	; Import separate test modules
	; These are imported into the current environment sequentially
	(import "tests/test_core.lisp")
	(import "tests/test_core_adv.lisp")
	(import "tests/test_math.lisp")
	(import "tests/test_math_adv.lisp")
	(import "tests/test_nums.lisp")
	(import "tests/test_fixeds.lisp")
	(import "tests/test_reals.lisp")
	(import "tests/test_list.lisp")
	(import "tests/test_functional.lisp")
	(import "tests/test_multi_seq.lisp")
	(import "tests/test_str.lisp")
	(import "tests/test_str_adv.lisp")
	(import "tests/test_buffer.lisp")
	(import "tests/test_buffer_new.lisp")
	(import "tests/test_document.lisp")
	(import "tests/test_splice.lisp")
	(import "tests/test_negative_indexing.lisp")
	(import "tests/test_str_utils.lisp")
	(import "tests/test_text.lisp")
	(import "tests/test_predicates.lisp")
	(import "tests/test_objects.lisp")
	(import "tests/test_sets.lisp")
	(import "tests/test_dim.lisp")
	(import "tests/test_struct.lisp")
	(import "tests/test_tree.lisp")
	(import "tests/test_low_level_io.lisp")
	(import "tests/test_system.lisp")
	(import "tests/test_root_utils.lisp")
	(import "tests/test_collections_all.lisp")
	(import "tests/test_regexp_all.lisp")

	(print-summary)

	; Return true if 0 failures
	(= *test_failures* 0))

; Outer safety block
(catch
	(run-suite)
	(progn
		(print "CRITICAL ERROR: Test suite crashed or threw exception.")
		(print "Error object: " _)
		:nil))

; Clean shutdown of the VP node
((ffi "service/gui/lisp_deinit"))

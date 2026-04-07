; Run with: ./run_tui.sh -e -n 1 -f -s tests/run_all.lisp

(import "lib/task/pipe.inc")
(import "./utils.inc")

(defun run-suite ()
	(print "Starting ChrysaLisp Functional Test Suite...")

	; Import separate test modules organized by category

	; Core Language
	(import "tests/core/test_core.lisp")
	(import "tests/core/test_core_adv.lisp")
	(import "tests/core/test_predicates.lisp")
	(import "tests/core/test_objects.lisp")
	(import "tests/core/test_struct.lisp")

	; Math
	(import "tests/math/test_math.lisp")
	(import "tests/math/test_math_adv.lisp")
	(import "tests/math/test_nums.lisp")
	(import "tests/math/test_fixeds.lisp")
	(import "tests/math/test_reals.lisp")

	; Collections
	(import "tests/collections/test_list.lisp")
	(import "tests/collections/test_sets.lisp")
	(import "tests/collections/test_tree.lisp")
	(import "tests/collections/test_collections_all.lisp")

	; Sequences
	(import "tests/sequences/test_functional.lisp")
	(import "tests/sequences/test_multi_seq.lisp")
	(import "tests/sequences/test_splice.lisp")
	(import "tests/sequences/test_negative_indexing.lisp")
	(import "tests/sequences/test_dim.lisp")

	; Text and Strings
	(import "tests/text/test_str.lisp")
	(import "tests/text/test_str_adv.lisp")
	(import "tests/text/test_str_utils.lisp")
	(import "tests/text/test_text.lisp")
	(import "tests/text/test_buffer.lisp")
	(import "tests/text/test_buffer_new.lisp")
	(import "tests/text/test_edit.lisp")
	(import "tests/text/test_document.lisp")
	(import "tests/text/test_regexp_all.lisp")

	; System and Low-level
	(import "tests/system/test_low_level_io.lisp")
	(import "tests/system/test_system.lisp")
	(import "tests/system/test_root_utils.lisp")
	(import "tests/system/test_extra_primitives.lisp")

	(print-summary)

	; Return true if 0 failures
	(= *test_failures* 0))

; Outer safety block
(catch
	(run-suite)
	(progn
		(print "CRITICAL ERROR: Test suite crashed or threw exception.")
		(print "Error object: " _)
		:t))

; Clean shutdown of the VP node
((ffi "service/gui/lisp_deinit"))

; Run with: ./run_tui.sh -e -n 1 -f -s tests/run_all.lisp

(import "lib/task/pipe.inc")
(import "./utils.inc")

(defun run-suite ()
	(print "Starting ChrysaLisp Functional Test Suite...")

	; Import separate test modules organized by category

	; Core Language
	(import "tests/core/test_flow.lisp")
	(import "tests/core/test_binding.lisp")
	(import "tests/core/test_predicates.lisp")
	(import "tests/core/test_objects.lisp")
	(import "tests/core/test_structs.lisp")
	(import "tests/core/test_memory.lisp")

	; Math
	(import "tests/math/test_integers.lisp")
	(import "tests/math/test_nums.lisp")
	(import "tests/math/test_fixeds.lisp")
	(import "tests/math/test_reals.lisp")

	; Collections
	(import "tests/collections/test_list.lisp")
	(import "tests/collections/test_sets.lisp")
	(import "tests/collections/test_maps.lisp")
	(import "tests/collections/test_tree.lisp")
	(import "tests/collections/test_scatter_gather.lisp")
	(import "tests/collections/test_memoize.lisp")

	; Sequences
	(import "tests/sequences/test_seq_ops.lisp")
	(import "tests/sequences/test_iteration.lisp")
	(import "tests/sequences/test_sorting.lisp")
	(import "tests/sequences/test_multi_seq.lisp")
	(import "tests/sequences/test_splice.lisp")
	(import "tests/sequences/test_negative_indexing.lisp")
	(import "tests/sequences/test_dim.lisp")

	; Text and Strings
	(import "tests/text/test_strings.lisp")
	(import "tests/text/test_string_utils.lisp")
	(import "tests/text/test_charclass.lisp")
	(import "tests/text/test_search.lisp")
	(import "tests/text/test_buffer.lisp")
	(import "tests/text/test_edit.lisp")
	(import "tests/text/test_document.lisp")
	(import "tests/text/test_regexp.lisp")
	(import "tests/text/test_ring.lisp")

	; Streams
	(import "tests/streams/test_rle.lisp")
	(import "tests/streams/test_huffman.lisp")
	(import "tests/streams/test_bits.lisp")
	(import "tests/streams/test_io.lisp")
	(import "tests/streams/test_lz4.lisp")
	(import "tests/streams/test_eof.lisp")

	; System and Low-level
	(import "tests/system/test_system.lisp")
	(import "tests/system/test_fs_paths.lisp")
	(import "tests/system/test_mail.lisp")

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

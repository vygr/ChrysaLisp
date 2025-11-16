;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FiveAM Testing Framework - Test Suites
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Test suites allow us to collect multiple tests into a single
;; object and run them all using a single name. Test suites do not
;; affect the way tests are run nor the way the results are handled,
;; they are simply a test organizing group.
;;
;; Copyright (c) 2002-2003, Edward Marco Baringer
;; Ported to ChrysaLisp 2025
;; See test.inc for full license details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global test registry - maps test names to test objects
(defq *test-registry* (Emap))
(defq *current-suite* :nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Object Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-test-case (name description test-fn &optional depends-on suite)
	; (make-test-case name desc fn [depends] [suite]) -> test-case
	; Create a test case hmap
	(defq test (Emap))
	(. test :insert :type :test-case)
	(. test :insert :name name)
	(. test :insert :description description)
	(. test :insert :test-fn test-fn)
	(. test :insert :depends-on depends-on)
	(. test :insert :suite (if suite suite *current-suite*))
	(. test :insert :status :unknown)
	test)

(defun make-test-suite (name &optional description parent-suite)
	; (make-test-suite name [desc] [parent]) -> test-suite
	; Create a test suite hmap
	(defq suite (Emap))
	(. suite :insert :type :test-suite)
	(. suite :insert :name name)
	(. suite :insert :description (if description description ""))
	(. suite :insert :parent parent-suite)
	(. suite :insert :tests (Emap))
	(. suite :insert :status :unknown)
	suite)

(defun get-test (name)
	; (get-test name) -> test | :nil
	; Get a test or suite by name
	(. *test-registry* :find name))

(defun set-test (name test)
	; (set-test name test)
	; Register a test or suite
	(. *test-registry* :insert name test))

(defun rem-test (name)
	; (rem-test name)
	; Remove a test or suite
	(. *test-registry* :erase name))

(defun test-names ()
	; (test-names) -> (name ...)
	; Get all registered test names
	(map (lambda ((k v)) k) (. *test-registry* :each)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suite Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-suite (name &rest opts)
	; (def-suite name [:description "desc"] [:in parent-suite])
	; Define a new test suite named NAME
	(defq description :nil parent :nil)

	;; Parse options
	(each (lambda ((k v))
		(cond
			((eql k :description) (setq description v))
			((eql k :in) (setq parent v))))
		(partition opts 2))

	`(progn
		(set-test ',name (make-test-suite ',name ,description ',parent))
		',name))

(defmacro in-suite (suite-name)
	; (in-suite suite-name)
	; Set the current suite for subsequent test definitions
	`(setq *current-suite* ',suite-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-test (name &rest body-and-opts)
	; (def-test name [:description "desc"] [:suite suite] [:depends-on deps] body ...)
	; Define a new test case
	(defq description :nil suite :nil depends-on :nil body (list))

	;; Parse options and body
	(defq parsing-opts :t idx 0)
	(while (< idx (length body-and-opts))
		(defq item (elem idx body-and-opts))
		(cond
			((and parsing-opts (eql item :description))
				(setq idx (inc idx))
				(setq description (elem idx body-and-opts)))
			((and parsing-opts (eql item :suite))
				(setq idx (inc idx))
				(setq suite (elem idx body-and-opts)))
			((and parsing-opts (eql item :depends-on))
				(setq idx (inc idx))
				(setq depends-on (elem idx body-and-opts)))
			(:t
				(setq parsing-opts :nil)
				(push body item)))
		(setq idx (inc idx)))

	;; If description not provided, use name as string
	(unless description
		(setq description (str name)))

	`(progn
		(set-test ',name
			(make-test-case ',name ,description
				(lambda () ~body)
				',depends-on
				,(if suite suite '*current-suite*)))
		',name))

(defmacro test (name &rest body)
	; (test name body ...)
	; Simplified test definition (sugar for def-test)
	`(def-test ,name ~body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suite Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun suite-tests (suite-name)
	; (suite-tests suite-name) -> (test ...)
	; Get all tests in a suite
	(defq suite (get-test suite-name))
	(if (and suite (eql :test-suite (. suite :find :type)))
		(let ()
			(defq tests (list))
			;; Find all tests that belong to this suite
			(each (lambda ((name test))
				(when (and (eql :test-case (. test :find :type))
					(eql suite-name (. test :find :suite)))
					(push tests (list name test))))
				(. *test-registry* :each))
			tests)
		(list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize default suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-test :nil (make-test-suite :nil "Global Suite" :nil))

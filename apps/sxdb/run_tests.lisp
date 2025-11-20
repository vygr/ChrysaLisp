;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SXDb Test Runner
; Starts service and runs tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Starting SXDb test runner...")
(print)

;start service in background
(print "Starting SXDb service...")
(import "apps/sxdb/app.lisp")

;give service time to start and register
(print "Waiting for service to start...")
(loop 50 (sleep 10))

;verify service is running
(print "Verifying service is running...")
(defq services (mail-enquire "SXDb,"))
(if (> (length services) 0)
	(print "Service is running: " (first services))
	(progn
		(print "ERROR: Service failed to start!")
		(quit 1)))

(print)
(print "Running SXDb test suite...")
(print)

;run tests
(import "apps/sxdb/test.lisp")

;run tests and get exit code
(defq exit_code (main))

(print)
(print "Test run complete.")

;exit with result code
(quit exit_code)

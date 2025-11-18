;; Test script for basic OpenGL demo
(print "=== Testing Basic OpenGL Demo ===")

;; Load the OpenGL demo
(load "apps/opengl_demo/app.lisp")

(print "Basic OpenGL demo loaded successfully!")
(print "Test PASSED")

;; Exit after a brief moment
(task-sleep 100000)
(quit 0)

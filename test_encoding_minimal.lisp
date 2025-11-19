(import "lib/class/class.inc")
(import "lib/html/encoding.inc")

(print "Creating encoding-detector...")
(defq ed (encoding-detector))
(print "Success!")

(print "Calling :get-encoding...")
(defq enc (. ed :get-encoding))
(print "Encoding: " enc)

(print "Setting encoding to iso-8859-1...")
(. ed :set-encoding "iso-8859-1" :http-header)
(defq new-enc (. ed :get-encoding))
(print "New encoding: " new-enc)

(print "Test complete - encoding.inc works!")

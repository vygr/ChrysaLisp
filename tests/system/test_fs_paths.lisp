(report-header "File System & Paths")

; --- age ---
; Should be > 0 for existing file
(assert-true "age" (> (age "README.md") 0))

; --- load-stream ---
(defq ls (load-stream "README.md"))
(assert-true "load-stream" (not (nil? ls)))

; --- path functions ---
(assert-true "path-to-file" (str? (path-to-file)))
(assert-eq "path-to-relative: child" "b" (path-to-relative "tmp/a/b" "tmp/a"))
(assert-eq "path-to-relative: same level" "./b" (path-to-relative "tmp/b" "tmp/a"))
(assert-eq "path-to-relative: up levels" "././c" (path-to-relative "tmp/c" "tmp/a/b"))

(assert-eq "path-to-absolute: simple" "tmp/a/b" (path-to-absolute "./b" "tmp/a/c"))
(assert-eq "path-to-absolute: nested" "tmp/a/b" (path-to-absolute "././b" "tmp/a/b/c"))
(assert-eq "path-to-absolute: dir" "tmp/a" (path-to-absolute "." "tmp/a/c"))
(assert-eq "path-to-absolute: up" "tmp" (path-to-absolute "./." "tmp/a/c"))

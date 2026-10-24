(report-header "Source Service: a task of another system sends a task of its own")
(import "service/source/app.inc")

(open-child "service/source/app.lisp" +kn_call_run)
(until (source-service) (task-sleep 10000))

;a task of another system that sends a task of its own sends it as of
;that system, which has its files, not as of the one it runs on
(defq mbox (mail-mbox))
(open-child (cat "(import {service/source/app.inc})" (ascii-char 10)
	"(source-import {Ulk} {" (hex-encode (source-service)) "})" (ascii-char 10)
	"(mail-send (hex-decode {" (hex-encode mbox) "}) (source-task {apps/system/netmon/child.lisp}))")
	+kn_call_pin)
(defq sl_task (mail-read mbox))
(assert-true "a task it sends names its originator" (found? sl_task "{Ulk}"))

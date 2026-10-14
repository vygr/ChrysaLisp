(report-header "System: PNG read, every kind of pixel and filter, by a pipeline of stages")

(import "gui/canvas/lisp.inc")
(import "lib/crypto/sha256.inc")

(defun tp-pixels (file)
	; (tp-pixels file) -> :nil | sha256 hex of its pixels, blue green red alpha
	(when (defq canvas (canvas-load file +load_flag_noswap))
		(defq out (string-stream (cat "")))
		(pixmap-write (getf canvas +canvas_pixmap 0) out 32)
		(hex-encode (sha256 (str out)))))

(assert-list-eq "the size of one, and 32 bits a pixel" '(13 11 32) (canvas-info "tests/system/data/png_rgba8.png"))
(each (lambda ((name want))
		(assert-eq (cat name ": each pixel what it was made") want (tp-pixels (cat "tests/system/data/png_" name ".png"))))
	'(("rgba8" "99CEDFA1445D53E2F9FB6AACC52B6E6D868F3C2EAE0EBB44E21044740D463AC3")
	("rgb8" "1D8588AC25803C70C13804922E65D865E13C7C98853E38DBDD1C323232DEC445")
	("pal4" "140B390540310F27BCE3C1F2FBB1C9115471A81E4C4FBCB4FD83194FB98302D7")
	("grey1" "5B5F599E83384EB24A05C85058414C4F72D96DD595DD70B33E1842B9E7B9E834")
	("ga16" "096CB9C072B606D142B2972613BF92A4E6C74E135C5642E93F1121CE79CCDE53")))
(assert-eq "not a .png is not taken for one" :nil (PNG-load (string-stream "not a png at all")))

(import "lib/options/options.inc")
(import "lib/files/zip.inc")
(import "lib/hash/sums.inc")

(defq usage `(
(("-h" "--help")
"Usage: unzip [options] archive [name ...]

    options:
        -h --help: this help info.
        -l --list: what is in it, not taken out.
        -d --dir folder: where to put what is taken out, default
            where it is run.

    Take what a .zip holds out of it, each to its name in the
    folder, or only those named, by the name in the zip or the
    last part of it. Each is checked against the sum it was
    made with. A name that would put a file outside the folder,
    one that begins with / or has .. in it, is not taken out.")
(("-l" "--list") ,(opt-flag 'opt_l))
(("-d" "--dir") ,(opt-str 'opt_d))
))

(defun unzip-safe? (name)
	; (unzip-safe? name) -> :t | :nil
	;a name that stays in the folder it is taken out to
	(not (or (empty? name) (starts-with "/" name) (find "\\" name) (find ":" name)
		(some (# (eql %0 "..")) (split name "/")))))

(defun main ()
	(and
		(defq stdio (create-stdio))
		(defq opt_l :nil opt_d "." args (options stdio usage))
		(cond
			((< (length args) 2) (print "unzip: an archive to take out of"))
			((not (defq data (load (second args)))) (print "unzip: no " (second args)))
			((not (defq entries (zip-list data))) (print "unzip: " (second args) " is not a zip"))
			(opt_l
				(each (lambda (entry)
						(print (pad (str (elem-get entry +zip_entry_full)) 10) "  " (elem-get entry +zip_entry_name)))
					entries)
				(print (length entries) " in " (second args)))
			(:t (defq names (slice args 2 -1) count 0
					want (if (empty? names) entries (filter (const identity) (map (# (zip-find entries %0)) names)))
					folder (if (ends-with "/" opt_d) (slice opt_d 0 -2) opt_d))
				(each (lambda (entry)
						(defq name (elem-get entry +zip_entry_name))
						(cond
							((ends-with "/" name))
							((not (unzip-safe? name)) (print "unzip: not taken out, outside the folder: " name))
							(:t (defq ss (string-stream (cat "")))
								(cond
									((not (zip-read data entry ss)) (print "unzip: can not read " name))
									((/= (crc32 (defq bytes (str ss))) (elem-get entry +zip_entry_crc))
										(print "unzip: not as it was made: " name))
									(:t (save bytes (cat folder "/" name))
										(setq count (inc count)))))))
					want)
				(print "unzip: " count " files to " folder)))))
